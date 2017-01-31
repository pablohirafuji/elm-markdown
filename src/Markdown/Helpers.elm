module Markdown.Helpers exposing (..)


import Dict exposing (Dict)
import Regex exposing (Regex)


type alias References =
    Dict String ( String, Maybe String ) -- Label ( Url, Maybe Title )


type alias Attribute = ( String, Maybe String )


insideSquareBracketRegex : String
insideSquareBracketRegex =
    "[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*"


titleRegex : String
titleRegex =
    "(?:[" ++ whiteSpaceChars ++ "]+"
        ++ "(?:'([^'\\\\]*(?:\\\\.[^'\\\\]*)*)'|"
        ++ "\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|"
        ++ "\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?"


prepareRefLabel : String -> String
prepareRefLabel =
    cleanWhitespaces
        >> String.toLower
        

indentLength : String -> Int
indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.find (Regex.AtMost 1) initSpacesRegex
        >> List.head
        >> Maybe.map (.match >> String.length)
        >> Maybe.withDefault 0


initSpacesRegex : Regex
initSpacesRegex =
    Regex.regex "^ +"


indentLine : Int -> String -> String
indentLine indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.replace
            (Regex.AtMost 1)
            (Regex.regex ("^ {0," ++ toString indentLength ++ "}" ))
            (\_ -> "")



escapableRegex : Regex
escapableRegex =
    Regex.regex "(\\\\+)([!\"#$%&\\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-])"


replaceEscapable : String -> String
replaceEscapable =
    Regex.replace Regex.All escapableRegex
        (\regexMatch ->
            case regexMatch.submatches of
                Just backslashes :: Just escapedStr :: _ ->
                    String.repeat
                        (String.length backslashes // 2) "\\"
                            ++ escapedStr

                _ ->
                    regexMatch.match
        )


returnFirstJust : List (Maybe a) -> Maybe a
returnFirstJust maybes =
    let
        process : Maybe a -> Maybe a -> Maybe a
        process a maybeFound =
            case maybeFound of
                Just found -> Just found
                Nothing -> a

    in
        List.foldl process Nothing maybes


whiteSpaceChars : String
whiteSpaceChars =
    " \\t\\f\\v\\r\\n"


cleanWhitespaces : String -> String
cleanWhitespaces =
    String.trim
        >> Regex.replace Regex.All
            (Regex.regex ("[" ++ whiteSpaceChars ++ "]+"))
            (\_ -> " ")


ifError : ( x -> Result x a) -> Result x a -> Result x a
ifError function result =
    case result of
        Result.Ok _ ->
            result

        Result.Err err ->
            function err

