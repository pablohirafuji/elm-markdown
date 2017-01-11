module CommonMark.List exposing (..)


import Regex exposing (Regex)



-- Model


type alias Line = ( Model, String )


type alias Model =
    { type_ : Type
    , indentLength : Int
    , delimiter : String
    , isLoose : Bool
    }


initModel : Model
initModel =
    { type_ = Unordered
    , indentLength = 2
    , delimiter = "-"
    , isLoose = False
    }


type Type
    = Unordered
    | Ordered Int



-- Regexes


orderedRegex : Regex
orderedRegex =
    Regex.regex "^( *(\\d{1,9})([.)])( {0,4}))(?:[ \\t](.*))?$"


unorderedRegex : Regex
unorderedRegex =
    Regex.regex "^( *([\\*\\-\\+])( {0,4}))(?:[ \\t](.*))?$"


initSpacesRegex : Regex
initSpacesRegex =
    Regex.regex "^ +"


fromOrderedMatch : Regex.Match -> Line
fromOrderedMatch match =
    case match.submatches of
        Just indentString
            :: Just start
            :: Just delimiter
            :: Just indentSpace
            :: maybeRawLine
            :: _ ->
                let type_ =
                    String.toInt start
                        |> Result.map Ordered
                        |> Result.withDefault Unordered

                in
                    newLine
                        type_
                        indentString
                        delimiter
                        indentSpace
                        (Maybe.withDefault "" maybeRawLine)

        _ ->
            ( initModel, "" )


fromUnorderedMatch : Regex.Match -> Line
fromUnorderedMatch match =
    case match.submatches of
        Just indentString
            :: Just delimiter
            :: Just indentSpace
            :: maybeRawLine
            :: [] ->
                newLine
                    (Unordered)
                    indentString
                    delimiter
                    indentSpace
                    (Maybe.withDefault "" maybeRawLine)

        _ ->
            ( initModel, "" )


fromMatch : Type -> Regex.Match -> Line
fromMatch type_ match =
    case type_ of
        Unordered ->
            fromUnorderedMatch match

        Ordered _ ->
            fromOrderedMatch match



-- Helpers


newLine : Type -> String -> String -> String -> String -> Line
newLine type_ indentString delimiter indentSpace rawLine =
    let
        indentSpaceLenth =
            String.length indentSpace

        isIndentedCode =
            indentSpaceLenth >= 4

        indentLength = 
            if isIndentedCode then
                1 + String.length indentString
                    - String.length indentSpace

            else
                1 + String.length indentString

        rawLine_ =
            if isIndentedCode then
                indentSpace ++ rawLine

            else
                rawLine

    in
        ( { initModel
            | type_ = type_
            , delimiter = delimiter
            , indentLength = indentLength
          }
        , rawLine_
        )


indentLength : String -> Int
indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.find (Regex.AtMost 1) initSpacesRegex
        >> List.head
        >> Maybe.map (.match >> String.length)
        >> Maybe.withDefault 0

