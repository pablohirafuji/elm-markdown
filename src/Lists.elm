module Lists exposing (..)


import Regex exposing (Regex)
import Html exposing (Html, ul, ol, text)
import Html.Attributes exposing (start)



-- Model


type alias Line = ( Info, String )


type alias Info =
    { type_ : Type
    , indentLength : Int
    , delimiter : String
    , isLoose : Maybe Bool
    , hasBlankLineAfter : Bool
    }


initInfo : Info
initInfo =
    { type_ = Unordered
    , indentLength = 0
    , delimiter = ""
    , isLoose = Nothing
    , hasBlankLineAfter = False
    }


type Type
    = Unordered
    | Ordered Int


initUnordered : Line
initUnordered =
    ( initInfo, "" )


initOrdered : Line
initOrdered =
    ( { initInfo | type_ = Ordered 0 }, "" )



-- Regexes


orderedRegex : Regex
orderedRegex =
    Regex.regex "^( {0,3}(\\d{1,9})([.)]))(?: (.*))?$"


unorderedRegex : Regex
unorderedRegex =
    Regex.regex "^( {0,3}([\\*\\-\\+]))(?: (.*))?$"


newLine : Type -> String -> String -> String -> Line
newLine type_ indentString delimiter rawLine =
    ( { initInfo
        | type_ = type_
        , indentLength = String.length indentString + 1
        , delimiter = delimiter
      }
    , rawLine
    )


fromOrderedMatch : Regex.Match -> Maybe Line
fromOrderedMatch match =
    case match.submatches of
        Just indentString :: Just start :: Just delimiter :: Just rawLine :: _ ->
            case String.toInt start of
                Result.Ok int ->
                    newLine (Ordered int) indentString delimiter rawLine
                        |> Just


                Result.Err _ ->
                    newLine (Unordered) indentString delimiter rawLine
                        |> Just

        _ ->
            Nothing


fromUnorderedMatch : Regex.Match -> Maybe Line
fromUnorderedMatch match =
    case match.submatches of
        Just indentString :: Just delimiter :: Just rawLine :: _ ->
            newLine (Unordered) indentString delimiter rawLine
                |> Just

        _ ->
            Nothing


fromMatch : Regex.Match -> Line -> Maybe Line
fromMatch match ( info, _ ) =
    case info.type_ of
        Unordered ->
            fromUnorderedMatch match

        Ordered _ ->
            fromOrderedMatch match


updateInfo : Info -> Info -> Info
updateInfo lineInfo blockInfo =
    { blockInfo
        | indentLength = lineInfo.indentLength
        , isLoose =
            if blockInfo.isLoose == Nothing then
                Nothing

            else
                Just True
        , hasBlankLineAfter = False
    }


blankLineFound : Info -> Info
blankLineFound blockInfo =
    { blockInfo
        | isLoose =
            if blockInfo.isLoose == Just True then
                Just True

            else
                Just False
        , hasBlankLineAfter = True
    }


isLoose : Info -> Bool
isLoose info =
    info.isLoose == Just True



-- View


view : Info -> List ( Html msg ) -> Html msg
view info =
    case info.type_ of
        Ordered startInt ->
            -- Just to comply with CommonMark tests output
            if startInt == 1 then
                ol []

            else
                ol [ start startInt ]

        Unordered ->
            ul []

