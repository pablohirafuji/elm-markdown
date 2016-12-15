module CommonMark.List exposing (..)


import Regex exposing (Regex)
import Html exposing (Html, ul, ol, text)
import Html.Attributes exposing (start)



-- Model


type alias Line = ( Model, String )


type alias Model =
    { type_ : Type
    , indentLength : Int
    , delimiter : String
    , isLoose : Maybe Bool
    , hasBlankLineAfter : Bool
    }


initModel : Model
initModel =
    { type_ = Unordered
    , indentLength = 0
    , delimiter = ""
    , isLoose = Nothing
    , hasBlankLineAfter = False
    }


type Type
    = Unordered
    | Ordered Int



-- Regexes


orderedRegex : Regex
orderedRegex =
    Regex.regex "^( {0,3}(\\d{1,9})([.)]))(?: (.*))?$"


unorderedRegex : Regex
unorderedRegex =
    Regex.regex "^( {0,3}([\\*\\-\\+]))(?: (.*))?$"


newLine : Type -> String -> String -> String -> Line
newLine type_ indentString delimiter rawLine =
    ( { initModel
        | type_ = type_
        , indentLength = String.length indentString + 1
        , delimiter = delimiter
      }
    , rawLine
    )


fromOrderedMatch : Regex.Match -> Line
fromOrderedMatch match =
    case match.submatches of
        Just indentString :: Just start :: Just delimiter :: Just rawLine :: _ ->
            case String.toInt start of
                Result.Ok int ->
                    newLine (Ordered int) indentString delimiter rawLine

                Result.Err _ ->
                    newLine (Unordered) indentString delimiter rawLine

        _ ->
            ( initModel, "" )


fromUnorderedMatch : Regex.Match -> Line
fromUnorderedMatch match =
    case match.submatches of
        Just indentString :: Just delimiter :: Just rawLine :: _ ->
            newLine (Unordered) indentString delimiter rawLine

        _ ->
            ( initModel, "" )


fromMatch : Type -> Regex.Match -> Line
fromMatch type_ match =
    case type_ of
        Unordered ->
            fromUnorderedMatch match

        Ordered _ ->
            fromOrderedMatch match


updateModel : Model -> Model -> Model
updateModel lineModel blockModel =
    { blockModel
        | indentLength = lineModel.indentLength
        , hasBlankLineAfter = False
        , isLoose =
            if blockModel.isLoose == Nothing
                then Nothing
                else Just True
    }


blankLineFound : Model -> Model
blankLineFound blockModel =
    { blockModel
        | hasBlankLineAfter = True
        , isLoose =
            if blockModel.isLoose == Just True
                then Just True
                else Just False
    }


isLoose : Model -> Bool
isLoose info =
    info.isLoose == Just True



-- View


view : Model -> List ( Html msg ) -> Html msg
view info =
    case info.type_ of
        Ordered startInt ->
            -- Just to comply with CommonMark tests output
            if startInt == 1
                then ol []
                else ol [ start startInt ]

        Unordered ->
            ul []

