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
    Regex.regex "^( *(\\d{1,9})([.)])( {0,4}))(?: (.*))?$"


unorderedRegex : Regex
unorderedRegex =
    Regex.regex "^( *([\\*\\-\\+])( {0,4}))(?: (.*))?$"


initSpacesRegex : Regex
initSpacesRegex =
    Regex.regex "^ +"


fromOrderedMatch : Regex.Match -> Line
fromOrderedMatch match =
    case match.submatches of
        Just indentString :: Just start :: Just delimiter :: Just indentSpace :: Just rawLine :: _ ->
            case String.toInt start of
                Result.Ok int ->
                    newLine (Ordered int) indentString delimiter indentSpace rawLine

                Result.Err _ ->
                    newLine (Unordered) indentString delimiter indentSpace rawLine

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


--updateModel : Model -> Model -> Model
--updateModel lineModel blockModel =
--    { blockModel
--        | indentLength = lineModel.indentLength
--        --, hasBlankLineAfter = False
--        --, isLoose =
--        --    if blockModel.isLoose == Nothing
--        --        then Nothing
--        --        else Just True
--    }


--blankLineFound : Model -> Model
--blankLineFound blockModel =
--    { blockModel
--        | hasBlankLineAfter = True
--        , isLoose =
--            if blockModel.isLoose == Just True
--                then Just True
--                else Just False
--    }


--isLoose : Model -> Bool
--isLoose info =
--    info.isLoose == Just True


indentLength : String -> Int
indentLength rawLine =
    Regex.find (Regex.AtMost 1) initSpacesRegex rawLine
        |> List.head
        |> Maybe.map (.match >> String.length)
        |> Maybe.withDefault 0



-- View


view : Model -> List ( Html msg ) -> Html msg
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



                --ListBlock model blocksList :: blocksTail->
                --    -- Verificar se é blankLine?
                --    if Lists.indentLength rawLine >= model.indentLength then
                --        case blocksList of
                --            blocks_ :: blocksListTail ->
                --                let
                --                    updtModel =
                --                        if isBlankBlockLast blocksList then
                --                            { model | isLoose = Just True }
                --                        else
                --                            model
                --                in
                --                    ListBlock updtModel
                --                        ( parseRawLines ( [ String.dropLeft model.indentLength rawLine ], blocks_ )
                --                            :: blocksListTail
                --                        ) :: blocksTail
                --                            |> (,) rawLinesTail
                --                            |> parseRawLines

                --            [] ->
                --                ListBlock model
                --                    [ parseRawLines ( [ String.dropLeft model.indentLength rawLine ], [] )
                --                    ] :: blocksTail
                --                        |> (,) rawLinesTail
                --                        |> parseRawLines

                --    else
                --        -- Pular indentedcode check? Só pode ocorrer dentro da list
                --        parseRawLine rawLine blocks
                --            |> (,) rawLinesTail
                --            |> parseRawLines
