module Code exposing (..)


import Regex exposing (Regex)
import Html exposing (Html, pre, code, text)
import Html.Attributes exposing (class)



-- Model


type Block
    = Indented ( List BlankLine, String )
    | Fenced Fence


type alias BlankLine = String


type alias Fence = ( IsOpen, FenceModel, String )


initFence : Fence
initFence =
    ( True, initFenceModel, "" )


type alias IsOpen = Bool


type alias FenceModel =
    { indentLength : Int
    , fenceLength : Int
    , fenceChar : String
    , language : String
    }


initFenceModel : FenceModel
initFenceModel =
    { indentLength = 0
    , fenceLength = 0
    , fenceChar = ""
    , language = ""
    }



-- Regexes


indentedRegex : Regex
indentedRegex =
    Regex.regex "^ {4,4}(.*)$"


openingFenceRegex : Regex
openingFenceRegex =
    Regex.regex "^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$"


closingFenceRegex : Regex
closingFenceRegex =
    Regex.regex "^ {0,3}(`{3,}|~{3,})[ \\t]*$"


fromIndentedMatch : Regex.Match -> Maybe Block
fromIndentedMatch =
    .submatches
        >> List.head
        >> Maybe.withDefault Nothing
        >> Maybe.map
            (\ code -> Indented ( [], code ) )


fromOpeningFenceMatch : Regex.Match -> Maybe Block
fromOpeningFenceMatch match =
    case match.submatches of
        Just indent :: Just fence :: Just language :: _ ->
            Fenced
                ( True
                ,   { indentLength = String.length indent
                    , fenceLength = String.length fence
                    , fenceChar = String.left 1 fence
                    , language =
                        String.words language
                            |> List.head
                            |> Maybe.withDefault ""
                    }
                , ""
                ) |> Just

        _ ->
            Nothing


-- Helpers


continueOrCloseFence : FenceModel -> String -> String -> Block
continueOrCloseFence fence previousCode rawLine =
    if isClosingFenceLine fence rawLine then
        Fenced ( False, fence, previousCode )

    else
        Fenced
            ( True
            , fence
            , previousCode
                ++ indentLine fence.indentLength rawLine
                ++ "\n"
            )


isClosingFenceLine : FenceModel -> String -> Bool
isClosingFenceLine fence =
    Regex.find (Regex.AtMost 1) closingFenceRegex
        >> List.head
        >> Maybe.map
            (\match ->
                case match.submatches of
                    Just fenceStr :: _ ->
                        String.length fenceStr >= fence.fenceLength
                            && String.left 1 fenceStr
                            == fence.fenceChar

                    _ ->
                        False
            )
        >> Maybe.withDefault False


indentLine : Int -> String -> String
indentLine indentLength =
    Regex.replace
        ( Regex.AtMost 1 )
        ( Regex.regex ( "^ {0," ++ toString indentLength ++ "}" ) )
        (\_ -> "" )


addIndented : ( List String, String ) -> ( List String, String ) -> Block
addIndented ( _, lineCode) ( blockBlankLines, blockCode ) =
    let
        indentBL blankLine = 
            indentLine 4 blankLine ++ "\n"
    in
        Indented
            ( []
            , blockCode
                ++ String.concat
                    ( List.map indentBL blockBlankLines )
                ++ lineCode
                ++ "\n"
            )


-- View


view : Block -> Html msg
view block =
    case block of
        Indented ( _, codeStr ) ->
            basicView [] codeStr

        Fenced ( _, { language }, codeStr ) ->
            if String.length language > 0 then
                basicView
                    [ class ("language-" ++ language) ]
                    codeStr

            else
                basicView [] codeStr


basicView : List (Html.Attribute msg) -> String -> Html msg
basicView attrs codeStr =
    pre []
        [ code attrs
            [ text codeStr ]
        ]

