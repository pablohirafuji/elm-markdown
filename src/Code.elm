module Code exposing (..)



import Regex exposing (Regex)
import Html exposing (Html, pre, code, text)
import Html.Attributes exposing (class)



-- Line


type Line
    = Indented String
    | Fenced IsOpen Fence String


type alias IsOpen = Bool


type alias Fence =
    { indentLength : Int
    , fenceLength : Int
    , fenceChar : String
    , language : String
    }


initCodeFenceState : Fence
initCodeFenceState =
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


closingFenceLineRegex : Regex
closingFenceLineRegex =
    Regex.regex "^ {0,3}(`{3,}|~{3,})[ \\t]*$"



-- Helpers


-- Pattern Matching (Code (Code.Fenced True fence code))
-- Code (Code.continueOrCloseFence fence rawLine)
continueOrCloseFence : Fence -> String -> Line
continueOrCloseFence fence rawLine =
    if isClosingFenceLine fence rawLine then
        Fenced False fence ""

    else
        Fenced True fence rawLine


isClosingFenceLine : Fence -> String -> Bool
isClosingFenceLine fence rawLine =
    Regex.find (Regex.AtMost 1) closingFenceLineRegex rawLine
        |> List.head
        |> Maybe.map
            (\match ->
                case match.submatches of
                    Just fenceStr :: _ ->
                        String.length fenceStr >= fence.fenceLength
                            && String.left 1 fenceStr == fence.fenceChar

                    _ ->
                        False
            )
        |> Maybe.withDefault False


indentLine : Fence -> String -> String
indentLine fence =
    Regex.replace
        (Regex.AtMost 1)
        (Regex.regex ("^( {0," ++ toString fence.indentLength ++ "})"))-- aprenteses necessário?
        (\_ -> "")


matchToFenceLine : Regex.Match -> Maybe Line
matchToFenceLine match =
    case match.submatches of
        Just indent :: Just fence :: Just language :: _ ->
            Fenced True
                { indentLength = String.length indent
                , fenceLength = String.length fence
                , fenceChar = String.left 1 fence
                , language =
                    String.words language
                        |> List.head
                        |> Maybe.withDefault ""
                } "" |> Just

        _ ->
            Nothing


matchToIndentedLine : Regex.Match -> Maybe Line
matchToIndentedLine match =
    case match.submatches of
        Just code :: _ ->
            Just (Indented code)

        _ ->
            Nothing



-- Block


type alias Block = ( Maybe Fence, String )



-- View


view : Block -> Html msg
view ( maybeFence, codeStr ) =
    case maybeFence of
        Just { language } ->
            if String.length language > 0 then
                basicView
                    [ class ("language-" ++ language) ]
                    codeStr

            else
                basicView [] codeStr

        Nothing ->
           basicView [] codeStr


basicView : List (Html.Attribute msg) -> String -> Html msg
basicView attrs codeStr =
    pre []
        [ code attrs
            [ text codeStr ]
        ]

