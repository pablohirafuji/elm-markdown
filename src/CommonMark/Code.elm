module CommonMark.Code exposing (..)


import Regex exposing (Regex)



-- Model


type Model
    = Indented ( List BlankLine, String )
    | Fenced Fence


type alias BlankLine = String

type alias Fence = ( IsOpen, FenceModel, String )

type alias IsOpen = Bool

type alias FenceModel =
    { indentLength : Int
    , fenceLength : Int
    , fenceChar : String
    , language : String
    }



-- Regexes


indentedRegex : Regex
indentedRegex =
    Regex.regex "^(?: {4,4}| {0,3}\\t)(.*)$"


openingFenceRegex : Regex
openingFenceRegex =
    Regex.regex "^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$"


closingFenceRegex : Regex
closingFenceRegex =
    Regex.regex "^ {0,3}(`{3,}|~{3,})\\s*$"


fromIndentedMatch : Regex.Match -> ( List String, String )
fromIndentedMatch =
    .submatches
        >> List.head
        >> Maybe.withDefault Nothing
        >> Maybe.map ( (,) [] )
        >> Maybe.withDefault ( [], "" )


fromOpeningFenceMatch : Regex.Match -> Fence
fromOpeningFenceMatch match =
    case match.submatches of
        Just indent :: Just fence :: Just language :: _ ->
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
                )

        _ ->
            ( True, FenceModel 0 0 "`" "", "" )



-- Helpers


continueOrCloseFence : FenceModel -> String -> String -> Model
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
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
    >> Regex.replace
        (Regex.AtMost 1)
        (Regex.regex
            ("^ {0," ++ toString indentLength ++ "}" ))
        (\_ -> "")


addIndented : ( List String, String ) -> ( List String, String ) -> Model
addIndented ( _, lineCode ) ( blockBlankLines, blockCode ) =
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


addBlankLineToIndented : String -> ( List BlankLine, String ) -> Model
addBlankLineToIndented blankLine ( blankLines, previousCode ) =
    Indented
        ( blankLines ++ [ blankLine ]
        , previousCode )


addBlankLineToFenced : String -> Fence -> Model
addBlankLineToFenced blankLine ( isOpen, fence, previousCode ) =
    Fenced ( isOpen, fence, previousCode ++ "\n" )


asToBlock : Model -> { language : Maybe String, code : String }
asToBlock model =
    case model of
        Indented ( _, codeStr ) ->
            { language = Nothing
            , code = codeStr
            }

        Fenced ( _, { language }, codeStr ) ->
            if String.length language > 0 then
                { language = Just language
                , code = codeStr
                }

            else
                { language = Nothing
                , code = codeStr
                }
