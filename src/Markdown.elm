module Markdown exposing (..)


import Html exposing (..)
import Html.Attributes exposing (class, start)
import Regex exposing (Regex)
import Code



type Line
    = BlankLine
    | TextLine String
    | ATXHeadingLine Int String
    | SetextHeadingLine Int String
    | ThematicBreakLine
    | IndentedCodeLine String
    | CodeFenceLine CodeFenceState
    | CodeLine CodeFenceState String
    | ClosingCodeFenceLine
    | BlockQuoteLine String
    | ListLine ListState String -- Ordered Number -> Delimiter -> RawText


type alias CodeFenceState =
    { indentSize : Int
    , fenceChar : Char
    , fenceSize : Int
    , language : String
    }


emptyCodeFenceState : CodeFenceState
emptyCodeFenceState =
    { indentSize = 0
    , fenceChar = ' '
    , fenceSize = 0
    , language = ""
    }


type alias ListState =
    { indentSize : Int
    , delimiter : String
    , start : String
    , isLoose : Maybe Bool
    }


emptyListState : ListState
emptyListState =
    { indentSize = 0
    , delimiter = ""
    , start = ""
    , isLoose = Nothing
    }


lineRegex : List (Line, Regex)
lineRegex =
    [ ( IndentedCodeLine "", Regex.regex "^ {4,4}(.*)$" )
    , ( BlankLine, Regex.regex "^\\s*$" )
    , ( SetextHeadingLine 0 "", Regex.regex "^ {0,3}(=+|-+)[ \\t]*$")
    , ( ATXHeadingLine 0 "", Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$" )
    , ( CodeFenceLine emptyCodeFenceState, Regex.regex "^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$" )
    , ( ThematicBreakLine, thematicBreakLineRegex )
    , ( BlockQuoteLine "", Regex.regex "^ {0,3}(?:>[ ]?)(.*)$" )
    , ( ListLine emptyListState "", Regex.regex "^( {0,3}(\\d{1,9})([.)]))(?: (.*))?$" )
    , ( ListLine emptyListState "", Regex.regex "^( {0,3}([\\*\\-\\+]))(?: (.*))?$" )
    , ( TextLine "", Regex.regex "^.*$" )
    ]


closingFenceLineRegex : Regex
closingFenceLineRegex =
    Regex.regex "^ {0,3}(`{3,}|~{3,})[ \\t]*$"

thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


typifyLines : ( List String, List Line ) -> ( List String, List Line )
typifyLines ( rawLines, typedLines ) =
    case rawLines of
        [] ->
            ( rawLines
            , List.reverse typedLines )

        rawLine :: rawLinesRest ->
            case typedLines of
                -- If last typed line is CodeLine, continue or close
                -- the fence based on fenceState
                CodeFenceLine fenceState :: _ ->
                    ( rawLinesRest
                    , continueOrCloseCodeFence fenceState rawLine
                        :: typedLines
                    ) |> typifyLines

                -- If last typed line is CodeLine, continue or close
                -- the fence based on fenceState
                CodeLine fenceState _ :: _ ->
                    ( rawLinesRest
                    , continueOrCloseCodeFence fenceState rawLine
                        :: typedLines
                    ) |> typifyLines

-- Se abrir listLine, verificar ident, se for maior que o da
--lista, faz parte da lista
--Checar indent logo depois de ListLine lá em cima, se for
--maior que o indent da listLine, faz parte da listline

                _ ->
                    ( rawLinesRest
                    , typifyLine rawLine :: typedLines
                    ) |> typifyLines


continueOrCloseCodeFence : CodeFenceState -> String -> Line
continueOrCloseCodeFence fenceState rawLine =
    if isClosingCodeFenceLine fenceState rawLine then
        ClosingCodeFenceLine

    else
        CodeLine fenceState rawLine


isClosingCodeFenceLine : CodeFenceState -> String -> Bool
isClosingCodeFenceLine fenceState rawLine =
    Regex.find (Regex.AtMost 1) closingFenceLineRegex rawLine
        |> List.head
        |> Maybe.map
            (\match ->
                case match.submatches of
                    Just fence :: _ ->
                        String.length fence >= fenceState.fenceSize
                            && (Maybe.withDefault
                                    (' ', "")
                                    (String.uncons fence)
                                        |> Tuple.first
                                ) == fenceState.fenceChar

                    _ ->
                        False
            )
        |> Maybe.withDefault False


indentCodeLine : CodeFenceState -> String -> String
indentCodeLine fenceState =
    Regex.replace
        (Regex.AtMost 1)
        (Regex.regex ("^( {0," ++ toString fenceState.indentSize ++ "})"))
        (\_ -> "")


typifyLine : String -> Line
typifyLine lineStr =
    let
        applyRegex : (Line, Regex) -> Maybe Line -> Maybe Line
        applyRegex (line, regex) maybeLine =
            if maybeLine == Nothing then
                let
                    matchs =
                        Regex.find (Regex.AtMost 1) regex lineStr
                in
                    if List.length matchs > 0 then
                        Just (matchToLine matchs line)

                    else
                        Nothing

            else
                maybeLine

    in
        List.foldl applyRegex Nothing lineRegex
            |> Maybe.withDefault BlankLine


matchToLine : List Regex.Match -> Line -> Line
matchToLine matchs line =
    case line of
        BlankLine ->
            BlankLine

        SetextHeadingLine _ _ ->
            matchsToLine matchs matchToSetextHeadingLine

        ATXHeadingLine _ _ ->
            matchsToLine matchs matchToHeadingLine

        CodeFenceLine _ ->
            matchsToLine matchs matchToCodeFenceLine

        IndentedCodeLine _ ->
            matchsToLine matchs matchToIndentedCodeLine

        ThematicBreakLine ->
            ThematicBreakLine

        TextLine _ ->
            matchsToLine matchs matchToTextLine

        CodeLine fenceState code -> -- Impossible
            CodeLine fenceState code

        ClosingCodeFenceLine -> -- Impossible
            ClosingCodeFenceLine

        BlockQuoteLine _ ->
            matchsToLine matchs matchToBlockQuoteLine

        ListLine _ _ ->
            matchsToLine matchs matchToListLine


matchsToLine : List Regex.Match -> (Regex.Match -> Line) -> Line
matchsToLine matchs matchToLine =
    List.head matchs
        |> Maybe.map matchToLine
        |> Maybe.withDefault BlankLine


matchToSetextHeadingLine : Regex.Match -> Line
matchToSetextHeadingLine match =
    case match.submatches of
        Just str :: _ ->
            if String.startsWith "=" str then
                SetextHeadingLine 1 str

            else
                SetextHeadingLine 2 str

        _ ->
            TextLine match.match


matchToHeadingLine : Regex.Match -> Line
matchToHeadingLine match =
    case match.submatches of
        Just lvl :: Just heading :: _ ->
            ATXHeadingLine (String.length lvl) heading

        Just lvl :: Nothing :: _ ->
            ATXHeadingLine (String.length lvl) ""

        _ ->
            TextLine match.match


matchToTextLine : Regex.Match -> Line
matchToTextLine match =
    TextLine match.match


matchToCodeFenceLine : Regex.Match -> Line
matchToCodeFenceLine match =
    case Debug.log "x" match.submatches of
        Just indent :: Just fence :: Just language :: _ ->
            CodeFenceLine
                { indentSize = String.length indent
                , fenceChar =
                    Maybe.withDefault ('`', "") (String.uncons fence)
                        |> Tuple.first
                , fenceSize = String.length fence
                , language =
                    String.words language
                        |> List.head
                        |> Maybe.withDefault ""
                }

        _ ->
            TextLine match.match


matchToIndentedCodeLine : Regex.Match -> Line
matchToIndentedCodeLine match =
    case match.submatches of
        Just code :: _ ->
            IndentedCodeLine code

        _ ->
            TextLine match.match


matchToBlockQuoteLine : Regex.Match -> Line
matchToBlockQuoteLine match =
    case match.submatches of
        Just quote :: _ ->
            BlockQuoteLine quote

        _ ->
            TextLine match.match


matchToListLine : Regex.Match -> Line
matchToListLine match =
    case match.submatches of
        Just indentString :: Just number :: Just delimiter :: Just rawText :: _ ->
            ListLine
                { indentSize = String.length indentString + 1
                , delimiter = delimiter
                , start = number
                , isLoose = Nothing
                }
                rawText

        Just indentString :: Just delimiter :: Just rawText :: [] ->
            ListLine
                { indentSize = String.length indentString + 1
                , delimiter = delimiter
                , start = ""
                , isLoose = Nothing
                }
                rawText

        _ ->
            TextLine match.match


type Block
    = HeadingBlock Int String
    | ThematicBreakBlock
    | ParagraphBlock String
    | CodeBlock (Maybe CodeFenceState) String
    | BlockQuote String
    | ListBlock ListState (List String)


type alias ParseState =
    { isCodeBlockOpen : Bool
    }


initParseState : ParseState
initParseState =
    { isCodeBlockOpen = False
    }


linesToBlocks : ( List Block, List Line, ParseState ) -> ( List Block, List Line, ParseState )
linesToBlocks ( blocks, lines, ({isCodeBlockOpen} as parseState) ) =
    case lines of
        [] ->
            ( List.reverse blocks
            , lines
            , parseState )


        TextLine paragraph
            :: BlankLine
            :: rest ->
                ( ParagraphBlock (String.trim paragraph) :: blocks
                , rest
                , parseState
                ) |> linesToBlocks


        TextLine line1
            :: TextLine line2
            :: rest ->
                ( blocks
                , TextLine (String.trim line1 ++ "\n" ++ String.trim line2)
                    :: rest
                , parseState
                ) |> linesToBlocks


        TextLine line1
            :: IndentedCodeLine line2
            :: rest ->
                ( blocks
                , TextLine (String.trim line1 ++ "\n" ++ String.trim line2)
                    :: rest
                , parseState
                ) |> linesToBlocks


        TextLine headingText
            :: SetextHeadingLine lvl _
            :: rest ->
                ( HeadingBlock lvl (String.trim headingText) :: blocks
                , rest
                , parseState
                ) |> linesToBlocks


        ATXHeadingLine lvl headingText :: rest ->
            ( HeadingBlock lvl (String.trim headingText) :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        ThematicBreakLine :: rest ->
            ( ThematicBreakBlock :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        SetextHeadingLine 1 paragraph :: rest ->
            ( ParagraphBlock (String.trim paragraph) :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        SetextHeadingLine 2 maybeParagraph :: rest ->
            if Regex.contains thematicBreakLineRegex maybeParagraph then
                ( ThematicBreakBlock :: blocks
                , rest
                , parseState
                ) |> linesToBlocks

            else
                ( blocks
                , TextLine (String.trim maybeParagraph)
                    :: rest
                , parseState
                ) |> linesToBlocks


        IndentedCodeLine code :: rest ->
            case blocks of
                CodeBlock Nothing previousCode :: restBlocks ->
                    ( CodeBlock Nothing (previousCode ++ code ++ "\n")
                        :: restBlocks
                    , rest
                    , parseState
                    ) |> linesToBlocks

                _ ->
                    ( CodeBlock Nothing (code ++ "\n") :: blocks
                    , rest
                    , parseState
                    ) |> linesToBlocks


        CodeFenceLine fenceState :: rest ->
            ( CodeBlock (Just fenceState) "" :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        CodeLine fenceState code :: rest ->
            case blocks of
                CodeBlock _ previousCode :: restBlocks ->
                    ( CodeBlock (Just fenceState)
                        (previousCode
                            ++ indentCodeLine fenceState code
                            ++ "\n")
                                :: restBlocks
                    , rest
                    , parseState
                    ) |> linesToBlocks

                _ ->
                    ( CodeBlock (Just fenceState) code :: blocks
                    , rest
                    , parseState
                    ) |> linesToBlocks


        -- TODO Só quando o último block do blockquote for parágrafo ou lista?
        BlockQuoteLine rawText
            :: TextLine paragraph
            :: rest ->
                ( blocks
                , BlockQuoteLine (rawText ++ "\n" ++ paragraph)
                    :: rest
                , parseState
                ) |> linesToBlocks


        BlockQuoteLine rawText1
            :: BlockQuoteLine rawText2
            :: rest ->
                ( blocks
                , BlockQuoteLine (rawText1 ++ "\n" ++ rawText2)
                    :: rest
                , parseState
                ) |> linesToBlocks


        BlockQuoteLine rawText :: rest ->
            ( BlockQuote rawText :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        -- List cases
        -- In order to solve of unwanted lists in paragraphs with hard-wrapped numerals, we allow only lists starting with 1 to interrupt paragraphs. Thus,
        --TextLine paragraph
        --    :: ListLine listState rawText
        --    :: rest ->
        --        if listState

        ListLine listState rawText
            :: TextLine line
            :: rest ->
                ( blocks
                , ListLine listState (rawText ++ "\n" ++ String.trim line)
                    :: rest
                , parseState
                ) |> linesToBlocks


        ListLine listState rawText
            :: IndentedCodeLine line
            :: rest ->
                ( blocks
                , ListLine listState (rawText ++ "\n" ++ String.trim line)
                    :: rest
                , parseState
                ) |> linesToBlocks


        ListLine listState rawText
            :: BlankLine
            :: rest ->
                ( blocks
                , ListLine { listState | isLoose = Just False } rawText
                    :: rest
                , parseState
                ) |> linesToBlocks


        ListLine listState rawText :: rest ->
            let
                verifyLoose : ListState -> ListState
                verifyLoose listState =
                    { listState |
                        isLoose = Just (listState.isLoose /= Nothing)
                    }

            in case blocks of
                ListBlock listBlockState rawTextList :: restBlocks ->
                    if listState.delimiter == listBlockState.delimiter then
                        ( ListBlock listBlockState (rawTextList ++ [ rawText ])
                            :: restBlocks
                        , rest
                        , parseState
                        ) |> linesToBlocks

                    else
                        ( ListBlock listState [ rawText ]
                            :: blocks
                        , rest
                        , parseState
                        ) |> linesToBlocks

                _ ->
                    ( ListBlock listState [ rawText ]
                        :: blocks
                    , rest
                    , parseState
                    ) |> linesToBlocks


        BlankLine :: rest ->
            case blocks of
                CodeBlock fenceState previousCode :: restBlocks ->
                    ( CodeBlock fenceState (previousCode ++ "\n")
                        :: restBlocks
                    , rest
                    , parseState
                    ) |> linesToBlocks

                _ ->
                    ( blocks
                    , rest
                    , parseState
                    ) |> linesToBlocks


        TextLine paragraph :: rest ->
            ( ParagraphBlock (String.trim paragraph) :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        _ :: rest ->
            ( blocks
            , rest
            , parseState
            ) |> linesToBlocks


toHtml : String -> List (Html msg)
toHtml rawText =
    ( []
    , typifyLines ( (String.lines rawText), [] )
        |> Tuple.second
    , initParseState )
        |> linesToBlocks 
        |> \( blocks, _, _ ) -> blocks
        |> List.map blockToHtml


blockToHtml : Block -> Html msg
blockToHtml block =
    case block of
        HeadingBlock lvl heading ->
            case lvl of
                1 -> h1 [] [ text heading ]
                2 -> h2 [] [ text heading ]
                3 -> h3 [] [ text heading ]
                4 -> h4 [] [ text heading ]
                5 -> h5 [] [ text heading ]
                _ -> h6 [] [ text heading ]

        ThematicBreakBlock ->
            hr [] []

        ParagraphBlock paragraph ->
            p [] [ text paragraph ]

        CodeBlock maybeFence codeStr ->
            --Code.view codeBlock
            case maybeFence of
                Just { language } ->
                    if String.length language > 0 then
                        pre []
                            [ code
                                [ class ("language-" ++ language) ]
                                [ text codeStr ]
                            ]

                    else
                        pre [] [ code [] [ text codeStr ] ]

                Nothing ->
                    pre [] [ code [] [ text codeStr ] ]

        BlockQuote rawText ->
            blockquote [] (toHtml rawText)

        ListBlock listState rawTextList ->
            let
                listItems =
                    List.map toHtml rawTextList
                        |> List.map (li [])

            in
            if listState.delimiter == "*"
                || listState.delimiter == "+"
                || listState.delimiter == "-" then
                    ul [] listItems
            else
                case String.toInt listState.start of
                    Result.Ok int ->
                        ol [ start int ] listItems

                    Result.Err _ ->
                        ul [] listItems
                        
