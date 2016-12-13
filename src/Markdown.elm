module Markdown exposing (..)


import Html exposing (..)
import Regex exposing (Regex)
import Code
import BlockQuote
import Lists



type Line
    = BlankLine String
    | TextLine String
    | ATXHeadingLine Int String
    | SetextHeadingLine Int String
    | ThematicBreakLine
    | BlockQuoteLine BlockQuote.Line
    | ListLine Lists.Line
    | CodeLine Code.Block


lineRegex : List (Line, Regex)
lineRegex =
    [ ( BlankLine "", Regex.regex "^\\s*$" )
    , ( CodeLine ( Code.Indented ( [], "" ) ), Code.indentedRegex )
    , ( CodeLine ( Code.Fenced Code.initFence ), Code.openingFenceRegex )
    , ( SetextHeadingLine 0 "", Regex.regex "^ {0,3}(=+|-+)[ \\t]*$")
    , ( ATXHeadingLine 0 "", Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$" )
    , ( ThematicBreakLine, thematicBreakLineRegex )
    , ( BlockQuoteLine "", BlockQuote.regex )
    , ( ListLine Lists.initUnordered, Lists.unorderedRegex )
    , ( ListLine Lists.initOrdered, Lists.orderedRegex )
    , ( TextLine "", Regex.regex "^.*$" )
    ]


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


toRawLines : String -> List String
toRawLines =
    String.lines


toLines : String -> List Line -> List Line
toLines rawText lines =
    typifyLines ( toRawLines rawText, lines )
        |> Tuple.second


rawLinesToLines : List String -> List Line -> List Line
rawLinesToLines rawLines lines =
    typifyLines ( rawLines, lines )
        |> Tuple.second


typifyLines : ( List String, List Line ) -> ( List String, List Line )
typifyLines ( rawLines, typedLines ) =
    case rawLines of
        [] ->
            ( rawLines
            , List.reverse typedLines )

        rawLine :: rawLinesRest ->
            case typedLines of
                CodeLine ( Code.Fenced ( True, fence, previousCode ) )
                    :: typedLinesTail ->
                        ( rawLinesRest
                        , CodeLine
                            ( Code.continueOrCloseFence
                                fence previousCode rawLine )
                                    :: typedLinesTail
                        ) |> typifyLines


-- Se abrir listLine, verificar ident, se for maior que o da
--lista, faz parte da lista
--Checar indent logo depois de ListLine lá em cima, se for
--maior que o indent da listLine, faz parte da listline

                _ ->
                    ( rawLinesRest
                    , typifyLine rawLine :: typedLines
                    ) |> typifyLines


typifyLine : String -> Line
typifyLine lineStr =
    let
        applyRegex : (Line, Regex) -> Maybe Line -> Maybe Line
        applyRegex (line, regex) maybeLine =
            if maybeLine == Nothing then
                Regex.find (Regex.AtMost 1) regex lineStr
                    |> List.head
                    |> Maybe.map (\match -> matchToLine match line)

            else
                maybeLine

    in
        List.foldl applyRegex Nothing lineRegex
            |> Maybe.withDefault (BlankLine "")


matchToLine : Regex.Match -> Line -> Line
matchToLine match line =
    case line of
        BlankLine _ ->
            BlankLine match.match

        SetextHeadingLine _ _ ->
            case match.submatches of
                Just str :: _ ->
                    if String.startsWith "=" str then
                        SetextHeadingLine 1 str

                    else
                        SetextHeadingLine 2 str

                _ ->
                    TextLine match.match

        ATXHeadingLine _ _ ->
            case match.submatches of
                Just lvl :: Just heading :: _ ->
                    ATXHeadingLine (String.length lvl) heading

                Just lvl :: Nothing :: _ ->
                    ATXHeadingLine (String.length lvl) ""

                _ ->
                    TextLine match.match

        ThematicBreakLine ->
            ThematicBreakLine

        TextLine _ ->
            TextLine match.match

        BlockQuoteLine _ ->
            BlockQuote.fromMatch match
                |> Maybe.map BlockQuoteLine
                |> Maybe.withDefault (TextLine match.match)

        ListLine info ->
            Lists.fromMatch match info
                |> Maybe.map ListLine
                |> Maybe.withDefault (TextLine match.match)

        CodeLine ( Code.Fenced _ ) ->
            Code.fromOpeningFenceMatch match
                |> Maybe.map CodeLine
                |> Maybe.withDefault (TextLine match.match)

        CodeLine ( Code.Indented _ ) ->
            Code.fromIndentedMatch match
                |> Maybe.map CodeLine
                |> Maybe.withDefault (TextLine match.match)


type Block
    = HeadingBlock Int String
    | ThematicBreakBlock
    | ParagraphBlock String
    | CodeBlock Code.Block
    | BlockQuote BlockContainer
    | ListBlock Lists.Info (List BlockContainer)
    | BlankBlock


type alias BlockContainer = ( List String, List Line, List Block )


type alias Info =
    { isCodeBlockOpen : Bool
    }


initInfo : Info
initInfo =
    { isCodeBlockOpen = False
    }


linesToBlocks : ( List Line, List Block, Info ) -> ( List Line, List Block, Info )
linesToBlocks =
    linesToReversedBlocks 
        >> \( lines, blocks, parseState ) ->
            ( lines
            , List.reverse blocks
            , parseState
            )


linesToReversedBlocks : ( List Line, List Block, Info ) -> ( List Line, List Block, Info )
linesToReversedBlocks ( lines, blocks, parseState ) =
    case lines of
        [] ->
            ( lines
            , blocks
            , parseState
            )


        ATXHeadingLine lvl headingText :: linesTail ->
            ( linesTail
            , HeadingBlock lvl (String.trim headingText) :: blocks
            , parseState
            ) |> linesToReversedBlocks


        ThematicBreakLine :: linesTail ->
            ( linesTail
            , ThematicBreakBlock :: blocks
            , parseState
            ) |> linesToReversedBlocks


        SetextHeadingLine lvl rawLine :: linesTail ->
            case blocks of
                ParagraphBlock paragraph :: blocksTail ->
                    ( linesTail
                    , HeadingBlock lvl paragraph :: blocksTail
                    , parseState
                    ) |> linesToReversedBlocks

                _ ->
                    if lvl == 1 then
                        ( TextLine rawLine :: linesTail
                        , blocks
                        , parseState
                        ) |> linesToReversedBlocks

                    else
                        if Regex.contains thematicBreakLineRegex rawLine then
                            ( linesTail
                            , ThematicBreakBlock :: blocks
                            , parseState
                            ) |> linesToReversedBlocks

                        else
                            ( TextLine rawLine :: linesTail
                            , blocks
                            , parseState
                            ) |> linesToReversedBlocks


        CodeLine ( Code.Indented ( blankLines, lineCode ) ) :: linesTail ->
            case blocks of
                CodeBlock ( Code.Indented blockArgs ) :: blocksTail ->
                        ( linesTail
                        , CodeBlock
                            ( Code.addIndented
                                ( blankLines, lineCode ) blockArgs
                            ) :: blocksTail
                        , parseState
                        ) |> linesToReversedBlocks

                ParagraphBlock paragraph :: blocksTail ->
                    ( linesTail
                    , ParagraphBlock (paragraph ++ "\n" ++ String.trim lineCode)
                        :: blocksTail
                    , parseState
                    ) |> linesToReversedBlocks

                _ ->
                    ( linesTail
                    , CodeBlock ( Code.Indented ( [], lineCode ++ "\n" ) )
                        :: blocks
                    , parseState
                    ) |> linesToReversedBlocks


        CodeLine ( Code.Fenced codeFenceModel ) :: linesTail ->
            ( linesTail
            , CodeBlock ( Code.Fenced codeFenceModel )
                :: blocks
            , parseState
            ) |> linesToReversedBlocks


        BlockQuoteLine rawLine :: linesTail ->
            case blocks of
                BlockQuote ( rawLines, lines, blocks )
                    :: blocksTail ->
                        ( linesTail
                        , BlockQuote ( rawLines ++ [ rawLine ], lines, blocks )
                            :: blocksTail
                        , parseState
                        ) |> linesToReversedBlocks

                _ ->
                    ( linesTail
                    , BlockQuote ( [ rawLine ], [], [] )
                        :: blocks
                    , parseState
                    ) |> linesToReversedBlocks


        ListLine ( lineInfo, rawLine ) :: linesTail ->
            case blocks of
                ListBlock blockInfo blockCs :: blocksTail ->
                    if lineInfo.delimiter == blockInfo.delimiter then
                        ( linesTail
                        , ListBlock
                            ( Lists.updateInfo lineInfo blockInfo )
                            ( ([ rawLine ], [], []) :: blockCs )
                                :: blocksTail
                        , parseState
                        ) |> linesToReversedBlocks

                    else
                        ( linesTail
                        , ListBlock lineInfo [ ( [ rawLine ], [], [] ) ]
                            :: blocks
                        , parseState
                        ) |> linesToReversedBlocks

                _ ->
                    ( linesTail
                    , ListBlock lineInfo [ ( [ rawLine ], [], [] ) ]
                        :: blocks
                    , parseState
                    ) |> linesToReversedBlocks


        BlankLine str :: linesTail ->
            case blocks of
                CodeBlock ( Code.Indented ( blankLines, previousCode ) )
                    :: blocksTail ->
                        ( linesTail
                        , CodeBlock
                            ( Code.Indented
                                ( blankLines ++ [ str ]
                                , previousCode
                                )
                            ) :: blocksTail
                        , parseState
                        ) |> linesToReversedBlocks

                ListBlock blockInfo blockCs :: blocksTail ->
                    ( linesTail
                    , ListBlock (Lists.blankLineFound blockInfo) blockCs
                        :: blocksTail
                    , parseState
                    ) |> linesToReversedBlocks

                _ ->
                    ( linesTail
                    , BlankBlock :: blocks
                    , parseState
                    ) |> linesToReversedBlocks


        TextLine rawLine :: linesTail ->
            case blocks of
                BlockQuote blockC :: blocksTail ->
                        case maybeContinueParagraph rawLine blockC of
                            Just updtBlocks ->
                                ( linesTail
                                , BlockQuote ( [], [], updtBlocks )
                                    :: blocksTail
                                , parseState
                                ) |> linesToReversedBlocks

                            Nothing ->
                                ( linesTail
                                , ParagraphBlock (String.trim rawLine)
                                    :: blocks
                                , parseState
                                ) |> linesToReversedBlocks

                ParagraphBlock paragraph :: blocksTail ->
                    ( linesTail
                    , ParagraphBlock (paragraph ++ "\n" ++ String.trim rawLine)
                        :: blocksTail
                    , parseState
                    ) |> linesToReversedBlocks

                _ ->
                    ( linesTail
                    , ParagraphBlock (String.trim rawLine) :: blocks
                    , parseState
                    ) |> linesToReversedBlocks


blockCToReversedBlocks : BlockContainer -> List Block
blockCToReversedBlocks ( rawLines, lines, blocks ) =
    ( rawLinesToLines rawLines lines
    , blocks
    , initInfo )
        |> linesToReversedBlocks
        |> \(_, blocks_, _) -> blocks_


maybeContinueParagraph : String -> BlockContainer -> Maybe (List Block)
maybeContinueParagraph rawLine blockC =
    case blockCToReversedBlocks blockC of
        ParagraphBlock paragraph :: containerBlocksTail ->
            ParagraphBlock (paragraph ++ "\n" ++ String.trim rawLine)
                :: containerBlocksTail
                    |> Just

        BlockQuote blockC_ :: containerBlocksTail ->
            case maybeContinueParagraph rawLine blockC_ of
                Just updtBlockC_ ->
                    BlockQuote ([], [], updtBlockC_)
                        :: containerBlocksTail
                            |> Just

                Nothing ->
                    Nothing

        _ ->
            Nothing


blockCToBlocks : BlockContainer -> List Block
blockCToBlocks =
    blockCToReversedBlocks
        >> List.reverse


toBlocks : String -> List Block
toBlocks rawText =
    blockCToBlocks ( toRawLines rawText, [], [] )


removeBlankBlock : List Block -> List Block
removeBlankBlock =
    List.filter (\block -> block /= BlankBlock )


blockToHtml : Bool -> Block -> Html msg
blockToHtml textAsParagraph block =
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
            if textAsParagraph then
                p [] [ text paragraph ]

            else
                text paragraph

        CodeBlock codeBlock ->
            Code.view codeBlock

        BlockQuote blockC ->
            blockCToBlocks blockC
                |> removeBlankBlock
                |> List.map (blockToHtml True)
                |> BlockQuote.view

        ListBlock info blockCs ->
            List.reverse blockCs
                |> List.map blockCToBlocks
                |> List.map
                    ( removeBlankBlock
                        >> List.map (blockToHtml (Lists.isLoose info))
                        >> li []
                    )
                |> Lists.view info

        BlankBlock ->
            text ""


toHtml : String -> List (Html msg)
toHtml =
    toBlocks
        >> removeBlankBlock
        >> List.map (blockToHtml True)


