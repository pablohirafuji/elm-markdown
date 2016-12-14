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
    [ ( BlankLine ""                         , Regex.regex "^\\s*$" )
    , ( CodeLine (Code.Indented ( [], "" ))  , Code.indentedRegex )
    , ( CodeLine (Code.Fenced Code.initFence), Code.openingFenceRegex )
    , ( SetextHeadingLine 0 ""               , Regex.regex "^ {0,3}(=+|-+)[ \\t]*$")
    , ( ATXHeadingLine 0 ""                  , Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$" )
    , ( ThematicBreakLine                    , thematicBreakLineRegex )
    , ( BlockQuoteLine ""                    , BlockQuote.regex )
    , ( ListLine Lists.initUnordered         , Lists.unorderedRegex )
    , ( ListLine Lists.initOrdered           , Lists.orderedRegex )
    , ( TextLine ""                          , Regex.regex "^.*$" )
    ]


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


toRawLines : String -> List String
toRawLines =
    String.lines


parseRawLines : BlockContainer -> BlockContainer
parseRawLines ({ rawLines, lines } as blockC) =
    case rawLines of
        [] ->
            { blockC | lines = List.reverse blockC.lines }

        rawLine :: rawLinesTail ->
            case lines of
                CodeLine ( Code.Fenced ( True, fence, previousCode ) )
                    :: linesTail ->
                        { blockC
                            | rawLines = rawLinesTail
                            , lines    =
                                CodeLine
                                    ( Code.continueOrCloseFence
                                        fence previousCode rawLine )
                                            :: linesTail
                        } |> parseRawLines

-- Se abrir listLine, verificar ident, se for maior que o da
--lista, faz parte da lista
--Checar indent logo depois de ListLine lá em cima, se for
--maior que o indent da listLine, faz parte da listline
                _ ->
                    { blockC
                        | rawLines = rawLinesTail
                        , lines    = typifyLine rawLine :: lines
                    } |> parseRawLines


typifyLine : String -> Line
typifyLine lineStr =
    let
        applyRegex : ( Line, Regex ) -> Maybe Line -> Maybe Line
        applyRegex ( line, regex ) maybeLine =
            if maybeLine == Nothing then
                Regex.find ( Regex.AtMost 1 ) regex lineStr
                    |> List.head
                    |> Maybe.map ( matchToLine line )

            else
                maybeLine

    in
        List.foldl applyRegex Nothing lineRegex
            |> Maybe.withDefault ( BlankLine "" )


matchToLine : Line -> Regex.Match -> Line
matchToLine line match =
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


type alias BlockContainer =
    { rawLines : List String
    , lines    : List Line
    , blocks   : List Block
    , info     : Info
    }


initBlockContainer : BlockContainer
initBlockContainer =
    { rawLines = []
    , lines    = []
    , blocks   = []
    , info     = initInfo
    }


type alias Info =
    { isCodeBlockOpen : Bool
    }


initInfo : Info
initInfo =
    { isCodeBlockOpen = False
    }


parseLines : BlockContainer -> BlockContainer
parseLines ({ lines, blocks } as blockC) =
    case lines of
        [] ->
            blockC


        ATXHeadingLine lvl headingText :: linesTail ->
            { blockC
                | lines  = linesTail
                , blocks =
                    HeadingBlock lvl (String.trim headingText) :: blocks
            } |> parseLines


        ThematicBreakLine :: linesTail ->
            { blockC
                | lines  = linesTail
                , blocks = ThematicBreakBlock :: blocks
            } |> parseLines


        SetextHeadingLine lvl rawLine :: linesTail ->
            case blocks of
                ParagraphBlock paragraph :: blocksTail ->
                    { blockC
                        | lines  = linesTail
                        , blocks = HeadingBlock lvl paragraph :: blocksTail
                    } |> parseLines

                _ ->
                    if lvl == 1 then
                        { blockC
                            | lines = TextLine rawLine :: linesTail
                        } |> parseLines

                    else
                        if Regex.contains thematicBreakLineRegex rawLine then
                            { blockC
                                | lines  = linesTail
                                , blocks = ThematicBreakBlock :: blocks
                            } |> parseLines

                        else
                            { blockC
                                | lines = TextLine rawLine :: linesTail
                            } |> parseLines


        CodeLine (Code.Indented ( blankLines, lineCode )) :: linesTail ->
            case blocks of
                CodeBlock ( Code.Indented blockArgs ) :: blocksTail ->
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            CodeBlock
                                ( Code.addIndented
                                    (blankLines, lineCode) blockArgs
                                ) :: blocksTail
                    } |> parseLines                    

                ParagraphBlock paragraph :: blocksTail ->
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            ParagraphBlock
                                (paragraph ++ "\n" ++ String.trim lineCode)
                                    :: blocksTail
                    } |> parseLines

                _ ->
                    { blockC
                        | lines  = linesTail
                        , blocks = 
                            CodeBlock
                                ( Code.Indented
                                    ( [], lineCode ++ "\n" )
                                ) :: blocks
                    } |> parseLines


        CodeLine ( Code.Fenced codeFenceModel ) :: linesTail ->
            { blockC
                | lines  = linesTail
                , blocks =
                    CodeBlock ( Code.Fenced codeFenceModel ) :: blocks
            } |> parseLines


        BlockQuoteLine rawLine :: linesTail ->
            case blocks of
                BlockQuote blockC_ :: blocksTail ->
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            BlockQuote
                                { blockC_
                                    | rawLines = 
                                        blockC_.rawLines ++ [ rawLine ]
                                } :: blocksTail
                    } |> parseLines

                _ ->
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            BlockQuote
                                { initBlockContainer
                                    | rawLines = [ rawLine ]
                                } :: blocks
                    } |> parseLines


        ListLine ( lineInfo, rawLine ) :: linesTail ->
            let addListBlock =
                { blockC
                    | lines  = linesTail
                    , blocks =
                        ListBlock lineInfo
                            [ { initBlockContainer
                                | rawLines = [ rawLine ]
                            } ] :: blocks
                } |> parseLines

            in case blocks of
                ListBlock blockInfo blockCs :: blocksTail ->
                    if lineInfo.delimiter == blockInfo.delimiter then
                        { blockC
                            | lines  = linesTail
                            , blocks =
                                ListBlock
                                    ( Lists.updateInfo lineInfo blockInfo )
                                    ( { initBlockContainer
                                            | rawLines = [ rawLine ]
                                      } :: blockCs
                                    ) :: blocksTail
                        } |> parseLines

                    else
                        addListBlock

                ParagraphBlock paragraph :: blocksTail ->
                    case lineInfo.type_ of
                        Lists.Ordered 1 ->
                            addListBlock

                        Lists.Ordered int ->
                            { blockC
                                | lines  = linesTail
                                , blocks =
                                    ParagraphBlock
                                        (paragraph ++ "\n"
                                            ++ String.trim (toString int ++ lineInfo.delimiter ++ " " ++ rawLine))
                                                :: blocksTail
                            } |> parseLines

                        _ ->
                            addListBlock

                _ ->
                    addListBlock


        BlankLine str :: linesTail ->
            case blocks of
                CodeBlock ( Code.Indented ( blankLines, previousCode ) )
                    :: blocksTail ->
                        { blockC
                            | lines  = linesTail
                            , blocks =
                                CodeBlock
                                    ( Code.Indented
                                        ( blankLines ++ [ str ]
                                        , previousCode
                                        )
                                    ) :: blocksTail
                        } |> parseLines

                ListBlock blockInfo blockCs :: blocksTail ->
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            ListBlock
                                (Lists.blankLineFound blockInfo) blockCs
                                    :: blocksTail
                    } |> parseLines

                _ ->
                    { blockC
                        | lines  = linesTail
                        , blocks = BlankBlock :: blocks
                    } |> parseLines


        TextLine rawLine :: linesTail ->
            let
                addParagraphBlock =
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            ParagraphBlock (String.trim rawLine)
                                :: blocks
                    } |> parseLines

            in case blocks of
                ParagraphBlock paragraph :: blocksTail ->
                    { blockC
                        | lines  = linesTail
                        , blocks =
                            ParagraphBlock (paragraph ++ "\n" ++ String.trim rawLine)
                                :: blocksTail
                    } |> parseLines

                BlockQuote blockC_ :: blocksTail ->
                    case maybeContinueParagraph rawLine blockC_ of
                        Just updtBlockC ->
                            { blockC
                                | lines  = linesTail
                                , blocks =
                                    BlockQuote updtBlockC
                                        :: blocksTail
                            } |> parseLines

                        Nothing ->
                            addParagraphBlock

                ListBlock info blockCs :: blocksTail ->
                    case blockCs of
                        blockC_ :: blockCsTail ->
                            case maybeContinueParagraph rawLine blockC_ of
                                Just updtBlockC ->
                                    { blockC
                                        | lines  = linesTail
                                        , blocks =
                                            ListBlock info
                                                (updtBlockC
                                                    :: blockCsTail)
                                                        :: blocksTail
                                    } |> parseLines

                                Nothing ->
                                    addParagraphBlock

                        _ ->
                            addParagraphBlock

                _ ->
                    addParagraphBlock


maybeContinueParagraph : String -> BlockContainer -> Maybe BlockContainer
maybeContinueParagraph rawLine blockC =
    let
        parsedBlockC =
            parseRawLines blockC
                |> parseLines

    in case parsedBlockC.blocks of
        ParagraphBlock paragraph :: blockCBlocksTail ->
            { parsedBlockC
                | blocks =
                    ParagraphBlock (paragraph ++ "\n" ++ String.trim rawLine)
                        :: blockCBlocksTail
            } |> Just


        BlockQuote blockC_ :: blockCBlocksTail ->
            case maybeContinueParagraph rawLine blockC_ of
                Just updtBlockC_ ->
                    { parsedBlockC
                        | blocks =
                            BlockQuote updtBlockC_
                                :: blockCBlocksTail
                    } |> Just

                Nothing ->
                    Nothing


        ListBlock info blockCs :: blockCBlocksTail ->
            case blockCs of
                blockC_ :: blockCsTail ->
                    case maybeContinueParagraph rawLine blockC_ of
                        Just updtBlockC_ ->
                            { parsedBlockC
                                | blocks =
                                    ListBlock info
                                        ( updtBlockC_ :: blockCsTail )
                                            :: blockCBlocksTail
                            } |> Just

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


blockCToBlocks : BlockContainer -> List Block
blockCToBlocks =
    parseRawLines
        >> parseLines
        >> .blocks
        >> List.reverse


toBlocks : String -> List Block
toBlocks rawText =
    { initBlockContainer
        | rawLines = toRawLines rawText
    } |> blockCToBlocks


removeBlankBlock : List Block -> List Block
removeBlankBlock =
    List.filter ((/=) BlankBlock)


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


