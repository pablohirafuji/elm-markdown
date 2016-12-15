module CommonMark exposing (..)


import Html exposing (..)
import Regex exposing (Regex)
import CommonMark.Code as Code
import CommonMark.List as Lists
import CommonMark.BlockQuote as BlockQuote
import CommonMark.Heading as Heading
--import CommonMark.Block as Block exposing (Block)
--import CommonMark.BlankBlock as BlankBlock


type Block
    = BlankBlock
    | HeadingBlock ( Int, List String )
    | ThematicBreakBlock
    | CodeBlock Code.Model
    | BlockQuote (List Block)
    | ListBlock Lists.Model (List (List Block))
    | ParagraphBlock (List String)


type Line
    = BlankLine
    | ATXHeadingLine
    | SetextHeadingLine
    | ThematicBreakLine
    | IndentedCodeLine
    | OpeningFenceCodeLine
    | BlockQuoteLine
    | OrderedListLine
    | UnorderedListLine


lineRegex : List (Line, Regex)
lineRegex =
    [ ( BlankLine           , Regex.regex "^\\s*$" )
    , ( IndentedCodeLine    , Code.indentedRegex )
    , ( OpeningFenceCodeLine, Code.openingFenceRegex )
    , ( SetextHeadingLine   , Heading.setextRegex )
    , ( ATXHeadingLine      , Heading.atxRegex )
    , ( ThematicBreakLine   , thematicBreakLineRegex )
    , ( BlockQuoteLine      , BlockQuote.regex )
    , ( OrderedListLine     , Lists.orderedRegex )
    , ( UnorderedListLine   , Lists.unorderedRegex )
    ]


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


toRawLines : String -> List String
toRawLines =
    String.lines


toBlocks : String -> List Block
toBlocks rawText =
    ( toRawLines rawText, [] )
        |> parseRawLines


parseRawLines : ( List String, List Block ) -> List Block
parseRawLines ( rawLines, blocks ) =
    case rawLines of
        [] ->
            blocks

        rawLine :: linesTail ->
            case blocks of
                --List info blocks_ ->
                    --verifica indent - se fizer parte, tipifica e adiciona no List Block
                    --se não, tipifica e verifica se continua parágrafo
                    --se não adiciona na lista de bloco
                --    blocks

                CodeBlock (Code.Fenced (True, fence, lines_)) :: blocksTail ->
                    Code.continueOrCloseFence fence lines_ rawLine
                        |> CodeBlock
                        |> \codeBlock -> codeBlock :: blocksTail
                        |> (,) linesTail
                        |> parseRawLines

                _ ->
                    parseRawLine rawLine blocks
                        |> (,) linesTail
                        |> parseRawLines


parseRawLine : String -> List Block -> List Block
parseRawLine rawLine blocks =
    List.foldl (applyRegex rawLine blocks) Nothing lineRegex
        |> Maybe.withDefault ( parseTextLine rawLine blocks )


applyRegex : String -> List Block -> ( Line, Regex ) -> Maybe (List Block) -> Maybe (List Block)
applyRegex rawLine blocks ( line, regex ) maybeBlocks =
    if maybeBlocks == Nothing then
        Regex.find ( Regex.AtMost 1 ) regex rawLine
            |> List.head
            |> Maybe.map ( parseLine line blocks )

    else
        maybeBlocks


parseLine : Line -> List Block -> Regex.Match -> List Block
parseLine line blocks match =
    case line of
        BlankLine ->
            parseBlankLine match blocks


        ATXHeadingLine ->
            HeadingBlock ( Heading.atxMatch match ) :: blocks


        SetextHeadingLine ->
            parseSetextHeadingLine match blocks


        ThematicBreakLine ->
            ThematicBreakBlock :: blocks

        IndentedCodeLine ->
            parseIndentedCodeLine match blocks

        OpeningFenceCodeLine ->
            parseFencedCodeLine match blocks

        BlockQuoteLine ->
            parseBlockQuoteLine match blocks

        OrderedListLine ->
            parseListLine (Lists.Ordered 0) match blocks
        
        UnorderedListLine ->
            parseListLine Lists.Unordered match blocks


parseBlankLine : Regex.Match -> List Block -> List Block
parseBlankLine match blocks =
    case blocks of
        CodeBlock ( Code.Indented indentedModel )
            :: blocksTail ->
                Code.addBlankLine match.match indentedModel
                    |> CodeBlock
                    |> \b -> b :: blocksTail


        ListBlock model blocks_ :: blocksTail ->
            ListBlock
                (Lists.blankLineFound model) blocks_
                    :: blocksTail

        _ ->
            BlankBlock :: blocks


parseSetextHeadingLine : Regex.Match -> List Block -> List Block
parseSetextHeadingLine match blocks =
    let ( lvl, _ ) =
        Heading.setextMatch match

    in case blocks of
        ParagraphBlock paragraph :: blocksTail ->
            HeadingBlock ( lvl, paragraph ) :: blocksTail

        _ ->
            if lvl == 1 then
                parseTextLine match.match blocks

            else
                if Regex.contains thematicBreakLineRegex match.match
                    then ThematicBreakBlock :: blocks
                    else parseTextLine match.match blocks


parseIndentedCodeLine : Regex.Match -> List Block -> List Block
parseIndentedCodeLine match blocks =
    let ( blankLines, codeLine ) =
        Code.fromIndentedMatch match

    in case blocks of
        CodeBlock ( Code.Indented blockArgs ) :: blocksTail ->
            CodeBlock
                ( Code.addIndented
                    (blankLines, codeLine) blockArgs
                ) :: blocksTail  

        _ ->
            maybeContinueParagraph codeLine blocks
                |> Maybe.withDefault
                    ( CodeBlock
                        ( Code.Indented
                            ( [], codeLine ++ "\n" )
                        ) :: blocks
                    )


parseFencedCodeLine : Regex.Match -> List Block -> List Block
parseFencedCodeLine match blocks =
    CodeBlock
        ( Code.Fenced
            (Code.fromOpeningFenceMatch match)
        ) :: blocks


parseBlockQuoteLine : Regex.Match -> List Block -> List Block
parseBlockQuoteLine match blocks =
    let rawLine =
        BlockQuote.fromMatch match

    in case blocks of
        BlockQuote blocks_ :: blocksTail ->
            BlockQuote
                ( parseRawLines ( [ rawLine ], blocks_ ) )
                    :: blocksTail

        _ ->
            BlockQuote ( parseRawLines ( [ rawLine ], [] ) )
                :: blocks


parseListLine : Lists.Type -> Regex.Match -> List Block -> List Block
parseListLine type_ match blocks =
    let
        ( lineModel, rawLine ) =
            Lists.fromMatch type_ match

        newListBlock =
            ListBlock lineModel
                [ parseRawLines ( [ rawLine ], [] ) ]
                    :: blocks

    in case blocks of
        ListBlock blockModel blocks_ :: blocksTail ->
            if lineModel.delimiter == blockModel.delimiter then
                ListBlock
                    ( Lists.updateModel lineModel blockModel )
                    ( parseRawLines ( [ rawLine ], [] ) :: blocks_ )
                        :: blocksTail

            else
                newListBlock

        ParagraphBlock paragraph :: blocksTail ->
            case lineModel.type_ of
                Lists.Ordered 1 ->
                    newListBlock

                Lists.Ordered int ->
                    ParagraphBlock ( paragraph ++ [ match.match ] )
                        :: blocksTail

                _ ->
                    newListBlock

        _ ->
            newListBlock


parseTextLine : String -> List Block -> List Block
parseTextLine rawLine blocks =
    maybeContinueParagraph rawLine blocks
        |> Maybe.withDefault
            ( ParagraphBlock [ rawLine ] :: blocks )


maybeContinueParagraph : String -> List Block -> Maybe ( List Block )
maybeContinueParagraph rawLine blocks =
    case blocks of
        ParagraphBlock paragraph :: blocksTail ->
            ParagraphBlock ( paragraph ++ [ rawLine ] )
                :: blocksTail
                    |> Just


        BlockQuote blocks_ :: blocksTail ->
            maybeContinueParagraph rawLine blocks_
                |> Maybe.map
                    (\updtBlocks_ ->
                        BlockQuote updtBlocks_ :: blocksTail
                    )


        ListBlock info blocks_ :: blocksTail ->
            if info.hasBlankLineAfter then
                Nothing

            else
                case blocks_ of
                    block_ :: blocksTail_ ->
                        maybeContinueParagraph rawLine block_
                            |> Maybe.map
                                (\updtBlocks_ ->
                                    ListBlock info
                                        ( updtBlocks_
                                            :: blocksTail_ )
                                                :: blocksTail
                                )

                    _ ->
                        Nothing

        _ ->
            Nothing


removeBlankBlocks : List Block -> List Block
removeBlankBlocks =
    List.filter ((/=) BlankBlock)


blockToHtml : Bool -> Block -> Html msg
blockToHtml textAsParagraph block =
    case block of
        HeadingBlock ( lvl, heading ) ->
            case lvl of
                1 -> h1 [] [ text <| concatLines heading ]
                2 -> h2 [] [ text <| concatLines heading ]
                3 -> h3 [] [ text <| concatLines heading ]
                4 -> h4 [] [ text <| concatLines heading ]
                5 -> h5 [] [ text <| concatLines heading ]
                _ -> h6 [] [ text <| concatLines heading ]

        ThematicBreakBlock ->
            hr [] []

        ParagraphBlock lines ->
            if textAsParagraph then
                p [] [ text <| concatLines lines ]

            else
                text <| concatLines lines

        CodeBlock codeBlock ->
            Code.view codeBlock

        BlockQuote blocks ->
            prepareBlocks blocks
                |> List.map (blockToHtml True)
                |> BlockQuote.view

        ListBlock model blocks ->
            List.reverse blocks
                |> List.map
                    ( prepareBlocks
                        >> List.map (blockToHtml (Lists.isLoose model))
                        >> li []
                    )
                |> Lists.view model

        BlankBlock ->
            text ""


prepareBlocks : List Block -> List Block
prepareBlocks =
    removeBlankBlocks
        >> List.reverse


concatLines : List String -> String
concatLines =
    List.map String.trim
        >> List.intersperse "\n"
        >> String.concat


toHtml : String -> List (Html msg)
toHtml =
    toBlocks
        >> prepareBlocks
        >> List.map (blockToHtml True)

