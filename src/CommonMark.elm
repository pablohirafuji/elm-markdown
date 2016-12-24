module CommonMark exposing (toHtml, toBlocks)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Common Helpers
@docs toHtml, toBlocks

-}


import Html exposing (..)
import Regex exposing (Regex)
import CommonMark.Code as Code
import CommonMark.List as Lists
import CommonMark.BlockQuote as BlockQuote
import CommonMark.Heading as Heading



-- Line


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
    [ ( BlankLine           , blankLineRegex )
    , ( IndentedCodeLine    , Code.indentedRegex )
    , ( OpeningFenceCodeLine, Code.openingFenceRegex )
    , ( SetextHeadingLine   , Heading.setextRegex )
    , ( ATXHeadingLine      , Heading.atxRegex )
    , ( ThematicBreakLine   , thematicBreakLineRegex )
    , ( BlockQuoteLine      , BlockQuote.regex )
    , ( OrderedListLine     , Lists.orderedRegex )
    , ( UnorderedListLine   , Lists.unorderedRegex )
    ]


blankLineRegex : Regex
blankLineRegex =
    Regex.regex "^\\s*$"


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


toRawLines : String -> List String
toRawLines =
    String.lines



-- Block


type Block
    = BlankBlock
    | HeadingBlock ( Int, List String )
    | ThematicBreakBlock
    | CodeBlock Code.Model
    | BlockQuote (List Block)
    | ListBlock Lists.Model (List (List Block))
    | ParagraphBlock (List String)


parseRawLines : ( List String, List Block ) -> List Block
parseRawLines ( rawLines, blocks ) =
    case rawLines of
        [] ->
            blocks

        rawLine :: rawLinesTail ->
            preParseRawLine ( rawLine, blocks )
                |> (,) rawLinesTail
                |> parseRawLines


preParseRawLine : ( String, List Block ) -> List Block
preParseRawLine ( rawLine, blocks ) =
    case blocks of
        ListBlock model blocksList :: blocksTail ->
            if Lists.indentLength rawLine >= model.indentLength then
                case blocksList of
                    blocks_ :: blocksListTail ->
                        let
                            unindentedRawLine : String
                            unindentedRawLine =
                                Code.indentLine model.indentLength rawLine

                            updtListBlock : Lists.Model -> List Block
                            updtListBlock model_ =
                                ListBlock model_
                                    ( parseRawLines ( [ unindentedRawLine ], blocks_ )
                                        :: blocksListTail
                                    ) :: blocksTail

                        in case blocks_ of
                            -- A list item can begin with at most
                            -- one blank line without begin loose.
                            [ BlankBlock ] ->
                                updtListBlock model

                            BlankBlock :: blocksTail_ ->
                                if List.all (\b -> b == BlankBlock) blocksTail_ then
                                    parseRawLine rawLine blocks

                                else
                                    updtListBlock { model | isLoose = True }

                            ListBlock model_ blocksList_ :: blocksTail_ ->
                                if Lists.indentLength unindentedRawLine >= model_.indentLength then
                                    updtListBlock model

                                else
                                    if isBlankBlockLast blocksList_ then
                                        updtListBlock { model | isLoose = True }

                                    else
                                        updtListBlock model

                            _ ->
                                updtListBlock model

                    [] ->
                        ListBlock model
                            ( [ parseRawLines ( [ Code.indentLine model.indentLength rawLine ], [] ) ]
                            ) :: blocksTail


            else
                parseRawLine rawLine blocks


        -- No need to typify the line if Fenced CodeBlock
        -- is open, just check for closing fence.
        CodeBlock (Code.Fenced (True, fence, lines_)) :: blocksTail ->
            Code.continueOrCloseFence fence lines_ rawLine
                |> CodeBlock
                |> \codeBlock -> codeBlock :: blocksTail


        _ ->
            parseRawLine rawLine blocks


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
        -- BlankLine after Indented CodeBlock may be added
        -- to the CodeBlock
        CodeBlock ( Code.Indented indentedModel )
            :: blocksTail ->
                Code.addBlankLineToIndented match.match indentedModel
                    |> CodeBlock
                    |> \b -> b :: blocksTail


        CodeBlock ( Code.Fenced ( True, fence, pCode ) )
            :: blocksTail ->
                Code.addBlankLineToFenced match.match ( True, fence, pCode )
                    |> CodeBlock
                    |> \b -> b :: blocksTail


        ListBlock model blocksList :: blocksTail ->
            ListBlock
                model
                (addBlankLineToBlocksList match blocksList)
                    :: blocksTail


        _ ->
            BlankBlock :: blocks


addBlankLineToBlocksList : Regex.Match -> List (List Block) -> List (List Block)
addBlankLineToBlocksList match blocksList =
    case blocksList of
        blocks :: blocksListTail ->
            parseBlankLine match blocks
                :: blocksListTail

        [] ->
            [ [ BlankBlock ] ]


parseSetextHeadingLine : Regex.Match -> List Block -> List Block
parseSetextHeadingLine match blocks =
    let ( lvl, str ) =
        Heading.setextMatch match

    in case blocks of
        -- Only occurs after ParagraphBlock.
        ParagraphBlock paragraph :: blocksTail ->
            HeadingBlock ( lvl, paragraph ) :: blocksTail

        _ ->
            -- If used marker is "=", always parse as TextLine.
            if lvl == 1 then
                parseTextLine match.match blocks

            -- If used marker is "-" and length is 1, it's
            -- an empty ListLine.
            else if str == "-" then
                parseListLine Lists.Unordered match blocks

            -- If matches with thematic break line regex, it's
            -- a ThematicBreakBlock. E.g.: "--" does not match.
            else if Regex.contains thematicBreakLineRegex match.match then
                ThematicBreakBlock :: blocks

            -- Otherwise, parse as TextLine
            else
                parseTextLine match.match blocks


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

        parsedRawLine =
            parseRawLines ( [ rawLine ], [] )

        newListBlock =
            ListBlock lineModel [ parsedRawLine ] :: blocks

    in case blocks of
        ListBlock blockModel blocksList :: blocksTail ->
            if lineModel.delimiter == blockModel.delimiter then
                ListBlock
                    { blockModel
                        | indentLength = lineModel.indentLength
                        , isLoose =
                            blockModel.isLoose
                                || isBlankBlockLast blocksList
                    }
                    ( parsedRawLine :: blocksList )
                        :: blocksTail

            else
                newListBlock

        ParagraphBlock paragraph :: blocksTail ->
            -- Empty list item cannot interrupt a paragraph.
            if parsedRawLine == [ BlankBlock ] then
                ParagraphBlock
                    ( paragraph ++ [ match.match ] )
                        :: blocksTail

            else
                case lineModel.type_ of
                    -- Ordered list with start 1 can interrupt.
                    Lists.Ordered 1 ->
                        newListBlock

                    Lists.Ordered int ->
                        ParagraphBlock
                            ( paragraph ++ [ match.match ] )
                                :: blocksTail

                    _ ->
                        newListBlock

        _ ->
            newListBlock


isBlankBlockLast : List (List Block) -> Bool
isBlankBlockLast blocksList =
    case blocksList of
        blocks :: blocksListTail ->
            case blocks of
                -- Ignore if it's an empty list item (example 242)
                BlankBlock :: [] ->
                    False

                BlankBlock :: _ ->
                    True

                ListBlock _ blocksList_ :: _ ->
                    isBlankBlockLast blocksList_

                _ ->
                    False
        
        [] ->
            False


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


        ListBlock model blocksList :: blocksTail ->
            case blocksList of
                blocks_ :: blocksListTail ->
                    maybeContinueParagraph rawLine blocks_
                        |> Maybe.map
                            (\updtBlocks_ ->
                                ListBlock model
                                    ( updtBlocks_ :: blocksListTail )
                                        :: blocksTail
                            )

                _ ->
                    Nothing

        _ ->
            Nothing


blockToHtml : Bool -> Block -> List (Html msg)
blockToHtml textAsParagraph block =
    case block of
        HeadingBlock ( lvl, heading ) ->
            case lvl of
                1 -> [ h1 [] [ text (concatLines heading) ] ]
                2 -> [ h2 [] [ text (concatLines heading) ] ]
                3 -> [ h3 [] [ text (concatLines heading) ] ]
                4 -> [ h4 [] [ text (concatLines heading) ] ]
                5 -> [ h5 [] [ text (concatLines heading) ] ]
                _ -> [ h6 [] [ text (concatLines heading) ] ]

        ThematicBreakBlock ->
            [ hr [] [] ]

        ParagraphBlock lines ->
            if textAsParagraph then
                [ p [] (linesToHtml lines) ]

            else
                linesToHtml lines

        CodeBlock codeBlock ->
            [ Code.view codeBlock ]

        BlockQuote blocks ->
            List.map (blockToHtml True) blocks
                |> List.concat
                |> BlockQuote.view
                |> (\bq -> [ bq ] )

        ListBlock model blocks ->
            List.map
                ( List.map (blockToHtml (model.isLoose))
                    >> List.concat
                    >> li []
                ) blocks
                |> Lists.view model
                |> (\list -> [ list ] )

        BlankBlock ->
            []


type Inline
    = Normal String
    | HardBreakLine


linesToHtml : List String -> List (Html msg)
linesToHtml =
    List.map hardBreakLine
        >> List.concat
        >> List.foldl
            (\inline inlines ->
                case inline of
                    Normal str ->
                        case inlines of
                            Normal str_ :: inlinesTail ->
                                Normal (str_ ++ "\n" ++ str)
                                    :: inlinesTail

                            _ ->
                                Normal str :: inlines

                    HardBreakLine ->
                        HardBreakLine :: inlines

            ) []
        >> (\reversedInlines ->
                case reversedInlines of
                    HardBreakLine :: reversedInlinesTail ->
                        reversedInlinesTail

                    _ ->
                        reversedInlines
            )
        >> List.reverse
        >> List.map inlineToHtml


hardBreakLine : String -> List Inline
hardBreakLine line =
    if String.endsWith "  " line then
        [ Normal
            <| String.trim line
        , HardBreakLine
        ]

    else if String.endsWith "\\" line then
        [ Normal
            <| String.trim
            <| String.dropRight 1 line
        , HardBreakLine
        ]

    else
        [ Normal
            <| String.trim line
        ]


inlineToHtml : Inline -> Html msg
inlineToHtml inline =
    case inline of
        Normal normal ->
            text normal

        HardBreakLine ->
            br [] []


concatLines : List String -> String
concatLines =
    List.map String.trim
        >> List.intersperse "\n"
        >> String.concat


reverseBlocks : List Block -> List Block
reverseBlocks =
    List.reverse
        >> List.map reverseContainedBlock


reverseContainedBlock : Block -> Block
reverseContainedBlock block =
    case block of
        ListBlock model blocksList ->
            List.reverse blocksList
                |> List.map reverseBlocks
                |> ListBlock model

        BlockQuote blocks ->
           reverseBlocks blocks
                |> BlockQuote

        block ->
            block


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    toBlocks "# Heading title" == [ h1 [] [ text "Heading title"" ] ]
-}
toBlocks : String -> List Block
toBlocks rawText =
    ( toRawLines rawText, [] )
        |> parseRawLines
        |> reverseBlocks


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

    toHtml "# Heading title" == [ h1 [] [ text "Heading title"" ] ]
-}
toHtml : String -> List (Html msg)
toHtml =
    toBlocks
        >> List.map (blockToHtml True)
        >> List.concat

