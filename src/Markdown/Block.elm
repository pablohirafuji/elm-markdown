module Markdown.Block
    exposing
        ( Block(..)
        , CodeBlock(..)
        , Fence
        , ListBlock
        , ListType(..)
        , defaultHtml
        , parse
        , query
        , queryInlines
        , toHtml
        , walk
        , walkInlines
        )

{-| Block parsing, rendering and helpers.


# Model

@docs Block, CodeBlock, Fence, ListBlock, ListType


# Parsing

@docs parse


# Rendering

@docs toHtml, defaultHtml


# Helpers

@docs walk, walkInlines, query, queryInlines

-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, start)
import Markdown.Config exposing (Options, defaultOptions)
import Markdown.Helpers exposing (References, formatStr, ifError, indentLength, indentLine, insideSquareBracketRegex, prepareRefLabel, returnFirstJust, titleRegex)
import Markdown.Inline as Inline exposing (Inline(..))
import Markdown.InlineParser as Inline
import Regex exposing (Regex)


-- Model


{-| The block type.

  - **BlankLine** | _Text_
  - **ThematicBreak**
  - **Heading** | _Raw text_ | _Level_ | _Inlines_
  - **CodeBlock** | _CodeBlock_ | _Code_
  - **Paragraph** | _Raw text_ | _Inlines_
  - **BlockQuote** | _Blocks_
  - **List** | _ListBlock_ | _Items_
  - **PlainInlines** | _Inlines_
  - **Custom** | _Custom type_ | _Blocks_

-}
type Block b i
    = BlankLine String
    | ThematicBreak
    | Heading String Int (List (Inline i))
    | CodeBlock CodeBlock String
    | Paragraph String (List (Inline i))
    | BlockQuote (List (Block b i))
    | List ListBlock (List (List (Block b i)))
    | PlainInlines (List (Inline i))
    | Custom b (List (Block b i))


{-| CodeBlock type.

  - **Indented**
  - **Fenced** | _Is fence open?_ | _Fence_

-}
type CodeBlock
    = Indented
    | Fenced Bool Fence -- isOpen Fence


{-| Code fence model.
-}
type alias Fence =
    { indentLength : Int
    , fenceLength : Int
    , fenceChar : String
    , language : Maybe String
    }


{-| List model.
-}
type alias ListBlock =
    { type_ : ListType
    , indentLength : Int
    , delimiter : String
    , isLoose : Bool
    }


{-| Types of list.

  - **Unordered**
  - **Ordered** | _Start_

-}
type ListType
    = Unordered
    | Ordered Int



-- Parser


{-| Turn a markdown string into a list of blocks.

    blocks : List (Block b i)
    blocks =
        parse Nothing "# Heading with *emphasis*"

It's the same of:

    blocks : List (Block b i)
    blocks =
        [ Heading "Heading with *emphasis*"
            1
            [ Text "Heading with "
            , Emphasis 1
                [ Text "emphasis" ]
            ]
        ]

**Note:** If `Maybe Options` is `Nothing`,
`Config.defaultOptions` will be used.

-}
parse : Maybe Options -> String -> List (Block b i)
parse maybeOptions =
    String.lines
        >> (\a -> incorporateLines a [])
        >> parseReferences Dict.empty
        >> parseInlines maybeOptions True


incorporateLines : List String -> List (Block b i) -> List (Block b i)
incorporateLines rawLines ast =
    case rawLines of
        [] ->
            ast

        rawLine :: rawLinesTail ->
            -- To get tail call optimization, use parentheses, not |> or <|.
            -- Technically |> and <| are function calls.
            -- If you use them, they will be in tail position, not the recursive call!
            incorporateLines rawLinesTail
                (incorporateLine rawLine ast)


incorporateLine : String -> List (Block b i) -> List (Block b i)
incorporateLine rawLine ast =
    case ast of
        -- No need to typify the line if Fenced Code
        -- is open, just check for closing fence.
        (CodeBlock (Fenced True fence) code) :: astTail ->
            continueOrCloseCodeFence fence code rawLine
                |> (\a -> (::) a astTail)

        (List model items) :: astTail ->
            if indentLength rawLine >= model.indentLength then
                parseIndentedListLine rawLine model items ast astTail
            else
                -- After a list, check for lists before
                -- indented code.
                -- When both a thematic break and a list item are
                -- possible interpretations of a line, the
                -- thematic break takes precedence.
                checkThematicBreakLine ( rawLine, ast )
                    |> ifError checkListLine
                    |> ifError checkBlankLine
                    |> ifError checkIndentedCode
                    |> ifError checkOpenCodeFenceLine
                    |> ifError checkSetextHeadingLine
                    |> ifError checkATXHeadingLine
                    |> ifError checkBlockQuote
                    |> Result.withDefault (parseTextLine rawLine ast)

        _ ->
            parseRawLine rawLine ast



-- Default parsing precedence


parseRawLine : String -> List (Block b i) -> List (Block b i)
parseRawLine rawLine ast =
    checkBlankLine ( rawLine, ast )
        |> ifError checkIndentedCode
        |> ifError checkOpenCodeFenceLine
        |> ifError checkSetextHeadingLine
        |> ifError checkATXHeadingLine
        |> ifError checkBlockQuote
        |> ifError checkThematicBreakLine
        |> ifError checkListLine
        |> Result.withDefault (parseTextLine rawLine ast)



-- BlankLine


checkBlankLine : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkBlankLine ( rawLine, ast ) =
    Regex.findAtMost 1 blankLineRegex rawLine
        |> List.head
        |> Maybe.map (parseBlankLine ast)
        |> Result.fromMaybe ( rawLine, ast )


blankLineRegex : Regex
blankLineRegex =
    Regex.fromString "^\\s*$"
        |> Maybe.withDefault Regex.never


parseBlankLine : List (Block b i) -> Regex.Match -> List (Block b i)
parseBlankLine ast match =
    case ast of
        (CodeBlock (Fenced True fence) code) :: astTail ->
            code
                ++ "\n"
                |> CodeBlock (Fenced True fence)
                |> (\a -> (::) a astTail)

        (List model items) :: astTail ->
            List model (addBlankLineToListBlock match items)
                :: astTail

        _ ->
            BlankLine match.match :: ast


addBlankLineToListBlock : Regex.Match -> List (List (Block b i)) -> List (List (Block b i))
addBlankLineToListBlock match asts =
    case asts of
        [] ->
            [ [ BlankLine match.match ] ]

        ast :: astsTail ->
            parseBlankLine ast match
                :: astsTail



-- ATX Heading


checkATXHeadingLine : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkATXHeadingLine ( rawLine, ast ) =
    Regex.findAtMost 1 atxHeadingLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractATXHeadingRM
        |> Maybe.map (\a -> (::) a ast)
        |> Result.fromMaybe ( rawLine, ast )


atxHeadingLineRegex : Regex
atxHeadingLineRegex =
    Regex.fromString
        ("^ {0,3}(#{1,6})"
            ++ "(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)"
            ++ "(.*?)(?:\\s+[ \\t#]*)?$"
        )
        |> Maybe.withDefault Regex.never


extractATXHeadingRM : Regex.Match -> Maybe (Block b i)
extractATXHeadingRM match =
    case match.submatches of
        (Just lvl) :: maybeHeading :: _ ->
            Heading (Maybe.withDefault "" maybeHeading) (String.length lvl) []
                |> Just

        _ ->
            Nothing



-- Setext Heading


checkSetextHeadingLine : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkSetextHeadingLine ( rawLine, ast ) =
    Regex.findAtMost 1 setextHeadingLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractSetextHeadingRM
        |> Maybe.andThen (parseSetextHeadingLine rawLine ast)
        |> Result.fromMaybe ( rawLine, ast )


setextHeadingLineRegex : Regex
setextHeadingLineRegex =
    Regex.fromString "^ {0,3}(=+|-+)[ \\t]*$"
        |> Maybe.withDefault Regex.never


extractSetextHeadingRM : Regex.Match -> Maybe ( Int, String )
extractSetextHeadingRM match =
    case match.submatches of
        (Just delimiter) :: _ ->
            if String.startsWith "=" delimiter then
                Just ( 1, delimiter )
            else
                Just ( 2, delimiter )

        _ ->
            Nothing


parseSetextHeadingLine : String -> List (Block b i) -> ( Int, String ) -> Maybe (List (Block b i))
parseSetextHeadingLine rawLine ast ( lvl, delimiter ) =
    case ast of
        -- Only occurs after a paragraph
        (Paragraph rawText _) :: astTail ->
            Heading rawText lvl []
                :: astTail
                |> Just

        _ ->
            Nothing



-- Thematic Break


checkThematicBreakLine : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkThematicBreakLine ( rawLine, ast ) =
    Regex.findAtMost 1 thematicBreakLineRegex rawLine
        |> List.head
        |> Maybe.map (\_ -> ThematicBreak :: ast)
        |> Result.fromMaybe ( rawLine, ast )


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.fromString
        ("^ {0,3}(?:"
            ++ "(?:\\*[ \\t]*){3,}"
            ++ "|(?:_[ \\t]*){3,}"
            ++ "|(?:-[ \\t]*){3,})[ \\t]*$"
        )
        |> Maybe.withDefault Regex.never



-- Block Quote


checkBlockQuote : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkBlockQuote ( rawLine, ast ) =
    Regex.findAtMost 1 blockQuoteLineRegex rawLine
        |> List.head
        |> Maybe.map
            (.submatches
                >> List.head
                >> Maybe.withDefault Nothing
                >> Maybe.withDefault ""
            )
        |> Maybe.map (parseBlockQuoteLine ast)
        |> Result.fromMaybe ( rawLine, ast )


blockQuoteLineRegex : Regex
blockQuoteLineRegex =
    Regex.fromString "^ {0,3}(?:>[ ]?)(.*)$"
        |> Maybe.withDefault Regex.never


parseBlockQuoteLine : List (Block b i) -> String -> List (Block b i)
parseBlockQuoteLine ast rawLine =
    case ast of
        (BlockQuote bqAST) :: astTail ->
            incorporateLine rawLine bqAST
                |> BlockQuote
                |> (\a -> (::) a astTail)

        _ ->
            incorporateLine rawLine []
                |> BlockQuote
                |> (\a -> (::) a ast)



-- Indented Code


checkIndentedCode : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkIndentedCode ( rawLine, ast ) =
    Regex.findAtMost 1 indentedCodeLineRegex rawLine
        |> List.head
        |> Maybe.map (.submatches >> List.head)
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault Nothing
        |> Maybe.map (parseIndentedCodeLine ast)
        |> Result.fromMaybe ( rawLine, ast )


indentedCodeLineRegex : Regex
indentedCodeLineRegex =
    Regex.fromString "^(?: {4,4}| {0,3}\\t)(.*)$"
        |> Maybe.withDefault Regex.never


parseIndentedCodeLine : List (Block b i) -> String -> List (Block b i)
parseIndentedCodeLine ast codeLine =
    case ast of
        -- Continue indented code block
        (CodeBlock Indented codeStr) :: astTail ->
            codeStr
                ++ codeLine
                ++ "\n"
                |> CodeBlock Indented
                |> (\a -> (::) a astTail)

        -- Possible blankline inside a indented code block
        (BlankLine blankStr) :: astTail ->
            [ blankStr ]
                |> blocksAfterBlankLines astTail
                |> resumeIndentedCodeBlock codeLine
                |> Maybe.withDefault
                    (codeLine
                        ++ "\n"
                        |> CodeBlock Indented
                        |> (\a -> (::) a ast)
                    )

        -- Continue paragraph or New indented code block
        _ ->
            maybeContinueParagraph codeLine ast
                |> Maybe.withDefault
                    (codeLine
                        ++ "\n"
                        |> CodeBlock Indented
                        |> (\a -> (::) a ast)
                    )



-- Return the blocks after blanklines
-- and the blanklines content in between


blocksAfterBlankLines : List (Block b i) -> List String -> ( List (Block b i), List String )
blocksAfterBlankLines ast blankLines =
    case ast of
        (BlankLine blankStr) :: astTail ->
            blocksAfterBlankLines astTail
                (blankStr :: blankLines)

        _ ->
            ( ast, blankLines )


resumeIndentedCodeBlock : String -> ( List (Block b i), List String ) -> Maybe (List (Block b i))
resumeIndentedCodeBlock codeLine ( remainBlocks, blankLines ) =
    case remainBlocks of
        (CodeBlock Indented codeStr) :: remainBlocksTail ->
            blankLines
                |> List.map (\bl -> indentLine 4 bl ++ "\n")
                |> String.concat
                |> (++) codeStr
                |> (\a -> (++) a (codeLine ++ "\n"))
                |> CodeBlock Indented
                |> (\a -> (::) a remainBlocksTail)
                |> Just

        _ ->
            Nothing



-- Fenced Code


checkOpenCodeFenceLine : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkOpenCodeFenceLine ( rawLine, ast ) =
    Regex.findAtMost 1 openCodeFenceLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractOpenCodeFenceRM
        |> Maybe.map (\f -> CodeBlock f "")
        |> Maybe.map (\a -> (::) a ast)
        |> Result.fromMaybe ( rawLine, ast )


openCodeFenceLineRegex : Regex
openCodeFenceLineRegex =
    Regex.fromString "^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$"
        |> Maybe.withDefault Regex.never


extractOpenCodeFenceRM : Regex.Match -> Maybe CodeBlock
extractOpenCodeFenceRM match =
    case match.submatches of
        maybeIndent :: (Just fence) :: maybeLanguage :: _ ->
            Fenced True
                { indentLength =
                    Maybe.map String.length maybeIndent
                        |> Maybe.withDefault 0
                , fenceLength = String.length fence
                , fenceChar = String.left 1 fence
                , language =
                    Maybe.map String.words maybeLanguage
                        |> Maybe.withDefault []
                        |> List.head
                        |> Maybe.andThen
                            (\lang ->
                                if lang == "" then
                                    Nothing
                                else
                                    Just lang
                            )
                        |> Maybe.map formatStr
                }
                |> Just

        _ ->
            Nothing


continueOrCloseCodeFence : Fence -> String -> String -> Block b i
continueOrCloseCodeFence fence previousCode rawLine =
    if isCloseFenceLine fence rawLine then
        CodeBlock (Fenced False fence) previousCode
    else
        previousCode
            ++ indentLine fence.indentLength rawLine
            ++ "\n"
            |> CodeBlock (Fenced True fence)


isCloseFenceLine : Fence -> String -> Bool
isCloseFenceLine fence =
    Regex.findAtMost 1 closeCodeFenceLineRegex
        >> List.head
        >> Maybe.map (isCloseFenceLineHelp fence)
        >> Maybe.withDefault False


closeCodeFenceLineRegex : Regex
closeCodeFenceLineRegex =
    Regex.fromString "^ {0,3}(`{3,}|~{3,})\\s*$"
        |> Maybe.withDefault Regex.never


isCloseFenceLineHelp : Fence -> Regex.Match -> Bool
isCloseFenceLineHelp fence match =
    case match.submatches of
        (Just fenceStr) :: _ ->
            String.length fenceStr
                >= fence.fenceLength
                && String.left 1 fenceStr
                == fence.fenceChar

        _ ->
            False



-- List


parseIndentedListLine : String -> ListBlock -> List (List (Block b i)) -> List (Block b i) -> List (Block b i) -> List (Block b i)
parseIndentedListLine rawLine model items ast astTail =
    case items of
        [] ->
            indentLine model.indentLength rawLine
                |> (\a -> incorporateLine a [])
                |> (\a -> (::) a [])
                |> List model
                |> (\a -> (::) a astTail)

        item :: itemsTail ->
            let
                indentedRawLine : String
                indentedRawLine =
                    indentLine model.indentLength rawLine

                updateList : ListBlock -> List (Block b i)
                updateList model_ =
                    incorporateLine indentedRawLine item
                        |> (\a -> (::) a itemsTail)
                        |> List model_
                        |> (\a -> (::) a astTail)
            in
            case item of
                -- A list item can begin with at most
                -- one blank line without begin loose.
                (BlankLine _) :: [] ->
                    updateList model

                (BlankLine _) :: itemTail ->
                    if
                        List.all
                            (\block ->
                                case block of
                                    BlankLine _ ->
                                        True

                                    _ ->
                                        False
                            )
                            itemTail
                    then
                        parseRawLine rawLine ast
                    else
                        updateList { model | isLoose = True }

                (List model_ items_) :: itemTail ->
                    if
                        indentLength indentedRawLine
                            >= model_.indentLength
                    then
                        updateList model
                    else if isBlankLineLast items_ then
                        updateList { model | isLoose = True }
                    else
                        updateList model

                _ ->
                    updateList model


checkListLine : ( String, List (Block b i) ) -> Result ( String, List (Block b i) ) (List (Block b i))
checkListLine ( rawLine, ast ) =
    checkOrderedListLine rawLine
        |> ifError checkUnorderedListLine
        |> Result.map calcListIndentLength
        |> Result.map (parseListLine rawLine ast)
        |> Result.mapError (\e -> ( e, ast ))



-- Ordered list


checkOrderedListLine : String -> Result String ( ListBlock, String, String )
checkOrderedListLine rawLine =
    Regex.findAtMost 1 orderedListLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractOrderedListRM
        |> Result.fromMaybe rawLine


orderedListLineRegex : Regex
orderedListLineRegex =
    Regex.fromString "^( *(\\d{1,9})([.)])( {0,4}))(?:[ \\t](.*))?$"
        |> Maybe.withDefault Regex.never


extractOrderedListRM : Regex.Match -> Maybe ( ListBlock, String, String )
extractOrderedListRM match =
    case match.submatches of
        (Just indentString) :: (Just start) :: (Just delimiter) :: maybeIndentSpace :: maybeRawLine :: _ ->
            ( { type_ =
                    String.toInt start
                        |> Maybe.map Ordered
                        |> Maybe.withDefault Unordered
              , indentLength = String.length indentString + 1
              , delimiter = delimiter
              , isLoose = False
              }
            , Maybe.withDefault "" maybeIndentSpace
            , Maybe.withDefault "" maybeRawLine
            )
                |> Just

        _ ->
            Nothing



-- Unordered list


checkUnorderedListLine : String -> Result String ( ListBlock, String, String )
checkUnorderedListLine rawLine =
    Regex.findAtMost 1 unorderedListLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractUnorderedListRM
        |> Result.fromMaybe rawLine


unorderedListLineRegex : Regex
unorderedListLineRegex =
    Regex.fromString "^( *([\\*\\-\\+])( {0,4}))(?:[ \\t](.*))?$"
        |> Maybe.withDefault Regex.never


extractUnorderedListRM : Regex.Match -> Maybe ( ListBlock, String, String )
extractUnorderedListRM match =
    case match.submatches of
        (Just indentString) :: (Just delimiter) :: maybeIndentSpace :: maybeRawLine :: [] ->
            ( { type_ = Unordered
              , indentLength = String.length indentString + 1
              , delimiter = delimiter
              , isLoose = False
              }
            , Maybe.withDefault "" maybeIndentSpace
            , Maybe.withDefault "" maybeRawLine
            )
                |> Just

        _ ->
            Nothing


calcListIndentLength : ( ListBlock, String, String ) -> ( ListBlock, String )
calcListIndentLength ( listBlock, indentSpace, rawLine ) =
    let
        indentSpaceLength : Int
        indentSpaceLength =
            String.length indentSpace

        isIndentedCode : Bool
        isIndentedCode =
            indentSpaceLength >= 4

        indentLength : Int
        indentLength =
            if
                isIndentedCode
                    || Regex.contains blankLineRegex rawLine
            then
                listBlock.indentLength - indentSpaceLength
            else
                listBlock.indentLength

        updtRawLine : String
        updtRawLine =
            if isIndentedCode then
                indentSpace ++ rawLine
            else
                rawLine
    in
    ( { listBlock | indentLength = indentLength }
    , updtRawLine
    )


parseListLine : String -> List (Block b i) -> ( ListBlock, String ) -> List (Block b i)
parseListLine rawLine ast ( listBlock, listRawLine ) =
    let
        parsedRawLine : List (Block b i)
        parsedRawLine =
            incorporateLine listRawLine []

        newList : List (Block b i)
        newList =
            List listBlock [ parsedRawLine ] :: ast
    in
    case ast of
        (List model items) :: astTail ->
            if listBlock.delimiter == model.delimiter then
                parsedRawLine
                    :: items
                    |> List
                        { model
                            | indentLength =
                                listBlock.indentLength
                            , isLoose =
                                model.isLoose
                                    || isBlankLineLast items
                        }
                    |> (\a -> (::) a astTail)
            else
                newList

        (Paragraph rawText inlines) :: astTail ->
            case parsedRawLine of
                (BlankLine _) :: [] ->
                    -- Empty list item cannot interrupt a paragraph.
                    addToParagraph rawText rawLine
                        :: astTail

                _ ->
                    case listBlock.type_ of
                        -- Ordered list with start 1 can interrupt.
                        Ordered 1 ->
                            newList

                        Ordered int ->
                            addToParagraph rawText rawLine
                                :: astTail

                        _ ->
                            newList

        _ ->
            newList


isBlankLineLast : List (List (Block b i)) -> Bool
isBlankLineLast items =
    case items of
        [] ->
            False

        item :: itemsTail ->
            case item of
                -- Ignore if it's an empty list item (example 242)
                (BlankLine _) :: [] ->
                    False

                (BlankLine _) :: _ ->
                    True

                (List _ items_) :: _ ->
                    isBlankLineLast items_

                _ ->
                    False



-- Paragraph


parseTextLine : String -> List (Block b i) -> List (Block b i)
parseTextLine rawLine ast =
    maybeContinueParagraph rawLine ast
        |> Maybe.withDefault
            (Paragraph (formatParagraphLine rawLine) [] :: ast)


addToParagraph : String -> String -> Block b i
addToParagraph paragraph rawLine =
    Paragraph
        (paragraph ++ "\n" ++ formatParagraphLine rawLine)
        []


formatParagraphLine : String -> String
formatParagraphLine rawParagraph =
    if String.right 2 rawParagraph == "  " then
        String.trim rawParagraph ++ "  "
    else
        String.trim rawParagraph


maybeContinueParagraph : String -> List (Block b i) -> Maybe (List (Block b i))
maybeContinueParagraph rawLine ast =
    case ast of
        (Paragraph paragraph _) :: astTail ->
            addToParagraph paragraph rawLine
                :: astTail
                |> Just

        (BlockQuote bqAST) :: astTail ->
            maybeContinueParagraph rawLine bqAST
                |> Maybe.map
                    (\updtBqAST ->
                        BlockQuote updtBqAST :: astTail
                    )

        (List model items) :: astTail ->
            case items of
                itemAST :: itemASTTail ->
                    maybeContinueParagraph rawLine itemAST
                        |> Maybe.map
                            ((\a -> (::) a itemASTTail)
                                >> List model
                                >> (\a -> (::) a astTail)
                            )

                _ ->
                    Nothing

        _ ->
            Nothing



-- References


type alias LinkMatch =
    { matchLength : Int
    , inside : String
    , url : String
    , maybeTitle : Maybe String
    }


parseReferences : References -> List (Block b i) -> ( References, List (Block b i) )
parseReferences refs =
    List.foldl parseReferencesHelp ( refs, [] )


parseReferencesHelp : Block b i -> ( References, List (Block b i) ) -> ( References, List (Block b i) )
parseReferencesHelp block ( refs, parsedAST ) =
    case block of
        Paragraph rawText _ ->
            let
                ( paragraphRefs, maybeUpdtText ) =
                    parseReference Dict.empty rawText

                updtRefs =
                    Dict.union paragraphRefs refs
            in
            case maybeUpdtText of
                Just updtText ->
                    ( updtRefs
                    , Paragraph updtText []
                        :: parsedAST
                    )

                Nothing ->
                    ( updtRefs, parsedAST )

        List model items ->
            let
                ( updtRefs, updtItems ) =
                    List.foldl
                        (\item ( refs__, parsedItems ) ->
                            parseReferences refs__ item
                                |> Tuple.mapSecond
                                    (\a -> (::) a parsedItems)
                        )
                        ( refs, [] )
                        items
            in
            ( updtRefs
            , List model updtItems
                :: parsedAST
            )

        BlockQuote blocks ->
            parseReferences refs blocks
                |> Tuple.mapSecond BlockQuote
                |> Tuple.mapSecond (\a -> (::) a parsedAST)

        Custom customBlock blocks ->
            parseReferences refs blocks
                |> Tuple.mapSecond (Custom customBlock)
                |> Tuple.mapSecond (\a -> (::) a parsedAST)

        _ ->
            ( refs, block :: parsedAST )


parseReference : References -> String -> ( References, Maybe String )
parseReference refs rawText =
    case maybeLinkMatch rawText of
        Just linkMatch ->
            let
                maybeStrippedText =
                    dropRefString rawText linkMatch

                updtRefs =
                    insertLinkMatch refs linkMatch
            in
            case maybeStrippedText of
                Just strippedText ->
                    parseReference updtRefs strippedText

                Nothing ->
                    ( updtRefs, Nothing )

        Nothing ->
            ( refs, Just rawText )


extractUrlTitleRegex : Regex.Match -> Maybe LinkMatch
extractUrlTitleRegex regexMatch =
    case regexMatch.submatches of
        (Just rawText) :: maybeRawUrlAngleBrackets :: maybeRawUrlWithoutBrackets :: maybeTitleSingleQuotes :: maybeTitleDoubleQuotes :: maybeTitleParenthesis :: _ ->
            let
                maybeRawUrl : Maybe String
                maybeRawUrl =
                    returnFirstJust
                        [ maybeRawUrlAngleBrackets
                        , maybeRawUrlWithoutBrackets
                        ]

                toReturn : String -> LinkMatch
                toReturn rawUrl =
                    { matchLength = String.length regexMatch.match
                    , inside = rawText
                    , url = rawUrl
                    , maybeTitle =
                        returnFirstJust
                            [ maybeTitleSingleQuotes
                            , maybeTitleDoubleQuotes
                            , maybeTitleParenthesis
                            ]
                    }
            in
            maybeRawUrl
                |> Maybe.map toReturn

        _ ->
            Nothing


hrefRegex : String
hrefRegex =
    "\\s*(?:<([^<>\\s]*)>|([^\\s]*))"


refRegex : Regex
refRegex =
    Regex.fromString
        ("^\\s*\\[("
            ++ insideSquareBracketRegex
            ++ ")\\]:"
            ++ hrefRegex
            ++ titleRegex
            ++ "\\s*(?![^\\n])"
        )
        |> Maybe.withDefault Regex.never


insertLinkMatch : References -> LinkMatch -> References
insertLinkMatch refs linkMatch =
    if Dict.member linkMatch.inside refs then
        refs
    else
        Dict.insert
            linkMatch.inside
            ( linkMatch.url, linkMatch.maybeTitle )
            refs


dropRefString : String -> LinkMatch -> Maybe String
dropRefString rawText inlineMatch =
    let
        strippedText =
            String.dropLeft inlineMatch.matchLength rawText
    in
    if Regex.contains blankLineRegex strippedText then
        Nothing
    else
        Just strippedText


maybeLinkMatch : String -> Maybe LinkMatch
maybeLinkMatch rawText =
    Regex.findAtMost 1 refRegex rawText
        |> List.head
        |> Maybe.andThen extractUrlTitleRegex
        |> Maybe.map
            (\linkMatch ->
                { linkMatch
                    | inside =
                        prepareRefLabel linkMatch.inside
                }
            )
        |> Maybe.andThen
            (\linkMatch ->
                if linkMatch.url == "" || linkMatch.inside == "" then
                    Nothing
                else
                    Just linkMatch
            )



-- Parse Inlines


parseInlines : Maybe Options -> Bool -> ( References, List (Block b i) ) -> List (Block b i)
parseInlines maybeOptions textAsParagraph ( refs, blocks ) =
    List.map
        (parseInline maybeOptions textAsParagraph refs)
        blocks


parseInline : Maybe Options -> Bool -> References -> Block b i -> Block b i
parseInline maybeOptions textAsParagraph refs block =
    let
        options : Options
        options =
            Maybe.withDefault defaultOptions maybeOptions
    in
    case block of
        Heading rawText lvl _ ->
            Inline.parse options refs rawText
                |> Heading rawText lvl

        Paragraph rawText _ ->
            let
                inlines : List (Inline i)
                inlines =
                    Inline.parse options refs rawText
            in
            case inlines of
                (HtmlInline _ _ _) :: [] ->
                    PlainInlines inlines

                _ ->
                    if textAsParagraph then
                        Paragraph rawText inlines
                    else
                        PlainInlines inlines

        BlockQuote blocks ->
            parseInlines maybeOptions True ( refs, blocks )
                |> BlockQuote

        List model items ->
            parseInlines maybeOptions model.isLoose
                << (\b -> ( refs, b ))
                |> (\a -> List.map a items)
                |> List model

        Custom customBlock blocks ->
            parseInlines maybeOptions True ( refs, blocks )
                |> Custom customBlock

        _ ->
            block



-- Html Renderer


{-| Transform a Block into a list of Html
using the default html elements.

    import Html exposing (Html, div)
    import Markdown.Block as Block

    view : Html msg
    view =
        myMarkdownString
            |> Block.parse Nothing
            -- using Config.defaultOptions
            |> List.map Block.toHtml
            |> List.concat
            |> div []

-}
toHtml : Block b i -> List (Html msg)
toHtml =
    defaultHtml Nothing Nothing


{-| If you want to customize the html output,
this function will help you.

Transform a block into a list of Html, optionally
using custom html elements to render inner blocks
or/and inlines.

Example of rendering:

  - All blockquotes as a detail element;
  - Images using figure and figcaption;
  - Links not starting with `http://elm-lang.org` with a `target="_blank"` attribute.

```
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))

view : Html msg
view =
    myMarkdownString
        |> Block.parse Nothing
        -- using Config.defaultOptions
        |> List.map customHtmlBlock
        |> List.concat
        |> article []

customHtmlBlock : Block b i -> List (Html msg)
customHtmlBlock block =
    case block of
        BlockQuote blocks ->
            List.map customHtmlBlock blocks
                |> List.concat
                |> details []
                |> (\a -> (::) a [])

        _ ->
            Block.defaultHtml
                (Just customHtmlBlock)
                (Just customHtmlInline)
                block

customHtmlInline : Inline i -> Html msg
customHtmlInline inline =
    case inline of
        Image url maybeTitle inlines ->
            figure []
                [ img
                    [ alt (Inline.extractText inlines)
                    , src url
                    , title (Maybe.withDefault "" maybeTitle)
                    ]
                    []
                , figcaption []
                    [ text (Inline.extractText inlines) ]
                ]

        Link url maybeTitle inlines ->
            if String.startsWith "http://elm-lang.org" url then
                a
                    [ href url
                    , title (Maybe.withDefault "" maybeTitle)
                    ]
                    (List.map customHtmlInline inlines)
            else
                a
                    [ href url
                    , title (Maybe.withDefault "" maybeTitle)
                    , target "_blank"
                    , rel "noopener noreferrer"
                    ]
                    (List.map customHtmlInline inlines)

        _ ->
            Inline.defaultHtml (Just customHtmlInline) inline
```

**Note:** If both `Maybe` arguments are `Nothing`,
the default html elements will be used to render
the inner blocks and inlines.

-}
defaultHtml : Maybe (Block b i -> List (Html msg)) -> Maybe (Inline i -> Html msg) -> Block b i -> List (Html msg)
defaultHtml customHtml customInlineHtml block =
    let
        inlineToHtml : Inline i -> Html msg
        inlineToHtml =
            Maybe.withDefault
                Inline.toHtml
                customInlineHtml

        blockToHtml : Block b i -> List (Html msg)
        blockToHtml =
            Maybe.withDefault
                (defaultHtml Nothing customInlineHtml)
                customHtml
    in
    case block of
        BlankLine _ ->
            []

        Heading _ level inlines ->
            let
                hElement : List (Html msg) -> Html msg
                hElement =
                    case level of
                        1 ->
                            h1 []

                        2 ->
                            h2 []

                        3 ->
                            h3 []

                        4 ->
                            h4 []

                        5 ->
                            h5 []

                        _ ->
                            h6 []
            in
            [ hElement
                (List.map inlineToHtml inlines)
            ]

        ThematicBreak ->
            [ hr [] [] ]

        Paragraph _ inlines ->
            [ p [] (List.map inlineToHtml inlines) ]

        CodeBlock (Fenced _ model) codeStr ->
            let
                basicView : List (Html.Attribute msg) -> List (Html msg)
                basicView attrs =
                    [ pre []
                        [ code attrs
                            [ text codeStr ]
                        ]
                    ]
            in
            case model.language of
                Just language ->
                    basicView
                        [ class ("language-" ++ language) ]

                Nothing ->
                    basicView []

        CodeBlock Indented codeStr ->
            [ pre []
                [ code []
                    [ text codeStr ]
                ]
            ]

        BlockQuote blocks ->
            List.map blockToHtml blocks
                |> List.concat
                |> blockquote []
                |> (\a -> (::) a [])

        List model items ->
            List.map
                (List.map blockToHtml
                    >> List.concat
                    >> li []
                )
                items
                |> (case model.type_ of
                        Ordered startInt ->
                            if startInt == 1 then
                                ol []
                            else
                                ol [ start startInt ]

                        Unordered ->
                            ul []
                   )
                |> (\a -> (::) a [])

        PlainInlines inlines ->
            List.map inlineToHtml inlines

        Custom customBlock blocks ->
            List.map blockToHtml blocks
                |> List.concat
                |> (::) (text "Unhandled custom block.")
                |> div []
                |> (\a -> (::) a [])



-- Helpers


{-| Apply a function to every block whithin a block recursively.

Example of replacing all **level 3+ heading** to
**regular paragraphs**:

    import Html exposing (Html, section)
    import Markdown.Block as Block exposing (Block(..))

    view : Html msg
    view =
        myMarkdownString
            |> Block.parse Nothing
            -- using Config.defaultOptions
            |> List.map (Block.walk modHeader)
            |> List.map Block.toHtml
            |> List.concat
            |> section []

    modHeader : Block b i -> Block b i
    modHeader block =
        case block of
            Heading rawText level inlines ->
                if level >= 3 then
                    Paragraph rawText inlines
                else
                    block

            _ ->
                block

-}
walk : (Block b i -> Block b i) -> Block b i -> Block b i
walk function block =
    case block of
        BlockQuote blocks ->
            List.map (walk function) blocks
                |> BlockQuote
                |> function

        List listBlock items ->
            List.map (List.map (walk function)) items
                |> List listBlock
                |> function

        Custom customBlock blocks ->
            List.map (walk function) blocks
                |> Custom customBlock
                |> function

        _ ->
            function block


{-| Apply a function to every block's inline recursively.

Example of converting all **Text** to **UPPERCASE**:

    import Html exposing (Html, section)
    import Markdown.Block as Block exposing (Block(..))
    import Markdown.Inline exposing (Inline(..))

    view : Html msg
    view =
        myMarkdownString
            |> Block.parse Nothing
            |> List.map (Block.walkInlines upperText)
            |> List.map Block.toHtml
            |> List.concat
            |> section []

    upperText : Inline i -> Inline i
    upperText inline =
        case inline of
            Text str ->
                Text (String.toUpper str)

            _ ->
                inline

-}
walkInlines : (Inline i -> Inline i) -> Block b i -> Block b i
walkInlines function block =
    walk (walkInlinesHelp function) block


walkInlinesHelp : (Inline i -> Inline i) -> Block b i -> Block b i
walkInlinesHelp function block =
    case block of
        Paragraph rawText inlines ->
            List.map (Inline.walk function) inlines
                |> Paragraph rawText

        Heading rawText level inlines ->
            List.map (Inline.walk function) inlines
                |> Heading rawText level

        PlainInlines inlines ->
            List.map (Inline.walk function) inlines
                |> PlainInlines

        _ ->
            block


{-| Walks a block and applies a function for every block,
appending the results.

Example of getting all headings of a list of blocks:

    toc : List ( Int, String )
    toc =
        myMarkdownString
            |> Block.parse Nothing
            |> List.map (Block.query getHeader)
            |> List.concat

    getHeader : Block b i -> List ( Int, String )
    getHeader block =
        case block of
            Heading _ lvl inlines ->
                [ ( lvl, Inline.extractText inlines ) ]

            _ ->
                []

-}
query : (Block b i -> List a) -> Block b i -> List a
query function block =
    case block of
        BlockQuote blocks ->
            List.map (query function) blocks
                |> List.concat
                |> (++) (function (BlockQuote blocks))

        List listBlock items ->
            List.map (List.map (query function)) items
                |> List.concat
                |> List.concat
                |> (++) (function (List listBlock items))

        Custom customBlock blocks ->
            List.map (query function) blocks
                |> List.concat
                |> (++) (function (Custom customBlock blocks))

        _ ->
            function block


{-| Walks a block and applies a function for every inline,
appending the results.

Example of getting all links within a list of blocks:

    links : List String
    links =
        myMarkdownString
            |> Block.parse Nothing
            |> List.map (Block.queryInlines getLinks)
            |> List.concat

    getLinks : Inline i -> List String
    getLinks inline =
        case inline of
            Link url _ _ ->
                [ url ]

            _ ->
                []

-}
queryInlines : (Inline i -> List a) -> Block b i -> List a
queryInlines function block =
    query (queryInlinesHelp function) block


queryInlinesHelp : (Inline i -> List a) -> Block b i -> List a
queryInlinesHelp function block =
    case block of
        Paragraph _ inlines ->
            List.map (Inline.query function) inlines
                |> List.concat

        Heading _ _ inlines ->
            List.map (Inline.query function) inlines
                |> List.concat

        PlainInlines inlines ->
            List.map (Inline.query function) inlines
                |> List.concat

        _ ->
            []
