module Markdown exposing
    ( toHtml
    , toAST
    , withOptions
    )


{-| A pure Elm package for markdown parsing and rendering.

# Parsing Markdown
@docs toHtml, toAST

# Parsing with Custom Options
@docs withOptions

-}


import Dict
import Html exposing (Html, node, text, em, br, hr, a, img, code, li, em, strong, ol, ul)
import Html.Attributes exposing (class, href, alt, src, title, attribute, start)
import Regex exposing (Regex)
import Markdown.Inline as Inline exposing (ifNothing, References)
import Markdown.Config exposing (Elements, defaultElements, Options, defaultOptions)



----------------------------------------------------------------------
-------------------------------- Model -------------------------------
----------------------------------------------------------------------


type alias AST b i =
    List (Block b i)


type Block b i
    = BlankLine String
    | ThematicBreak
    | Heading String Int (List (Inline i))
    | CodeBlock CodeBlock String
    | Paragraph String (List (Inline i))
    | BlockQuote (AST b i)
    | List ListBlock (List (AST b i))
    | HtmlBlock (List (Inline i))
    | CustomBlock b (List (Inline i))


type Inline i
    = Text String
    | HardLineBreak
    | CodeInline String
    | Link String (Maybe String) (List (Inline i))
    | Image String (Maybe String) (List (Inline i))
    | HtmlInline String (List ( String, Maybe String )) (List (Inline i))
    | Emphasis Int (List (Inline i))
    | CustomInline i


type CodeBlock
    = Indented
    | Fenced Bool Fence -- isOpen Fence


type alias Fence =
    { indentLength : Int
    , fenceLength : Int
    , fenceChar : String
    , language : Maybe String
    }


type alias ListBlock =
    { type_ : ListType
    , indentLength : Int
    , delimiter : String
    , isLoose : Bool
    }


type ListType
    = Unordered
    | Ordered Int



{-| Turn a markdown string into a list of HTML elements
using `Config.defaultOptions` and `Config.defaultElements`.

```

markdownView : Html msg
markdownView =
    div []
        <| Markdown.toHtml "# Title with *emphasis*"

```
-}
toHtml : String -> List (Html msg)
toHtml =
    withOptions defaultOptions



{-| Customize how soft line breaks (`\n`) are rendered and raw html
tags are parsed.

```
customOptions : Options
customOptions =
    { softAsHardLineBreak = True
    , rawHtml = DontParse
    }


view : Html msg
view =
    div []
        <| Markdown.withOptions customOptions myString
```

The [demo](https://pablohirafuji.github.io/elm-markdown/examples/Demo.html)
demonstrate how each option affects the output.
-}


withOptions : Options -> String -> List (Html msg)
withOptions options =
    toAST options
        >> blocksToHtml options defaultElements True


{-| Customize how soft line breaks (`\n`) are rendered and raw html
tags are parsed.

```
customOptions : Options
customOptions =
    { softAsHardLineBreak = True
    , rawHtml = DontParse
    }


view : Html msg
view =
    div []
        <| Markdown.withOptions customOptions myString
```

The [demo](https://pablohirafuji.github.io/elm-markdown/examples/Demo.html)
demonstrate how each option affects the output.
-}


toAST : Options -> String -> AST b i
toAST options rawText =
    linesToAST (toRawLines rawText) []
        |> parseReferences Dict.empty
        |> parseInlines options



----------------------------------------------------------------------
---------------------------- Block Parser ----------------------------
----------------------------------------------------------------------


linesToAST : List String -> AST b i -> AST b i
linesToAST rawLines ast =
    case rawLines of
        [] ->
            ast


        rawLine :: rawLinesTail ->
            lineToAST rawLine ast
                |> linesToAST rawLinesTail


lineToAST : String -> AST b i -> AST b i
lineToAST rawLine ast =
    case ast of
        -- No need to typify the line if Fenced Code
        -- is open, just check for closing fence.
        CodeBlock (Fenced True fence) code :: astTail ->
            continueOrCloseCodeFence fence code rawLine
                |> flip (::) astTail


        List model items :: astTail ->
            if indentLength rawLine >= model.indentLength then
                parseIndentedListLine rawLine model items ast astTail


            else
                -- After a list, check for lists before
                -- indented code.
                -- When both a thematic break and a list item are
                -- possible interpretations of a line, the
                -- thematic break takes precedence.
                checkThematicBreakLine rawLine ast
                    |> ifNothing (checkListLine rawLine ast)
                    |> ifNothing (checkBlankLine rawLine ast)
                    |> ifNothing (checkIndentedCode rawLine ast)
                    |> ifNothing (checkOpenCodeFenceLine rawLine ast)
                    |> ifNothing (checkSetextHeadingLine rawLine ast)
                    |> ifNothing (checkATXHeadingLine rawLine ast)
                    |> ifNothing (checkBlockQuote rawLine ast)
                    |> Maybe.withDefault (parseTextLine rawLine ast)


        _ ->
            parseRawLine rawLine ast


-- Default parsing precedence
parseRawLine : String -> AST b i -> AST b i
parseRawLine rawLine ast =
    checkBlankLine rawLine ast
        |> ifNothing (checkIndentedCode rawLine ast)
        |> ifNothing (checkOpenCodeFenceLine rawLine ast)
        |> ifNothing (checkSetextHeadingLine rawLine ast)
        |> ifNothing (checkATXHeadingLine rawLine ast)
        |> ifNothing (checkBlockQuote rawLine ast)
        |> ifNothing (checkThematicBreakLine rawLine ast)
        |> ifNothing (checkListLine rawLine ast)
        |> Maybe.withDefault (parseTextLine rawLine ast)



----------------------------------------------------------------------
------------------------------ BlankLine -----------------------------
----------------------------------------------------------------------


checkBlankLine : String -> AST b i -> Maybe (AST b i)
checkBlankLine rawLine ast =
    Regex.find (Regex.AtMost 1) blankLineRegex rawLine
        |> List.head
        |> Maybe.map (parseBlankLine ast)


blankLineRegex : Regex
blankLineRegex =
    Regex.regex "^\\s*$"


parseBlankLine : AST b i -> Regex.Match -> AST b i
parseBlankLine ast match =
    case ast of
        CodeBlock (Fenced True fence) code :: astTail ->
            code ++ "\n"
                |> CodeBlock (Fenced True fence)
                |> flip (::) astTail


        List model items :: astTail ->
            List model (addBlankLineToListBlock match items)
                :: astTail


        _ ->
            BlankLine match.match :: ast


addBlankLineToListBlock : Regex.Match -> List (AST b i) -> List (AST b i)
addBlankLineToListBlock match asts =
    case asts of
        [] ->
            [ [ BlankLine match.match ] ]


        ast :: astsTail ->
            parseBlankLine ast match
                :: astsTail



----------------------------------------------------------------------
----------------------------- ATX Heading ----------------------------
----------------------------------------------------------------------


checkATXHeadingLine : String -> AST b i -> Maybe (AST b i)
checkATXHeadingLine rawLine ast =
    Regex.find (Regex.AtMost 1) atxHeadingLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractATXHeadingRM
        |> Maybe.map (flip (::) ast)


atxHeadingLineRegex : Regex
atxHeadingLineRegex =
    Regex.regex ("^ {0,3}(#{1,6})"
        ++ "(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)"
        ++ "(.*?)(?:\\s+[ \\t#]*)?$")


extractATXHeadingRM : Regex.Match -> Maybe (Block b i)
extractATXHeadingRM match =
    case match.submatches of
        Just lvl :: Just heading :: _ ->
            Heading heading (String.length lvl) []
                |> Just


        _ ->
            Nothing
            


----------------------------------------------------------------------
--------------------------- Setext Heading ---------------------------
----------------------------------------------------------------------


checkSetextHeadingLine : String -> AST b i -> Maybe (AST b i)
checkSetextHeadingLine rawLine ast =
    Regex.find (Regex.AtMost 1) setextHeadingLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractSetextHeadingRM
        |> Maybe.andThen (parseSetextHeadingLine rawLine ast)


setextHeadingLineRegex : Regex
setextHeadingLineRegex =
    Regex.regex "^ {0,3}(=+|-+)[ \\t]*$"


extractSetextHeadingRM : Regex.Match -> Maybe ( Int, String )
extractSetextHeadingRM match =
    case match.submatches of
        Just delimiter :: _ ->
            if String.startsWith "=" delimiter then
                Just ( 1, delimiter )

            else
                Just ( 2, delimiter )


        _ ->
            Nothing


parseSetextHeadingLine : String -> AST b i -> ( Int, String ) -> Maybe (AST b i)
parseSetextHeadingLine rawLine ast ( lvl, delimiter ) =
    case ast of
        -- Only occurs after a paragraph
        Paragraph rawText _ :: astTail ->
            Heading rawText lvl [] :: astTail
                |> Just


        _ ->
            Nothing



----------------------------------------------------------------------
--------------------------- Thematic Break ---------------------------
----------------------------------------------------------------------


checkThematicBreakLine : String -> AST b i -> Maybe (AST b i)
checkThematicBreakLine rawLine ast =
    Regex.find (Regex.AtMost 1) thematicBreakLineRegex rawLine
        |> List.head
        |> Maybe.map (\_ -> ThematicBreak :: ast)


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex ("^ {0,3}(?:"
        ++ "(?:\\*[ \\t]*){3,}"
        ++ "|(?:_[ \\t]*){3,}"
        ++ "|(?:-[ \\t]*){3,})[ \\t]*$")



----------------------------------------------------------------------
----------------------------- Block Quote ----------------------------
----------------------------------------------------------------------


checkBlockQuote : String -> AST b i -> Maybe (AST b i)
checkBlockQuote rawLine ast =
    Regex.find (Regex.AtMost 1) blockQuoteLineRegex rawLine
        |> List.head
        |> Maybe.map (.submatches >> List.head)
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault Nothing
        |> Maybe.map (parseBlockQuoteLine ast)


blockQuoteLineRegex : Regex
blockQuoteLineRegex =
    Regex.regex "^ {0,3}(?:>[ ]?)(.*)$"


parseBlockQuoteLine : AST b i -> String -> AST b i
parseBlockQuoteLine ast rawLine =
    case ast of
        BlockQuote bqAST :: astTail ->
            lineToAST rawLine bqAST
                |> BlockQuote
                |> flip (::) astTail


        _ ->
            lineToAST rawLine []
                |> BlockQuote
                |> flip (::) ast



----------------------------------------------------------------------
---------------------------- Indented Code ---------------------------
----------------------------------------------------------------------


checkIndentedCode : String -> AST b i -> Maybe (AST b i)
checkIndentedCode rawLine ast =
    Regex.find (Regex.AtMost 1) indentedCodeLineRegex rawLine
        |> List.head
        |> Maybe.map (.submatches >> List.head)
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault Nothing
        |> Maybe.map (parseIndentedCodeLine ast)


indentedCodeLineRegex : Regex
indentedCodeLineRegex =
    Regex.regex "^(?: {4,4}| {0,3}\\t)(.*)$"


parseIndentedCodeLine : AST b i -> String -> AST b i
parseIndentedCodeLine ast codeLine =
    case ast of
        -- Continue indented code block
        CodeBlock Indented codeStr :: astTail ->
            codeStr ++ codeLine ++ "\n"
                |> CodeBlock Indented
                |> flip (::) astTail


        -- Possible blankline inside a indented code block
        BlankLine blankStr :: astTail ->
            [ blankStr ]
                |> blocksAfterBlankLines astTail
                |> resumeIndentedCodeBlock codeLine
                |> Maybe.withDefault
                    (codeLine ++ "\n"
                        |> CodeBlock Indented
                        |> flip (::) ast)


        -- Continue paragraph or New indented code block
        _ ->
             maybeContinueParagraph codeLine ast
                |> Maybe.withDefault
                    (codeLine ++ "\n"
                        |> CodeBlock Indented
                        |> flip (::) ast)


-- Return the blocks after blanklines
-- and the blanklines content in between
blocksAfterBlankLines : AST b i -> List String -> ( AST b i, List String )
blocksAfterBlankLines ast blankLines =
    case ast of
        BlankLine blankStr :: astTail ->
            blocksAfterBlankLines astTail
                (blankStr :: blankLines)


        _ ->
            ( ast, blankLines )


resumeIndentedCodeBlock : String -> ( AST b i, List String ) -> Maybe (AST b i)
resumeIndentedCodeBlock codeLine ( remainBlocks, blankLines ) =
    case remainBlocks of
        CodeBlock Indented codeStr :: remainBlocksTail ->
            blankLines
                |> List.map (\bl -> indentLine 4 bl ++ "\n")
                |> String.concat
                |> (++) codeStr
                |> flip (++) (codeLine ++ "\n")
                |> CodeBlock Indented
                |> flip (::) remainBlocksTail
                |> Just


        _ ->
            Nothing



----------------------------------------------------------------------
----------------------------- Fenced Code ----------------------------
----------------------------------------------------------------------


checkOpenCodeFenceLine : String -> AST b i -> Maybe (AST b i)
checkOpenCodeFenceLine rawLine ast =
    Regex.find (Regex.AtMost 1) openCodeFenceLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractOpenCodeFenceRM
        |> Maybe.map (\f -> CodeBlock f "")
        |> Maybe.map (flip (::) ast)


openCodeFenceLineRegex : Regex
openCodeFenceLineRegex =
    Regex.regex "^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$"


extractOpenCodeFenceRM : Regex.Match -> Maybe CodeBlock
extractOpenCodeFenceRM match =
    case match.submatches of
        Just indent :: Just fence :: Just language :: _ ->
                Fenced True
                    { indentLength = String.length indent
                    , fenceLength = String.length fence
                    , fenceChar = String.left 1 fence
                    , language =
                        String.words language
                            |> List.head
                            |> Maybe.andThen
                                (\lang ->
                                    if lang == "" then Nothing
                                    else Just lang
                                )
                            |> Maybe.map Inline.replaceEscapable
                    } |> Just


        _ ->
            Nothing


continueOrCloseCodeFence : Fence -> String -> String -> Block b i
continueOrCloseCodeFence fence previousCode rawLine =
    if isCloseFenceLine fence rawLine then
        CodeBlock (Fenced False fence) previousCode

    else
        previousCode ++ indentLine fence.indentLength rawLine ++ "\n"
            |> CodeBlock (Fenced True fence)


isCloseFenceLine : Fence -> String -> Bool
isCloseFenceLine fence =
    Regex.find (Regex.AtMost 1) closeCodeFenceLineRegex
        >> List.head
        >> Maybe.map (isCloseFenceLineHelp fence)
        >> Maybe.withDefault False


closeCodeFenceLineRegex : Regex
closeCodeFenceLineRegex =
    Regex.regex "^ {0,3}(`{3,}|~{3,})\\s*$"


isCloseFenceLineHelp : Fence -> Regex.Match -> Bool
isCloseFenceLineHelp fence match =
    case match.submatches of
        Just fenceStr :: _ ->
            String.length fenceStr >= fence.fenceLength
                && String.left 1 fenceStr == fence.fenceChar

        _ ->
            False



----------------------------------------------------------------------
-------------------------------- List --------------------------------
----------------------------------------------------------------------


parseIndentedListLine : String -> ListBlock -> List (AST b i) -> AST b i -> AST b i -> AST b i
parseIndentedListLine rawLine model items ast astTail =
    case items of
        [] ->
            indentLine model.indentLength rawLine
                |> flip lineToAST []
                |> flip (::) []
                |> List model
                |> flip (::) astTail


        item :: itemsTail ->
            let
                indentedRawLine : String
                indentedRawLine =
                    indentLine model.indentLength rawLine


                updateList : ListBlock -> AST b i
                updateList model_ =
                    lineToAST indentedRawLine item
                        |> flip (::) itemsTail
                        |> List model_
                        |> flip (::) astTail


            in case item of
                -- A list item can begin with at most
                -- one blank line without begin loose.
                BlankLine _ :: [] ->
                    updateList model


                BlankLine _ :: itemTail ->
                    if List.all (\block ->
                            case block of
                                BlankLine _ -> True
                                _           -> False
                        ) itemTail then
                            parseRawLine rawLine ast


                    else
                        updateList { model | isLoose = True }


                List model_ items_ :: itemTail ->
                    if indentLength indentedRawLine
                        >= model_.indentLength then
                            updateList model


                    else
                        if isBlankLineLast items_ then
                            updateList { model | isLoose = True }

                        else
                            updateList model


                _ ->
                    updateList model



checkListLine : String -> AST b i -> Maybe (AST b i)
checkListLine rawLine ast =
    checkOrderedListLine rawLine
        |> ifNothing (checkUnorderedListLine rawLine)
        |> Maybe.map calcListIndentLength
        |> Maybe.map (parseListLine rawLine ast)


-- Ordered list
checkOrderedListLine : String -> Maybe ( ListBlock, String, String )
checkOrderedListLine rawLine =
    Regex.find (Regex.AtMost 1) orderedListLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractOrderedListRM


orderedListLineRegex : Regex
orderedListLineRegex =
    Regex.regex "^( *(\\d{1,9})([.)])( {0,4}))(?:[ \\t](.*))?$"


extractOrderedListRM : Regex.Match -> Maybe ( ListBlock, String, String )
extractOrderedListRM match =
    case match.submatches of
        Just indentString
            :: Just start
            :: Just delimiter
            :: Just indentSpace
            :: maybeRawLine
            :: _ ->
                ( { type_ =
                        String.toInt start
                            |> Result.map Ordered
                            |> Result.withDefault Unordered
                  , indentLength = String.length indentString + 1
                  , delimiter = delimiter
                  , isLoose = False
                  }
                , indentSpace
                , Maybe.withDefault "" maybeRawLine
                ) |> Just


        _ ->
            Nothing


-- Unordered list
checkUnorderedListLine : String -> Maybe ( ListBlock, String, String )
checkUnorderedListLine rawLine =
    Regex.find (Regex.AtMost 1) unorderedListLineRegex rawLine
        |> List.head
        |> Maybe.andThen extractUnorderedListRM


unorderedListLineRegex : Regex
unorderedListLineRegex =
    Regex.regex "^( *([\\*\\-\\+])( {0,4}))(?:[ \\t](.*))?$"


extractUnorderedListRM : Regex.Match -> Maybe ( ListBlock, String, String )
extractUnorderedListRM match =
    case match.submatches of
        Just indentString
            :: Just delimiter
            :: Just indentSpace
            :: maybeRawLine
            :: [] ->
                ( { type_ = Unordered
                  , indentLength = String.length indentString + 1
                  , delimiter = delimiter
                  , isLoose = False
                  }
                , indentSpace
                , Maybe.withDefault "" maybeRawLine
                ) |> Just


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
            if isIndentedCode
                || Regex.contains blankLineRegex rawLine then
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


parseListLine : String -> AST b i -> ( ListBlock, String ) -> AST b i
parseListLine rawLine ast ( listBlock, listRawLine ) =
    let
        parsedRawLine : AST b i
        parsedRawLine =
            lineToAST listRawLine []


        newList : AST b i
        newList =
            List listBlock [ parsedRawLine ] :: ast


    in
        case ast of
            List model items :: astTail ->
                if listBlock.delimiter == model.delimiter then
                    parsedRawLine :: items
                        |> List
                            { model
                                | indentLength =
                                    listBlock.indentLength
                                , isLoose =
                                    model.isLoose
                                        || isBlankLineLast items
                            }
                        |> flip (::) astTail


                else
                    newList


            Paragraph rawText inlines :: astTail ->
                case parsedRawLine of
                    BlankLine _ :: [] ->
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


isBlankLineLast : List (AST b i) -> Bool
isBlankLineLast items =
    case items of
        [] ->
            False


        item :: itemsTail ->
            case item of
                -- Ignore if it's an empty list item (example 242)
                BlankLine _ :: [] ->
                    False

                BlankLine _ :: _ ->
                    True

                List _ items_ :: _ ->
                    isBlankLineLast items_

                _ ->
                    False



----------------------------------------------------------------------
------------------------------ Paragraph -----------------------------
----------------------------------------------------------------------


parseTextLine : String -> AST b i -> AST b i
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


maybeContinueParagraph : String -> AST b i -> Maybe (AST b i)
maybeContinueParagraph rawLine ast =
    case ast of
        Paragraph paragraph _ :: astTail ->
            addToParagraph paragraph rawLine
                :: astTail |> Just


        BlockQuote bqAST :: astTail ->
            maybeContinueParagraph rawLine bqAST
                |> Maybe.map (\updtBqAST ->
                    BlockQuote updtBqAST :: astTail)


        List model items :: astTail ->
            case items of
                itemAST :: itemASTTail ->
                    maybeContinueParagraph rawLine itemAST
                        |> Maybe.map
                            (flip (::) itemASTTail
                                >> List model
                                >> flip (::) astTail)


                _ ->
                    Nothing


        _ ->
            Nothing



----------------------------------------------------------------------
----------------------------- References -----------------------------
----------------------------------------------------------------------


type alias LinkMatch =
    { matchLength : Int
    , inside : String
    , url : String
    , maybeTitle : Maybe String
    }


parseReferences : References -> AST b i -> ( References, AST b i )
parseReferences refs =
    List.foldl parseReferencesHelp ( refs, [] )


parseReferencesHelp : Block b i -> ( References, AST b i ) -> ( References, AST b i )
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


        List model absSynsList ->
            let
                ( updtRefs, updtAbsSynsList ) =
                    List.foldl
                        (\absSyns ( refs__, parsedASsList ) ->
                            parseReferences refs__ absSyns
                                |> Tuple.mapSecond
                                    (flip (::) parsedASsList)
                        )
                        ( refs, [] )
                        absSynsList

            in
                ( updtRefs
                , List model updtAbsSynsList
                    :: parsedAST
                )


        BlockQuote bqAST ->
            parseReferences refs bqAST
                |> Tuple.mapSecond BlockQuote
                |> Tuple.mapSecond (flip (::) parsedAST)


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
        Just rawText
            :: maybeRawUrlAB -- with angle brackets: <http://url.com>
            :: maybeRawUrlW  -- without angle brackets : http://url.com
            :: maybeTitleSQ  -- with single quotes: 'title'
            :: maybeTitleDQ  -- with double quotes: "title"
            :: maybeTitleP   -- with parenthesis: (title)
            :: _ ->
                let
                    maybeRawUrl : Maybe String
                    maybeRawUrl =
                        Inline.returnFirstJust
                            [ maybeRawUrlAB, maybeRawUrlW ]


                    toReturn : String -> LinkMatch
                    toReturn rawUrl =
                        { matchLength = String.length regexMatch.match
                        , inside = rawText
                        , url = rawUrl
                        , maybeTitle =
                            Inline.returnFirstJust
                                [ maybeTitleSQ
                                , maybeTitleDQ
                                , maybeTitleP
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
    Regex.regex
        (  "^\\s*\\[("
        ++ Inline.insideSquareBracketRegex
        ++ ")\\]:"
        ++ hrefRegex
        ++ Inline.titleRegex
        ++ "\\s*(?![^\\n])"
        )


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
    Regex.find (Regex.AtMost 1) refRegex rawText
        |> List.head
        |> Maybe.andThen extractUrlTitleRegex
        |> Maybe.map
            (\linkMatch ->
                { linkMatch
                    | inside =
                        Inline.prepareRefLabel linkMatch.inside
                }
            )
        |> Maybe.andThen
            (\linkMatch ->
                if linkMatch.url == "" || linkMatch.inside == "" then
                    Nothing

                else
                    Just linkMatch
            )



----------------------------------------------------------------------
---------------------------- Parse Inlines ---------------------------
----------------------------------------------------------------------


parseInlines : Options -> ( References, AST b i ) -> AST b i
parseInlines options ( refs, ast ) =
    List.map (parseInline options refs) ast


parseInline : Options -> References -> Block b i -> Block b i
parseInline options refs block =
    case block of
        Heading rawText lvl _ ->
            Inline.parse options refs rawText
                |> inlineMatchesToInlines
                |> Heading rawText lvl


        Paragraph rawText _ ->
            let
                inlines : List (Inline i)
                inlines =
                    Inline.parse options refs rawText
                        |> inlineMatchesToInlines


            in
                case inlines of
                    HtmlInline _ _ _ :: [] ->
                        HtmlBlock inlines


                    _ ->
                        Paragraph rawText inlines


        BlockQuote bqAST ->
            parseInlines options ( refs, bqAST )
                |> BlockQuote


        List model items ->
            List.map (parseInlines options << (,) refs) items
                |> List model


        block_ ->
            block_


inlineMatchesToInlines : List Inline.Match -> List (Inline customInline)
inlineMatchesToInlines matches =
    List.map inlineMatchToInline matches


inlineMatchToInline : Inline.Match -> Inline customInline
inlineMatchToInline (Inline.Match match) =
    case match.type_ of
        Inline.Normal ->
            Text match.text


        Inline.HardLineBreak ->
            HardLineBreak


        Inline.Code ->
            CodeInline match.text


        Inline.Autolink ( text, url ) ->
            Link url Nothing [ Text text ]


        Inline.Link ( url, maybeTitle ) ->
            Link url maybeTitle
                (inlineMatchesToInlines match.matches)


        Inline.Image ( url, maybeTitle ) ->
            Image url maybeTitle
                (inlineMatchesToInlines match.matches)


        Inline.Html model ->
            HtmlInline model.tag model.attributes
                (inlineMatchesToInlines match.matches)


        Inline.Emphasis length ->
            Emphasis length
                (inlineMatchesToInlines match.matches)



----------------------------------------------------------------------
----------------------------- Html Render ----------------------------
----------------------------------------------------------------------


blocksToHtml : Options -> Elements msg -> Bool -> AST b i -> List (Html msg)
blocksToHtml options elements textAsParagraph =
    List.map (blockToHtml options elements textAsParagraph)
        >> List.concat


blockToHtml : Options -> Elements msg -> Bool -> Block b i -> List (Html msg)
blockToHtml options elements textAsParagraph block =
    case block of
        BlankLine _ ->
            []


        Heading _ level inlines ->
            [ elements.heading
                level
                (inlinesToHtml elements inlines)
            ]


        ThematicBreak ->
            [ hr [] [] ]


        Paragraph _ inlines ->
            elements.paragraph
                textAsParagraph
                (inlinesToHtml elements inlines)


        CodeBlock (Fenced _ model) codeStr ->
            [ elements.code model.language codeStr ]


        CodeBlock Indented codeStr ->
            [ elements.code Nothing codeStr ]


        BlockQuote blocks ->
            blocksToHtml options elements True blocks
                |> elements.blockQuote
                |> flip (::) []


        List model items ->
            List.map
                (blocksToHtml options elements model.isLoose
                    >> li []) items
                |> (case model.type_ of
                        Ordered startInt ->
                            if startInt == 1 then
                                ol []
                            
                            else
                                ol [ start startInt ]

                        Unordered ->
                            ul [])
                |> flip (::) []


        HtmlBlock inlines ->
            inlinesToHtml elements inlines


        CustomBlock name inlines ->
            [ Html.div [ class (toString name) ] [] ]



----------------------------------------------------------------------
------------------------ Inline Html Renderer ------------------------
----------------------------------------------------------------------


inlinesToHtml : Elements msg -> List (Inline i) -> List (Html msg)
inlinesToHtml elements =
    List.map (inlineToHtml elements)


inlineToHtml : Elements msg -> Inline i -> Html msg
inlineToHtml elements inline =
    case inline of
        Text str ->
            text str


        HardLineBreak ->
            br [] []


        CodeInline codeStr ->
            code [] [ text codeStr ]


        Link url maybeTitle inlines ->
            case maybeTitle of
                Just title_ ->
                    a [ href url, title title_ ]
                        (inlinesToHtml elements inlines)


                Nothing ->
                    a [ href url ]
                        (inlinesToHtml elements inlines)
    

        Image url maybeTitle inlines ->
            case maybeTitle of
                Just title_ ->
                    img
                        [ alt (extractText inlines)
                        , src url
                        , title title_
                        ] []


                Nothing ->
                    img
                        [ alt (extractText inlines)
                        , src url
                        ] []


        HtmlInline tag attrs inlines ->
            node tag
                (attributesToHtmlAttributes attrs)
                (inlinesToHtml elements inlines)


        Emphasis length inlines ->
            case length of
                1 ->
                    em [] (inlinesToHtml elements inlines)


                2 ->
                    strong [] (inlinesToHtml elements inlines)
                    

                _ ->
                    if length - 2 > 0 then
                        strong []
                            <| flip (::) []
                            <| inlineToHtml elements
                            <| Emphasis (length - 2) inlines


                    else
                        em [] (inlinesToHtml elements inlines)

        CustomInline _ ->
            text ""


attributesToHtmlAttributes : List Inline.Attribute -> List (Html.Attribute msg)
attributesToHtmlAttributes =
    List.map attributeToAttribute


attributeToAttribute : Inline.Attribute -> Html.Attribute msg
attributeToAttribute ( name, maybeValue ) =
    attribute name (Maybe.withDefault name maybeValue)



----------------------------------------------------------------------
------------------------------- Helpers ------------------------------
----------------------------------------------------------------------


toRawLines : String -> List String
toRawLines =
    String.lines


indentLength : String -> Int
indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.find (Regex.AtMost 1) initSpacesRegex
        >> List.head
        >> Maybe.map (.match >> String.length)
        >> Maybe.withDefault 0


initSpacesRegex : Regex
initSpacesRegex =
    Regex.regex "^ +"


indentLine : Int -> String -> String
indentLine indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.replace
            (Regex.AtMost 1)
            (Regex.regex ("^ {0," ++ toString indentLength ++ "}" ))
            (\_ -> "")


extractText : List (Inline i) -> String
extractText inlines =
    List.foldl extractTextHelp "" inlines


extractTextHelp : Inline i -> String -> String
extractTextHelp inline text =
    case inline of
        Text str ->
            text ++ str


        HardLineBreak ->
            text ++ " "


        CodeInline str ->
            text ++ str


        Link _ _ inlines ->
            text ++ extractText inlines


        Image _ _ inlines ->
            text ++ extractText inlines


        HtmlInline _ _ inlines ->
            text ++ extractText inlines


        Emphasis _ inlines ->
            text ++ extractText inlines


        CustomInline _ ->
            text

