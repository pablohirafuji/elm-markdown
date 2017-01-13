module Markdown exposing
    ( toHtml
    , withOptions
    , customHtml
    )


{-| A library for markdown parsing and rendering.

# Parsing Markdown
@docs toHtml

# Parsing with Custom Options
@docs withOptions, customHtml

-}


import Html exposing (..)
import Dict exposing (Dict)
import Regex exposing (Regex)
import Markdown.Code as Code
import Markdown.List as Lists
import Markdown.Inline as Inline exposing (References)
import Markdown.Config as Config exposing (Elements, defaultElements, Options, defaultOptions)



----------------------------------------------------------------------
-------------------------------- Line --------------------------------
----------------------------------------------------------------------


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


toRawLines : String -> List String
toRawLines =
    String.lines



----------------------------------------------------------------------
------------------------------ Regexes -------------------------------
----------------------------------------------------------------------


lineMinusListRegexes : List ( Line, Regex )
lineMinusListRegexes =
    [ ( BlankLine           , blankLineRegex )
    , ( IndentedCodeLine    , Code.indentedRegex )
    , ( OpeningFenceCodeLine, Code.openingFenceRegex )
    , ( SetextHeadingLine   , headingSetextRegex )
    , ( ATXHeadingLine      , headingAtxRegex )
    , ( BlockQuoteLine      , blockQuoteLineRegex )
    ]


listLineRegexes : List ( Line, Regex )
listLineRegexes =
    -- When both a thematic break and a list item are possible
    -- interpretations of a line, the thematic break takes
    -- precedence
    [ ( ThematicBreakLine, thematicBreakLineRegex )
    , ( OrderedListLine  , Lists.orderedRegex )
    , ( UnorderedListLine, Lists.unorderedRegex )
    ]


lineRegexes : List ( Line, Regex )
lineRegexes =
    lineMinusListRegexes ++ listLineRegexes


listLineFirstRegexes : List ( Line, Regex )
listLineFirstRegexes =
    listLineRegexes ++ lineMinusListRegexes


blankLineRegex : Regex
blankLineRegex =
    Regex.regex "^\\s*$"


headingAtxRegex : Regex
headingAtxRegex =
    Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$"


headingSetextRegex : Regex
headingSetextRegex =
    Regex.regex "^ {0,3}(=+|-+)[ \\t]*$"


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


blockQuoteLineRegex : Regex
blockQuoteLineRegex =
    Regex.regex "^ {0,3}(?:>[ ]?)(.*)$"



----------------------------------------------------------------------
----------------------------- Regex Match ----------------------------
----------------------------------------------------------------------


headingAtxMatch : Regex.Match -> ( Int, String )
headingAtxMatch match =
    case match.submatches of
        Just lvl :: Just heading :: _ ->
            ( String.length lvl, heading )

        _ ->
            ( 1, match.match )


headingSetextMatch : Regex.Match -> ( Int, String )
headingSetextMatch match =
    case match.submatches of
        Just str :: _ ->
            if String.startsWith "=" str then
                ( 1, str )

            else
                ( 2, str )

        _ ->
            ( 1, "" )


blockQuoteFromMatch : Regex.Match -> String
blockQuoteFromMatch match =
    match.submatches
        |> List.head
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ""



----------------------------------------------------------------------
--------------------------- Abstract Syntax --------------------------
----------------------------------------------------------------------


type AS
    = BlankAS
    | HeadingAS ( Int, String )
    | ThematicBreakAS
    | CodeAS Code.Model
    | BlockQuoteAS (List AS)
    | ListAS Lists.Model (List (List AS))
    | ParagraphAS String


parseRawLines : ( List String, List AS ) -> List AS
parseRawLines ( rawLines, absSyns ) =
    case rawLines of
        [] ->
            absSyns

        rawLine :: rawLinesTail ->
            preParseRawLine ( rawLine, absSyns )
                |> (,) rawLinesTail
                |> parseRawLines


preParseRawLine : ( String, List AS ) -> List AS
preParseRawLine ( rawLine, absSyns ) =
    case absSyns of
        ListAS model absSynsList :: absSynsTail ->
            if Lists.indentLength rawLine >= model.indentLength then
                case absSynsList of
                    absSyns_ :: absSynsListTail ->
                        let
                            unindentedRawLine : String
                            unindentedRawLine =
                                Code.indentLine model.indentLength rawLine


                            updtListAS : Lists.Model -> List AS
                            updtListAS model_ =
                                ListAS model_
                                    ( parseRawLines ( [ unindentedRawLine ], absSyns_ )
                                        :: absSynsListTail
                                    ) :: absSynsTail


                        in case absSyns_ of
                            -- A list item can begin with at most
                            -- one blank line without begin loose.
                            [ BlankAS ] ->
                                updtListAS model


                            BlankAS :: absSynsTail_ ->
                                if List.all ((==) BlankAS) absSynsTail_ then
                                    parseRawLine rawLine absSyns

                                else
                                    updtListAS { model | isLoose = True }


                            ListAS model_ absSynsList_ :: absSynsTail_ ->
                                if Lists.indentLength unindentedRawLine >= model_.indentLength then
                                    updtListAS model

                                else
                                    if isBlankASLast absSynsList_ then
                                        updtListAS { model | isLoose = True }

                                    else
                                        updtListAS model


                            _ ->
                                updtListAS model


                    [] ->
                        ListAS model
                            ( [ parseRawLines ( [ Code.indentLine model.indentLength rawLine ], [] ) ]
                            ) :: absSynsTail


            else
                -- parseRawLine with list priority
                parseRawLineConfigFirst rawLine absSyns


        -- No need to typify the line if Fenced CodeAS
        -- is open, just check for closing fence.
        CodeAS (Code.Fenced (True, fence, lines_)) :: absSynsTail ->
            Code.continueOrCloseFence fence lines_ rawLine
                |> CodeAS
                |> \codeAS -> codeAS :: absSynsTail


        _ ->
            parseRawLine rawLine absSyns


parseRawLine : String -> List AS -> List AS
parseRawLine rawLine absSyns =
    List.foldl (applyRegex rawLine absSyns) Nothing lineRegexes
        |> Maybe.withDefault ( parseTextLine rawLine absSyns )


parseRawLineConfigFirst : String -> List AS -> List AS
parseRawLineConfigFirst rawLine absSyns =
    List.foldl (applyRegex rawLine absSyns) Nothing listLineFirstRegexes
        |> Maybe.withDefault ( parseTextLine rawLine absSyns )


applyRegex : String -> List AS -> ( Line, Regex ) -> Maybe (List AS) -> Maybe (List AS)
applyRegex rawLine absSyns ( line, regex ) maybeASs =
    if maybeASs == Nothing then
        Regex.find ( Regex.AtMost 1 ) regex rawLine
            |> List.head
            |> Maybe.map ( parseLine line absSyns )

    else
        maybeASs


parseLine : Line -> List AS -> Regex.Match -> List AS
parseLine line absSyns match =
    case line of
        BlankLine ->
            parseBlankLine match absSyns

        ATXHeadingLine ->
            HeadingAS (headingAtxMatch match) :: absSyns

        SetextHeadingLine ->
            parseSetextHeadingLine match absSyns

        ThematicBreakLine ->
            ThematicBreakAS :: absSyns

        IndentedCodeLine ->
            parseIndentedCodeLine match absSyns

        OpeningFenceCodeLine ->
            parseFencedCodeLine match absSyns

        BlockQuoteLine ->
            parseBlockQuoteLine match absSyns

        OrderedListLine ->
            parseListLine (Config.Ordered 0) match absSyns
        
        UnorderedListLine ->
            parseListLine Config.Unordered match absSyns


parseBlankLine : Regex.Match -> List AS -> List AS
parseBlankLine match absSyns =
    case absSyns of
        -- BlankLine after Indented CodeAS may be added to
        -- the CodeAS if another Indented CodeAS is found
        CodeAS ( Code.Indented indentedModel )
            :: absSynsTail ->
                Code.addBlankLineToIndented match.match indentedModel
                    |> CodeAS
                    |> \b -> b :: absSynsTail


        CodeAS ( Code.Fenced ( True, fence, pCode ) )
            :: absSynsTail ->
                Code.addBlankLineToFenced match.match ( True, fence, pCode )
                    |> CodeAS
                    |> \b -> b :: absSynsTail


        ListAS model absSynsList :: absSynsTail ->
            ListAS
                model
                (addBlankLineToASsList match absSynsList)
                    :: absSynsTail


        _ ->
            BlankAS :: absSyns


addBlankLineToASsList : Regex.Match -> List (List AS) -> List (List AS)
addBlankLineToASsList match absSynsList =
    case absSynsList of
        absSyns :: absSynsListTail ->
            parseBlankLine match absSyns
                :: absSynsListTail

        [] ->
            [ [ BlankAS ] ]


parseSetextHeadingLine : Regex.Match -> List AS -> List AS
parseSetextHeadingLine match absSyns =
    let ( lvl, str ) =
        headingSetextMatch match

    in case absSyns of
        -- Only occurs after ParagraphAS.
        ParagraphAS paragraph :: absSynsTail ->
            HeadingAS ( lvl, paragraph ) :: absSynsTail

        _ ->
            -- If marker is "=" (lvl == 1), always parse as TextLine.
            if lvl == 1 then
                parseTextLine match.match absSyns

            -- If marker is "-" and length is 1, it's
            -- an empty ListLine.
            else if str == "-" then
                parseListLine Config.Unordered match absSyns

            -- If matches with thematic break line regex, it's
            -- a ThematicBreakAS. Ps: "--" does not match.
            else if Regex.contains thematicBreakLineRegex match.match then
                ThematicBreakAS :: absSyns

            -- Otherwise, parse as TextLine
            else
                parseTextLine match.match absSyns


parseIndentedCodeLine : Regex.Match -> List AS -> List AS
parseIndentedCodeLine match absSyns =
    let ( blankLines, codeLine ) =
        Code.fromIndentedMatch match

    in case absSyns of
        CodeAS (Code.Indented absSynArgs) :: absSynsTail ->
            CodeAS
                ( Code.addIndented
                    ( blankLines, codeLine ) absSynArgs
                ) :: absSynsTail  

        _ ->
            maybeContinueParagraph codeLine absSyns
                |> Maybe.withDefault
                    ( CodeAS
                        ( Code.Indented
                            ( [], codeLine ++ "\n" )
                        ) :: absSyns
                    )


parseFencedCodeLine : Regex.Match -> List AS -> List AS
parseFencedCodeLine match absSyns =
    CodeAS
        ( Code.Fenced
            (Code.fromOpeningFenceMatch match)
        ) :: absSyns


parseBlockQuoteLine : Regex.Match -> List AS -> List AS
parseBlockQuoteLine match absSyns =
    let rawLine =
        blockQuoteFromMatch match

    in case absSyns of
        BlockQuoteAS absSyns_ :: absSynsTail ->
            BlockQuoteAS
                (parseRawLines ( [ rawLine ], absSyns_ ))
                    :: absSynsTail

        _ ->
            BlockQuoteAS (parseRawLines ( [ rawLine ], [] ))
                :: absSyns


parseListLine : Config.ListElement -> Regex.Match -> List AS -> List AS
parseListLine type_ match absSyns =
    let
        ( lineModel, rawLine ) =
            Lists.fromMatch type_ match

        parsedRawLine =
            parseRawLines ( [ rawLine ], [] )

        newListAS =
            ListAS lineModel [ parsedRawLine ] :: absSyns

    in case absSyns of
        ListAS absSynModel absSynsList :: absSynsTail ->
            if lineModel.delimiter == absSynModel.delimiter then
                ListAS
                    { absSynModel
                        | indentLength = lineModel.indentLength
                        , isLoose =
                            absSynModel.isLoose
                                || isBlankASLast absSynsList
                    }
                    (parsedRawLine :: absSynsList)
                        :: absSynsTail

            else
                newListAS

        ParagraphAS paragraph :: absSynsTail ->
            -- Empty list item cannot interrupt a paragraph.
            if parsedRawLine == [ BlankAS ] then
                addToParagraph paragraph match.match
                    :: absSynsTail

            else
                case lineModel.type_ of
                    -- Ordered list with start 1 can interrupt.
                    Config.Ordered 1 ->
                        newListAS

                    Config.Ordered int ->
                        addToParagraph paragraph match.match
                            :: absSynsTail

                    _ ->
                        newListAS

        _ ->
            newListAS


isBlankASLast : List (List AS) -> Bool
isBlankASLast absSynsList =
    case absSynsList of
        absSyns :: absSynsListTail ->
            case absSyns of
                -- Ignore if it's an empty list item (example 242)
                BlankAS :: [] ->
                    False

                BlankAS :: _ ->
                    True

                ListAS _ absSynsList_ :: _ ->
                    isBlankASLast absSynsList_

                _ ->
                    False
        
        [] ->
            False


parseTextLine : String -> List AS -> List AS
parseTextLine rawLine absSyns =
    maybeContinueParagraph rawLine absSyns
        |> Maybe.withDefault
            (ParagraphAS (String.trimLeft rawLine) :: absSyns)


addToParagraph : String -> String -> AS
addToParagraph paragraph rawLine =
    ParagraphAS (paragraph ++ "\n" ++ String.trimLeft rawLine)


maybeContinueParagraph : String -> List AS -> Maybe ( List AS )
maybeContinueParagraph rawLine absSyns =
    case absSyns of
        ParagraphAS paragraph :: absSynsTail ->
            addToParagraph paragraph rawLine
                :: absSynsTail
                    |> Just


        BlockQuoteAS absSyns_ :: absSynsTail ->
            maybeContinueParagraph rawLine absSyns_
                |> Maybe.map
                    (\updtASs_ ->
                        BlockQuoteAS updtASs_ :: absSynsTail
                    )


        ListAS model absSynsList :: absSynsTail ->
            case absSynsList of
                absSyns_ :: absSynsListTail ->
                    maybeContinueParagraph rawLine absSyns_
                        |> Maybe.map
                            (\updtASs_ ->
                                ListAS model
                                    (updtASs_ :: absSynsListTail)
                                        :: absSynsTail
                            )

                _ ->
                    Nothing

        _ ->
            Nothing



----------------------------------------------------------------------
----------------------------- References -----------------------------
----------------------------------------------------------------------


hrefRegex : String
hrefRegex =
    "\\s*(?:<([^<>\\s]*)>|([^\\s]*))"


refRegex : Regex
refRegex =
    Regex.regex
        (  "^\\s*\\[("
        ++ Inline.insideRegex
        ++ ")\\]:"
        ++ hrefRegex
        ++ Inline.titleRegex
        ++ "\\s*(?![^\\n])"
        )


insertLinkMatch : References -> Inline.LinkMatch -> References
insertLinkMatch refs linkMatch =
    if Dict.member linkMatch.inside refs then
        refs

    else
        Dict.insert
            linkMatch.inside
            ( linkMatch.url, linkMatch.maybeTitle )
            refs


dropRefString : String -> Inline.LinkMatch -> Maybe String
dropRefString rawText inlineMatch =
    let
        strippedText =
            String.dropLeft inlineMatch.matchLength rawText

    in
        if Regex.contains blankLineRegex strippedText then
            Nothing

        else
            Just strippedText


maybeLinkMatch : String -> Maybe Inline.LinkMatch
maybeLinkMatch rawText =
    Regex.find (Regex.AtMost 1) refRegex rawText
        |> List.head
        |> Maybe.andThen Inline.extractUrlTitleRegex
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


parseReferences : References -> List AS -> ( References, List AS )
parseReferences refs =
    let
        applyParser : AS -> ( References, List AS ) -> ( References, List AS )
        applyParser absSyn ( refs_, parsedASs ) =
            case absSyn of
                ParagraphAS rawText ->
                    let
                        ( paragraphRefs, maybeUpdtText ) =
                            parseReference Dict.empty rawText

                        updtRefs =
                            Dict.union paragraphRefs refs_
                    
                    in
                        case maybeUpdtText of
                            Just updtText ->
                                ( updtRefs
                                , ParagraphAS updtText
                                    :: parsedASs
                                )

                            Nothing ->
                                ( updtRefs, parsedASs )


                ListAS model absSynsList ->
                    let
                        ( updtRefs, updtAbsSynsList ) =
                            List.foldl
                                (\absSyns ( refs__, parsedASsList ) ->
                                    parseReferences refs__ absSyns
                                        |> Tuple.mapSecond
                                            (flip (::) parsedASsList)
                                )
                                ( refs_, [] )
                                absSynsList

                    in
                        ( updtRefs
                        , ListAS model updtAbsSynsList
                            :: parsedASs
                        )


                BlockQuoteAS absSyns ->
                    parseReferences refs_ absSyns
                        |> Tuple.mapSecond BlockQuoteAS
                        |> Tuple.mapSecond (flip (::) parsedASs)


                _ ->
                    ( refs_, absSyn :: parsedASs )

    in
        List.foldl applyParser ( refs, [] )



----------------------------------------------------------------------
-------------------------------- Block -------------------------------
----------------------------------------------------------------------


type Block
    = ThematicBreak
    | Heading HeadingBlock
    | Code CodeBlock
    | Paragraph ParagraphBlock
    | BlockQuote BlockQuoteBlock
    | List ListBlock
    | Html HtmlBlock 


type alias HeadingBlock =
    { level : Int
    , inlines : List Inline.Match
    }


type alias CodeBlock =
    Config.CodeBlock


type alias ParagraphBlock =
    { inlines : List Inline.Match }


type alias BlockQuoteBlock =
    { blocks : List Block }


type alias ListBlock =
    { type_ : Config.ListElement
    , isLoose : Bool
    , items : List (List Block)
    }


type alias HtmlBlock =
    { inlines : List Inline.Match }


absSynToBlock : Options -> References -> AS -> Maybe Block
absSynToBlock options refs absSyn =
    case absSyn of
        HeadingAS ( lvl, rawText ) ->
            Just
                <| Heading
                    { level = lvl
                    , inlines = Inline.parse options refs rawText
                    }


        ThematicBreakAS ->
            Just ThematicBreak


        ParagraphAS rawText ->
            let
                parsedInline : List Inline.Match
                parsedInline =
                    Inline.parse options refs rawText

                returnParagraph : Maybe Block
                returnParagraph =
                    Just (Paragraph { inlines = parsedInline })


            in
                case parsedInline of
                    [ Inline.Match match ] ->
                        case match.type_ of
                            Inline.Html _ ->
                                Just (Html { inlines = parsedInline })

                            _ ->
                                returnParagraph

                    _ ->
                        returnParagraph


        CodeAS codeAS ->
            Just
                <| Code
                <| Code.asToBlock codeAS


        BlockQuoteAS absSyns ->
            Just
                <| BlockQuote
                    { blocks =
                        absSynsToBlocks options ( refs, absSyns )
                    }


        ListAS model absSynsList ->
            Just
                <| List
                    { type_ = model.type_
                    , isLoose = model.isLoose
                    , items =
                        List.map
                            (absSynsToBlocks options << (,) refs)
                            absSynsList
                    }


        BlankAS ->
            Nothing


absSynsToBlocks : Options -> ( References, List AS ) -> List Block
absSynsToBlocks options ( refs, absSyns ) =
    List.filterMap (absSynToBlock options refs) absSyns


toBlocks : Options -> String -> List Block
toBlocks options rawText =
    ( toRawLines rawText, [] )
        |> parseRawLines
        |> parseReferences Dict.empty
        |> absSynsToBlocks options



----------------------------------------------------------------------
----------------------------- Html Render ----------------------------
----------------------------------------------------------------------


blockToHtml : Options -> Elements -> Bool -> Block -> List (Html Never)
blockToHtml options elements textAsParagraph block =
    case block of
        Heading { level, inlines } ->
            [ elements.heading
                level
                (Inline.toHtml elements inlines)
            ]


        ThematicBreak ->
            [ elements.thematicBreak ]


        Paragraph { inlines } ->
            elements.paragraph
                textAsParagraph
                (Inline.toHtml elements inlines)


        Code model ->
            [ elements.code model ]


        BlockQuote { blocks } ->
            blocksToHtml options elements True blocks
                |> elements.blockQuote
                |> flip (::) []


        List { type_, isLoose, items } ->
            List.map
                (blocksToHtml options elements isLoose
                    >> li []) items
                |> elements.list type_
                |> (\list -> [ list ] )


        Html { inlines } ->
            (Inline.toHtml elements inlines)


blocksToHtml : Options -> Elements -> Bool -> List Block -> List (Html Never)
blocksToHtml options elements textAsParagraph =
    List.map (blockToHtml options elements textAsParagraph)
        >> List.concat


{-| Customize how to render each element. The following examples
demonstrate how to use it.

- Render `target="_blank"` on links depending on the url.
- Render images using `figure` and `figcaption` elements.
-}
customHtml : Config.Options -> Config.Elements -> String -> List (Html Never)
customHtml options elements =
    toBlocks options
        >> blocksToHtml options elements True


{-| Customize how soft line breaks (`\n`) are rendered and html
tags are parsed.
Play with the options of the demo to see what each option does.
-}
withOptions : Config.Options -> String -> List (Html Never)
withOptions options =
    customHtml options defaultElements


{-| Turn a markdown string into a list of HTML elements,
using the `Config.defaultOptions` and `Config.defaultElements`.

```

type Msg
    = MsgOfmyApp1
    | MsgOfmyApp2
    | MsgOfmyApp3
    | Markdown


markdownView : Html Msg
markdownView =
    Html.map (always Markdown)
        <| section []
        <| Markdown.toHtml "# Title with *emphasis*"

```
-}
toHtml : String -> List (Html Never)
toHtml =
    customHtml defaultOptions defaultElements

