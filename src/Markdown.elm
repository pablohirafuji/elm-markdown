module Markdown exposing
    ( toHtml
    , withOptions
    , customHtml
    )


{-| A pure Elm package for markdown parsing and rendering.

# Parsing Markdown
@docs toHtml

# Parsing with Custom Options
@docs withOptions, customHtml

-}


import Html exposing (Html, node, text, em, br, hr, a, img, code, li)
import Html.Attributes exposing (class, href, alt, src, title, attribute)
import Dict exposing (Dict)
import Regex exposing (Regex)
import Markdown.Inline as Inline exposing (References)
import Markdown.Config as Config exposing (Elements, defaultElements, Options, defaultOptions, ListElement(..))



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
    , ( IndentedCodeLine    , indentedCodeLineRegex )
    , ( OpeningFenceCodeLine, openingFenceCodeLineRegex )
    , ( SetextHeadingLine   , headingSetextLineRegex )
    , ( ATXHeadingLine      , headingAtxLineRegex )
    , ( BlockQuoteLine      , blockQuoteLineRegex )
    ]


listLineRegexes : List ( Line, Regex )
listLineRegexes =
    -- When both a thematic break and a list item are possible
    -- interpretations of a line, the thematic break takes
    -- precedence
    [ ( ThematicBreakLine, thematicBreakLineRegex )
    , ( OrderedListLine  , orderedListRegex )
    , ( UnorderedListLine, unorderedListRegex )
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


headingAtxLineRegex : Regex
headingAtxLineRegex =
    Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$"


headingSetextLineRegex : Regex
headingSetextLineRegex =
    Regex.regex "^ {0,3}(=+|-+)[ \\t]*$"


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


blockQuoteLineRegex : Regex
blockQuoteLineRegex =
    Regex.regex "^ {0,3}(?:>[ ]?)(.*)$"


indentedCodeLineRegex : Regex
indentedCodeLineRegex =
    Regex.regex "^(?: {4,4}| {0,3}\\t)(.*)$"


openingFenceCodeLineRegex : Regex
openingFenceCodeLineRegex =
    Regex.regex "^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$"


closingFenceCodeLineRegex : Regex
closingFenceCodeLineRegex =
    Regex.regex "^ {0,3}(`{3,}|~{3,})\\s*$"


orderedListRegex : Regex
orderedListRegex =
    Regex.regex "^( *(\\d{1,9})([.)])( {0,4}))(?:[ \\t](.*))?$"


unorderedListRegex : Regex
unorderedListRegex =
    Regex.regex "^( *([\\*\\-\\+])( {0,4}))(?:[ \\t](.*))?$"


initSpacesRegex : Regex
initSpacesRegex =
    Regex.regex "^ +"



----------------------------------------------------------------------
----------------------------- Regex Match ----------------------------
----------------------------------------------------------------------

-- TODO: Return maybe or result

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


blockQuoteMatch : Regex.Match -> String
blockQuoteMatch match =
    match.submatches
        |> List.head
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ""


indentedCodeMatch : Regex.Match -> ( List String, String )
indentedCodeMatch =
    .submatches
        >> List.head
        >> Maybe.withDefault Nothing
        >> Maybe.map ( (,) [] )
        >> Maybe.withDefault ( [], "" )


openingFenceCodeMatch : Regex.Match -> Fence
openingFenceCodeMatch match =
    case match.submatches of
        Just indent :: Just fence :: Just language :: _ ->
                ( True
                ,   { indentLength = String.length indent
                    , fenceLength = String.length fence
                    , fenceChar = String.left 1 fence
                    , language =
                        String.words language
                            |> List.head
                            |> Maybe.map Inline.replaceEscapable
                            |> Maybe.withDefault ""
                    }
                , ""
                )

        _ ->
            ( True, FenceModel 0 0 "`" "", "" )



orderedListMatch : Regex.Match -> ListLine
orderedListMatch match =
    case match.submatches of
        Just indentString
            :: Just start
            :: Just delimiter
            :: Just indentSpace
            :: maybeRawLine
            :: _ ->
                let type_ =
                    String.toInt start
                        |> Result.map Ordered
                        |> Result.withDefault Unordered

                in
                    newListLine
                        type_
                        indentString
                        delimiter
                        indentSpace
                        (Maybe.withDefault "" maybeRawLine)

        _ ->
            ( initListASModel, "" )


unorderedListMatch : Regex.Match -> ListLine
unorderedListMatch match =
    case match.submatches of
        Just indentString
            :: Just delimiter
            :: Just indentSpace
            :: maybeRawLine
            :: [] ->
                newListLine
                    (Unordered)
                    indentString
                    delimiter
                    indentSpace
                    (Maybe.withDefault "" maybeRawLine)

        _ ->
            ( initListASModel, "" )


listMatch : ListElement -> Regex.Match -> ListLine
listMatch type_ match =
    case type_ of
        Unordered ->
            unorderedListMatch match

        Ordered _ ->
            orderedListMatch match


----------------------------------------------------------------------
--------------------------- Abstract Syntax --------------------------
----------------------------------------------------------------------


type AS
    = BlankAS
    | HeadingAS ( Int, String )
    | ThematicBreakAS
    | CodeAS CodeASModel
    | BlockQuoteAS (List AS)
    | ListAS ListASModel (List (List AS))
    | ParagraphAS String


----------------------------------------------------------------------
------------------------------- Parser -------------------------------
----------------------------------------------------------------------


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
            if indentLength rawLine >= model.indentLength then
                case absSynsList of
                    absSyns_ :: absSynsListTail ->
                        let
                            unindentedRawLine : String
                            unindentedRawLine =
                                indentLine model.indentLength rawLine


                            updtListAS : ListASModel -> List AS
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
                                if indentLength unindentedRawLine >= model_.indentLength then
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
                            ( [ parseRawLines ( [ indentLine model.indentLength rawLine ], [] ) ]
                            ) :: absSynsTail


            else
                -- parseRawLine with list priority
                parseRawLineConfigFirst rawLine absSyns


        -- No need to typify the line if Fenced CodeAS
        -- is open, just check for closing fence.
        CodeAS (Fenced (True, fence, lines_)) :: absSynsTail ->
            continueOrCloseFence fence lines_ rawLine
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



----------------------------------------------------------------------
------------------------------ BlankLine -----------------------------
----------------------------------------------------------------------


parseBlankLine : Regex.Match -> List AS -> List AS
parseBlankLine match absSyns =
    case absSyns of
        -- BlankLine after Indented CodeAS may be added to
        -- the CodeAS if another Indented CodeAS is found
        CodeAS (Indented ( blankLines, code ))
            :: absSynsTail ->
                Indented ( match.match :: blankLines, code )
                    |> CodeAS
                    |> \b -> b :: absSynsTail


        CodeAS (Fenced ( True, fence, code ))
            :: absSynsTail ->
                Fenced ( True, fence, code ++ "\n" )
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



----------------------------------------------------------------------
---------------------------- SetextHeading ---------------------------
----------------------------------------------------------------------


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



----------------------------------------------------------------------
-------------------------------- Code --------------------------------
----------------------------------------------------------------------


type CodeASModel
    = Indented ( List String, String ) -- ( After Blanklines lines, Code )
    | Fenced Fence


type alias Fence =
    ( Bool, FenceModel, String ) -- ( isOpen, FenceModel, Code )


type alias FenceModel =
    { indentLength : Int
    , fenceLength : Int
    , fenceChar : String
    , language : String
    }


parseIndentedCodeLine : Regex.Match -> List AS -> List AS
parseIndentedCodeLine match absSyns =
    let ( blankLines, codeLine ) =
        indentedCodeMatch match

    in case absSyns of
        CodeAS (Indented indentedModel) :: absSynsTail ->
            CodeAS
                ( appendIndentedCode
                    ( blankLines, codeLine ) indentedModel
                ) :: absSynsTail  

        _ ->
            maybeContinueParagraph codeLine absSyns
                |> Maybe.withDefault
                    (CodeAS
                        (Indented ( [], codeLine ++ "\n" ))
                            :: absSyns)


parseFencedCodeLine : Regex.Match -> List AS -> List AS
parseFencedCodeLine match absSyns =
    openingFenceCodeMatch match
        |> Fenced
        |> CodeAS
        |> flip (::) absSyns


continueOrCloseFence : FenceModel -> String -> String -> CodeASModel
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
    Regex.find (Regex.AtMost 1) closingFenceCodeLineRegex
        >> List.head
        >> Maybe.map
            (\match ->
                case match.submatches of
                    Just fenceStr :: _ ->
                        String.length fenceStr >= fence.fenceLength
                            && String.left 1 fenceStr == fence.fenceChar

                    _ ->
                        False
            )
        >> Maybe.withDefault False


indentLine : Int -> String -> String
indentLine indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.replace
            (Regex.AtMost 1)
            (Regex.regex ("^ {0," ++ toString indentLength ++ "}" ))
            (\_ -> "")


appendIndentedCode : ( List String, String ) -> ( List String, String ) -> CodeASModel
appendIndentedCode ( _, lineCode ) ( blankLines, blockCode ) =
    let
        indentBL : String -> String
        indentBL blankLine = 
            indentLine 4 blankLine ++ "\n"


        blankLinesStr : String
        blankLinesStr =
            List.reverse blankLines
                |> List.map indentBL
                |> String.concat

    in
        Indented
            ( [], blockCode ++ blankLinesStr ++ lineCode ++ "\n" )


codeASToBlock : CodeASModel -> Block b i
codeASToBlock model =
    case model of
        Indented ( _, codeStr ) ->
            Code Nothing codeStr


        Fenced ( _, { language }, codeStr ) ->
            if String.length language > 0 then
                Code (Just language) codeStr


            else
                Code Nothing codeStr



----------------------------------------------------------------------
----------------------------- Block Quote ----------------------------
----------------------------------------------------------------------


parseBlockQuoteLine : Regex.Match -> List AS -> List AS
parseBlockQuoteLine match absSyns =
    let rawLine =
        blockQuoteMatch match

    in case absSyns of
        BlockQuoteAS absSyns_ :: absSynsTail ->
            BlockQuoteAS
                (parseRawLines ( [ rawLine ], absSyns_ ))
                    :: absSynsTail

        _ ->
            BlockQuoteAS (parseRawLines ( [ rawLine ], [] ))
                :: absSyns



----------------------------------------------------------------------
-------------------------------- List --------------------------------
----------------------------------------------------------------------


type alias ListLine = ( ListASModel, String )


type alias ListASModel =
    { type_ : ListElement
    , indentLength : Int
    , delimiter : String
    , isLoose : Bool
    }


initListASModel : ListASModel
initListASModel =
    { type_ = Unordered
    , indentLength = 2
    , delimiter = "-"
    , isLoose = False
    }


parseListLine : Config.ListElement -> Regex.Match -> List AS -> List AS
parseListLine type_ match absSyns =
    let
        ( lineModel, rawLine ) =
            listMatch type_ match

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


indentLength : String -> Int
indentLength =
    Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "    ")
        >> Regex.find (Regex.AtMost 1) initSpacesRegex
        >> List.head
        >> Maybe.map (.match >> String.length)
        >> Maybe.withDefault 0


newListLine : ListElement -> String -> String -> String -> String -> ListLine
newListLine type_ indentString delimiter indentSpace rawLine =
    let
        indentSpaceLenth =
            String.length indentSpace

        isIndentedCode =
            indentSpaceLenth >= 4

        indentLength = 
            if isIndentedCode then
                1 + String.length indentString
                    - String.length indentSpace

            else
                1 + String.length indentString

        rawLine_ =
            if isIndentedCode then
                indentSpace ++ rawLine

            else
                rawLine

    in
        ( { initListASModel
            | type_ = type_
            , delimiter = delimiter
            , indentLength = indentLength
          }
        , rawLine_
        )



----------------------------------------------------------------------
------------------------------ Paragraph -----------------------------
----------------------------------------------------------------------


parseTextLine : String -> List AS -> List AS
parseTextLine rawLine absSyns =
    maybeContinueParagraph rawLine absSyns
        |> Maybe.withDefault
            (ParagraphAS (formatParagraphLine rawLine) :: absSyns)


addToParagraph : String -> String -> AS
addToParagraph paragraph rawLine =
    ParagraphAS (paragraph ++ "\n" ++ formatParagraphLine rawLine)


formatParagraphLine : String -> String
formatParagraphLine rawParagraph =
    if String.right 2 rawParagraph == "  " then
        String.trim rawParagraph ++ "  "

    else
        String.trim rawParagraph



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

type alias LinkMatch =
    { matchLength : Int
    , inside : String
    , url : String
    , maybeTitle : Maybe String
    }


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


type alias AST customBlock customInline =
    List (Block customBlock customInline)


type Block customBlock customInline
    = ThematicBreak
    | Heading Int (List (Inline customInline))
    | Code (Maybe String) String
    | Paragraph (List (Inline customInline))
    | BlockQuote (List (Block customBlock customInline))
    | List Config.ListElement Bool (List (List (Block customBlock customInline)))
    | Html (List (Inline customInline))
    | CustomBlock customBlock (List (Inline customInline))


absSynToBlock : Options -> References -> AS -> Maybe (Block b i)
absSynToBlock options refs absSyn =
    case absSyn of
        HeadingAS ( lvl, rawText ) ->
            Inline.parse options refs rawText
                |> inlineMatchesToInlines
                |> Heading lvl
                |> Just


        ThematicBreakAS ->
            Just ThematicBreak


        ParagraphAS rawText ->
            let
                inlines : List (Inline i)
                inlines =
                    Inline.parse options refs rawText
                        |> inlineMatchesToInlines


            in
                case inlines of
                    HtmlInline _ _ _ :: [] ->
                        Just (Html inlines)


                    _ ->
                        Just (Paragraph inlines)


        CodeAS codeAS ->
            Just (codeASToBlock codeAS)


        BlockQuoteAS absSyns ->
            absSynsToBlocks options ( refs, absSyns )
                |> BlockQuote
                |> Just


        ListAS model assList ->
            List.map (absSynsToBlocks options << (,) refs) assList
                |> List model.type_ model.isLoose
                |> Just


        BlankAS ->
            Nothing


absSynsToBlocks : Options -> ( References, List AS ) -> List (Block b i)
absSynsToBlocks options ( refs, absSyns ) =
    List.filterMap (absSynToBlock options refs) absSyns


toBlocks : Options -> String -> List (Block b i)
toBlocks options rawText =
    ( toRawLines rawText, [] )
        |> parseRawLines
        |> parseReferences Dict.empty
        |> absSynsToBlocks options



----------------------------------------------------------------------
------------------------------- Inline -------------------------------
----------------------------------------------------------------------


type Inline customInline
    = Text String
    | HardLineBreak
    | CodeInline String
    | Link String (Maybe String) (List (Inline customInline))
    | Image String (Maybe String) (List (Inline customInline))
    | HtmlInline String (List ( String, Maybe String )) (List (Inline customInline))
    | Emphasis Int (List (Inline customInline))
    | CustomInline customInline


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



----------------------------------------------------------------------
----------------------------- Html Render ----------------------------
----------------------------------------------------------------------


blockToHtml : Options -> Elements msg -> Bool -> Block b i -> List (Html msg)
blockToHtml options elements textAsParagraph block =
    case block of
        Heading level inlines ->
            [ elements.heading
                level
                (inlinesToHtml elements inlines)
            ]


        ThematicBreak ->
            [ hr [] [] ]


        Paragraph inlines ->
            elements.paragraph
                textAsParagraph
                (inlinesToHtml elements inlines)


        Code maybeLanguage codeStr ->
            [ elements.code maybeLanguage codeStr ]


        BlockQuote blocks ->
            blocksToHtml options elements True blocks
                |> elements.blockQuote
                |> flip (::) []


        List type_ isLoose items ->
            List.map
                (blocksToHtml options elements isLoose
                    >> li []) items
                |> elements.list type_
                |> flip (::) []


        Html inlines ->
            inlinesToHtml elements inlines


        CustomBlock name inlines ->
            [ Html.div [ class (toString name) ] [] ]


blocksToHtml : Options -> Elements msg -> Bool -> AST b i -> List (Html msg)
blocksToHtml options elements textAsParagraph =
    List.map (blockToHtml options elements textAsParagraph)
        >> List.concat


{-| Customize how to render each element. The following examples
demonstrate how to use it.

- Render `target="_blank"` on links depending on the url.
[Demo](https://pablohirafuji.github.io/elm-markdown/examples/CustomLinkTag.html)
/ [Code](https://github.com/pablohirafuji/elm-markdown/blob/master/examples/CustomLinkTag.elm)
- Render images using `figure` and `figcaption` elements.
[Demo](https://pablohirafuji.github.io/elm-markdown/examples/CustomImageTag.html)
/ [Code](https://github.com/pablohirafuji/elm-markdown/blob/master/examples/CustomImageTag.elm)
-}
customHtml : Config.Options -> Config.Elements msg -> String -> List (Html msg)
customHtml options elements =
    toBlocks options
        >> blocksToHtml options elements True


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
withOptions : Config.Options -> String -> List (Html msg)
withOptions options =
    customHtml options defaultElements


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
    customHtml defaultOptions defaultElements



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
                    elements.emphasis
                        (inlinesToHtml elements inlines)


                2 ->
                    elements.strongEmphasis
                        (inlinesToHtml elements inlines)
                    

                _ ->
                    if length - 2 > 0 then
                        elements.strongEmphasis
                            <| flip (::) []
                            <| inlineToHtml elements
                            <| Emphasis (length - 2) inlines


                    else
                        elements.emphasis
                            (inlinesToHtml elements inlines)

        CustomInline _ ->
            text ""

          

attributesToHtmlAttributes : List Inline.Attribute -> List (Html.Attribute msg)
attributesToHtmlAttributes =
    List.map attributeToAttribute


attributeToAttribute : Inline.Attribute -> Html.Attribute msg
attributeToAttribute ( name, maybeValue ) =
    attribute name (Maybe.withDefault name maybeValue)
