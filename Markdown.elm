module Markdown exposing (..)


import Html exposing (..)
import Regex exposing (Regex)



type Line
    = BlankLine
    | SetextHeadingLine Int String
    | ATXHeadingLine Int String
    | CodeFenceLine String
    | IndentedCodeLine String
    | ThematicBreakLine
    | TextLine String


lineRegex : List (Line, Regex)
lineRegex =
    [ ( IndentedCodeLine "", Regex.regex "^ {4,4}(.*)$" )
    , ( BlankLine, Regex.regex "^\\s*$" )
    , ( SetextHeadingLine 0 "", Regex.regex "^ {0,3}(=+|-+)[ \\t]*$")
    , ( ATXHeadingLine 0 "", Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$" )
    , ( CodeFenceLine "", Regex.regex "^`{3,}(?!.*`)(.*)$|^~{3,}(?!.*~)(.*)$" )
    , ( ThematicBreakLine, Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$" )
    , ( TextLine "", Regex.regex "^.*$" )
    ]


typeOfLine : String -> Maybe Line
typeOfLine lineStr =
    let
        process : (Line, Regex) -> Maybe Line -> Maybe Line
        process (line, regex) maybeLine =
            if maybeLine == Nothing then
                let
                    matchs =
                        Regex.find Regex.All regex lineStr
                in
                    if List.length matchs > 0 then
                        Just (processTypeOfLine matchs line)

                    else
                        Nothing

            else
                maybeLine

    in
        List.foldl process Nothing lineRegex


processTypeOfLine : List Regex.Match -> Line -> Line
processTypeOfLine matchs line =
    case line of
        BlankLine ->
            BlankLine

        SetextHeadingLine _ _ ->
            processHeadMatch matchToSetextHeadingLine matchs

        ATXHeadingLine _ _ ->
            processHeadMatch matchToHeadingLine matchs

        CodeFenceLine _ ->
            processHeadMatch matchToCodeFenceLine matchs

        IndentedCodeLine _ ->
            processHeadMatch matchToIndentedCodeLine matchs

        ThematicBreakLine ->
            ThematicBreakLine

        TextLine _ ->
            processHeadMatch (matchToTextLine) matchs



processHeadMatch : (Regex.Match -> Line) -> List Regex.Match -> Line
processHeadMatch fun matchs =
    List.head matchs
        |> Maybe.map fun
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
    case match.submatches of
        Just language :: _ ->
            CodeFenceLine language

        _ ->
            TextLine match.match


matchToIndentedCodeLine : Regex.Match -> Line
matchToIndentedCodeLine match =
    case match.submatches of
        Just code :: _ ->
            IndentedCodeLine code

        _ ->
            TextLine match.match


type Block
    = HeadingBlock Int String
    | ThematicBreakBlock
    | ParagraphBlock String
    | CodeBlock String


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


        SetextHeadingLine 2 _ :: rest ->
            -- TODO Testar para ver se passa no teste de ThematicBreakBlock
            ( ThematicBreakBlock :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        IndentedCodeLine code :: rest ->
            case blocks of
                CodeBlock previousCode :: restBlocks ->
                    ( CodeBlock (previousCode ++ "\n" ++ code) :: restBlocks
                    , rest
                    , { parseState | isCodeBlockOpen = True }
                    ) |> linesToBlocks

                _ ->
                    ( CodeBlock code :: blocks
                    , rest
                    , { parseState | isCodeBlockOpen = True }
                    ) |> linesToBlocks


        TextLine paragraph
        :: rest ->
            ( ParagraphBlock (String.trim paragraph) :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


        BlankLine :: rest ->
            case blocks of
                CodeBlock previousCode :: restBlocks ->
                    ( CodeBlock (previousCode ++ "\n") :: restBlocks
                    , rest
                    , { parseState | isCodeBlockOpen = True }
                    ) |> linesToBlocks

                _ ->
                    ( blocks
                    , rest
                    , parseState
                    ) |> linesToBlocks


        anything_ :: rest ->
            ( ParagraphBlock (toString anything_) :: blocks
            , rest
            , parseState
            ) |> linesToBlocks


toHtml : String -> List (Html msg)
toHtml commmonMark =
    ( []
    , List.filterMap (\item -> item) (List.map typeOfLine (String.lines commmonMark))
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

        CodeBlock codeStr ->
            pre [] [ code [] [ text codeStr ] ]

