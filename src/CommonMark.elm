module CommonMark exposing (..)


import Html exposing (..)
import Regex exposing (Regex)
import Code
import BlockQuote
import Lists



type Block
    = Blank String
    | ThematicBreak
    | Paragraph (List String)
    | Heading Int (List String)
    | Code CodeInfo (List String)
    | BlockQuote (List Block)
    | List ListInfo (List Block)


type Line
    = BlankLine
    --| TextLine
    | ATXHeadingLine
    | SetextHeadingLine
    | ThematicBreakLine
    | BlockQuoteLine
    | IndentedCodeLine
    | OpeningFenceCodeLine
    | OrderedListLine
    | UnorderedListLine


lineRegex : List (Line, Regex)
lineRegex =
    [ ( BlankLine           , Regex.regex "^\\s*$" )
    , ( IndentedCodeLine    , Code.indentedRegex )
    , ( OpeningFenceCodeLine, Code.openingFenceRegex )
    , ( SetextHeadingLine   , Regex.regex "^ {0,3}(=+|-+)[ \\t]*$")
    , ( ATXHeadingLine      , Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$" )
    , ( ThematicBreakLine   , thematicBreakLineRegex )
    , ( BlockQuoteLine      , BlockQuote.regex )
    , ( OrderedListLine     , Lists.unorderedRegex )
    , ( UnorderedListLine   , Lists.orderedRegex )
    --, ( TextLine            , Regex.regex "^.*$" ) É textLine se não for nenhum desses
    ]


thematicBreakLineRegex : Regex
thematicBreakLineRegex =
    Regex.regex "^ {0,3}(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$"


toRawLines : String -> List String
toRawLines =
    String.lines


linesToBlocks : ( List String, List Block ) -> List Block
linesToBlocks ( lines, blocks ) =
    case lines of
        [] ->
            blocks

        line :: linesTail ->
            case blocks of
                List info blocks_ ->
                    verifica indent
                    verifica se continua parágrafo
                    se não, tipifica e adiciona na lista de bloco

                Code (Code.Fenced (True, fence)) pCode
                    :: blocksTail ->
                        Code.continueOrCloseFence fence pCode line
                            |> Code
                            |> flip (::) blocksTail
                            |> Code
                            |> (,) linesTail
                            |> linesToBlocks

                Paragraph lines_ ->


