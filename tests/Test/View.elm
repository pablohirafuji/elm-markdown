module Test.View exposing (..)


import Html exposing (..)
import Test.Helpers exposing (Output)
import Test.Initial
import Test.ThematicBreak
import Test.ATXHeading
import Test.SetextHeading
import Test.IndentedCode
import Test.FencedCode
--import Test.HTMLBlocks
import Test.LinkReferenceDefinition
import Test.Paragraph
import Test.BlankLine
import Test.BlockQuote
import Test.ListItem
import Test.List
import Test.Inline.Escape
import Test.Inline.Code
import Test.Inline.EmphasisStrong
import Test.Inline.Link
import Test.Inline.Images
import Test.Inline.Autolinks
import Test.Inline.RawHtml
import Test.Inline.LineBreak


-- Based on http://spec.commonmark.org/0.27/


tests : List ( String, List (Output) )
tests =
    [ ( "1-4 Initial examples", Test.Initial.run )
    , ( "4.1 Thematic breaks", Test.ThematicBreak.run )
    , ( "4.2 ATX headings", Test.ATXHeading.run )
    , ( "4.3 Setext headings", Test.SetextHeading.run )
    , ( "4.4 Indented code blocks", Test.IndentedCode.run )
    , ( "4.5 Fenced code blocks", Test.FencedCode.run )
    --, ( "4.6 HTML Blocks", Test.HTMLBlock.run )
    , ( "4.7 Link reference definitions", Test.LinkReferenceDefinition.run )
    , ( "4.8 Paragraphs", Test.Paragraph.run )
    , ( "4.9 Blank lines", Test.BlankLine.run )
    , ( "5.1 Block quotes", Test.BlockQuote.run )
    , ( "5.2 List Items", Test.ListItem.run )
    , ( "5.3 Lists", Test.List.run )
    , ( "6.1 Backslash escapes", Test.Inline.Escape.run )
    --, ( "6.2 Entity and numeric character references", Test.Inline.Code.run )
    , ( "6.3 Code spans", Test.Inline.Code.run )
    , ( "6.4 Emphasis and strong emphasis", Test.Inline.EmphasisStrong.run )
    , ( "6.5 Links", Test.Inline.Link.run )
    , ( "6.6 Images", Test.Inline.Images.run )
    , ( "6.7 Autolinks", Test.Inline.Autolinks.run )
    , ( "6.8 Raw HTML", Test.Inline.RawHtml.run )
    , ( "6.9-11 Line breaks and Textual content", Test.Inline.LineBreak.run )
    ]


view : Html Never
view =
    div [] <|
        [ h1 []
            [ text <|
                "Tests ("
                    ++ toString (successTestCount tests)
                    ++ "/"
                    ++ toString (totalTestCount tests)
                    ++ ")"
            ]
        ] ++ showTests tests


totalTestCount : List ( String, List (Output) ) -> Int
totalTestCount tests =
    let
        sumTests : ( String, List (Output) ) -> Int -> Int
        sumTests ( _, outputs ) count =
            List.length outputs
                |> (+) count

    in
        List.foldl sumTests 0 tests


successTestCount : List ( String, List (Output) ) -> Int
successTestCount tests =
    let
        sumSuccessTests : ( String, List (Output) ) -> Int -> Int
        sumSuccessTests ( _, outputs ) count =
            List.filter (\i -> i == Nothing) outputs
                |> List.length
                |> (+) count

    in
        List.foldl sumSuccessTests 0 tests


showTests : List ( String, List (Output) ) -> List (Html Never)
showTests tests =
    List.map showTest tests


showTest : ( String, List (Output) ) -> Html Never
showTest ( testTitle, outputs ) =
    let
        passed =
            successTestCount [ ( testTitle, outputs ) ]

        failed =
            List.filterMap (\i -> i) outputs

    in
        details [] <|
            [ summary [] <|
                [ text <|
                    testTitle
                        ++ " ("
                        ++ toString passed
                        ++ "/"
                        ++ toString (List.length outputs)
                        ++ ")"
                ]
            ] ++ failed

