module Test.View exposing (..)


import Html exposing (..)
import Test.Helpers exposing (Output)
import Test.ThematicBreak
import Test.ATXHeading
import Test.SetextHeading
import Test.IndentedCode
import Test.FencedCode
--import Test.HTMLBlocks
--import Test.LinkReferenceDefinitions
import Test.Paragraph
import Test.BlankLine
import Test.BlockQuote
import Test.ListItem
import Test.List
--import Test.Spec
import Test.Inline.Code
import Test.Inline.EmphasisStrong
import Test.Inline.Link
import Test.Inline.LineBreak


-- Based on http://spec.commonmark.org/0.27/


tests : List ( String, List (Output msg) )
tests =
    [ ( "Thematic breaks", Test.ThematicBreak.run )
    , ( "ATX headings", Test.ATXHeading.run )
    , ( "Setext headings", Test.SetextHeading.run )
    , ( "Indented code blocks", Test.IndentedCode.run )
    , ( "Fenced code blocks", Test.FencedCode.run )
    --, ( "HTML Blocks", Test.HTMLBlock.run )
    --, ( "Link reference definitions", Test.LinkReferenceDefinition.run )
    , ( "Paragraphs", Test.Paragraph.run )
    , ( "Blank lines", Test.BlankLine.run )
    , ( "Block quotes", Test.BlockQuote.run )
    , ( "List Items", Test.ListItem.run )
    , ( "Lists", Test.List.run )
    , ( "6.3 Code spans", Test.Inline.Code.run )
    , ( "6.4 Emphasis and strong emphasis", Test.Inline.EmphasisStrong.run )
    , ( "6.5 Links", Test.Inline.Link.run )
    , ( "6.9-10 Line breaks", Test.Inline.LineBreak.run )
    --, ( "All", Test.Spec.run )
    ]


view : Html msg
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


totalTestCount : List ( String, List (Output msg) ) -> Int
totalTestCount tests =
    let
        sumTests : ( String, List (Output msg) ) -> Int -> Int
        sumTests ( _, outputs ) count =
            List.length outputs
                |> (+) count

    in
        List.foldl sumTests 0 tests


successTestCount : List ( String, List (Output msg) ) -> Int
successTestCount tests =
    let
        sumSuccessTests : ( String, List (Output msg) ) -> Int -> Int
        sumSuccessTests ( _, outputs ) count =
            List.filter (\i -> i == Nothing) outputs
                |> List.length
                |> (+) count

    in
        List.foldl sumSuccessTests 0 tests


showTests : List ( String, List (Output msg) ) -> List (Html msg)
showTests tests =
    List.map showTest tests


showTest : ( String, List (Output msg) ) -> Html msg
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

