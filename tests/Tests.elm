module Tests exposing (..)

--import Test.HTMLBlocks

import Test.ATXHeading
import Test.BlankLine
import Test.BlockQuote
import Test.FencedCode
import Test.Helpers exposing (Output)
import Test.IndentedCode
import Test.Initial
import Test.Inline.Autolinks
import Test.Inline.Code
import Test.Inline.Custom
import Test.Inline.EmphasisStrong
import Test.Inline.Entity
import Test.Inline.Escape
import Test.Inline.Images
import Test.Inline.LineBreak
import Test.Inline.Link
import Test.Inline.RawHtml
import Test.LinkReferenceDefinition
import Test.List
import Test.ListItem
import Test.Paragraph
import Test.SetextHeading
import Test.ThematicBreak
import Test exposing (..)
import Test.Helpers exposing (Output, toTest)





-- Based on http://spec.commonmark.org/0.27/


run : Test
run =
    List.map
        (\( description, outputs ) ->
            describe description (List.map toTest outputs)
        )
        all
        |> describe "Elm Markdown Test Suite"


all : List ( String, List (Output msg) )
all =
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
    , ( "6.2 Entity and numeric character references", Test.Inline.Entity.run )
    , ( "6.3 Code spans", Test.Inline.Code.run )
    , ( "6.4 Emphasis and strong emphasis", Test.Inline.EmphasisStrong.run )
    , ( "6.5 Links", Test.Inline.Link.run )
    , ( "6.6 Images", Test.Inline.Images.run )
    , ( "6.7 Autolinks", Test.Inline.Autolinks.run )
    , ( "6.8 Raw HTML", Test.Inline.RawHtml.run )
    , ( "6.9-11 Line breaks and Textual content", Test.Inline.LineBreak.run )
    , ( "Customs", Test.Inline.Custom.run )
    ]
