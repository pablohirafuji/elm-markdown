module Test.ThematicBreak exposing (..)

import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/


run : List (Output msg)
run =
    [ testEq 13
        [ p [] [ text "A line consisting of 0-3 spaces of indentation, followed by a sequence of three or more matching -, _, or * characters, each followed optionally by any number of spaces, forms a thematic break." ] ]
        "***\n---\n___"
        [ hr [] [], hr [] [], hr [] [] ]
    , testEq 14
        [ p [] [ text "Wrong characters:" ] ]
        "+++"
        [ p [] [ text "+++" ] ]
    , testEq 15
        []
        "==="
        [ p [] [ text "===" ] ]
    , testEq 16
        [ p [] [ text "Not enough characters:" ] ]
        "--\n**\n__"
        [ p [] [ text "--\n**\n__" ] ]
    , testEq 17
        [ p [] [ text "One to three spaces indent are allowed:" ] ]
        " ***\n  ***\n   ***"
        [ hr [] [], hr [] [], hr [] [] ]
    , testEq 18
        [ p [] [ text "Four spaces is too many:" ] ]
        "    ***"
        [ pre [] [ code [] [ text "***\n" ] ] ]
    , testEq 19
        []
        "Foo\n    ***"
        [ p [] [ text "Foo\n***" ] ]
    , testEq 20
        [ p [] [ text "More than three characters may be used:" ] ]
        "_____________________________________"
        [ hr [] [] ]
    , testEq 21
        [ p [] [ text "Spaces are allowed between the characters:" ] ]
        " - - -"
        [ hr [] [] ]
    , testEq 22
        []
        " **  * ** * ** * **"
        [ hr [] [] ]
    , testEq 23
        []
        "-     -      -      -"
        [ hr [] [] ]
    , testEq 24
        [ p [] [ text "Spaces are allowed at the end:" ] ]
        "- - - -    "
        [ hr [] [] ]
    , testEq 25
        [ p [] [ text "However, no other characters may occur in the line:" ] ]
        "_ _ _ _ a\n\na------\n\n---a---"
        [ p [] [ text "_ _ _ _ a" ], p [] [ text "a------" ], p [] [ text "---a---" ] ]
    , testEq 26
        [ p [] [ text "It is required that all of the non-whitespace characters be the same. So, this is not a thematic break:" ] ]
        " *-*"
        [ p [] [ em [] [ text "-" ] ] ]
    , testEq 27
        [ p [] [ text "Thematic breaks do not need blank lines before or after:" ] ]
        "- foo\n***\n- bar"
        [ ul [] [ li [] [ text "foo" ] ], hr [] [], ul [] [ li [] [ text "bar" ] ] ]
    , testEq 28
        [ p [] [ text "Thematic breaks can interrupt a paragraph:" ] ]
        "Foo\n***\nbar"
        [ p [] [ text "Foo" ], hr [] [], p [] [ text "bar" ] ]
    , testEq 29
        [ p [] [ text "If a line of dashes that meets the above conditions for being a thematic break could also be interpreted as the underline of a setext heading, the interpretation as a setext heading takes precedence. Thus, for example, this is a setext heading, not a paragraph followed by a thematic break:" ] ]
        "Foo\n---\nbar"
        [ h2 [] [ text "Foo" ], p [] [ text "bar" ] ]
    , testEq 30
        [ p [] [ text "When both a thematic break and a list item are possible interpretations of a line, the thematic break takes precedence:" ] ]
        "* Foo\n* * *\n* Bar"
        [ ul [] [ li [] [ text "Foo" ] ], hr [] [], ul [] [ li [] [ text "Bar" ] ] ]
    , testEq 31
        [ p [] [ text "If you want a thematic break in a list item, use a different bullet:" ] ]
        "- Foo\n- * * *"
        [ ul [] [ li [] [ text "Foo" ], li [] [ hr [] [] ] ] ]
    ]
