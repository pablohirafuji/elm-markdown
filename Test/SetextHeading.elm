module Test.SetextHeading exposing (..)


import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/


run : List (Output msg)
run =
    [ testEq 50
        [ p [] [ text "A setext heading consists of one or more lines of text, each containing at least one non-whitespace character, with no more than 3 spaces indentation, followed by a setext heading underline. The lines of text must be such that, were they not followed by the setext heading underline, they would be interpreted as a paragraph: they cannot be interpretable as a code fence, ATX heading, block quote, thematic break, list item, or HTML block." ]
        , p [] [ text "A setext heading underline is a sequence of = characters or a sequence of - characters, with no more than 3 spaces indentation and any number of trailing spaces. If a line containing a single - can be interpreted as an empty list items, it should be interpreted this way and not as a setext heading underline." ]
        , p [] [ text "The heading is a level 1 heading if = characters are used in the setext heading underline, and a level 2 heading if - characters are used. The contents of the heading are the result of parsing the preceding lines of text as CommonMark inline content." ]
        , p [] [ text "In general, a setext heading need not be preceded or followed by a blank line. However, it cannot interrupt a paragraph, so when a setext heading comes after a paragraph, a blank line is needed between them." ]
        , p [] [ text "Simple examples:" ]
        ]
        "Foo *bar*\n=========\n\nFoo *bar*\n---------"
        [ h1 [] [ text "Foo ", em [] [ text "bar"] ], h2 [] [ text "Foo ", em [] [ text "bar"] ] ]


    , testEq 51
        [ p [] [ text "The content of the header may span more than one line:" ] ]
        "Foo *bar\nbaz*\n===="
        [ h1 [] [ text "Foo ", em [] [ text "bar\nbaz"] ] ]


    , testEq 52
        [ p [] [ text "The underlining can be any length:" ] ]
        "Foo\n-------------------------\n\nFoo\n="
        [ h2 [] [ text "Foo"], h1 [] [ text "Foo" ] ]


    , testEq 53
        [ p [] [ text "The heading content can be indented up to three spaces, and need not line up with the underlining:" ] ]
        "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ==="
        [ h2 [] [ text "Foo"], h2 [] [ text "Foo"], h1 [] [ text "Foo"] ]


    , testEq 54
        [ p [] [ text "Four spaces indent is too much:" ] ]
        "    Foo\n    ---\n\n    Foo\n---"
        [ pre [] [ code [] [ text "Foo\n---\n\nFoo\n" ] ], hr [] [] ]


    , testEq 55
        [ p [] [ text "The setext heading underline can be indented up to three spaces, and may have trailing spaces:" ] ]
        "Foo\n   ----      "
        [ h2 [] [ text "Foo" ] ]


    , testEq 56
        [ p [] [ text "Four spaces is too much:" ] ]
        "Foo\n    ---"
        [ p [] [ text "Foo\n---" ] ]


    , testEq 57
        [ p [] [ text "The setext heading underline cannot contain internal spaces:" ] ]
        "Foo\n= =\n\nFoo\n--- -"
        [ p [] [ text "Foo\n= =" ], p [] [ text "Foo" ], hr [] [] ]


    , testEq 58
        [ p [] [ text "Trailing spaces in the content line do not cause a line break:" ] ]
        "Foo  \n-----"
        [ h2 [] [ text "Foo" ] ]


    , testEq 59
        [ p [] [ text "Nor does a backslash at the end:" ] ]
        "Foo\\\n----"
        [ h2 [] [ text "Foo\\" ] ]


    , testEq 60
        [ p [] [ text "Since indicators of block structure take precedence over indicators of inline structure, the following are setext headings:" ] ]
        "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>"
        [ h2 [] [ text "`Foo" ], p [] [ text "`" ], h2 [] [ text "<a title=\"a lot" ], p [] [ text "of dashes\"/>" ] ]


    , testEq 61
        [ p [] [ text "The setext heading underline cannot be a lazy continuation line in a list item or block quote:" ] ]
        "> Foo\n---"
        [ blockquote [] [ p [] [ text "Foo" ] ], hr [] [] ]

    , testEq 62
        []
        "> foo\nbar\n==="
        [ blockquote [] [ p [] [ text "foo\nbar\n===" ] ] ]

    , testEq 63
        []
        "- Foo\n---"
        [ ul [] [ li [] [ text "Foo" ] ], hr [] [] ]


    , testEq 64
        [ p [] [ text "A blank line is needed between a paragraph and a following setext heading, since otherwise the paragraph becomes part of the heading’s content:" ] ]
        "Foo\nBar\n---"
        [ h2 [] [ text "Foo\nBar" ] ]


    , testEq 65
        [ p [] [ text "But in general a blank line is not required before or after setext headings:" ] ]
        "---\nFoo\n---\nBar\n---\nBaz"
        [ hr [] [], h2 [] [ text "Foo" ], h2 [] [ text "Bar" ], p [] [ text "Baz"] ]


    , testEq 66
        [ p [] [ text "Setext headings cannot be empty:" ] ]
        "\n===="
        [ p [] [ text "====" ] ]


    , testEq 67
        [ p [] [ text "Setext heading text lines must not be interpretable as block constructs other than paragraphs. So, the line of dashes in these examples gets interpreted as a thematic break:" ] ]
        "---\n---"
        [ hr [] [], hr [] [] ]

    , testEq 68
        []
        "- foo\n-----"
        [ ul [] [ li [] [ text "foo" ] ], hr [] [] ]

    , testEq 69
        []
        "    foo\n---"
        [ pre [] [ code [] [ text "foo\n" ] ], hr [] [] ]

    , testEq 70
        []
        "> foo\n-----"
        [ blockquote [] [ p [] [ text "foo" ] ], hr [] [] ]


    , testEq 71
        [ p [] [ text "If you want a heading with > foo as its literal text, you can use backslash escapes:" ] ]
        "\\> foo\n------"
        [ h2 [] [ text "&gt; foo" ] ]

    , testEq 72
        [ p [] [ text "Compatibility note: Most existing Markdown implementations do not allow the text of setext headings to span multiple lines. But there is no consensus about how to interpret" ]
        , pre [] [ code [] [ text "Foo\nbar\n---\nbaz" ] ]
        , p [] [ text "One can find four different interpretations:" ]
        , ol []
            [ li [] [ text "paragraph “Foo”, heading “bar”, paragraph “baz”" ]
            , li [] [ text "paragraph “Foo bar”, thematic break, paragraph “baz”" ]
            , li [] [ text "paragraph “Foo bar — baz”" ]
            , li [] [ text "heading “Foo bar”, paragraph “baz”" ]
            ]
        , p [] [ text "We find interpretation 4 most natural, and interpretation 4 increases the expressive power of CommonMark, by allowing multiline headings. Authors who want interpretation 1 can put a blank line after the first paragraph:" ]
        ]
        "Foo\n\nbar\n---\nbaz"
        [ p [] [ text "Foo" ], h2 [] [ text "bar" ], p [] [ text "baz" ] ]


    , testEq 73
        [ p [] [ text "Authors who want interpretation 2 can put blank lines around the thematic break," ] ]
        "Foo\nbar\n\n---\n\nbaz"
        [ p [] [ text "Foo\nbar" ], hr [] [], p [] [ text "baz" ] ]


    , testEq 74
        [ p [] [ text "or use a thematic break that cannot count as a setext heading underline, such as" ] ]
        "Foo\nbar\n* * *\nbaz"
        [ p [] [ text "Foo\nbar" ], hr [] [], p [] [ text "baz" ] ]


    , testEq 75
        [ p [] [ text "Authors who want interpretation 3 can use backslash escapes:" ] ]
        "Foo\nbar\n\\---\nbaz"
        [ p [] [ text "Foo\nbar\n---\nbaz" ] ]
    ]

