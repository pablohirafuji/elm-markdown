

-- Setext headings


setextHeadings : List (Html msg)
setextHeadings =
    [ h2 [] [ text "Setext headings" ]
    , p [] [ text "A setext heading consists of one or more lines of text, each containing at least one non-whitespace character, with no more than 3 spaces indentation, followed by a setext heading underline. The lines of text must be such that, were they not followed by the setext heading underline, they would be interpreted as a paragraph: they cannot be interpretable as a code fence, ATX heading, block quote, thematic break, list item, or HTML block." ]
    , p [] [ text "A setext heading underline is a sequence of = characters or a sequence of - characters, with no more than 3 spaces indentation and any number of trailing spaces. If a line containing a single - can be interpreted as an empty list items, it should be interpreted this way and not as a setext heading underline." ]
    , p [] [ text "The heading is a level 1 heading if = characters are used in the setext heading underline, and a level 2 heading if - characters are used. The contents of the heading are the result of parsing the preceding lines of text as CommonMark inline content." ]
    , p [] [ text "In general, a setext heading need not be preceded or followed by a blank line. However, it cannot interrupt a paragraph, so when a setext heading comes after a paragraph, a blank line is needed between them." ]
    , p [] [ text "Simple examples:" ]
    -- Example 50
    , testTypeOfLine
        "Foo *bar*\n=========\n\nFoo *bar*\n---------" Equal
        [ h1 [] [ text "Foo ", em [] [ text "bar"] ], h2 [] [ text "Foo ", em [] [ text "bar"] ] ]

    , p [] [ text "The content of the header may span more than one line:" ]
    -- Example 51
    , testTypeOfLine
        "Foo *bar\nbaz*\n====" Equal
        [ h1 [] [ text "Foo ", em [] [ text "bar\nbaz"] ] ]

    , p [] [ text "The underlining can be any length:" ]
    -- Example 52
    , testTypeOfLine
        "Foo\n-------------------------\n\nFoo\n=" Equal
        [ h2 [] [ text "Foo"], h1 [] [ text "Foo" ] ]

    , p [] [ text "The heading content can be indented up to three spaces, and need not line up with the underlining:" ]
    -- Example 53
    , testTypeOfLine
        "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===" Equal
        [ h2 [] [ text "Foo"], h2 [] [ text "Foo"], h1 [] [ text "Foo"] ]

    , p [] [ text "Four spaces indent is too much:" ]
    -- Example 54
    , testTypeOfLine
        "    Foo\n    ---\n\n    Foo\n---" Equal
        [ pre [] [ code [] [ text "    Foo\n    ---\n\n    Foo" ] ], hr [] [] ]

    , p [] [ text "The setext heading underline can be indented up to three spaces, and may have trailing spaces:" ]
    -- Example 55
    , testTypeOfLine
        "Foo\n   ----      " Equal
        [ h2 [] [ text "Foo" ] ]

    , p [] [ text "Four spaces is too much:" ]
    -- Example 56
    , testTypeOfLine
        "Foo\n    ---" Equal
        [ p [] [ text "Foo\n---" ] ]

    , p [] [ text "The setext heading underline cannot contain internal spaces:" ]
    -- Example 57
    , testTypeOfLine
        "Foo\n= =\n\nFoo\n--- -" Equal
        [ p [] [ text "Foo\n= =" ], p [] [ text "Foo" ], hr [] [] ]

    , p [] [ text "Trailing spaces in the content line do not cause a line break:" ]
    -- Example 58
    , testTypeOfLine
        "Foo  \n-----" Equal
        [ h2 [] [ text "Foo" ] ]

    , p [] [ text "Nor does a backslash at the end:" ]
    -- Example 59
    , testTypeOfLine
        "Foo\\\n----" Equal
        [ h2 [] [ text "Foo\\" ] ]

    , p [] [ text "Since indicators of block structure take precedence over indicators of inline structure, the following are setext headings:" ]
    -- Example 60
    , testTypeOfLine
        "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>" Equal
        [ h2 [] [ text "`Foo" ], p [] [ text "`" ], h2 [] [ text "<a title=\"a lot" ], p [] [ text "of dashes\"/>" ] ]

    , p [] [ text "The setext heading underline cannot be a lazy continuation line in a list item or block quote:" ]
    -- Example 61
    , testTypeOfLine
        "> Foo\n---" Equal
        [ blockquote [] [ p [] [ text "Foo" ] ], hr [] [] ]

    -- Example 62
    , testTypeOfLine
        "> foo\nbar\n===" Equal
        [ blockquote [] [ p [] [ text "foo\nbar\n===" ] ] ]

    -- Example 63
    , testTypeOfLine
        "- Foo\n---" Equal
        [ ul [] [ li [] [ text "Foo" ] ], hr [] [] ]

    , p [] [ text "A blank line is needed between a paragraph and a following setext heading, since otherwise the paragraph becomes part of the heading’s content:" ]
    -- Example 64
    , testTypeOfLine
        "Foo\nBar\n---" Equal
        [ h2 [] [ text "Foo\nBar" ] ]

    , p [] [ text "But in general a blank line is not required before or after setext headings:" ]
    -- Example 65
    , testTypeOfLine
        "---\nFoo\n---\nBar\n---\nBaz" Equal
        [ hr [] [], h2 [] [ text "Foo" ], h2 [] [ text "Bar" ], p [] [ text "Baz"] ]

    , p [] [ text "Setext headings cannot be empty:" ]
    -- Example 66
    , testTypeOfLine
        "\n====" Equal
        [ p [] [ text "====" ] ]

    , p [] [ text "Setext heading text lines must not be interpretable as block constructs other than paragraphs. So, the line of dashes in these examples gets interpreted as a thematic break:" ]
    -- Example 67
    , testTypeOfLine
        "---\n---" Equal
        [ hr [] [], hr [] [] ]

    -- Example 68
    , testTypeOfLine
        "- foo\n-----" Equal
        [ ul [] [ li [] [ text "foo" ] ], hr [] [] ]

    -- Example 69
    , testTypeOfLine
        "    foo\n---" Equal
        [ pre [] [ code [] [ text "foo" ] ], hr [] [] ]

    -- Example 70
    , testTypeOfLine
        "> foo\n-----" Equal
        [ blockquote [] [ p [] [ text "foo" ] ], hr [] [] ]

    , p [] [ text "If you want a heading with > foo as its literal text, you can use backslash escapes:" ]
    -- Example 71
    , testTypeOfLine
        "\\> foo\n------" Equal
        [ h2 [] [ text "&gt; foo" ] ]

    , p [] [ text "Compatibility note: Most existing Markdown implementations do not allow the text of setext headings to span multiple lines. But there is no consensus about how to interpret" ]
    , pre [] [ code [] [ text "Foo\nbar\n---\nbaz" ] ]
    , p [] [ text "One can find four different interpretations:" ]
    , ol []
        [ li [] [ text "paragraph “Foo”, heading “bar”, paragraph “baz”" ]
        , li [] [ text "paragraph “Foo bar”, thematic break, paragraph “baz”" ]
        , li [] [ text "paragraph “Foo bar — baz”" ]
        , li [] [ text "heading “Foo bar”, paragraph “baz”" ]
        ]
    , p [] [ text "We find interpretation 4 most natural, and interpretation 4 increases the expressive power of CommonMark, by allowing multiline headings. Authors who want interpretation 1 can put a blank line after the first paragraph:" ]
    -- Example 72
    , testTypeOfLine
        "Foo\n\nbar\n---\nbaz" Equal
        [ p [] [ text "Foo" ], h2 [] [ text "bar" ], p [] [ text "baz" ] ]

    , p [] [ text "Authors who want interpretation 2 can put blank lines around the thematic break," ]
    -- Example 73
    , testTypeOfLine
        "Foo\nbar\n\n---\n\nbaz" Equal
        [ p [] [ text "Foo\nbar" ], hr [] [], p [] [ text "baz" ] ]

    , p [] [ text "or use a thematic break that cannot count as a setext heading underline, such as" ]
    -- Example 74
    , testTypeOfLine
        "Foo\nbar\n* * *\nbaz" Equal
        [ p [] [ text "Foo\nbar" ], hr [] [], p [] [ text "baz" ] ]

    , p [] [ text "Authors who want interpretation 3 can use backslash escapes:" ]
    -- Example 75
    , testTypeOfLine
        "Foo\nbar\n\\---\nbaz" Equal
        [ p [] [ text "Foo\nbar\n---\nbaz" ] ]

    ]

