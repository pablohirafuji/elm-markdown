module Test.ATXHeading exposing (..)


import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/


run : List (Output)
run =
    [ testEq 32
        [ p [] [ text "An ATX heading consists of a string of characters, parsed as inline content, between an opening sequence of 1–6 unescaped # characters and an optional closing sequence of any number of unescaped # characters. The opening sequence of # characters must be followed by a space or by the end of line. The optional closing sequence of #s must be preceded by a space and may be followed by spaces only. The opening # character may be indented 0-3 spaces. The raw contents of the heading are stripped of leading and trailing spaces before being parsed as inline content. The heading level is equal to the number of # characters in the opening sequence." ]
        , p [] [ text "Simple headings:" ]
        ]
        "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo"
        [ h1 [] [ text "foo"], h2 [] [ text "foo"], h3 [] [ text "foo"], h4 [] [ text "foo"], h5 [] [ text "foo"], h6 [] [ text "foo"] ]


    , testEq 33
        [ p [] [ text "More than six # characters is not a heading:" ] ]
        "####### foo"
        [ p [] [ text "####### foo" ] ]


    , testEq 34
        [ p [] [ text "At least one space is required between the # characters and the heading’s contents, unless the heading is empty. Note that many implementations currently do not require the space. However, the space was required by the original ATX implementation, and it helps prevent things like the following from being parsed as headings:" ] ]
        "#5 bolt\n\n#hashtag"
        [ p [] [ text "#5 bolt"], p [] [ text "#hashtag"] ]


    , testEq 25
        [ p [] [ text "This is not a heading, because the first # is escaped:" ] ]
        "\\## foo"
        [ p [] [ text "## foo"] ]


    , testEq 36
        [ p [] [ text "Contents are parsed as inlines:" ] ]
        "# foo *bar* \\*baz\\*"
        [ h1 [] [ text "foo ", em [] [ text "bar"], text " *baz*" ] ]


    , testEq 37
        [ p [] [ text "Leading and trailing blanks are ignored in parsing inline content:" ] ]
        "#                  foo                     "
        [ h1 [] [ text "foo" ] ]


    , testEq 38
        [ p [] [ text "One to three spaces indentation are allowed:" ] ]
        "### foo\n  ## foo\n   # foo"
        [ h3 [] [ text "foo" ], h2 [] [ text "foo" ], h1 [] [ text "foo" ] ]


    , testEq 39
        [ p [] [ text "Four spaces are too much:" ] ]
        "    # foo"
        [ pre [] [ code [] [ text "# foo\n" ] ] ]

    , testEq 40
        []
        "foo\n    # bar"
        [ p [] [ text "foo\n# bar" ] ]


    , testEq 41
        [ p [] [ text "A closing sequence of # characters is optional:" ] ]
        "## foo ##\n  ###   bar    ###"
        [ h2 [] [ text "foo"], h3 [] [ text "bar" ] ]


    , testEq 42
        [ p [] [ text "It need not be the same length as the opening sequence:" ] ]
        "# foo ##################################\n##### foo ##"
        [ h1 [] [ text "foo"], h5 [] [ text "foo" ] ]


    , testEq 43
        [ p [] [ text "Spaces are allowed after the closing sequence:" ] ]
        "### foo ###     "
        [ h3 [] [ text "foo"] ]


    , testEq 44
        [ p [] [ text "A sequence of # characters with anything but spaces following it is not a closing sequence, but counts as part of the contents of the heading:" ] ]
        "### foo ### b"
        [ h3 [] [ text "foo ### b"] ]


    , testEq 45
        [ p [] [ text "The closing sequence must be preceded by a space:" ] ]
        "# foo#"
        [ h1 [] [ text "foo#"] ]


    , testEq 46
        [ p [] [ text "Backslash-escaped # characters do not count as part of the closing sequence:" ] ]
        "### foo \\###\n## foo #\\##\n# foo \\#"
        [ h3 [] [ text "foo ###"], h2 [] [ text "foo ###"], h1 [] [ text "foo #"] ]


    , testEq 47
        [ p [] [ text "ATX headings need not be separated from surrounding content by blank lines, and they can interrupt paragraphs:" ] ]
        "****\n## foo\n****"
        [ hr [] [], h2 [] [ text "foo"], hr [] [] ]


    , testEq 48
        []
        "Foo bar\n# baz\nBar foo"
        [ p [] [ text "Foo bar" ], h1 [] [ text "baz"], p [] [ text "Bar foo" ] ]


    , testEq 49
        [ p [] [ text "ATX headings can be empty:" ] ]
        "## \n#\n### ###"
        [ h2 [] [], h1 [] [], h3 [] [] ]
    ]
