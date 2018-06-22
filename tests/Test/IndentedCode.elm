module Test.IndentedCode exposing (..)

import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#indented-code-blocks


run : List (Output msg)
run =
    [ testEq 76
        [ p [] [ text "An indented code block is composed of one or more indented chunks separated by blank lines. An indented chunk is a sequence of non-blank lines, each indented four or more spaces. The contents of the code block are the literal contents of the lines, including trailing line endings, minus four spaces of indentation. An indented code block has no info string." ]
        , p [] [ text "An indented code block cannot interrupt a paragraph, so there must be a blank line between a paragraph and a following indented code block. (A blank line is not needed, however, between a code block and a following paragraph.)" ]
        ]
        "    a simple\n      indented code block"
        [ pre [] [ code [] [ text "a simple\n  indented code block\n" ] ] ]
    , testEq 77
        [ p [] [ text "If there is any ambiguity between an interpretation of indentation as a code block and as indicating that material belongs to a list item, the list item interpretation takes precedence:" ]
        ]
        "  - foo\n\n    bar"
        [ ul [] [ li [] [ p [] [ text "foo" ], p [] [ text "bar" ] ] ] ]
    , testEq 78
        []
        "1.  foo\n\n    - bar"
        [ ol [] [ li [] [ p [] [ text "foo" ], ul [] [ li [] [ text "bar" ] ] ] ] ]
    , testEq 79
        [ p [] [ text "The contents of a code block are literal text, and do not get parsed as Markdown:" ] ]
        "    <a/>\n    *hi*\n\n    - one"
        [ pre [] [ code [] [ text "<a/>\n*hi*\n\n- one\n" ] ] ]
    , testEq 80
        [ p [] [ text "Here we have three chunks separated by blank lines:" ] ]
        "    chunk1\n\n    chunk2\n  \n \n \n    chunk3"
        [ pre [] [ code [] [ text "chunk1\n\nchunk2\n\n\n\nchunk3\n" ] ] ]
    , testEq 81
        [ p [] [ text "Any initial spaces beyond four will be included in the content, even in interior blank lines:" ] ]
        "    chunk1\n      \n      chunk2"
        [ pre [] [ code [] [ text "chunk1\n  \n  chunk2\n" ] ] ]
    , testEq 82
        [ p [] [ text "An indented code block cannot interrupt a paragraph. (This allows hanging indents and the like.)" ] ]
        "Foo\n    bar"
        [ p [] [ text "Foo\nbar" ] ]
    , testEq 83
        [ p [] [ text "However, any non-blank line with fewer than four leading spaces ends the code block immediately. So a paragraph may occur immediately after indented code:" ] ]
        "    foo\nbar"
        [ pre [] [ code [] [ text "foo\n" ] ], p [] [ text "bar" ] ]
    , testEq 84
        [ p [] [ text "And indented code can occur immediately before and after other kinds of blocks:" ] ]
        "# Heading\n    foo\nHeading\n------\n    foo\n----"
        [ h1 [] [ text "Heading" ], pre [] [ code [] [ text "foo\n" ] ], h2 [] [ text "Heading" ], pre [] [ code [] [ text "foo\n" ] ], hr [] [] ]
    , testEq 85
        [ p [] [ text "The first line can be indented more than four spaces:" ] ]
        "        foo\n    bar"
        [ pre [] [ code [] [ text "    foo\nbar\n" ] ] ]
    , testEq 86
        [ p [] [ text "Blank lines preceding or following an indented code block are not included in it:" ] ]
        "\n    \n    foo\n    "
        [ pre [] [ code [] [ text "foo\n" ] ] ]
    , testEq 87
        [ p [] [ text "Trailing spaces are included in the code block’s content:" ] ]
        "    foo  "
        [ pre [] [ code [] [ text "foo  \n" ] ] ]
    ]
