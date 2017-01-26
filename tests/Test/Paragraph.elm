module Test.Paragraph exposing (..)


import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#paragraphs


run : List (Output msg)
run =
    [ testEq 180
        [ p [] [ text "A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph. The contents of the paragraph are the result of parsing the paragraph’s raw content as inlines. The paragraph’s raw content is formed by concatenating the lines and removing initial and final whitespace." ]
        , p [] [ text "A simple example with two paragraphs:" ]
        ]
        "aaa\n\nbbb"
        [ p [] [ text "aaa" ], p [] [ text "bbb" ] ]


    , testEq 181
        [ p [] [ text "Paragraphs can contain multiple lines, but no blank lines:" ]
        ]
        "aaa\nbbb\n\nccc\nddd"
        [ p [] [ text "aaa\nbbb" ], p [] [ text "ccc\nddd" ] ]


    , testEq 182
        [ p [] [ text "Multiple blank lines between paragraph have no effect:" ]
        ]
        "aaa\n\n\nbbb"
        [ p [] [ text "aaa" ], p [] [ text "bbb" ] ]


    , testEq 183
        [ p [] [ text "Leading spaces are skipped:" ]
        ]
        "  aaa\n bbb"
        [ p [] [ text "aaa\nbbb" ] ]


    , testEq 184
        [ p [] [ text "Lines after the first may be indented any amount, since indented code blocks cannot interrupt paragraphs." ]
        ]
        "aaa\n             bbb\n                                       ccc"
        [ p [] [ text "aaa\nbbb\nccc" ] ]


    , testEq 185
        [ p [] [ text "However, the first line may be indented at most three spaces, or an indented code block will be triggered:" ]
        ]
        "   aaa\nbbb"
        [ p [] [ text "aaa\nbbb" ] ]


    , testEq 186
        []
        "    aaa\nbbb"
        [ pre [] [ code [] [ text "aaa\n" ] ], p [] [ text "bbb" ] ]


    , testEq 187
        [ p [] [ text "Final spaces are stripped before inline parsing, so a paragraph that ends with two or more spaces will not end with a hard line break:" ]
        ]
        "aaa     \nbbb     "
        [ p [] [ text "aaa", br [] [], text "bbb" ] ]

    ]

