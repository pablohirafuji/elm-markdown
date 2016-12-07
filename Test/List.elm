module Test.List exposing (..)


import Html exposing (..)
import Html.Attributes exposing (start)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#lists


run : List (Output msg)
run =
    [ testEq 262
        [ p [] [ text "A list is a sequence of one or more list items of the same type. The list items may be separated by any number of blank lines." ]
        , p [] [ text "Two list items are of the same type if they begin with a list marker of the same type. Two list markers are of the same type if (a) they are bullet list markers using the same character (-, +, or *) or (b) they are ordered list numbers with the same delimiter (either . or ))." ]
        , p [] [ text "A list is an ordered list if its constituent list items begin with ordered list markers, and a bullet list if its constituent list items begin with bullet list markers." ]
        , p [] [ text "The start number of an ordered list is determined by the list number of its initial list item. The numbers of subsequent list items are disregarded." ]
        , p [] [ text "A list is loose if any of its constituent list items are separated by blank lines, or if any of its constituent list items directly contain two block-level elements with a blank line between them. Otherwise a list is tight. (The difference in HTML output is that paragraphs in a loose list are wrapped in <p> tags, while paragraphs in a tight list are not.)" ]
        , p [] [ text "Changing the bullet or ordered list delimiter starts a new list:" ]
        ]
        "- foo\n- bar\n+ baz"
        [ ul [] [ li [] [ text "foo" ], li [] [ text "bar" ] ], ul [] [ li [] [ text "baz" ] ] ]


    , testEq 263
        []
        "1. foo\n2. bar\n3) baz"
        [ ol [] [ li [] [ text "foo" ], li [] [ text "bar" ] ], ol [ start 3 ] [ li [] [ text "baz" ] ] ]


    , testEq 264
        [ p [] [ text "In CommonMark, a list can interrupt a paragraph. That is, no blank line is needed to separate a paragraph from a following list:" ]
        ]
        "Foo\n- bar\n- baz"
        [ p [] [ text "Foo" ], ul [] [ li [] [ text "bar" ], li [] [ text "baz" ] ] ]


    , testEq 265
        [ p [] [ text "" ]
        ]
        ""
        [ ul [] [ li [] [  ] ] ]


    , testEq 266
        [ p [] [ text "" ]
        ]
        ""
        [ ul [] [ li [] [  ] ] ]


    , testEq 267
        [ p [] [ text "" ]
        ]
        ""
        [ ul [] [ li [] [  ] ] ]


    , testEq 268
        [ p [] [ text "" ]
        ]
        ""
        [ ul [] [ li [] [  ] ] ]


    , testEq 269
        [ p [] [ text "" ]
        ]
        ""
        [ ul [] [ li [] [  ] ] ]


    , testEq 27
        [ p [] [ text "" ]
        ]
        ""
        [ ul [] [ li [] [  ] ] ]


    ]

