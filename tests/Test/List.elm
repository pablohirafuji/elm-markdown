module Test.List exposing (..)


import Html exposing (..)
import Html.Attributes exposing (start)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#lists


run : List (Output)
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
        [ p [] [ text "In order to solve of unwanted lists in paragraphs with hard-wrapped numerals, we allow only lists starting with 1 to interrupt paragraphs. Thus," ]
        ]
        "The number of windows in my house is\n14.  The number of doors is 6."
        [ p [] [ text "The number of windows in my house is\n14.  The number of doors is 6." ] ]


    , testEq 266
        [ p [] [ text "We may still get an unintended result in cases like" ]
        ]
        "The number of windows in my house is\n1.  The number of doors is 6."
        [ p [] [ text "The number of windows in my house is" ], ol [] [ li [] [ text "The number of doors is 6." ] ] ]


    , testEq 267
        [ p [] [ text "There can be any number of blank lines between items:" ]
        ]
        "- foo\n\n- bar\n\n\n- baz"
        [ ul [] [ li [] [ p [] [ text "foo" ] ], li [] [ p [] [ text "bar" ] ], li [] [ p [] [ text "baz" ] ] ] ]


    , testEq 268
        []
        "- foo\n  - bar\n    - baz\n\n\n      bim"
        [ ul [] [ li [] [ text "foo", ul [] [ li [] [ text "bar", ul [] [ li [] [ p [] [ text "baz" ], p [] [ text "bim" ] ] ] ] ] ] ] ]


    , testEq 269
        [ p [] [ text "To separate consecutive lists of the same type, or to separate a list from an indented code block that would otherwise be parsed as a subparagraph of the final list item, you can insert a blank HTML comment:" ]
        ]
        "- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim"
        [ ul []
            [ li [] [ text "foo" ]
            , li [] [ text "bar" ]
            ]
        , p [] [ text "<!-- -->" ]
        , ul []
            [ li [] [ text "baz" ]
            , li [] [ text "bim" ]
            ]
        ]


    , testEq 270
        []
        "-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code"
        [ ul []
            [ li []
                [ p [] [ text "foo" ]
                , p [] [ text "notcode" ]
                ]
            , li [] [ p [] [ text "foo" ] ] ]
            , p [] [ text "<!-- -->" ]
            , pre [] [ code [] [ text "code\n" ]
            ]
        ]


    , testEq 271
        [ p [] [ text "List items need not be indented to the same level. The following list items will be treated as items at the same list level, since none is indented enough to belong to the previous list item:" ]
        ]
        "- a\n - b\n  - c\n   - d\n    - e\n   - f\n  - g\n - h\n- i"
        [ ul [] [ li [] [ text "a" ], li [] [ text "b" ], li [] [ text "c" ], li [] [ text "d" ], li [] [ text "e" ], li [] [ text "f" ], li [] [ text "g" ], li [] [ text "h" ], li [] [ text "i" ] ] ]


    , testEq 272
        []
        "1. a\n\n  2. b\n\n    3. c"
        [ ol [] [ li [] [ p [] [ text "a" ] ], li [] [ p [] [ text "b" ] ], li [] [ p [] [ text "c" ] ] ] ]


    , testEq 273
        [ p [] [ text "This is a loose list, because there is a blank line between two of the list items:" ]
        ]
        "- a\n- b\n\n- c"
        [ ul [] [ li [] [ p [] [ text "a" ] ], li [] [ p [] [ text "b" ] ], li [] [ p [] [ text "c" ] ] ] ]


    , testEq 274
        [ p [] [ text "So is this, with a empty second item:" ]
        ]
        "* a\n*\n\n* c"
        [ ul [] [ li [] [ p [] [ text "a" ] ], li [] [], li [] [ p [] [ text "c" ] ] ] ]


    , testEq 275
        [ p [] [ text "These are loose lists, even though there is no space between the items, because one of the items directly contains two block-level elements with a blank line between them:" ]
        ]
        "- a\n- b\n\n  c\n- d"
        [ ul [] [ li [] [ p [] [ text "a" ] ], li [] [ p [] [ text "b" ], p [] [ text "c" ] ], li [] [ p [] [ text "d" ] ] ] ]


    , testEq 276
        []
        "- a\n- b\n\n  [ref]: /url\n- d"
        [ ul [] [ li [] [ p [] [ text "a" ] ], li [] [ p [] [ text "b" ] ], li [] [ p [] [ text "d" ] ] ] ]


    , testEq 277
        [ p [] [ text "This is a tight list, because the blank lines are in a code block:" ]
        ]
        "- a\n- ```\n  b\n\n\n  ```\n- c"
        [ ul []
            [ li [] [ text "a" ]
            , li []
                [ pre []
                    [ code []
                        [ text "b\n\n\n" ]
                    ]
                ]
            , li [] [ text "c" ]
            ]
        ]


    , testEq 278
        [ p [] [ text "This is a tight list, because the blank line is between two paragraphs of a sublist. So the sublist is loose while the outer list is tight:" ]
        ]
        "- a\n  - b\n\n    c\n- d"
        [ ul [] [ li [] [ text "a", ul [] [ li [] [ p [] [ text "b" ], p [] [ text "c" ] ] ] ], li [] [ text "d" ] ] ]


    , testEq 279
        [ p [] [ text "This is a tight list, because the blank line is inside the block quote:" ]
        ]
        "* a\n  > b\n  >\n* c"
        [ ul [] [ li [] [ text "a", blockquote [] [ p [] [ text "b" ] ] ], li [] [ text "c" ] ] ]


    , testEq 280
        [ p [] [ text "This list is tight, because the consecutive block elements are not separated by blank lines:" ]
        ]
        "- a\n  > b\n  ```\n  c\n  ```\n- d"
        [ ul [] [ li [] [ text "a", blockquote [] [ p [] [ text "b" ] ], pre [] [ code [] [ text "c\n" ] ] ], li [] [ text "d" ] ] ]


    , testEq 281
        [ p [] [ text "A single-paragraph list is tight:" ]
        ]
        "- a"
        [ ul [] [ li [] [ text "a" ] ] ]


    , testEq 282
        []
        "- a\n  - b"
        [ ul [] [ li [] [ text "a", ul [] [ li [] [ text "b" ] ] ] ] ]


    , testEq 283
        [ p [] [ text "This list is loose, because of the blank line between the two block elements in the list item:" ]
        ]
        "1. ```\n   foo\n   ```\n\n   bar"
        [ ol [] [ li [] [ pre [] [ code [] [ text "foo\n" ] ], p [] [ text "bar" ] ] ] ]


    , testEq 284
        [ p [] [ text "Here the outer list is loose, the inner list tight:" ]
        ]
        "* foo\n  * bar\n\n  baz"
        [ ul [] [ li [] [ p [] [ text "foo" ], ul [] [ li [] [ text "bar" ] ], p [] [ text "baz" ] ] ] ]


    , testEq 285
        []
        "- a\n  - b\n  - c\n\n- d\n  - e\n  - f"
        [ ul [] [ li [] [ p [] [ text "a" ], ul [] [ li [] [ text "b" ], li [] [ text "c" ] ] ], li [] [ p [] [ text "d" ], ul [] [ li [] [ text "e" ], li [] [ text "f" ] ] ] ] ]

    ]

