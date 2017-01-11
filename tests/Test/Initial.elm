module Test.Initial exposing (run)


import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/


run : List (Output)
run =
    [ testEq 1
        []
        "\tfoo\tbaz\t\tbim\n"
        [ pre []
            [ code []
                [ text "foo\tbaz\t\tbim\n" ]
            ]
        ]

    , testEq 2
        []
        "  \tfoo\tbaz\t\tbim\n"
        [ pre []
            [ code []
                [ text "foo\tbaz\t\tbim\n" ]
            ]
        ]

    , testEq 3
        []
        "    a\ta\n    ὐ\ta\n"
        [ pre []
            [ code []
                [ text "a\ta\nὐ\ta\n" ]
            ]
        ]

    , testEq 4
        []
        "  - foo\n\n\tbar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , p []
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 5
        []
        "- foo\n\n\t\tbar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , pre []
                    [ code []
                        [ text "bar\n" ]
                    ]
                ]
            ]
        ]

    , testEq 6
        []
        ">\t\tfoo\n"
        [ blockquote []
            [ pre []
                [ code []
                    [ text "\tfoo\n" ]
                ]
            ]
        ]

    , testEq 7
        []
        "-\t\tfoo\n"
        [ ul []
            [ li []
                [ pre []
                    [ code []
                        [ text "foo\n" ]
                    ]
                ]
            ]
        ]

    , testEq 8
        []
        "    foo\n\tbar\n"
        [ pre []
            [ code []
                [ text "foo\nbar\n" ]
            ]
        ]

    , testEq 9
        []
        " - foo\n   - bar\n\t - baz\n"
        [ ul []
            [ li []
                [ text "foo"
                , ul []
                    [ li []
                        [ text "bar"
                        , ul []
                            [ li []
                                [ text "baz" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    , testEq 10
        []
        "#\tFoo\n"
        [ h1 []
            [ text "Foo" ]
        ]

    , testEq 11
        []
        "*\t*\t*\t\n"
        [ hr []
            []
        ]

    , testEq 12
        []
        "- `one\n- two`\n"
        [ ul []
            [ li []
                [ text "`one" ]
            , li []
                [ text "two`" ]
            ]
        ]
    ]