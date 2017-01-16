module Test.Inline.Code exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, attribute)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#code-spans


run : List (Output)
run =
    [ testEq 312
        []
        "`foo`\n"
        [ p []
            [ code []
                [ text "foo" ]
            ]
        ]

    , testEq 313
        []
        "`` foo ` bar  ``\n"
        [ p []
            [ code []
                [ text "foo ` bar" ]
            ]
        ]

    , testEq 314
        []
        "` `` `\n"
        [ p []
            [ code []
                [ text "``" ]
            ]
        ]

    , testEq 315
        []
        "``\nfoo\n``\n"
        [ p []
            [ code []
                [ text "foo" ]
            ]
        ]

    , testEq 316
        []
        "`foo   bar\n  baz`\n"
        [ p []
            [ code []
                [ text "foo bar baz" ]
            ]
        ]

    , testEq 317
        []
        "`a  b`\n"
        [ p []
            [ code []
                [ text "a  b" ]
            ]
        ]

    , testEq 318
        []
        "`foo `` bar`\n"
        [ p []
            [ code []
                [ text "foo `` bar" ]
            ]
        ]

    , testEq 319
        []
        "`foo\\`bar`\n"
        [ p []
            [ code []
                [ text "foo\\" ]
            , text "bar`"
            ]
        ]

    , testEq 320
        []
        "*foo`*`\n"
        [ p []
            [ text "*foo"
            , code []
                [ text "*" ]
            ]
        ]

    , testEq 321
        []
        "[not a `link](/foo`)\n"
        [ p []
            [ text "[not a "
            , code []
                [ text "link](/foo" ]
            , text ")"
            ]
        ]

    , testEq 322
        []
        "`<a href=\"`\">`\n"
        [ p []
            [ code []
                [ text "<a href=\"" ]
            , text "\">`"
            ]
        ]

    , testEq 323
        []
        "<a href=\"`\">`\n"
        [ p []
            [ a [ attribute "href" "`" ] []
            , text "`"
            ]
        ]

    , testEq 324
        []
        "`<http://foo.bar.`baz>`\n"
        [ p []
            [ code []
                [ text "<http://foo.bar." ]
            , text "baz>`"
            ]
        ]

    , testEq 325
        []
        "<http://foo.bar.`baz>`\n"
        [ p []
            [ a [ href "http://foo.bar.%60baz" ]
                [ text "http://foo.bar.`baz" ]
            , text "`"
            ]
        ]

    , testEq 326
        []
        "```foo``\n"
        [ p []
            [ text "```foo``" ]
        ]

    , testEq 327
        []
        "`foo\n"
        [ p []
            [ text "`foo" ]
        ]
    ]