module Test.Inline.Escape exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, src, class, title, attribute)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#backslash-escapes


run : List (Output msg)
run =
    [ testEq 286
        []
        "`hi`lo`\n"
        [ p []
            [ code []
                [ text "hi" ]
            , text "lo`"
            ]
        ]

    , testEq 287
        []
        "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n"
        [ p []
            [ text "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" ]
        ]

    , testEq 288
        []
        "\\→\\A\\a\\ \\3\\φ\\«\n"
        [ p []
            [ text "\\→\\A\\a\\ \\3\\φ\\«" ]
        ]

    , testEq 289
        []
        "\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n"
        [ p []
            [ text "*not emphasized*\n<br/> not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url \"not a reference\"" ]
        ]

    , testEq 290
        []
        "\\\\*emphasis*\n"
        [ p []
            [ text "\\"
            , em []
                [ text "emphasis" ]
            ]
        ]

    , testEq 291
        []
        "foo\\\nbar\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "bar"
            ]
        ]

    , testEq 292
        []
        "`` \\[\\` ``\n"
        [ p []
            [ code []
                [ text "\\[\\`" ]
            ]
        ]

    , testEq 293
        []
        "    \\[\\]\n"
        [ pre []
            [ code []
                [ text "\\[\\]\n" ]
            ]
        ]

    , testEq 294
        []
        "~~~\n\\[\\]\n~~~\n"
        [ pre []
            [ code []
                [ text "\\[\\]\n" ]
            ]
        ]

    , testEq 295
        []
        "<http://example.com?find=\\*>\n"
        [ p []
            [ a [ href "http://example.com?find=%5C*" ]
                [ text "http://example.com?find=\\*" ]
            ]
        ]

    , testEq 296
        []
        "<a href=\"/bar\\/)\">\n"
        [ a [ attribute "href" "/bar\\/)" ] [] ]

    , testEq 297
        []
        "[foo](/bar\\* \"ti\\*tle\")\n"
        [ p []
            [ a [ href "/bar*", title "ti*tle" ]
                [ text "foo" ]
            ]
        ]

    , testEq 298
        []
        "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"\n"
        [ p []
            [ a [ href "/bar*", title "ti*tle" ]
                [ text "foo" ]
            ]
        ]

    , testEq 299
        []
        "``` foo\\+bar\nfoo\n```\n"
        [ pre []
            [ code [ class "language-foo+bar" ]
                [ text "foo\n" ]
            ]
        ]
       ]