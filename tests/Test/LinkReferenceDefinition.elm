module Test.LinkReferenceDefinition exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, title)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#link-reference-definitions


run : List (Output msg)
run =
    [ testEq 157
        []
        "[foo]: /url \"title\"\n\n[foo]\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]

    , testEq 158
        []
        "   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n"
        [ p []
            [ a [ href "/url", title "the title" ]
                [ text "foo" ]
            ]
        ]

    , testEq 159
        []
        "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n"
        [ p []
            [ a [ href "my_(url)", title "title (with parens)" ]
                [ text "Foo*bar]" ]
            ]
        ]

    , testEq 160
        []
        "[Foo bar]:\n<my%20url>\n'title'\n\n[Foo bar]\n"
        [ p []
            [ a [ href "my%20url", title "title" ]
                [ text "Foo bar" ]
            ]
        ]

    , testEq 161
        []
        "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n"
        [ p []
            [ a [ href "/url", title "\ntitle\nline1\nline2\n" ]
                [ text "foo" ]
            ]
        ]

    , testEq 162
        []
        "[foo]: /url 'title\n\nwith blank line'\n\n[foo]\n"
        [ p []
            [ text "[foo]: /url 'title" ]
        , p []
            [ text "with blank line'" ]
        , p []
            [ text "[foo]" ]
        ]

    , testEq 163
        []
        "[foo]:\n/url\n\n[foo]\n"
        [ p []
            [ a [ href "/url" ]
                [ text "foo" ]
            ]
        ]

    , testEq 164
        []
        "[foo]:\n\n[foo]\n"
        [ p []
            [ text "[foo]:" ]
        , p []
            [ text "[foo]" ]
        ]

    , testEq 165
        []
        "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n"
        [ p []
            [ a [ href "/url%5Cbar*baz", title "foo\"bar\\baz" ]
                [ text "foo" ]
            ]
        ]

    , testEq 166
        []
        "[foo]\n\n[foo]: url\n"
        [ p []
            [ a [ href "url" ]
                [ text "foo" ]
            ]
        ]

    , testEq 167
        []
        "[foo]\n\n[foo]: first\n[foo]: second\n"
        [ p []
            [ a [ href "first" ]
                [ text "foo" ]
            ]
        ]

    , testEq 168
        []
        "[FOO]: /url\n\n[Foo]\n"
        [ p []
            [ a [ href "/url" ]
                [ text "Foo" ]
            ]
        ]

    , testEq 169
        []
        "[ΑΓΩ]: /φου\n\n[αγω]\n"
        [ p []
            [ a [ href "/%CF%86%CE%BF%CF%85" ]
                [ text "αγω" ]
            ]
        ]

    , testEq 170
        []
        "[foo]: /url\n"
        []

    , testEq 171
        []
        "[\nfoo\n]: /url\nbar\n"
        [ p []
            [ text "bar" ]
        ]

    , testEq 172
        []
        "[foo]: /url \"title\" ok\n"
        [ p []
            [ text "[foo]: /url \"title\" ok" ]
        ]

    , testEq 173
        []
        "[foo]: /url\n\"title\" ok\n"
        [ p []
            [ text "\"title\" ok" ]
        ]

    , testEq 174
        []
        "    [foo]: /url \"title\"\n\n[foo]\n"
        [ pre []
            [ code []
                [ text "[foo]: /url \"title\"\n" ]
            ]
        , p []
            [ text "[foo]" ]
        ]

    , testEq 175
        []
        "```\n[foo]: /url\n```\n\n[foo]\n"
        [ pre []
            [ code []
                [ text "[foo]: /url\n" ]
            ]
        , p []
            [ text "[foo]" ]
        ]

    , testEq 176
        []
        "Foo\n[bar]: /baz\n\n[bar]\n"
        [ p []
            [ text "Foo\n[bar]: /baz" ]
        , p []
            [ text "[bar]" ]
        ]

    , testEq 177
        []
        "# [Foo]\n[foo]: /url\n> bar\n"
        [ h1 []
            [ a [ href "/url" ]
                [ text "Foo" ]
            ]
        , blockquote []
            [ p []
                [ text "bar" ]
            ]
        ]

    , testEq 178
        []
        "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]\n"
        [ p []
            [ a [ href "/foo-url", title "foo" ]
                [ text "foo" ]
            , text ",\n"
            , a [ href "/bar-url", title "bar" ]
                [ text "bar" ]
            , text ",\n"
            , a [ href "/baz-url" ]
                [ text "baz" ]
            ]
        ]

    , testEq 179
        []
        "[foo]\n\n> [foo]: /url\n"
        [ p []
            [ a [ href "/url" ]
                [ text "foo" ]
            ]
        , blockquote []
            []
        ]
    ]
