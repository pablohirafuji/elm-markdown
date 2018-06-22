module Test.Inline.Images exposing (run)

import Html exposing (..)
import Html.Attributes exposing (alt, href, src, title)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#images


run : List (Output msg)
run =
    [ testEq 540
        []
        "![foo](/url \"title\")\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 541
        []
        "![foo *bar*]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n"
        [ p []
            [ img [ alt "foo bar", src "train.jpg", title "train & tracks" ]
                []
            ]
        ]
    , testEq 542
        []
        "![foo ![bar](/url)](/url2)\n"
        [ p []
            [ img [ alt "foo bar", src "/url2" ]
                []
            ]
        ]
    , testEq 543
        []
        "![foo [bar](/url)](/url2)\n"
        [ p []
            [ img [ alt "foo bar", src "/url2" ]
                []
            ]
        ]
    , testEq 544
        []
        "![foo *bar*][]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n"
        [ p []
            [ img [ alt "foo bar", src "train.jpg", title "train & tracks" ]
                []
            ]
        ]
    , testEq 545
        []
        "![foo *bar*][foobar]\n\n[FOOBAR]: train.jpg \"train & tracks\"\n"
        [ p []
            [ img [ alt "foo bar", src "train.jpg", title "train & tracks" ]
                []
            ]
        ]
    , testEq 546
        []
        "![foo](train.jpg)\n"
        [ p []
            [ img [ alt "foo", src "train.jpg" ]
                []
            ]
        ]
    , testEq 547
        []
        "My ![foo bar](/path/to/train.jpg  \"title\"   )\n"
        [ p []
            [ text "My "
            , img [ alt "foo bar", src "/path/to/train.jpg", title "title" ]
                []
            ]
        ]
    , testEq 548
        []
        "![foo](<url>)\n"
        [ p []
            [ img [ alt "foo", src "url" ]
                []
            ]
        ]
    , testEq 549
        []
        "![](/url)\n"
        [ p []
            [ img [ alt "", src "/url" ]
                []
            ]
        ]
    , testEq 550
        []
        "![foo][bar]\n\n[bar]: /url\n"
        [ p []
            [ img [ alt "foo", src "/url" ]
                []
            ]
        ]
    , testEq 551
        []
        "![foo][bar]\n\n[BAR]: /url\n"
        [ p []
            [ img [ alt "foo", src "/url" ]
                []
            ]
        ]
    , testEq 552
        []
        "![foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 553
        []
        "![*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo bar", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 554
        []
        "![Foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "Foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 555
        []
        "![foo] \n[]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            , text "\n[]"
            ]
        ]
    , testEq 556
        []
        "![foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 557
        []
        "![*foo* bar]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo bar", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 558
        []
        "![[foo]]\n\n[[foo]]: /url \"title\"\n"
        [ p []
            [ text "![[foo]]" ]
        , p []
            [ text "[[foo]]: /url \"title\"" ]
        ]
    , testEq 559
        []
        "![Foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "Foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 560
        []
        "\\!\\[foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ text "![foo]" ]
        ]
    , testEq 561
        []
        "\\![foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ text "!"
            , a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    ]
