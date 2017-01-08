module Test.Inline.Autolinks exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#autolinks


run : List (Output msg)
run =
    [ testEq 562
        []
        "<http://foo.bar.baz>\n"
        [ p []
            [ a [ href "http://foo.bar.baz" ]
                [ text "http://foo.bar.baz" ]
            ]
        ]

    , testEq 563
        []
        "<http://foo.bar.baz/test?q=hello&id=22&boolean>\n"
        [ p []
            [ a [ href "http://foo.bar.baz/test?q=hello&id=22&boolean" ]
                [ text "http://foo.bar.baz/test?q=hello&id=22&boolean" ]
            ]
        ]

    , testEq 564
        []
        "<irc://foo.bar:2233/baz>\n"
        [ p []
            [ a [ href "irc://foo.bar:2233/baz" ]
                [ text "irc://foo.bar:2233/baz" ]
            ]
        ]

    , testEq 565
        []
        "<MAILTO:FOO@BAR.BAZ>\n"
        [ p []
            [ a [ href "MAILTO:FOO@BAR.BAZ" ]
                [ text "MAILTO:FOO@BAR.BAZ" ]
            ]
        ]

    , testEq 566
        []
        "<a+b+c:d>\n"
        [ p []
            [ a [ href "a+b+c:d" ]
                [ text "a+b+c:d" ]
            ]
        ]

    , testEq 567
        []
        "<made-up-scheme://foo,bar>\n"
        [ p []
            [ a [ href "made-up-scheme://foo,bar" ]
                [ text "made-up-scheme://foo,bar" ]
            ]
        ]

    , testEq 568
        []
        "<http://../>\n"
        [ p []
            [ a [ href "http://../" ]
                [ text "http://../" ]
            ]
        ]

    , testEq 569
        []
        "<localhost:5001/foo>\n"
        [ p []
            [ a [ href "localhost:5001/foo" ]
                [ text "localhost:5001/foo" ]
            ]
        ]

    , testEq 570
        []
        "<http://foo.bar/baz bim>\n"
        [ p []
            [ text "<http://foo.bar/baz bim>" ]
        ]

    , testEq 571
        []
        "<http://example.com/\\[\\>\n"
        [ p []
            [ a [ href "http://example.com/%5C%5B%5C" ]
                [ text "http://example.com/\\[\\" ]
            ]
        ]

    , testEq 572
        []
        "<foo@bar.example.com>\n"
        [ p []
            [ a [ href "mailto:foo@bar.example.com" ]
                [ text "foo@bar.example.com" ]
            ]
        ]

    , testEq 573
        []
        "<foo+special@Bar.baz-bar0.com>\n"
        [ p []
            [ a [ href "mailto:foo+special@Bar.baz-bar0.com" ]
                [ text "foo+special@Bar.baz-bar0.com" ]
            ]
        ]

    , testEq 574
        []
        "<foo\\+@bar.example.com>\n"
        [ p []
            [ text "<foo+@bar.example.com>" ]
        ]

    , testEq 575
        []
        "<>\n"
        [ p []
            [ text "<>" ]
        ]

    , testEq 576
        []
        "< http://foo.bar >\n"
        [ p []
            [ text "< http://foo.bar >" ]
        ]

    , testEq 577
        []
        "<m:abc>\n"
        [ p []
            [ text "<m:abc>" ]
        ]

    , testEq 578
        []
        "<foo.bar.baz>\n"
        [ p []
            [ text "<foo.bar.baz>" ]
        ]

    , testEq 579
        []
        "http://example.com\n"
        [ p []
            [ text "http://example.com" ]
        ]

    , testEq 580
        []
        "foo@bar.example.com\n"
        [ p []
            [ text "foo@bar.example.com" ]
        ]
    ]