module Test.Inline.Link exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, title, src, alt, attribute)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#links


run : List (Output)
run =
    [ testEq 456
        []
        "[link](/uri \"title\")\n"
        [ p []
            [ a [ href "/uri", title "title" ]
                [ text "link" ]
            ]
        ]

    , testEq 457
        []
        "[link](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link" ]
            ]
        ]

    , testEq 458
        []
        "[link]()\n"
        [ p []
            [ a [ href "" ]
                [ text "link" ]
            ]
        ]

    , testEq 459
        []
        "[link](<>)\n"
        [ p []
            [ a [ href "" ]
                [ text "link" ]
            ]
        ]

    , testEq 460
        []
        "[link](/my uri)\n"
        [ p []
            [ text "[link](/my uri)" ]
        ]

    , testEq 461
        []
        "[link](</my uri>)\n"
        [ p []
            [ text "[link](</my uri>)" ]
        ]

    , testEq 462
        []
        "[link](foo\nbar)\n"
        [ p []
            [ text "[link](foo\nbar)" ]
        ]

    , testEq 463
        []
        "[link](<foo\nbar>)\n"
        [ p []
            [ text "[link]("
            , node "foo" [ attribute "bar" "bar" ] []
            , text ")"
            ]
        ]

    , testEq 464
        []
        "[link](\\(foo\\))\n"
        [ p []
            [ a [ href "(foo)" ]
                [ text "link" ]
            ]
        ]

    , testEq 465
        []
        "[link](foo(and(bar)))\n"
        [ p []
            [ a [ href "foo(and(bar))" ]
                [ text "link" ]
            ]
        ]

    , testEq 466
        []
        "[link](foo\\(and\\(bar\\))\n"
        [ p []
            [ a [ href "foo(and(bar)" ]
                [ text "link" ]
            ]
        ]

    , testEq 467
        []
        "[link](<foo(and(bar)>)\n"
        [ p []
            [ a [ href "foo(and(bar)" ]
                [ text "link" ]
            ]
        ]

    , testEq 468
        []
        "[link](foo\\)\\:)\n"
        [ p []
            [ a [ href "foo):" ]
                [ text "link" ]
            ]
        ]

    , testEq 469
        []
        "[link](#fragment)\n\n[link](http://example.com#fragment)\n\n[link](http://example.com?foo=3#frag)\n"
        [ p []
            [ a [ href "#fragment" ]
                [ text "link" ]
            ]
        , p []
            [ a [ href "http://example.com#fragment" ]
                [ text "link" ]
            ]
        , p []
            [ a [ href "http://example.com?foo=3#frag" ]
                [ text "link" ]
            ]
        ]

    , testEq 470
        []
        "[link](foo\\bar)\n"
        [ p []
            [ a [ href "foo%5Cbar" ]
                [ text "link" ]
            ]
        ]

    , testEq 471
        []
        "[link](foo%20b&auml;)\n"
        [ p []
            [ a [ href "foo%20b%C3%A4" ]
                [ text "link" ]
            ]
        ]

    , testEq 472
        []
        "[link](\"title\")\n"
        [ p []
            [ a [ href "%22title%22" ]
                [ text "link" ]
            ]
        ]

    , testEq 473
        []
        "[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "link" ]
            , text "\n"
            , a [ href "/url", title "title" ]
                [ text "link" ]
            , text "\n"
            , a [ href "/url", title "title" ]
                [ text "link" ]
            ]
        ]

    , testEq 474
        []
        "[link](/url \"title \\\"&quot;\")\n"
        [ p []
            [ a [ href "/url", title "title \"\"" ]
                [ text "link" ]
            ]
        ]

    , testEq 475
        []
        "[link](/url \"title\")\n"
        [ p []
            [ a [ href "/url%C2%A0%22title%22" ]
                [ text "link" ]
            ]
        ]

    , testEq 476
        []
        "[link](/url \"title \"and\" title\")\n"
        [ p []
            [ text "[link](/url \"title \"and\" title\")" ]
        ]

    , testEq 477
        []
        "[link](/url 'title \"and\" title')\n"
        [ p []
            [ a [ href "/url", title "title \"and\" title" ]
                [ text "link" ]
            ]
        ]

    , testEq 478
        []
        "[link](   /uri\n  \"title\"  )\n"
        [ p []
            [ a [ href "/uri", title "title" ]
                [ text "link" ]
            ]
        ]

    , testEq 479
        []
        "[link] (/uri)\n"
        [ p []
            [ text "[link] (/uri)" ]
        ]

    , testEq 480
        []
        "[link [foo [bar]]](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [foo [bar]]" ]
            ]
        ]

    , testEq 481
        []
        "[link] bar](/uri)\n"
        [ p []
            [ text "[link] bar](/uri)" ]
        ]

    , testEq 482
        []
        "[link [bar](/uri)\n"
        [ p []
            [ text "[link "
            , a [ href "/uri" ]
                [ text "bar" ]
            ]
        ]

    , testEq 483
        []
        "[link \\[bar](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [bar" ]
            ]
        ]

    , testEq 484
        []
        "[link *foo **bar** `#`*](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link "
                , em []
                    [ text "foo "
                    , strong []
                        [ text "bar" ]
                    , text " "
                    , code []
                        [ text "#" ]
                    ]
                ]
            ]
        ]

    , testEq 485
        []
        "[![moon](moon.jpg)](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ img [ alt "moon", src "moon.jpg" ]
                    []
                ]
            ]
        ]

    , testEq 486
        []
        "[foo [bar](/uri)](/uri)\n"
        [ p []
            [ text "[foo "
            , a [ href "/uri" ]
                [ text "bar" ]
            , text "](/uri)"
            ]
        ]

    , testEq 487
        []
        "[foo *[bar [baz](/uri)](/uri)*](/uri)\n"
        [ p []
            [ text "[foo "
            , em []
                [ text "[bar "
                , a [ href "/uri" ]
                    [ text "baz" ]
                , text "](/uri)"
                ]
            , text "](/uri)"
            ]
        ]

    , testEq 488
        []
        "![[[foo](uri1)](uri2)](uri3)\n"
        [ p []
            [ img [ alt "[foo](uri2)", src "uri3" ]
                []
            ]
        ]

    , testEq 489
        []
        "*[foo*](/uri)\n"
        [ p []
            [ text "*"
            , a [ href "/uri" ]
                [ text "foo*" ]
            ]
        ]

    , testEq 490
        []
        "[foo *bar](baz*)\n"
        [ p []
            [ a [ href "baz*" ]
                [ text "foo *bar" ]
            ]
        ]

    , testEq 491
        []
        "*foo [bar* baz]\n"
        [ p []
            [ em []
                [ text "foo [bar" ]
            , text " baz]"
            ]
        ]

    , testEq 492
        []
        "[foo <bar attr=\"](baz)\">\n"
        [ p []
            [ text "[foo "
            , node "bar" [ attribute "attr" "](baz)" ] []
            ]
        ]

    , testEq 493
        []
        "[foo`](/uri)`\n"
        [ p []
            [ text "[foo"
            , code []
                [ text "](/uri)" ]
            ]
        ]

    , testEq 494
        []
        "[foo<http://example.com/?search=](uri)>\n"
        [ p []
            [ text "[foo"
            , a [ href "http://example.com/?search=%5D(uri)" ]
                [ text "http://example.com/?search=](uri)" ]
            ]
        ]

    , testEq 495
        []
        "[foo][bar]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]

    , testEq 496
        []
        "[link [foo [bar]]][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [foo [bar]]" ]
            ]
        ]

    , testEq 497
        []
        "[link \\[bar][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [bar" ]
            ]
        ]

    , testEq 498
        []
        "[link *foo **bar** `#`*][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link "
                , em []
                    [ text "foo "
                    , strong []
                        [ text "bar" ]
                    , text " "
                    , code []
                        [ text "#" ]
                    ]
                ]
            ]
        ]

    , testEq 499
        []
        "[![moon](moon.jpg)][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ img [ alt "moon", src "moon.jpg" ]
                    []
                ]
            ]
        ]

    , testEq 500
        []
        "[foo [bar](/uri)][ref]\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo "
            , a [ href "/uri" ]
                [ text "bar" ]
            , text "]"
            , a [ href "/uri" ]
                [ text "ref" ]
            ]
        ]

    , testEq 501
        []
        "[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo "
            , em []
                [ text "bar "
                , a [ href "/uri" ]
                    [ text "baz" ]
                ]
            , text "]"
            , a [ href "/uri" ]
                [ text "ref" ]
            ]
        ]

    , testEq 502
        []
        "*[foo*][ref]\n\n[ref]: /uri\n"
        [ p []
            [ text "*"
            , a [ href "/uri" ]
                [ text "foo*" ]
            ]
        ]

    , testEq 503
        []
        "[foo *bar][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "foo *bar" ]
            ]
        ]

    , testEq 504
        []
        "[foo <bar attr=\"][ref]\">\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo "
            , node "bar" [ attribute "attr" "][ref]" ] []
            ]
        ]

    , testEq 505
        []
        "[foo`][ref]`\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo"
            , code [] [ text "][ref]" ]
            ]
        ]

    , testEq 506
        []
        "[foo<http://example.com/?search=][ref]>\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo"
            , a [ href "http://example.com/?search=%5D%5Bref%5D" ]
                [ text "http://example.com/?search=][ref]" ]
            ]
        ]

    , testEq 507
        []
        "[foo][BaR]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]

    , testEq 508
        []
        "[Толпой][Толпой] is a Russian word.\n\n[ТОЛПОЙ]: /url\n"
        [ p []
            [ a [ href "/url" ]
                [ text "Толпой" ]
            , text " is a Russian word."
            ]
        ]

    , testEq 509
        []
        "[Foo\n  bar]: /url\n\n[Baz][Foo bar]\n"
        [ p []
            [ a [ href "/url" ]
                [ text "Baz" ]
            ]
        ]

    , testEq 510
        []
        "[foo] [bar]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ text "[foo] "
            , a [ href "/url", title "title" ]
                [ text "bar" ]
            ]
        ]

    , testEq 511
        []
        "[foo]\n[bar]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ text "[foo]\n"
            , a [ href "/url", title "title" ]
                [ text "bar" ]
            ]
        ]

    , testEq 512
        []
        "[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]\n"
        [ p []
            [ a [ href "/url1" ]
                [ text "bar" ]
            ]
        ]

    , testEq 513
        []
        "[bar][foo\\!]\n\n[foo!]: /url\n"
        [ p []
            [ text "[bar][foo!]" ]
        ]

    , testEq 514
        []
        "[foo][ref[]\n\n[ref[]: /uri\n"
        [ p []
            [ text "[foo][ref[]" ]
        , p []
            [ text "[ref[]: /uri" ]
        ]

    , testEq 515
        []
        "[foo][ref[bar]]\n\n[ref[bar]]: /uri\n"
        [ p []
            [ text "[foo][ref[bar]]" ]
        , p []
            [ text "[ref[bar]]: /uri" ]
        ]

    , testEq 516
        []
        "[[[foo]]]\n\n[[[foo]]]: /url\n"
        [ p []
            [ text "[[[foo]]]" ]
        , p []
            [ text "[[[foo]]]: /url" ]
        ]

    , testEq 517
        []
        "[foo][ref\\[]\n\n[ref\\[]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "foo" ]
            ]
        ]

    , testEq 518
        []
        "[bar\\\\]: /uri\n\n[bar\\\\]\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "bar\\" ]
            ]
        ]

    , testEq 519
        []
        "[]\n\n[]: /uri\n"
        [ p []
            [ text "[]" ]
        , p []
            [ text "[]: /uri" ]
        ]

    , testEq 520
        []
        "[\n ]\n\n[\n ]: /uri\n"
        [ p []
            [ text "[\n]" ]
        , p []
            [ text "[\n]: /uri" ]
        ]

    , testEq 521
        []
        "[foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]

    , testEq 522
        []
        "[*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ em []
                    [ text "foo" ]
                , text " bar"
                ]
            ]
        ]

    , testEq 523
        []
        "[Foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "Foo" ]
            ]
        ]

    , testEq 524
        []
        "[foo] \n[]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            , text "\n[]"
            ]
        ]

    , testEq 525
        []
        "[foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]

    , testEq 526
        []
        "[*foo* bar]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ em []
                    [ text "foo" ]
                , text " bar"
                ]
            ]
        ]

    , testEq 527
        []
        "[[*foo* bar]]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ text "["
            , a [ href "/url", title "title" ]
                [ em []
                    [ text "foo" ]
                , text " bar"
                ]
            , text "]"
            ]
        ]

    , testEq 528
        []
        "[[bar [foo]\n\n[foo]: /url\n"
        [ p []
            [ text "[[bar "
            , a [ href "/url" ]
                [ text "foo" ]
            ]
        ]

    , testEq 529
        []
        "[Foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "Foo" ]
            ]
        ]

    , testEq 530
        []
        "[foo] bar\n\n[foo]: /url\n"
        [ p []
            [ a [ href "/url" ]
                [ text "foo" ]
            , text " bar"
            ]
        ]

    , testEq 531
        []
        "\\[foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ text "[foo]" ]
        ]

    , testEq 532
        []
        "[foo*]: /url\n\n*[foo*]\n"
        [ p []
            [ text "*"
            , a [ href "/url" ]
                [ text "foo*" ]
            ]
        ]

    , testEq 533
        []
        "[foo][bar]\n\n[foo]: /url1\n[bar]: /url2\n"
        [ p []
            [ a [ href "/url2" ]
                [ text "foo" ]
            ]
        ]

    , testEq 534
        []
        "[foo][]\n\n[foo]: /url1\n"
        [ p []
            [ a [ href "/url1" ]
                [ text "foo" ]
            ]
        ]

    , testEq 535
        []
        "[foo]()\n\n[foo]: /url1\n"
        [ p []
            [ a [ href "" ]
                [ text "foo" ]
            ]
        ]

    , testEq 536
        []
        "[foo](not a link)\n\n[foo]: /url1\n"
        [ p []
            [ a [ href "/url1" ]
                [ text "foo" ]
            , text "(not a link)"
            ]
        ]

    , testEq 537
        []
        "[foo][bar][baz]\n\n[baz]: /url\n"
        [ p []
            [ text "[foo]"
            , a [ href "/url" ]
                [ text "bar" ]
            ]
        ]

    , testEq 538
        []
        "[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2\n"
        [ p []
            [ a [ href "/url2" ]
                [ text "foo" ]
            , a [ href "/url1" ]
                [ text "baz" ]
            ]
        ]

    , testEq 539
        []
        "[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2\n"
        [ p []
            [ text "[foo]"
            , a [ href "/url1" ]
                [ text "bar" ]
            ]
        ]
    ]