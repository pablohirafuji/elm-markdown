module Test.Inline.LineBreak exposing (run)

import Html exposing (..)
import Html.Attributes exposing (attribute, href)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#hard-line-breaks


run : List (Output msg)
run =
    [ testEq 602
        []
        "foo  \nbaz\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "baz"
            ]
        ]
    , testEq 603
        []
        "foo\\\nbaz\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "baz"
            ]
        ]
    , testEq 604
        []
        "foo       \nbaz\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "baz"
            ]
        ]
    , testEq 605
        []
        "foo  \n     bar\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "bar"
            ]
        ]
    , testEq 606
        []
        "foo\\\n     bar\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "bar"
            ]
        ]
    , testEq 607
        []
        "*foo  \nbar*\n"
        [ p []
            [ em []
                [ text "foo"
                , br []
                    []
                , text "bar"
                ]
            ]
        ]
    , testEq 608
        []
        "*foo\\\nbar*\n"
        [ p []
            [ em []
                [ text "foo"
                , br []
                    []
                , text "bar"
                ]
            ]
        ]
    , testEq 609
        []
        "`code  \nspan`\n"
        [ p []
            [ code []
                [ text "code span" ]
            ]
        ]
    , testEq 610
        []
        "`code\\\nspan`\n"
        [ p []
            [ code []
                [ text "code\\ span" ]
            ]
        ]
    , testEq 611
        []
        "<a href=\"foo  \nbar\">\n"
        [ a [ attribute "href" "foo  \nbar" ] [] ]
    , testEq 612
        []
        "<a href=\"foo\\\nbar\">\n"
        [ a [ attribute "href" "foo\\\nbar" ] [] ]
    , testEq 613
        []
        "foo\\\n"
        [ p []
            [ text "foo\\" ]
        ]
    , testEq 614
        []
        "foo  \n"
        [ p []
            [ text "foo" ]
        ]
    , testEq 615
        []
        "### foo\\\n"
        [ h3 []
            [ text "foo\\" ]
        ]
    , testEq 616
        []
        "### foo  \n"
        [ h3 []
            [ text "foo" ]
        ]
    , testEq 617
        []
        "foo\nbaz\n"
        [ p []
            [ text "foo\nbaz" ]
        ]
    , testEq 618
        []
        "foo \n baz\n"
        [ p []
            [ text "foo\nbaz" ]
        ]
    , testEq 619
        []
        "hello $.;'there\n"
        [ p []
            [ text "hello $.;'there" ]
        ]
    , testEq 620
        []
        "Foo χρῆν\n"
        [ p []
            [ text "Foo χρῆν" ]
        ]
    , testEq 621
        []
        "Multiple     spaces\n"
        [ p []
            [ text "Multiple     spaces" ]
        ]
    ]
