module Test.Inline.RawHtml exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, src, attribute)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#raw-html


run : List (Output)
run =
    [ testEq 581
        []
        "<a><bab><c2c>\n"
        [ a [] [ node "bab" [] [ node "c2c" [] [] ] ] ]

    , testEq 582
        []
        "<a/><b2/>\n"
        []

    , testEq 583
        []
        "<a  /><b2\ndata=\"foo\" >\n"
        [ a [] [], node "b2" [ attribute "data" "foo" ] [] ]

    , testEq 584
        []
        "<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n"
        []

    , testEq 585
        []
        "Foo <responsive-image src=\"foo.jpg\" />\n"
        [ p []
            [ text "Foo "
            , node "responsive-image" [ src "foo.jpg" ]
                []
            ]
        ]

    , testEq 586
        []
        "<33> <__>\n"
        [ p []
            [ text "<33> <__>" ]
        ]

    , testEq 587
        []
        "<a h*#ref=\"hi\">\n"
        [ p []
            [ text "<a h*#ref=\"hi\">" ]
        ]

    , testEq 588
        []
        "<a href=\"hi'> <a href=hi'>\n"
        [ p []
            [ text "<a href=\"hi'> <a href=hi'>" ]
        ]

    , testEq 589
        []
        "< a><\nfoo><bar/ >\n"
        [ p []
            [ text "< a><\nfoo><bar/ >" ]
        ]

    , testEq 590
        []
        "<a href='bar'title=title>\n"
        [ p []
            [ text "<a href='bar'title=title>" ]
        ]

    , testEq 591
        []
        "</a></foo >\n"
        []

    , testEq 592
        []
        "</a href=\"foo\">\n"
        [ p []
            [ text "</a href=\"foo\">" ]
        ]

    , testEq 593
        []
        "foo <!-- this is a\ncomment - with hyphen -->\n"
        [ p []
            [ text "foo " ]
        ]

    , testEq 594
        []
        "foo <!-- not a comment -- two hyphens -->\n"
        [ p []
            [ text "foo <!-- not a comment -- two hyphens -->" ]
        ]

    , testEq 595
        []
        "foo <!--> foo -->\n\nfoo <!-- foo--->\n"
        [ p []
            [ text "foo <!--> foo -->" ]
        , p []
            [ text "foo <!-- foo--->" ]
        ]

    , testEq 596
        []
        "foo <?php echo $a; ?>\n"
        []

    , testEq 597
        []
        "foo <!ELEMENT br EMPTY>\n"
        []

    , testEq 598
        []
        "foo <![CDATA[>&<]]>\n"
        []

    , testEq 599
        []
        "foo <a href=\"&ouml;\">\n"
        []

    , testEq 600
        []
        "foo <a href=\"\\*\">\n"
        []

    , testEq 601
        []
        "<a href=\"\\\"\">\n"
        [ p []
            [ text "<a href=\"\"\">" ]
        ]
    ]