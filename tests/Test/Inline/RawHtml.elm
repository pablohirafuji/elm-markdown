module Test.Inline.RawHtml exposing (run)

import Html exposing (..)
import Html.Attributes exposing (attribute, href, src)
import Test.Helpers exposing (..)


-- Based on http://spec.commonmark.org/0.27/#raw-html


run : List (Output msg)
run =
    [ testEq 581
        []
        "<a><bab><c2c>\n"
        [ p []
            [ a [] []
            , node "bab" [] []
            , node "c2c" [] []
            ]
        ]
    , testEq 582
        []
        "<a/><b2/>\n"
        [ p []
            [ node "a" [] []
            , node "b2" [] []
            ]
        ]
    , testEq 583
        []
        "<a  /><b2\ndata=\"foo\" >\n"
        [ p []
            [ a [] []
            , node "b2" [ attribute "data" "foo" ] []
            ]
        ]
    , testEq 584
        []
        "<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n"
        [ p []
            [ text "<a foo=\"bar\" bam = 'baz "
            , em [] [ text "\"" ]
            , text "'\n_boolean zoop:33=zoop:33 />"
            ]
        ]
    , testEq 585
        []
        "Foo <responsive-image src=\"foo.jpg\" />\n"
        [ p []
            [ text "Foo "
            , node "responsive-image"
                [ attribute "src" "foo.jpg" ]
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
        [ a
            [ attribute "h" "h"
            , attribute "ref" "hi"
            ]
            []
        ]
    , testEq 588
        []
        "<a href=\"hi'> <a href=hi'>\n"
        [ p []
            [ a
                [ attribute "href" "href"
                , attribute "hi" "hi"
                ]
                []
            , text " "
            , a
                [ attribute "href" "hi"
                ]
                []
            ]
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
        [ a
            [ attribute "href" "bar"
            , attribute "title" "title"
            ]
            []
        ]
    , testEq 591
        []
        "</a></foo >\n"
        [ p [] [ a [] [], node "foo" [] [] ] ]
    , testEq 592
        []
        "</a href=\"foo\">\n"
        [ p [] [ text "</a href=\"foo\">" ]
        ]
    , testEq 593
        []
        "foo <!-- this is a\ncomment - with hyphen -->\n"
        [ p []
            [ text "foo <!-- this is a\ncomment - with hyphen -->" ]
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
        [ p []
            [ text "foo <?php echo $a; ?>" ]
        ]
    , testEq 597
        []
        "foo <!ELEMENT br EMPTY>\n"
        [ p []
            [ text "foo <!ELEMENT br EMPTY>" ]
        ]
    , testEq 598
        []
        "foo <![CDATA[>&<]]>\n"
        [ p []
            [ text "foo <![CDATA[>&<]]>" ]
        ]
    , testEq 599
        []
        "foo <a href=\"&ouml;\">\n"
        [ p []
            [ text "foo "
            , a [ attribute "href" "&ouml;" ] []
            ]
        ]
    , testEq 600
        []
        "foo <a href=\"\\*\">\n"
        [ p []
            [ text "foo "
            , a [ attribute "href" "\\*" ] []
            ]
        ]
    , testEq 601
        []
        "<a href=\"\\\"\">\n"
        [ a [ attribute "href" "\\" ] []
        ]
    ]
