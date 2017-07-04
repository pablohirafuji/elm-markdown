module Test.Inline.Entity exposing (run)

import Html exposing (..)
import Html.Attributes exposing (href, src, class, title, attribute)
import Test.Helpers exposing (..)


-- Based on http://spec.commonmark.org/0.27/#entity-and-numeric-character-references


run : List (Output msg)
run =
    --[ testEq 300
    --    []
    --    "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n"
    --    [ p []
    --        [ text "& © Æ Ď¾ ℋ ⅆ∲ ≧̸" ]
    --    ]
    [ testEq 301
        []
        "&#35; &#1234; &#992; &#98765432; &#0;\n"
        [ p []
            [ text "# Ӓ Ϡ � �" ]
        ]
    , testEq 302
        []
        "&#X22; &#XD06; &#xcab;\n"
        [ p []
            [ text "\" ആ ಫ" ]
        ]
    , testEq 303
        []
        "&nbsp &x; &#; &#x;\n&ThisIsNotDefined; &hi?;\n"
        [ p []
            [ text "&nbsp &x; &#; &#x;\n&ThisIsNotDefined; &hi?;" ]
        ]
    , testEq 304
        []
        "&copy\n"
        [ p []
            [ text "&copy" ]
        ]
    , testEq 305
        []
        "&MadeUpEntity;\n"
        [ p []
            [ text "&MadeUpEntity;" ]
        ]
    , testEq 306
        []
        "<a href=\"&ouml;&ouml;.html\">foo</a>\n"
        [ a [ attribute "href" "&ouml;&ouml;.html" ]
            [ text "foo" ]
        ]
    , testEq 307
        []
        "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n"
        [ p []
            [ a [ href "/f%C3%B6%C3%B6", title "föö" ]
                [ text "foo" ]
            ]
        ]
    , testEq 308
        []
        "[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n"
        [ p []
            [ a [ href "/f%C3%B6%C3%B6", title "föö" ]
                [ text "foo" ]
            ]
        ]
    , testEq 309
        []
        "``` f&ouml;&ouml;\nfoo\n```\n"
        [ pre []
            [ code [ class "language-föö" ]
                [ text "foo\n" ]
            ]
        ]
    , testEq 310
        []
        "`f&ouml;&ouml;`\n"
        [ p []
            [ code []
                [ text "f&ouml;&ouml;" ]
            ]
        ]
    , testEq 311
        []
        "    f&ouml;f&ouml;\n"
        [ pre []
            [ code []
                [ text "f&ouml;f&ouml;\n" ]
            ]
        ]
    ]
