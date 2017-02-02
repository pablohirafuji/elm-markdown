module Test.Inline.Custom exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, src, class, title, attribute)
import Test.Helpers exposing (..)



run : List (Output msg)
run =
    [ testEq 1000
        []
        "[check](code``) ``precedence``"
        [ p []
            [ text "[check](code"
            , code [] [ text ")" ]
            , text "precedence``"
            ]
        ]

    , testEq 1001
        []
        "**[test](link \"**em\") precedence**"
        [ p []
            [ strong []
                [ a [ href "link", title "**em" ] [ text "test"]
                , text " precedence"
                ]
            ]
        ]

    , testEq 1002
        [ p [] [ text "List indentation test" ] ]
        " -   \n  foo"
        [ ul []
            [ li [] []
            ]
        , p [] [ text "foo" ]
        ]

    , testEq 1003
        [ p [] [ text "Hard break escaping line test" ] ]
        "bar \\\\\nfoo"
        [ p [] [ text "bar \\\nfoo" ]
        ]

    , testEqSoftAsHard 1004
        [ p [] [ text "softAsHardLineBreak = True" ] ]
        "foo\\\nbar\nbaz  \nbazinga"
        [ p []
            [ text "foo"
            , br [] []
            , text "bar"
            , br [] []
            , text "baz"
            , br [] []
            , text "bazinga"
            ]
        ]

    , testEqDefaultOptions 1005
        [ p [] [ text "defaultOptions test" ] ]
        "foo\\\nbar\nbaz  \nbazinga\n\n<section class=\"className\" onerror=\"javascript();\"><script>alert('XSS Alert!');</script></section>"
        [ p []
            [ text "foo"
            , br [] []
            , text "bar\nbaz"
            , br [] []
            , text "bazinga"
            ]
        , section [ attribute "class" "className" ]
            [ text "<script>alert('XSS Alert!');</script>" ]
        ]

    ]