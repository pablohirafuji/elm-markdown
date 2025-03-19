module Test.Inline.Custom exposing (run)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, src, title)
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
                [ a [ href "link", title "**em" ] [ text "test" ]
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
    , testEq 1005
        [ p [] [ text "Tail call optimization in block parsing" ] ]
        ("Hello" ++ String.repeat 10000 "\n" ++ "World")
        [ p [] [ text "Hello" ]
        , p [] [ text "World" ]
        ]

    -- TODO
    --, testEqDefaultOptions 1005
    --    [ p [] [ text "defaultOptions test" ] ]
    --    "foo\\\nbar\nbaz  \nbazinga\n\n<section class=\"className\" onerror=\"javascript();\"><script>alert(\"XSS Alert!\");</script></section>"
    --    [ p []
    --        [ text "foo"
    --        , br [] []
    --        , text "bar\nbaz"
    --        , br [] []
    --        , text "bazinga"
    --        ]
    --    , section [ attribute "class" "className" ]
    --        [ text "<script>alert('XSS Alert!');</script>" ]
    --    ]
    ]
