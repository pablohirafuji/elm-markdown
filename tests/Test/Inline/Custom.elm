module Test.Inline.Custom exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, src, class, title, attribute)
import Test.Helpers exposing (..)



run : List (Output)
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

    ]