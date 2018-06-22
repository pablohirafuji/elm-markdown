module Test.BlankLine exposing (..)

import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#blank-lines


run : List (Output msg)
run =
    [ testEq 188
        [ p [] [ text "Blank lines between block-level elements are ignored, except for the role they play in determining whether a list is tight or loose." ]
        , p [] [ text "Blank lines at the beginning and end of the document are also ignored." ]
        ]
        "  \n\naaa\n  \n\n# aaa\n\n  "
        [ p [] [ text "aaa" ], h1 [] [ text "aaa" ] ]
    ]
