module CLI exposing (..)

import Test exposing (..)
import Test.Helpers exposing (Output, toTest)
import Tests



-- Tests to run on a command line interface


all : Test
all =
    List.map
        (\( description, outputs ) ->
            describe description (List.map toTest outputs)
        )
        Tests.all
        |> describe "Elm Markdown Test Suite"
