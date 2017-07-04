module Test.Helpers exposing (..)

import Test exposing (Test)
import Expect exposing (Expectation)
import Html exposing (..)
import Markdown
import Markdown.Config as Config exposing (defaultOptions)


type alias Output msg =
    { number : Int
    , description : List (Html msg)
    , input : String
    , option : Maybe Config.Options
    , expectedResult : List (Html msg)
    , result : List (Html msg)
    }


testEq : Int -> List (Html msg) -> String -> List (Html msg) -> Output msg
testEq =
    testEqHelp (Just customOptions)


testEqSoftAsHard : Int -> List (Html msg) -> String -> List (Html msg) -> Output msg
testEqSoftAsHard =
    testEqHelp (Just softAsHardOptions)


testEqDefaultOptions : Int -> List (Html msg) -> String -> List (Html msg) -> Output msg
testEqDefaultOptions =
    testEqHelp Nothing


testEqHelp : Maybe Config.Options -> Int -> List (Html msg) -> String -> List (Html msg) -> Output msg
testEqHelp option number description input expectedResult =
    { number = number
    , description = description
    , input = input
    , option = option
    , expectedResult = expectedResult
    , result = Markdown.toHtml option input
    }


customOptions : Config.Options
customOptions =
    { defaultOptions
        | rawHtml = Config.ParseUnsafe
    }


softAsHardOptions : Config.Options
softAsHardOptions =
    { defaultOptions
        | softAsHardLineBreak = True
    }


toExpect : Output msg -> Expectation
toExpect { expectedResult, result } =
    Expect.equal expectedResult result


toTest : Output msg -> Test
toTest output =
    (\() -> toExpect output)
        |> Test.test (toString output.number)
