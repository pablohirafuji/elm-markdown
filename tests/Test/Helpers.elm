module Test.Helpers exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Markdown.Config as Config exposing (defaultOptions)

type alias Output msg = Result (Html msg) (Html msg)


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
testEqHelp options number description input expectedResult =
    let
        result : List (Html msg)
        result =
            Markdown.toHtml options input

        isValid : Bool
        isValid =
            toString result == toString expectedResult

    in
        if isValid then
            Result.Ok (view number description input expectedResult result isValid)
            
        else
            Result.Err (view number description input expectedResult result isValid)


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

backgroundColor : Bool -> String
backgroundColor isValid =
    if isValid then
        "#90EE90"

    else
        "#EEB4B4"


view : Int -> List (Html msg) -> String -> List (Html msg) -> List (Html msg) -> Bool -> Html msg
view number description input expectedResult result isValid =
    div [] <|
        [ h3 [] [ text ("Example " ++ toString number) ] ]
        ++ description ++
        [ pre []
            [ code
                [ style
                    [ ("background-color", backgroundColor isValid) ]
                ] [ text input ]
            ]
        , details []
            [ h4 [] [ text "Expected" ]
            , pre [] [ code [] expectedResult ]
            , p [] [ text (toString expectedResult) ]
            , h4 [] [ text "Result" ]
            , pre [] [ code [] result ]
            , p [] [ text (toString result) ]
            ]
        ]

