module Test.Helpers exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Markdown.Config as Config exposing (defaultOptions)

type alias Output msg = Result (Html msg) (Html msg)


testEq : Int -> List (Html msg) -> String -> List (Html msg) -> Output msg
testEq number description input expectedResult =
    let
        result : List (Html msg)
        result =
            Markdown.toHtml (Just customOptions) input

        isValid =
            toString result == toString expectedResult

        backgroundColor =
            if isValid then
                "#90EE90"

            else
                "#EEB4B4"

        view : Html msg
        view =
            div [] <|
                description ++
                [ h3 [] [ text ("Example " ++ toString number) ]
                , pre []
                    [ code [ style [("background-color", backgroundColor)] ]
                        [ text input ]
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

    in
        if isValid then
            Result.Ok view
            
        else
            Result.Err view


customOptions : Config.Options
customOptions =
    { defaultOptions
        | rawHtml = Config.ParseUnsafe
    }
