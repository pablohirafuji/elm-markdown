module Test.Helpers exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import CommonMark


type alias Output msg = Maybe (Html msg)


testEq : Int -> List (Html msg) -> String -> List (Html msg) -> (Output msg)
testEq number description input expectedResult =
    let
        result : List (Html msg)
        result =
            CommonMark.toHtml input

        isValid =
            toString result == toString expectedResult

        backgroundColor =
            if isValid then
                "green"

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
            Nothing
            
        else
            Just view
