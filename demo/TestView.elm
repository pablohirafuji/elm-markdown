module TestView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (checked, href, style, type_)
import Html.Events exposing (onCheck)
import Test.Helpers exposing (Output, toExpect)
import Test.Runner exposing (getFailureReason)
import Tests



-- Model


type alias Model =
    { showFailed : Bool
    , showSucceed : Bool
    }


initModel : Model
initModel =
    { showFailed = True
    , showSucceed = False
    }



-- Msg/Update


type Msg
    = ShowFailed Bool
    | ShowSucceed Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowFailed bool ->
            { model | showFailed = bool }

        ShowSucceed bool ->
            { model | showSucceed = bool }


resultOut : List ( String, List ( Bool, Output msg ) )
resultOut =
    let
        evalList : ( String, List (Output msg) ) -> ( String, List ( Bool, Output msg ) )
        evalList =
            Tuple.mapSecond (List.map eval)

        eval : Output msg -> ( Bool, Output msg )
        eval output =
            toExpect output
                |> getFailureReason
                |> Maybe.map (always False)
                |> Maybe.withDefault True
                |> (\a -> ( a, output ))
    in
    List.map evalList Tests.all


totalTestCount : Int
totalTestCount =
    let
        sumTests : ( String, List (Output msg) ) -> Int -> Int
        sumTests ( _, outputs ) count =
            List.length outputs
                |> (+) count
    in
    List.foldl sumTests 0 Tests.all


successTestCount : Int
successTestCount =
    let
        sumSuccessTests : ( String, List ( Bool, Output msg ) ) -> Int -> Int
        sumSuccessTests ( _, outputs ) count =
            List.filter Tuple.first outputs
                |> List.length
                |> (+) count
    in
    List.foldl sumSuccessTests 0 resultOut


view : Model -> Html Msg
view model =
    div [] <|
        [ h2 []
            [ text <|
                "Tests ("
                    ++ String.fromInt successTestCount
                    ++ "/"
                    ++ String.fromInt totalTestCount
                    ++ ")"
            ]
        , p []
            [ text "Based on "
            , a [ href "http://spec.commonmark.org/0.27/" ]
                [ text "CommonMark Spec" ]
            , text "."
            ]
        , label []
            [ input
                [ type_ "checkbox"
                , onCheck ShowFailed
                , checked model.showFailed
                ]
                []
            , text "Show Failed"
            ]
        , label []
            [ input
                [ type_ "checkbox"
                , onCheck ShowSucceed
                , checked model.showSucceed
                ]
                []
            , text "Show Succeed"
            ]
        ]
            ++ List.map (showTest model) resultOut


showTest : Model -> ( String, List ( Bool, Output msg ) ) -> Html msg
showTest model ( testTitle, outputs ) =
    let
        passed : Int
        passed =
            List.filter Tuple.first outputs
                |> List.length

        bgStyle : List (Html.Attribute msg)
        bgStyle =
            [ style "background-color" bgColor ]

        bgColor : String
        bgColor =
            testColor (List.length outputs == passed)
    in
    details [] <|
        [ summary [] <|
            [ text (testTitle ++ " ")
            , span bgStyle
                [ text <|
                    "("
                        ++ String.fromInt passed
                        ++ "/"
                        ++ String.fromInt (List.length outputs)
                        ++ ")"
                ]
            ]
        , ul [] (List.map (testView model) outputs)
        ]


testView : Model -> ( Bool, Output msg ) -> Html msg
testView model ( isSuccess, output ) =
    if isSuccess then
        if model.showSucceed then
            li [] [ testViewHelp ( isSuccess, output ) ]

        else
            text ""

    else if model.showFailed then
        li [] [ testViewHelp ( isSuccess, output ) ]

    else
        text ""


testViewHelp : ( Bool, Output msg ) -> Html msg
testViewHelp ( isSuccess, { number, description, input, expectedResult, result } ) =
    div [] <|
        [ h3 [] [ text ("Example " ++ String.fromInt number) ] ]
            ++ description
            ++ [ pre []
                    [ code
                        [ style "background-color" (testColor isSuccess)
                        ]
                        [ text input ]
                    ]
               , details []
                    [ h4 [] [ text "Expected" ]
                    , pre [] [ code [] expectedResult ]
                    , p [] [ text (Debug.toString expectedResult) ]
                    , h4 [] [ text "Result" ]
                    , pre [] [ code [] result ]
                    , p [] [ text (Debug.toString result) ]
                    ]
               ]


testColor : Bool -> String
testColor isSuccess =
    if isSuccess then
        "#90EE90"

    else
        "#EEB4B4"
