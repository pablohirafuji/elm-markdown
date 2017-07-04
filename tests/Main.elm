module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Markdown
import Markdown.Config as Config exposing (defaultOptions)
import View


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { textarea : String
    , testModel : View.Model
    }


init : Model
init =
    { textarea = ""
    , testModel = View.initModel
    }


type Msg
    = TextAreaInput String
    | TestMsg View.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaInput str ->
            { model | textarea = str } ! []

        TestMsg testMsg ->
            { model
                | testModel =
                    View.update testMsg model.testModel
            }
                ! []


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Pure Elm markdown tests" ]
        , textarea
            [ onInput TextAreaInput
            , defaultValue model.textarea
            ]
            []
        , br [] []
        , div [] <|
            Markdown.toHtml (Just customOptions) model.textarea
        , Html.map TestMsg <|
            View.view model.testModel
        ]


customOptions : Config.Options
customOptions =
    { defaultOptions
        | rawHtml = Config.ParseUnsafe
    }
