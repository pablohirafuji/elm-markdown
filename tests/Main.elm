module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Markdown
import Markdown.Config as Config exposing (defaultOptions)
import View


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( init, Cmd.none )
        , view = page
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
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
    | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaInput str ->
            ( { model | textarea = str }
            , Cmd.none
            )

        TestMsg testMsg ->
            ( { model
                | testModel =
                    View.update testMsg model.testModel
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


page : Model -> Document Msg
page model =
    { title = "Elm Markdown Tests"
    , body = view model
    }

view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "Pure Elm markdown tests" ]
    , textarea [ onInput TextAreaInput ] []
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
