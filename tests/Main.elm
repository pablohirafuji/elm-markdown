module Main exposing (..)

import Browser exposing (Page)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Markdown
import Markdown.Config as Config exposing (defaultOptions)
import View


main : Program () Model Msg
main =
    Browser.fullscreen
        { init = \_ -> ( init, Cmd.none )
        , view = page
        , update = update
        , subscriptions = \_ -> Sub.none
        , onNavigation = Nothing
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


page : Model -> Page Msg
page model =
    Page "Elm Markdown Tests" (view model)


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
