port module Main exposing (..)

import Markdown
import Markdown.Block as Block


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type alias Model =
    Bool


type Msg
    = Parse String
    | ToHtml String



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( True, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse str ->
            ( True
            , Block.parse Nothing str
                |> (\_ -> "ss")
                |> markdownHtml
            )

        ToHtml str ->
            ( True
            , Markdown.toHtml Nothing str
                |> (\_ -> "ss")
                |> markdownHtml
            )



-- SUBSCRIPTIONS


port toHtml : (String -> msg) -> Sub msg


port parse : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ toHtml ToHtml
        , parse Parse
        ]


port markdownHtml : String -> Cmd msg
