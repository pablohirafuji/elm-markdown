module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Test.View


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
    }


init : Model
init =
    { textarea = strTest
    }


type Msg
    = TextAreaInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaInput str ->
            { model | textarea = str } ! []



view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput TextAreaInput, value model.textarea ] []
        , br [] []
        --, text (toString (String.lines model.textarea))
        --, br [] []
        --, p []
        --      [ text <| toString <| (List.map Markdown.typeOfLine (String.lines model.textarea)) ]
        , div [] <| Markdown.toHtml model.textarea
        , textarea
            [ value <| String.dropRight 1 <| String.dropLeft 1 <| toString model.textarea
            , style [ ("font-size", "16px"), ("width", "90%") ] ] []
        , Test.View.view
        ]


strTest : String
strTest = """
and here a paragraph ends.

This is the Heading
===================

And here is the body…
# H1
## H2
____
### H3
#### H4
#####
``
````javascript
"""

