module BlockQuote exposing (..)


import Regex exposing (Regex)
import Html exposing (Html, blockquote)



-- Model


type alias Line = String


regex : Regex
regex =
    Regex.regex "^ {0,3}(?:>[ ]?)(.*)$"


fromMatch : Regex.Match -> Maybe Line
fromMatch match =
    match.submatches
        |> List.head
        |> Maybe.withDefault Nothing


view : List ( Html msg ) -> Html msg
view =
    blockquote []

