module CommonMark.BlockQuote exposing (..)


import Regex exposing (Regex)
import Html exposing (Html, blockquote)



-- Model


regex : Regex
regex =
    Regex.regex "^ {0,3}(?:>[ ]?)(.*)$"


fromMatch : Regex.Match -> String
fromMatch match =
    match.submatches
        |> List.head
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault ""


view : List ( Html msg ) -> Html msg
view =
    blockquote []

