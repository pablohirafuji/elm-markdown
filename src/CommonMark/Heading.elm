module CommonMark.Heading exposing (..)


import Regex exposing (Regex)
--import Html exposing (Html, blockquote)



-- Model




-- Regex


atxRegex : Regex
atxRegex =
    Regex.regex "^ {0,3}(#{1,6})(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)(.*?)(?:\\s+[ \\t#]*)?$"


setextRegex : Regex
setextRegex =
    Regex.regex "^ {0,3}(=+|-+)[ \\t]*$"


-- Tranformar em result?
atxMatch : Regex.Match -> ( Int, String )
atxMatch match =
    case match.submatches of
        Just lvl :: Just heading :: _ ->
            ( String.length lvl, heading )

        _ ->
            ( 1, match.match )


-- Tranformar em result?
setextMatch : Regex.Match -> ( Int, String )
setextMatch match =
    case match.submatches of
        Just str :: _ ->
            if String.startsWith "=" str then
                ( 1, str )

            else
                ( 2, str )

        _ ->
            ( 1, "" )



-- Parser


