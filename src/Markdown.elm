module Markdown exposing (toHtml)


{-| A pure Elm package for markdown parsing and rendering.

# Parsing and rendering Markdown
@docs toHtml
-}


import Html exposing (..)
import Markdown.Block as Block
import Markdown.Config exposing (Options)



{-| Turn a markdown string into a list of HTML elements.

```
view : Html msg
view =
    div []
        <| toHtml Nothing "# Title with *emphasis*"

```

**Note:** If `Maybe Options` is `Nothing`,
`Config.defaultOptions` will be used.
-}

toHtml : Maybe Options -> String -> List (Html msg)
toHtml maybeOptions rawText =
    Block.parse maybeOptions rawText
        |> List.map (Block.toHtml)
        |> List.concat

