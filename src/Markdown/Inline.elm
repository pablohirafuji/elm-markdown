module Markdown.Inline exposing
    ( Inline(..)
    , toHtml, defaultHtml
    , extractText
    )


{-| Inline rendering and helpers.

# Model
@docs Inline

# Renders
@docs toHtml, defaultHtml

# Helpers
@docs extractText

-}


import Html exposing (Html, em, strong, br, code, node, text, img, a)
import Html.Attributes exposing (src, href, attribute, alt, title)
import Markdown.Helpers exposing (Attribute)


----------------------------------------------------------------------
-------------------------------- Model -------------------------------
----------------------------------------------------------------------


{-| The inline type.

- **Text** | *Text*
- **HardLineBreak**
- **CodeInline** | *Code*
- **Link** | *Url* | *Maybe Title* | *Inlines*
- **Image** | *Source* | *Maybe Title* | *Inlines*
- **HtmlInline** | *Tag* | *List ( Attribute, Maybe Value )* | *Inlines*
- **Emphasis** | *Delimiter Length* | *Inlines*
- **Custom** | *Custom type* | *Inlines*
-}


type Inline i
    = Text String
    | HardLineBreak
    | CodeInline String
    | Link String (Maybe String) (List (Inline i))
    | Image String (Maybe String) (List (Inline i))
    | HtmlInline String (List ( String, Maybe String )) (List (Inline i))
    | Emphasis Int (List (Inline i))
    | Custom i (List (Inline i))



----------------------------------------------------------------------
------------------------ Inline Html Renderer ------------------------
----------------------------------------------------------------------


{-| Transform an Inline into Html
using the default html elements.

```
toHtml (Text "Inner text") == Html.text "Inner text"
toHtml HardLineBreak == Html.br [] []
toHtml (CodeInline code) == Html.code [] [ Html.text code ]
```
-}
toHtml : Inline i -> Html msg
toHtml =
    defaultHtml Nothing



{-| Transform an Inline into a Html, optionally
using custom html elements to render inner inlines.

```
upperText : Inline i -> Html msg
upperText inline =
    case inline of
        Text str ->
            Html.text (String.toUpper str)

        _ ->
            defaultHtml (Just upperText) inline


defaultHtml (Just upperText) (Text "hello") == text "hello"
defaultHtml (Just upperText) (Emphasis 2 [ Text "hello" ]) == strong [] [ text "HELLO" ]
upperText (Text "hello") == text "HELLO"

```
**Note:** If the first argument is `Nothing`,
the default html elements will be used to render
the inner inlines.
-}

defaultHtml : Maybe ( Inline i -> Html msg ) -> Inline i -> Html msg
defaultHtml customTransformer inline =
    let
        transformer : Inline i -> Html msg
        transformer =
            Maybe.withDefault (defaultHtml Nothing) customTransformer


    in case inline of
        Text str ->
            text str


        HardLineBreak ->
            br [] []


        CodeInline codeStr ->
            code [] [ text codeStr ]


        Link url maybeTitle inlines ->
            case maybeTitle of
                Just title_ ->
                    a [ href url, title title_ ]
                        (List.map transformer inlines)


                Nothing ->
                    a [ href url ]
                        (List.map transformer inlines)
    

        Image url maybeTitle inlines ->
            case maybeTitle of
                Just title_ ->
                    img
                        [ alt (extractText inlines)
                        , src url
                        , title title_
                        ] []


                Nothing ->
                    img
                        [ alt (extractText inlines)
                        , src url
                        ] []


        HtmlInline tag attrs inlines ->
            node tag
                (attributesToHtmlAttributes attrs)
                (List.map transformer inlines)


        Emphasis length inlines ->
            case length of
                1 ->
                    em [] (List.map transformer inlines)


                2 ->
                    strong [] (List.map transformer inlines)
                    

                _ ->
                    if length - 2 > 0 then
                        strong []
                            <| flip (::) []
                            <| transformer
                            <| Emphasis (length - 2) inlines


                    else
                        em [] (List.map transformer inlines)


        Custom _ inlines ->
            text ""


attributesToHtmlAttributes : List Attribute -> List (Html.Attribute msg)
attributesToHtmlAttributes =
    List.map attributeToAttribute


attributeToAttribute : Attribute -> Html.Attribute msg
attributeToAttribute ( name, maybeValue ) =
    attribute name (Maybe.withDefault name maybeValue)



----------------------------------------------------------------------
------------------------------- Helpers ------------------------------
----------------------------------------------------------------------


{-| Extract the text from a list of inlines.

```
inlines : List (Inline i)
inlines =
    [ Text "Heading with "
    , Emphasis 1
        [ Text "emphasis" ]
    ]

extractText inlines == "Heading with emphasis"

-- Original string: "Heading with *emphasis*"
```

-}

extractText : List (Inline i) -> String
extractText inlines =
    List.foldl extractTextHelp "" inlines


extractTextHelp : Inline i -> String -> String
extractTextHelp inline text =
    case inline of
        Text str ->
            text ++ str


        HardLineBreak ->
            text ++ " "


        CodeInline str ->
            text ++ str


        Link _ _ inlines ->
            text ++ extractText inlines


        Image _ _ inlines ->
            text ++ extractText inlines


        HtmlInline _ _ inlines ->
            text ++ extractText inlines


        Emphasis _ inlines ->
            text ++ extractText inlines


        Custom _ inlines ->
            text ++ extractText inlines

