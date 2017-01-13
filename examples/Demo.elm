import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick)
import Markdown
import Markdown.Config exposing (defaultOptions, defaultSanitizeOptions, HtmlOption(..))



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
    , options : Markdown.Config.Options
    }


init : Model
init =
    { textarea = readmeMD
    , options = defaultOptions
    }


type Msg
    = TextAreaInput String
    | SoftAsHardLineBreak Bool
    | HtmlOption HtmlOption
    | Markdown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaInput str ->
            { model | textarea = str } ! []


        SoftAsHardLineBreak bool ->
            let
                options = model.options
                updtOptions =
                    { options
                        | softAsHardLineBreak = bool
                    }
            
            in
                { model | options = updtOptions } ! []


        HtmlOption htmlConfig ->
            let
                options = model.options
                updtOptions =
                    { options
                        | html = htmlConfig
                    }
            
            in
                { model | options = updtOptions } ! []


        Markdown ->
            model ! []


view : Model -> Html Msg
view model =
    div
        [ style
            [ ("font-family", "sans-serif")
            , ("color", "rgba(0,0,0,0.8)")
            , ("margin", "0 auto")
            , ("padding", "20px")
            , ("max-width", "1080px")
            ]
        ]
        [ h1 [] [ text "Pure Elm Markdown" ]
        , div [ style [ ("display", "flex") ] ]
            [ div [ style [ ("width", "50%") ] ]
                [ textarea
                    [ onInput TextAreaInput
                    , defaultValue model.textarea
                    , style
                        [ ("width", "90%")
                        , ("height", "400px")
                        ]
                    ] []
                , h2 [] [ text "Options" ]
                , label []
                    [ input
                        [ type_ "checkbox"
                        , onCheck SoftAsHardLineBreak
                        ] []
                    , text " softAsHardLineBreak"
                    ]
                , h3 [] [ text "Html" ]
                , ul
                    [ style
                        [ ("list-style", "none")
                        , ("padding-left", "0")
                        ]
                    ]
                    [ li []
                        [ radio "ParseUnsafe" (ParseUnsafe) ]
                    , li []
                        [ radio "Sanitize defaultAllowed"
                            (Sanitize defaultSanitizeOptions)
                        ]
                    , li []
                        [ radio "DontParse" (DontParse) ]
                    ]
                ]
            , div [ style [ ("width", "50%") ] ]
                [ Html.map (always Markdown)
                    <| div []
                    <| Markdown.withOptions
                        model.options
                        model.textarea
                ]
            ]
        ]


radio : String -> HtmlOption -> Html Msg
radio value msg =
    label []
        [ input
            [ type_ "radio"
            , name "htmlOption"
            , onClick (HtmlOption msg)
            ] []
        , text value
        ]


readmeMD : String
readmeMD = """
# Elm Markdown

Pure elm markdown.

## Usage

```elm
...

import Markdown

...


type Msg
    = MsgOfmyApp1
    | MsgOfmyApp2
    | MsgOfmyApp3
    | Markdown


markdownView : Model -> Html Msg
markdownView model =
    Html.map (always Markdown)
        <| section []
        <| Markdown.toHtml model.markdownText

...

```


## Supported syntax


### Heading

To create a heading, add one to six `#` symbols before
your heading text. The number of `#` you use will determine
the size of the heading.

```
# Heading 1
## Heading 2
###### Heading 6
```


### Quoting

Lines starting with a `>` will be a block quote.

```
> Block quote
```


### Code

Use a sequence of single backticks (`` ` ``) to output code.
The text within the backticks will not be formatted.
To format code or text into its own distinct block, use
at least three backticks or four spaces or tab.

````
Example of `inline code`
    
    Example of block code

```optionalLang
Example of block code
```
````

If the language in the fenced code block is defined,
it will be added a `class="language-optionalLang"` to
the output code element.


### Link

You can create an inline link by wrapping link text in
brackets `[ ]`, and then wrapping the URL in parentheses `( )`.

```
Do you know the Elm [slack channel](https://elmlang.slack.com/)?
```

Or create a reference link:

```
[slackLink]: https://elmlang.slack.com/

Do you know the Elm [slack channel][slackLink]?
```

Or even:

```
[slack channel]: https://elmlang.slack.com/

Do you know the Elm [slack channel]?
```

All examples output the same html.


### Lists

You can make a list by preceding one or more lines of
text with `-` or `*`.

```
- Unordered list
  * Nested unordered list
5. Ordered list starting at 5
    1) Nested ordered list
```

### Paragraphs and line breaks

You can create a new paragraph by leaving a blank line
between lines of text.

```
Here's a paragraph with soft break line
at the end.

Here's a paragraph with hard break line\
at the end.
```

By default, soft line break (`\\n`) will be rendered as it is,
unless it's preceded by two spaces or `\\`, witch will output
hard break line (`<br>`).
You can customize to always render soft line breaks as hard
line breaks setting `softAsHardLineBreak = True` in the options.


### Thematic Break Line

You can create a thematic break line with a sequence of three
or more matching `-`, `_`, or `*` characters.

```
***
---
___
```


### Emphasis

You can create emphasis using the `*` or `_` characters.
Double emphasis is strong emphasis.

```
*Emphasis*, **strong emphasis**, ***both***

_Emphasis_, __strong emphasis__, ___both___
```


### Image

You can insert image using the following syntax:

```
![alt text](src-url "title")
```

For more information, see [CommonMark Spec](http://spec.commonmark.org/0.27/).



## Options

The following options are available:


```elm
softAsHardLineBreak : Bool
```

Default `False`. If set to `True`, will render `\\n` as `<br>`.


```elm
html : HtmlOption


type HtmlOption
    = ParseUnsafe
    | Sanitize SanitizeOptions
    | DontParse


type alias SanitizeOptions =
    { allowedHtmlElements : List String
    , allowedHtmlAttributes : List String
    }
```

Default `Sanitize`. You can choose to not parse any
html tags (`DontParse`) or allow any html tag without
any sanitization (`ParseUnsafe`).


Default allowed elements and attributes:

```elm
defaultAllowedHtmlElements : List String
defaultAllowedHtmlElements =
    [ "address", "article", "aside", "b", "blockquote"
    , "body","br", "caption", "center", "cite", "code", "col"
    , "colgroup", "dd", "details", "div", "dl", "dt", "figcaption"
    , "figure", "footer", "h1", "h2", "h3", "h4", "h5", "h6", "hr"
    , "i", "legend", "li", "link", "main", "menu", "menuitem"
    , "nav", "ol", "optgroup", "option", "p", "pre", "section"
    , "strike", "summary", "small", "table", "tbody", "td"
    , "tfoot", "th", "thead", "title", "tr", "ul" ]


defaultAllowedHtmlAttributes : List String
defaultAllowedHtmlAttributes =
    [ "name", "class" ]
```

Please note that is provided basic sanitization.
If you are accepting user submmited content, use a specific library to sanitize.


## Customization

- Example of rendering all links with `target=_blank` if does not start with a specific string.
- Example of rendering all images using `figure` and `figcaption`.


## TODO

- Improve tab parser
- Get feedback if encoded characters replacement is needed
- Get feedback about missing wanted features
- Get feedback about the API

"""
