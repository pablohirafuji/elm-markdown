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
                        | rawHtml = htmlConfig
                    }
            
            in
                { model | options = updtOptions } ! []


        Markdown ->
            model ! []


view : Model -> Html Msg
view model =
    div [ containerStyle ]
        [ h1 []
            [ a [ href "http://package.elm-lang.org/packages/pablohirafuji/elm-markdown/latest" ]
                [ text "Elm Markdown" ]
            , text " / "
            , a [ href "https://github.com/pablohirafuji/elm-markdown/blob/master/examples/Demo.elm"]
                [ text "Code" ]
            ]
        , div [ displayFlex ]
            [ div [ width50Style ]
                [ textarea
                    [ onInput TextAreaInput
                    , defaultValue model.textarea
                    , textareaStyle
                    ] []
                , h2 [] [ text "Options" ]
                , label []
                    [ input
                        [ type_ "checkbox"
                        , onCheck SoftAsHardLineBreak
                        , checked (model.options.softAsHardLineBreak)
                        ] []
                    , text " softAsHardLineBreak"
                    ]
                , h3 [] [ text "Html" ]
                , ul [ listStyle ]
                    [ radioItem model "ParseUnsafe" (ParseUnsafe)
                    , radioItem model "Sanitize defaultAllowed"
                        (Sanitize defaultSanitizeOptions)
                    , radioItem model "DontParse" (DontParse)
                    ]
                ]
            , Html.map (always Markdown)
                <| div [ width50Style ]
                <| Markdown.withOptions
                    model.options
                    model.textarea
            ]
        ]


radioItem : Model -> String -> HtmlOption -> Html Msg
radioItem model value msg =
    li []
        [ label []
            [ input
                [ type_ "radio"
                , name "htmlOption"
                , onClick (HtmlOption msg)
                , checked (model.options.rawHtml == msg)
                ] []
            , text value
            ]
        ]



-- Styles


containerStyle : Attribute msg
containerStyle =
    style
        [ ("font-family", "sans-serif")
        , ("color", "rgba(0,0,0,0.8)")
        , ("margin", "0 auto")
        , ("padding", "20px")
        , ("max-width", "1080px")
        ]


displayFlex : Attribute msg
displayFlex =
    style [ ("display", "flex") ]


width50Style : Attribute msg
width50Style =
    style [ ("width", "50%") ]


textareaStyle : Attribute msg
textareaStyle =
    style
        [ ("width", "90%")
        , ("height", "400px")
        ]


listStyle : Attribute msg
listStyle =
    style
        [ ("list-style", "none")
        , ("padding-left", "0")
        ]


-- Readme


readmeMD : String
readmeMD = """
# Elm Markdown

Pure Elm markdown parsing and rendering. [Demo](https://pablohirafuji.github.io/elm-markdown/examples/Demo.html).

## Basic Usage


    type Msg
        = MsgOfmyApp1
        | MsgOfmyApp2
        | MsgOfmyApp3
        | Markdown


    markdownView : Html Msg
    markdownView =
        Html.map (always Markdown)
            <| section []
            <| Markdown.toHtml "# Heading with *emphasis*"


## Supported syntax


### Heading

To create a heading, add one to six `#` symbols before
your heading text. The number of `#` you use will determine
the size of the heading.

    # Heading 1
    ## Heading 2
    ###### Heading 6

You can also use `=` or `-` after a paragraph for level 1 or 2 heading.

    Heading 1
    ==========

    Heading 2
    ----------


### Quoting

Lines starting with a `>` will be a block quote.


    > Block quote



### Code

Use a sequence of single backticks (`` ` ``) to output code.
The text within the backticks will not be formatted.
To format code or text into its own distinct block, use
at least three backticks or four spaces or tab.


    Example of `inline code`
    
        Example of block code

    ```optionalLang
    Example of block code with defined language
    ```

If the language in the fenced code block is defined,
it will be added a `class="language-optionalLang"` to
the code element.


### Link

You can create an inline link by wrapping link text in
brackets `[ ]`, and then wrapping the URL in parentheses `( )`.

    Do you know the Elm [slack channel](https://elmlang.slack.com/)?

Or create a reference link:

    [slackLink]: https://elmlang.slack.com/

    Do you know the Elm [slack channel][slackLink]?

Or even:

    [slack channel]: https://elmlang.slack.com/

    Do you know the Elm [slack channel]?

All examples output the same html.

Autolinks and emails are supported with `< >`:

    Autolink: <http://elm-lang.org/>
    Email link: <google@google.com>


### Lists

You can make a list by preceding one or more lines of
text with `-` or `*`.


    - Unordered list
      * Nested unordered list
    5. Ordered list starting at 5
        1) Nested ordered list starting at 1


### Paragraphs and line breaks

You can create a new paragraph by leaving a blank line
between lines of text.


    Here's a paragraph with soft break line
    at the end.

    Here's a paragraph with hard break line\
    at the end.


By default, soft line break (`\n`) will be rendered as it is,
unless it's preceded by two spaces or `\\`, which will output
hard break line (`<br>`).

You can customize to always render soft line breaks as hard
line breaks setting `softAsHardLineBreak = True` in the options.


### Thematic Break Line

You can create a thematic break line with a sequence of three
or more matching `-`, `_`, or `*` characters.


    ***
    ---
    ___



### Emphasis

You can create emphasis using the `*` or `_` characters.
Double emphasis is strong emphasis.


    *Emphasis*, **strong emphasis**, ***both***

    _Emphasis_, __strong emphasis__, ___both___



### Image

You can insert images using the following syntax:


    ![alt text](src-url "title")



For more information about supported syntax and parsing rules, see [CommonMark Spec](http://spec.commonmark.org/0.27/).



## Options

The following options are available:


```elm
type alias Options =
    { softAsHardLineBreak : Bool
    , rawHtml : HtmlOption
    }


type HtmlOption
    = ParseUnsafe
    | Sanitize SanitizeOptions
    | DontParse


type alias SanitizeOptions =
    { allowedHtmlElements : List String
    , allowedHtmlAttributes : List String
    }
```

- `softAsHardLineBreak`: Default `False`. If set to `True`, will render `\n` as `<br>`.
- `rawHtml`: Default `Sanitize defaultSanitizeOptions`.
You can choose to not parse any html tags (`DontParse`), parse any html tag without any sanitization (`ParseUnsafe`) or parse only specific html elements and attributes (`Sanitize SanitizeOptions`).


Default allowed elements and attributes:

```elm
defaultSanitizeOptions : SanitizeOptions
defaultSanitizeOptions =
    { allowedHtmlElements =
        [ "address", "article", "aside", "b", "blockquote"
        , "body","br", "caption", "center", "cite", "code", "col"
        , "colgroup", "dd", "details", "div", "dl", "dt", "figcaption"
        , "figure", "footer", "h1", "h2", "h3", "h4", "h5", "h6", "hr"
        , "i", "legend", "li", "link", "main", "menu", "menuitem"
        , "nav", "ol", "optgroup", "option", "p", "pre", "section"
        , "strike", "summary", "small", "table", "tbody", "td"
        , "tfoot", "th", "thead", "title", "tr", "ul" ]
    , allowedHtmlAttributes =
        [ "name", "class" ]
    }
```

Please note that is provided basic sanitization.
If you are receiving user submitted content, you should use a specific library to sanitize the user input.


## Customization

You can customize how each markdown element is rendered.
The following examples demonstrate how to do it.

- Example of rendering all links with `target="_blank"` if does not start with a specific string. [Demo](https://pablohirafuji.github.io/elm-markdown/examples/CustomLinkTag.html) / [Code](https://github.com/pablohirafuji/elm-markdown/blob/master/examples/CustomLinkTag.elm)
- Example of rendering all images using `figure` and `figcaption`.
[Demo](https://pablohirafuji.github.io/elm-markdown/examples/CustomImageTag.html) / [Code](https://github.com/pablohirafuji/elm-markdown/blob/master/examples/CustomImageTag.elm)


## TODO

- Improve docs;
- Improve tab parser;
- Get feedback if encoded characters replacement is needed;
- Get feedback about missing wanted features;
- Get feedback about the API;
"""
