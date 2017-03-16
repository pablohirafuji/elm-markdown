# Elm Markdown

Pure Elm markdown parsing and rendering.

Based on the latest [CommonMark Spec](http://spec.commonmark.org/0.27/), with [some differences](#differences-from-commonmark).
[Demo](https://pablohirafuji.github.io/elm-markdown/examples/Demo.html).

- [Basic Usage](#basic-usage)
- [Supported Syntax](#supported-syntax)
  - [Heading](#heading)
  - [Quoting](#quoting)
  - [Code](#code)
  - [Link](#link)
  - [Lists](#lists)
  - [Paragraphs and line breaks](#paragraphs-and-line-breaks)
  - [Thematic Break Line](#thematic-break-line)
  - [Emphasis](#emphasis)
  - [Image](#image)
- [Differences from CommonMark](#differences-from-commonmark)
- [Options](#options)
- [Customization](#customization)
- [Performance](#performance)
- [Advanced Usage](#advanced-usage)
  - [Implementing GFM Task List](#implementing-gfm-task-list)
- [Changelog](#changelog)

## Basic Usage


```elm
import Markdown


view : Html msg
view =
    div []
        <| Markdown.toHtml Nothing "# Heading with *emphasis*"
```



## Supported Syntax



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
brackets `[ ]`, and then wrapping the URL in parentheses `( )`, with a optional title using single quotes, double quotes or parentheses.

    Do you know the Elm [slack channel](https://elmlang.slack.com/ "title")?

Or create a reference link:

    [slackLink]: https://elmlang.slack.com/ 'title'

    Do you know the Elm [slack channel][slackLink]?

Or even:

    [slack channel]: https://elmlang.slack.com/ (title)

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
unless it's preceded by two spaces or `\`, which will output
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



## Differences from CommonMark

- No html entity encoding support, as Elm does not need it (e.g.: `<` to `&lt;`, `>` to `&gt;`);
- Limited html entity decoding support, due to compiler issues with large Dicts;
- No comment tag support (`<!-- -->`);
- No CDATA tag support (`<![CDATA[ ]]>`);
- No processing instruction tag support (`<? ?>`);
- No declaration tag support (`<! >`);
- No [malformed](http://spec.commonmark.org/0.27/#example-122) html tag support (e.g.: `<div class`);
- No balanced parenthesis in inline link's url support (e.g.: `[link](url() "title")`, use `[link](<url()> "title")` instead);
- To create a HTML block, wich is not surrounded by paragraph tag (`<p>`), start and finish a paragraph with the html tag you want the HTML block to be, with no blankline between the start and end tag. E.g.:

        First paragraph.

        <table>
            <tr>
                <td>
                    Table element
                </td>
            </tr>
        </table>

        Next paragraph.




## Options

Use `Markdown.toHtml` to specify parsing options:

```elm
import Markdown
import Markdown.Config exposing (Options, defaultOptions)


customOptions : Options
customOptions =
    { defaultOptions
        | softAsHardLineBreak = True
    }


view : Html msg
view =
    div []
        <| Markdown.toHtml (Just customOptions)
        <| "# Heading with *emphasis*"
```

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
        [ "address", "article", "aside", "b", "blockquote", "br"
        , "caption", "center", "cite", "code", "col", "colgroup"
        , "dd", "details", "div", "dl", "dt", "figcaption", "figure"
        , "footer", "h1", "h2", "h3", "h4", "h5", "h6", "hr", "i"
        , "legend", "li", "menu", "menuitem", "nav", "ol", "optgroup"
        , "option", "p", "pre", "section", "strike", "summary"
        , "small", "table", "tbody", "td", "tfoot", "th", "thead"
        , "tr", "ul" ]
    , allowedHtmlAttributes =
        [ "name", "class" ]
    }
```

> **Note:** Only basic sanitization is provided.
If you are receiving user submitted content, you should use a specific library to sanitize user input.



## Customization

You can customize how each markdown element is rendered by
first parsing the markdown string into blocks, then mapping the resulting blocks through a custom renderer, created with the help of `Blocks.defaultHtml` and/or `Inline.defaultHtml`, then concatenate the resulting list.

Example of rendering:
- All blockquotes as a detail element;
- Images using figure and figcaption;
- Links not starting with `http://elm-lang.org` with a `target="_blank"` attribute.

```elm
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))


view : Html msg
view =
    myMarkdownString
        |> Block.parse Nothing -- using Config.defaultOptions
        |> List.map (customHtmlBlock)
        |> List.concat
        |> article []


customHtmlBlock : Block b i -> List (Html msg)
customHtmlBlock block =
    case block of
        BlockQuote blocks ->
            List.map customHtmlBlock blocks
                |> List.concat
                |> details []
                |> flip (::) []


        _ ->
            Block.defaultHtml
                (Just customHtmlBlock)
                (Just customHtmlInline)
                block


customHtmlInline : Inline i -> Html msg
customHtmlInline inline =
    case inline of
        Image url maybeTitle inlines ->
            figure []
                [ img
                    [ alt (Inline.extractText inlines)
                    , src url
                    , title (Maybe.withDefault "" maybeTitle)
                    ] []
                , figcaption []
                    [ text (Inline.extractText inlines) ]
                ]


        Link url maybeTitle inlines ->
            if String.startsWith "http://elm-lang.org" url then
                a [ href url
                  , title (Maybe.withDefault "" maybeTitle)
                  ] (List.map customHtmlInline inlines)

            else
                a [ href url
                  , title (Maybe.withDefault "" maybeTitle)
                  , target "_blank"
                  , rel "noopener noreferrer"
                  ] (List.map customHtmlInline inlines)


        _ ->
            Inline.defaultHtml (Just customHtmlInline) inline
```


## Performance

Performance improvement is being made constantly. [Feedbacks](https://github.com/pablohirafuji/elm-markdown/issues) are welcome.

Parsing a 1.2MB (~30k lines) markdown text file to html in my notebook using node:

- [Marked](https://github.com/chjj/marked): ~130ms
- [CommonMark JS](https://github.com/jgm/commonmark.js): ~250ms
- This package: ~1150ms


## Advanced Usage

Guides to help advanced usage.

### Implementing GFM Task List

Let's implement a [Task List](https://github.github.com/gfm/#task-list-items-extension-), like the GitHub Flavored Markdown
has.

First, let's create a type for our task:

```elm
type GFMInline
    = Task Bool
```

Where `Bool` is where we will store the information about the
`Task` state, i.e. if is checked or not.

According to the [GFM Spec](https://github.github.com/gfm/#task-list-items-extension-)
a task list item occurs only in lists and is the first thing in the
list item.

The `Block.walk` function seens a perfect match for our use case!
It will walk the given block and apply a function to every inner
`Block`, if is a container block, and the `Block` itself.


The `Block.walk` function has this signature:
`(Block b i -> Block b i) -> Block b i -> Block b i`. The first argument is a function that receives and return a `Block`. So let's
create that function:

```elm
parseTaskList : Block b GFMInline -> Block b GFMInline
parseTaskList block =
    case block of
        Block.List listBlock items ->
            List.map parseTaskListItem items
                |> Block.List listBlock

        _ ->
            block
```

Note the function signature: we replaced the `i` in
`Block b i` for `GFMInline`, the type we created ealier.

The function will match any `List` block type and map over
it's items, witch type is `List (List (Block b i))`, applying
the `parseTaskListItem` function to each item.

Now let's create the `parseTaskListItem` function:

```elm
parseTaskListItem : List (Block b GFMInline) -> List (Block b GFMInline)
parseTaskListItem item =
    case item of
        Block.Paragraph rawText (Inline.Text text :: inlinesTail)
            :: tail ->
                parseTaskListText text ++ inlinesTail
                    |> Block.Paragraph rawText
                    |> flip (::) tail

        Block.PlainInlines (Inline.Text text :: inlinesTail)
            :: tail ->
                parseTaskListText text ++ inlinesTail
                    |> Block.PlainInlines
                    |> flip (::) tail

        _ ->
            item


parseTaskListText : String -> List (Inline GFMInline)
parseTaskListText text =
    if String.startsWith "[x]" text then
        [ Inline.Custom (Task True) []
        , String.dropLeft 3 text
            |> String.trimLeft
            |> Inline.Text
        ]

    else if String.startsWith "[ ]" text then
        [ Inline.Custom (Task False) []
        , String.dropLeft 3 text
            |> String.trimLeft
            |> Inline.Text
        ]

    else
        [ Inline.Text text ]
```

Our `parseTaskListItem` function will match any `Paragraph` or
`PlainInlines` type (a list can be loose or tight), extracting
the interesting parts (thanks to pattern matching!) that we use
in the `parseTaskListText` function. Then we check the text for
valid Task List and return the appropriate result.


Now it's view time! Let's create our `Task`'s view:

```elm
gfmInlineView : Inline GFMInline -> Html msg
gfmInlineView inline =
    case inline of
        Inline.Custom (Task isChecked) _ ->
            Html.input
                [ Html.Attributes.disabled True
                , Html.Attributes.checked isChecked
                , Html.Attributes.type_ "checkbox"
                ] []

        _ ->
            Inline.defaultHtml (Just gfmInlineView) inline
```

And we finish glueing everything together:

```elm
gfmToHtml : String -> Html msg
gfmToHtml str =
    str
        |> Block.parse Nothing -- using Config.defaultOptions
        |> List.map (Block.walk parseTaskList)
        |> List.concatMap gfmBlockView
        |> Html.div []


gfmBlockView : Block b GFMInline -> List (Html msg)
gfmBlockView block =
    Block.defaultHtml
        Nothing -- using Block.defaultHtml to render the inner blocks
        (Just gfmInlineView)
        block
```

That's it! We implemented a fully functional GFM Task List!



## Changelog

- v2.0.4: Added "Implementing GFM Task List guide" to README.
- v2.0.3: Added most common html entities and numeric character references support.


## Thanks

Thank you John MacFarlane, for creating [CommonMark](http://commonmark.org/) specification and tests.

Thank you everyone who gave feedback. Special thanks to Jan Tojnar, for discussing about the API.

Thank you Evan for bringing joy to the frontend.
