module Markdown.Config
    exposing
        ( HtmlOption(..)
        , Options
        , SanitizeOptions
        , defaultOptions
        , defaultSanitizeOptions
        )

{-| Configure markdown to fit your needs.


# Options

@docs Options, HtmlOption, SanitizeOptions, defaultOptions, defaultSanitizeOptions

-}

----------------------------------------------------------------------
------------------------------- Options ------------------------------
----------------------------------------------------------------------


{-| Some parser options so you can tweak things for your
particular case.

  - `softAsHardLineBreak`: If set to `True`, will render `\n` as `<br>`.
  - `rawHtml`: This determine what to do with raw html elements.

-}
type alias Options =
    { softAsHardLineBreak : Bool
    , rawHtml : HtmlOption
    }


{-| The `Options` used in `Markdown.toHtml`.

    { softAsHardLineBreak = False
    , rawHtml = Sanitize defaultSanitizeOptions
    }

-}
defaultOptions : Options
defaultOptions =
    { softAsHardLineBreak = False
    , rawHtml = Sanitize defaultSanitizeOptions
    }


{-| Choose what to do with raw html tags.

  - `ParseUnsafe`: Will parse any valid html tag and attribute. This includes malicious code like `<script>alert("XSS ALERT!!");</script>`.
  - `Sanitize SanitizeOptions`: Will parse only specific html elements and attributes.
  - `DontParse`: Do not parse any html tag. It will include the raw text in the output.

-}
type HtmlOption
    = ParseUnsafe
    | Sanitize SanitizeOptions
    | DontParse


{-| Choose what html elements and attributes are allowed to parse.

  - `allowedHtmlElements`: List of allowed html elements.
  - `allowedHtmlAttributes`: List of allowed attributes.

-}
type alias SanitizeOptions =
    { allowedHtmlElements : List String
    , allowedHtmlAttributes : List String
    }


{-| The `SanitizeOptions` used by `defaultOptions`.

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

-}
defaultSanitizeOptions : SanitizeOptions
defaultSanitizeOptions =
    { allowedHtmlElements = defaultAllowedHtmlElements
    , allowedHtmlAttributes = defaultAllowedHtmlAttributes
    }


defaultAllowedHtmlElements : List String
defaultAllowedHtmlElements =
    [ "address"
    , "article"
    , "aside"
    , "b"
    , "blockquote"
    , "br"
    , "caption"
    , "center"
    , "cite"
    , "code"
    , "col"
    , "colgroup"
    , "dd"
    , "details"
    , "div"
    , "dl"
    , "dt"
    , "figcaption"
    , "figure"
    , "footer"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "hr"
    , "i"
    , "legend"
    , "li"
    , "menu"
    , "menuitem"
    , "nav"
    , "ol"
    , "optgroup"
    , "option"
    , "p"
    , "pre"
    , "section"
    , "strike"
    , "summary"
    , "small"
    , "table"
    , "tbody"
    , "td"
    , "tfoot"
    , "th"
    , "thead"
    , "tr"
    , "ul"
    ]


defaultAllowedHtmlAttributes : List String
defaultAllowedHtmlAttributes =
    [ "name", "class" ]
