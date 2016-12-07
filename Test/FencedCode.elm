module Test.FencedCode exposing (..)


import Html exposing (..)
import Html.Attributes exposing (class)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#fenced-code-blocks


run : List (Output msg)
run =
    [ testEq 88
        [ p [] [ text "A code fence is a sequence of at least three consecutive backtick characters (`) or tildes (~). (Tildes and backticks cannot be mixed.) A fenced code block begins with a code fence, indented no more than three spaces." ]
        , p [] [ text "The line with the opening code fence may optionally contain some text following the code fence; this is trimmed of leading and trailing spaces and called the info string. The info string may not contain any backtick characters. (The reason for this restriction is that otherwise some inline code would be incorrectly interpreted as the beginning of a fenced code block.)" ]
        , p [] [ text "The content of the code block consists of all subsequent lines, until a closing code fence of the same type as the code block began with (backticks or tildes), and with at least as many backticks or tildes as the opening code fence. If the leading code fence is indented N spaces, then up to N spaces of indentation are removed from each line of the content (if present). (If a content line is not indented, it is preserved unchanged. If it is indented less than N spaces, all of the indentation is removed.)" ]
        , p [] [ text "The closing code fence may be indented up to three spaces, and may be followed only by spaces, which are ignored. If the end of the containing block (or document) is reached and no closing code fence has been found, the code block contains all of the lines after the opening code fence until the end of the containing block (or document). (An alternative spec would require backtracking in the event that a closing code fence is not found. But this makes parsing much less efficient, and there seems to be no real down side to the behavior described here.)" ]
        , p [] [ text "A fenced code block may interrupt a paragraph, and does not require a blank line either before or after." ]
        , p [] [ text "The content of a code fence is treated as literal text, not parsed as inlines. The first word of the info string is typically used to specify the language of the code sample, and rendered in the class attribute of the code tag. However, this spec does not mandate any particular treatment of the info string." ]
        , p [] [ text "Here is a simple example with backticks:" ]
        ]
        "```\n<\n >\n```"
        [ pre [] [ code [] [ text "<\n >" ] ] ]


    , testEq 89
        [ p [] [ text "With tildes:" ]
        ]
        "~~~\n<\n >\n~~~"
        [ pre [] [ code [] [ text "<\n >" ] ] ]


    , testEq 90
        [ p [] [ text "The closing code fence must use the same character as the opening fence:" ]
        ]
        "```\naaa\n~~~\n```"
        [ pre [] [ code [] [ text "aaa\n~~~" ] ] ]


    , testEq 91
        []
        "~~~\naaa\n```\n~~~"
        [ pre [] [ code [] [ text "aaa\n```" ] ] ]


    , testEq 92
        [ p [] [ text "The closing code fence must be at least as long as the opening fence:" ]
        ]
        "````\naaa\n```\n``````"
        [ pre [] [ code [] [ text "aaa\n```" ] ] ]


    , testEq 93
        []
        "~~~~\naaa\n~~~\n~~~~"
        [ pre [] [ code [] [ text "aaa\n~~~" ] ] ]


    , testEq 94
        [ p [] [ text "Unclosed code blocks are closed by the end of the document (or the enclosing block quote or list item):" ]
        ]
        "```"
        [ pre [] [ code [] [ text "" ] ] ]


    , testEq 95
        []
        "`````\n\n```\naaa"
        [ pre [] [ code [] [ text "\n```\naaa" ] ] ]


    , testEq 96
        []
        "> ```\n> aaa\n\nbbb"
        [ blockquote [] [ pre [] [ code [] [ text "" ] ] ], p [] [ text "bbb"] ]


    , testEq 97
        [ p [] [ text "A code block can have all empty lines as its content:" ]
        ]
        "```\n\n  \n```"
        [ pre [] [ code [] [ text "\n  " ] ] ]


    , testEq 98
        [ p [] [ text "A code block can be empty:" ]
        ]
        "```\n```"
        [ pre [] [ code [] [ text "" ] ] ]


    , testEq 99
        [ p [] [ text "Fences can be indented. If the opening fence is indented, content lines will have equivalent opening indentation removed, if present:" ]
        ]
        " ```\n aaa\naaa\n```"
        [ pre [] [ code [] [ text "aaa\naaa" ] ] ]


    , testEq 100
        []
        "  ```\naaa\n  aaa\naaa\n  ```"
        [ pre [] [ code [] [ text "aaa\naaa\naaa" ] ] ]


    , testEq 101
        []
        "   ```\n   aaa\n    aaa\n  aaa\n   ```"
        [ pre [] [ code [] [ text "aaa\n aaa\naaa" ] ] ]


    , testEq 102
        [ p [] [ text "Four spaces indentation produces an indented code block:" ]
        ]
        "    ```\n    aaa\n    ```"
        [ pre [] [ code [] [ text "```\naaa\n```" ] ] ]


    , testEq 103
        [ p [] [ text "Closing fences may be indented by 0-3 spaces, and their indentation need not match that of the opening fence:" ]
        ]
        "```\naaa\n  ```"
        [ pre [] [ code [] [ text "aaa" ] ] ]


    , testEq 104
        []
        "   ```\naaa\n  ```"
        [ pre [] [ code [] [ text "aaa" ] ] ]


    , testEq 105
        [ p [] [ text "This is not a closing fence, because it is indented 4 spaces:" ]
        ]
        "```\naaa\n    ```"
        [ pre [] [ code [] [ text "aaa\n    ```" ] ] ]


    , testEq 106
        [ p [] [ text "Code fences (opening and closing) cannot contain internal spaces:" ]
        ]
        "``` ```\naaa"
        [ p [] [ code [] [ text "" ], text "\naaa" ] ]


    , testEq 107
        []
        "~~~~~~\naaa\n~~~ ~~"
        [ pre [] [ code [] [ text "aaa\n~~~ ~~" ] ] ]


    , testEq 108
        [ p [] [ text "Fenced code blocks can interrupt paragraphs, and can be followed directly by paragraphs, without a blank line between:" ]
        ]
        "foo\n```\nbar\n```\nbaz"
        [ p [] [ text "foo"], pre [] [ code [] [ text "bar" ] ], p [] [ text "baz"] ]


    , testEq 109
        [ p [] [ text "Other blocks can also occur before and after fenced code blocks without an intervening blank line:" ]
        ]
        "foo\n---\n~~~\nbar\n~~~\n# baz"
        [ h2 [] [ text "foo" ], pre [] [ code [] [ text "bar" ] ], h1 [] [ text "baz" ] ]


    , testEq 110
        [ p [] [ text "An info string can be provided after the opening code fence. Opening and closing spaces will be stripped, and the first word, prefixed with language-, is used as the value for the class attribute of the code element within the enclosing pre element." ]
        ]
        "```ruby\ndef foo(x)\n  return 3\nend\n```"
        [ pre [] [ code [ class "language-ruby" ] [ text "def foo(x)\n  return 3\nend" ] ] ]


    , testEq 111
        []
        "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~"
        [ pre [] [ code [ class "language-ruby" ] [ text "def foo(x)\n  return 3\nend" ] ] ]


    , testEq 112
        []
        "````;\n````"
        [ pre [] [ code [ class "language-;" ] [ text "" ] ] ]


    , testEq 113
        [ p [] [ text "Info strings for backtick code blocks cannot contain backticks" ]
        ]
        "``` aa ```\nfoo"
        [ p [] [ code [] [ text "aa" ], text "\nfoo" ] ]


    , testEq 114
        [ p [] [ text "Closing code fences cannot have info strings:" ]
        ]
        "```\n``` aaa\n```"
        [ pre [] [ code [] [ text "``` aaa" ] ] ]

    ]