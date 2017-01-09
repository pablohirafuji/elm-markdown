module Test.BlockQuote exposing (..)


import Html exposing (..)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#block-quotes


run : List (Output)
run =
    [ testEq 189
        [ p [] [ text "A block quote marker consists of 0-3 spaces of initial indent, plus (a) the character > together with a following space, or (b) a single character > not followed by a space." ]
        , p [] [ text "The following rules define block quotes:" ]
        , ol []
            [ li [] [ text "Basic case. If a string of lines Ls constitute a sequence of blocks Bs, then the result of prepending a block quote marker to the beginning of each line in Ls is a block quote containing Bs." ]
            , li [] [ text "Laziness. If a string of lines Ls constitute a block quote with contents Bs, then the result of deleting the initial block quote marker from one or more lines in which the next non-whitespace character after the block quote marker is paragraph continuation text is a block quote with Bs as its content. Paragraph continuation text is text that will be parsed as part of the content of a paragraph, but does not occur at the beginning of the paragraph." ]
            , li [] [ text "Consecutiveness. A document cannot contain two block quotes in a row unless there is a blank line between them." ]
            ]
        , p [] [ text "Nothing else counts as a block quote." ]
        , p [] [ text "Here is a simple example:" ]
        ]
        "> # Foo\n> bar\n> baz"
        [ blockquote [] [ h1 [] [ text "Foo" ], p [] [ text "bar\nbaz"] ] ]


    , testEq 190
        [ p [] [ text "The spaces after the > characters can be omitted:" ]
        ]
        "># Foo\n>bar\n> baz"
        [ blockquote [] [ h1 [] [ text "Foo" ], p [] [ text "bar\nbaz"] ] ]


    , testEq 191
        [ p [] [ text "The > characters can be indented 1-3 spaces:" ]
        ]
        "   > # Foo\n   > bar\n > baz"
        [ blockquote [] [ h1 [] [ text "Foo" ], p [] [ text "bar\nbaz"] ] ]


    , testEq 192
        [ p [] [ text "Four spaces gives us a code block:" ]
        ]
        "    > # Foo\n    > bar\n    > baz"
        [ pre [] [ code [] [ text "> # Foo\n> bar\n> baz\n" ] ] ]


    , testEq 193
        [ p [] [ text "The Laziness clause allows us to omit the > before paragraph continuation text:" ]
        ]
        "> # Foo\n> bar\nbaz"
        [ blockquote [] [ h1 [] [ text "Foo" ], p [] [ text "bar\nbaz"] ] ]


    , testEq 194
        [ p [] [ text "A block quote can contain some lazy and some non-lazy continuation lines:" ]
        ]
        "> bar\nbaz\n> foo"
        [ blockquote [] [ p [] [ text "bar\nbaz\nfoo"] ] ]


    , testEq 195
        [ p [] [ text "Laziness only applies to lines that would have been continuations of paragraphs had they been prepended with block quote markers. For example, the > cannot be omitted in the second line of" ]
        , pre [] [ code [] [ text "> foo\n> ---\n" ] ]
        , p [] [ text "without changing the meaning:" ]
        ]
        "> foo\n---"
        [ blockquote [] [ p [] [ text "foo"] ], hr [] [] ]


    , testEq 196
        [ p [] [ text "Similarly, if we omit the > in the second line of" ]
        , pre [] [ code [] [ text "> - foo\n> - bar\n" ] ]
        , p [] [ text "then the block quote ends after the first line:" ]
        ]
        "> - foo\n- bar"
        [ blockquote [] [ ul [] [ li [] [ text "foo" ] ] ], ul [] [ li [] [ text "bar" ] ] ]


    , testEq 197
        [ p [] [ text "For the same reason, we can’t omit the > in front of subsequent lines of an indented or fenced code block:" ]
        ]
        ">     foo\n    bar"
        [ blockquote [] [ pre [] [ code [] [ text "foo\n" ] ] ], pre [] [ code [] [ text "bar\n" ] ] ]


    , testEq 198
        [ p [] [ text "" ]
        ]
        "> ```\nfoo\n```"
        [ blockquote [] [ pre [] [ code [] [ text "" ] ] ], p [] [ text "foo" ], pre [] [ code [] [ text "" ] ] ]


    , testEq 199
        [ p [] [ text "Note that in the following case, we have a lazy continuation line:" ]
        ]
        "> foo\n    - bar"
        [ blockquote [] [ p [] [ text "foo\n- bar" ] ] ]


    , testEq 200
        [ p [] [ text "A block quote can be empty:" ]
        ]
        ">"
        [ blockquote [] [] ]


    , testEq 201
        []
        ">\n>  \n> "
        [ blockquote [] [] ]


    , testEq 202
        [ p [] [ text "A block quote can have initial or final blank lines:" ]
        ]
        ">\n> foo\n>  "
        [ blockquote [] [ p [] [ text "foo" ] ] ]


    , testEq 203
        [ p [] [ text "A blank line always separates block quotes:" ]
        ]
        "> foo\n\n> bar"
        [ blockquote [] [ p [] [ text "foo" ] ], blockquote [] [ p [] [ text "bar" ] ] ]


    , testEq 204
        [ p [] [ text "Consecutiveness means that if we put these block quotes together, we get a single block quote:" ]
        ]
        "> foo\n> bar"
        [ blockquote [] [ p [] [ text "foo\nbar" ] ] ]


    , testEq 205
        [ p [] [ text "To get a block quote with two paragraphs, use:" ]
        ]
        "> foo\n>\n> bar"
        [ blockquote [] [ p [] [ text "foo" ], p [] [ text "bar" ] ] ]


    , testEq 206
        [ p [] [ text "Block quotes can interrupt paragraphs:" ]
        ]
        "foo\n> bar"
        [  p [] [ text "foo" ], blockquote [] [  p [] [ text "bar" ] ] ]


    , testEq 207
        [ p [] [ text "In general, blank lines are not needed before or after block quotes:" ]
        ]
        "> aaa\n***\n> bbb"
        [ blockquote [] [ p [] [ text "aaa" ] ], hr [] [], blockquote [] [ p [] [ text "bbb" ] ] ]


    , testEq 208
        [ p [] [ text "However, because of laziness, a blank line is needed between a block quote and a following paragraph:" ]
        ]
        "> bar\nbaz"
        [ blockquote [] [ p [] [ text "bar\nbaz" ] ] ]


    , testEq 209
        []
        "> bar\n\nbaz"
        [ blockquote [] [ p [] [ text "bar" ] ], p [] [ text "baz" ] ]


    , testEq 210
        []
        "> bar\n>\nbaz"
        [ blockquote [] [ p [] [ text "bar" ] ], p [] [ text "baz" ] ]


    , testEq 211
        [ p [] [ text "It is a consequence of the Laziness rule that any number of initial >s may be omitted on a continuation line of a nested block quote:" ]
        ]
        "> > > foo\nbar"
        [ blockquote [] [ blockquote [] [ blockquote [] [ p [] [ text "foo\nbar" ] ] ] ] ]


    , testEq 212
        []
        ">>> foo\n> bar\n>>baz"
        [ blockquote [] [ blockquote [] [ blockquote [] [ p [] [ text "foo\nbar\nbaz" ] ] ] ] ]


    , testEq 213
        [ p [] [ text "When including an indented code block in a block quote, remember that the block quote marker includes both the > and a following space. So five spaces are needed after the >:" ]
        ]
        ">     code\n\n>    not code"
        [ blockquote [] [ pre [] [ code [] [ text "code\n" ] ] ], blockquote [] [ p [] [ text "not code" ] ] ]

    ]

