module Test.ListItem exposing (..)


import Html exposing (..)
import Html.Attributes exposing (start)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#list-items


run : List (Output msg)
run =
    [ testEq 214
        [ p [] [ text "A list marker is a bullet list marker or an ordered list marker." ]
        , p [] [ text "A bullet list marker is a -, +, or * character." ]
        , p [] [ text "An ordered list marker is a sequence of 1–9 arabic digits (0-9), followed by either a . character or a ) character. (The reason for the length limit is that with 10 digits we start seeing integer overflows in some browsers.)" ]
        , p [] [ text "The following rules define list items:" ]
        , ol []
            [ li []
                [ p [] [ text "Basic case. If a sequence of lines Ls constitute a sequence of blocks Bs starting with a non-whitespace character and not separated from each other by more than one blank line, and M is a list marker of width W followed by 1 ≤ N ≤ 4 spaces, then the result of prepending M and the following spaces to the first line of Ls, and indenting subsequent lines of Ls by W + N spaces, is a list item with Bs as its contents. The type of the list item (bullet or ordered) is determined by the type of its list marker. If the list item is ordered, then it is also assigned a start number, based on the ordered list marker." ]
                , p [] [ text "Exceptions: When the first list item in a list interrupts a paragraph—that is, when it starts on a line that would otherwise count as paragraph continuation text—then (a) the lines Ls must not begin with a blank line, and (b) if the list item is ordered, the start number must be 1." ]
                ]
            ]
        , p [] [ text "For example, let Ls be the lines" ]
        ]
        "A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote."
        [ p [] [ text "A paragraph\nwith two lines." ], pre [] [ code [] [ text "indented code\n" ] ], blockquote [] [ p [] [ text "A block quote." ] ] ]


    , testEq 215
        [ p [] [ text "And let M be the marker 1., and N = 2. Then rule #1 says that the following is an ordered list item with start number 1, and the same contents as Ls:" ]
        ]
        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."
        [ ol [] [ li [] [ p [] [ text "A paragraph\nwith two lines." ], pre [] [ code [] [ text "indented code\n" ] ], blockquote [] [ p [] [ text "A block quote." ] ] ] ] ]


    , testEq 216
        [ p [] [ text "The most important thing to notice is that the position of the text after the list marker determines how much indentation is needed in subsequent blocks in the list item. If the list marker takes up two spaces, and there are three spaces between the list marker and the next non-whitespace character, then blocks must be indented five spaces in order to fall under the list item." ]
        , p [] [ text "Here are some examples showing how far content must be indented to be put under the list item:" ]
        ]
        "- one\n\n two"
        [ ul [] [ li [] [ text "one" ] ], p [] [ text "two" ] ]


    , testEq 217
        []
        "- one\n\n  two"
        [ ul [] [ li [] [ p [] [ text "one" ], p [] [ text "two" ] ] ] ]


    , testEq 218
        []
        " -    one\n\n     two"
        [ ul [] [ li [] [ text "one" ] ], pre [] [ code [] [ text " two\n" ] ] ]


    , testEq 219
        []
        " -    one\n\n      two"
        [ ul [] [ li [] [ p [] [ text "one" ], p [] [ text "two" ] ] ] ]


    , testEq 220
        [ p [] [ text "It is tempting to think of this in terms of columns: the continuation blocks must be indented at least to the column of the first non-whitespace character after the list marker. However, that is not quite right. The spaces after the list marker determine how much relative indentation is needed. Which column this indentation reaches will depend on how the list item is embedded in other constructions, as shown by this example:" ]
        ]
        "   > > 1.  one\n>>\n>>     two"
        [ blockquote [] [ blockquote [] [ ol [] [ li [] [ p [] [ text "one" ], p [] [ text "two" ] ] ] ] ] ]


    , testEq 221
        [ p [] [ text "Here two occurs in the same column as the list marker 1., but is actually contained in the list item, because there is sufficient indentation after the last containing blockquote marker." ]
        , p [] [ text "The converse is also possible. In the following example, the word two occurs far to the right of the initial text of the list item, one, but it is not considered part of the list item, because it is not indented far enough past the blockquote marker:" ]
        ]
        ">>- one\n>>\n  >  > two"
        [ blockquote [] [ blockquote [] [ ul [] [ li [] [ text "one" ] ], p [] [ text "two" ] ] ] ]


    , testEq 222
        [ p [] [ text "Note that at least one space is needed between the list marker and any following content, so these are not list items:" ]
        ]
        "-one\n\n2.two"
        [ p [] [ text "-one" ], p [] [ text "2.two" ] ]


    , testEq 223
        [ p [] [ text "A list item may contain blocks that are separated by more than one blank line." ]
        ]
        "- foo\n\n\n  bar"
        [ ul [] [ li [] [ p [] [ text "foo" ], p [] [ text "bar" ] ] ] ]


    , testEq 224
        [ p [] [ text "A list item may contain any kind of block:" ]
        ]
        "1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam"
        [ ol [] [ li [] [ p [] [ text "foo" ], pre [] [ code [] [ text "bar\n" ] ], p [] [ text "baz"], blockquote [] [ p [] [ text "bam" ] ] ] ] ]


    , testEq 225
        [ p [] [ text "A list item that contains an indented code block will preserve empty lines within the code block verbatim." ]
        ]
        "- Foo\n\n      bar\n\n\n      baz"
        [ ul [] [ li [] [ p [] [ text "Foo"], pre [] [ code [] [ text "bar\n\n\nbaz\n" ] ] ] ] ]


    , testEq 226
        [ p [] [ text "Note that ordered list start numbers must be nine digits or less:" ]
        ]
        "123456789. ok"
        [ ol [ start 123456789 ] [ li [] [ text "ok" ] ] ]


    , testEq 227
        []
        "1234567890. not ok"
        [ p [] [ text "1234567890. not ok" ] ]


    , testEq 228
        [ p [] [ text "A start number may begin with 0s:" ]
        ]
        "0. ok"
        [ ol [ start 0 ] [ li [] [ text "ok" ] ] ]


    , testEq 229
        []
        "003. ok"
        [ ol [ start 3 ] [ li [] [ text "ok" ] ] ]


    , testEq 230
        [ p [] [ text "A start number may not be negative:" ]
        ]
        "-1. not ok"
        [ p [] [ text "-1. not ok" ] ]


    , testEq 231
        [ ol [ start 2 ] [ li [] [ text "Item starting with indented code. If a sequence of lines Ls constitute a sequence of blocks Bs starting with an indented code block and not separated from each other by more than one blank line, and M is a list marker of width W followed by one space, then the result of prepending M and the following space to the first line of Ls, and indenting subsequent lines of Ls by W + 1 spaces, is a list item with Bs as its contents. If a line is empty, then it need not be indented. The type of the list item (bullet or ordered) is determined by the type of its list marker. If the list item is ordered, then it is also assigned a start number, based on the ordered list marker." ] ]
        , p [] [ text "An indented code block will have to be indented four spaces beyond the edge of the region where text will be included in the list item. In the following case that is 6 spaces:" ]
        ]
        "- foo\n\n      bar"
        [ ul [] [ li [] [ p [] [ text "foo" ], pre [] [ code [] [ text "bar\n" ] ] ] ] ]


    , testEq 232
        [ p [] [ text "And in this case it is 11 spaces:" ]
        ]
        "  10.  foo\n\n           bar"
        [ ol [ start 10 ] [ li [] [ p [] [ text "foo" ], pre [] [ code [] [ text "bar\n" ] ] ] ] ]


    , testEq 233
        [ p [] [ text "If the first block in the list item is an indented code block, then by rule #2, the contents must be indented one space after the list marker:" ]
        ]
        "    indented code\n\nparagraph\n\n    more code"
        [ pre [] [ code [] [ text "indented code\n" ] ], p [] [ text "nparagraph" ], pre [] [ code [] [ text "more code\n" ] ] ]


    , testEq 234
        []
        "1.     indented code\n\n   paragraph\n\n       more code"
        [ ol [] [ li [] [ pre [] [ code [] [ text "indented code\n" ] ], p [] [ text "nparagraph" ], pre [] [ code [] [ text "more code\n" ] ] ] ] ]


    , testEq 235
        [ p [] [ text "Note that an additional space indent is interpreted as space inside the code block:" ]
        ]
        "1.      indented code\n\n   paragraph\n\n       more code"
        [ ol [] [ li [] [ pre [] [ code [] [ text " indented code\n" ] ], p [] [ text "nparagraph" ], pre [] [ code [] [ text "more code\n" ] ] ] ] ]


    , testEq 236
        [ p [] [ text "Note that rules #1 and #2 only apply to two cases: (a) cases in which the lines to be included in a list item begin with a non-whitespace character, and (b) cases in which they begin with an indented code block. In a case like the following, where the first block begins with a three-space indent, the rules do not allow us to form a list item by indenting the whole thing and prepending a list marker:" ]
        ]
        "   foo\n\nbar"
        [ p [] [ text "foo" ], p [] [ text "bar" ] ]


    , testEq 237
        []
        "-    foo\n\n  bar"
        [ ul [] [ li [] [ text "foo" ] ], p [] [ text "bar" ] ]


    , testEq 238
        [ p [] [ text "This is not a significant restriction, because when a block begins with 1-3 spaces indent, the indentation can always be removed without a change in interpretation, allowing rule #1 to be applied. So, in the above case:" ]
        ]
        "-  foo\n\n   bar"
        [ ul [] [ li [] [ p [] [ text "foo" ], p [] [ text "bar" ] ] ] ]


    , testEq 239
        [ ol [ start 3 ] [ li [] [ text "Item starting with a blank line. If a sequence of lines Ls starting with a single blank line constitute a (possibly empty) sequence of blocks Bs, not separated from each other by more than one blank line, and M is a list marker of width W, then the result of prepending M to the first line of Ls, and indenting subsequent lines of Ls by W + 1 spaces, is a list item with Bs as its contents. If a line is empty, then it need not be indented. The type of the list item (bullet or ordered) is determined by the type of its list marker. If the list item is ordered, then it is also assigned a start number, based on the ordered list marker." ] ]
        , p [] [ text "Here are some list items that start with a blank line but are not empty:" ]
        ]
        "-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz"
        [ ul [] [ li [] [ text "foo" ], li [] [ pre [] [ code [] [ text "bar\n" ] ] ], li [] [ pre [] [ code [] [ text "baz\n" ] ] ] ] ]


    , testEq 240
        [ p [] [ text "When the list item starts with a blank line, the number of spaces following the list marker doesn’t change the required indentation:" ]
        ]
        "-   \n  foo"
        [ ul [] [ li [] [ text "foo" ] ] ]


    , testEq 241
        [ p [] [ text "A list item can begin with at most one blank line. In the following example, foo is not part of the list item:" ]
        ]
        "-\n\n  foo"
        [ ul [] [ li [] [  ] ], p [] [ text "foo" ] ]


    , testEq 242
        [ p [] [ text "Here is an empty bullet list item:" ]
        ]
        "- foo\n-\n- bar"
        [ ul [] [ li [] [ text "foo" ], li [] [], li [] [ text "bar" ] ] ]


    , testEq 243
        [ p [] [ text "It does not matter whether there are spaces following the list marker:" ]
        ]
        "- foo\n-   \n- bar"
        [ ul [] [ li [] [ text "foo" ], li [] [], li [] [ text "bar" ] ] ]


    , testEq 244
        [ p [] [ text "Here is an empty ordered list item:" ]
        ]
        "1. foo\n2.\n3. bar"
        [ ol [] [ li [] [ text "foo" ], li [] [], li [] [ text "bar" ] ] ]


    , testEq 245
        [ p [] [ text "A list may start or end with an empty list item:" ]
        ]
        "*"
        [ ul [] [ li [] [] ] ]


    , testEq 246
        [ p [] [ text "However, an empty list item cannot interrupt a paragraph:" ]
        ]
        "foo\n*\n\nfoo\n1."
        [ p [] [ text "foo\n*" ], p [] [ text "foo\n1." ] ]


    , testEq 247
        [ ol [ start 4 ] [ li [] [text "Indentation. If a sequence of lines Ls constitutes a list item according to rule #1, #2, or #3, then the result of indenting each line of Ls by 1-3 spaces (the same for each line) also constitutes a list item with the same contents and attributes. If a line is empty, then it need not be indented." ] ]
        , p [] [ text "Indented one space:" ]
        ]
        " 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote."
        [ ol [] [ li [] [ p [] [ text "A paragraph\nwith two lines." ], pre [] [ code [] [ text "indented code\n" ] ], blockquote [] [ p [] [ text "A block quote." ] ] ] ] ]


    , testEq 248
        [ p [] [ text "Indented two spaces:" ]
        ]
        "  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote."
        [ ol [] [ li [] [ p [] [ text "A paragraph\nwith two lines." ], pre [] [ code [] [ text "indented code\n" ] ], blockquote [] [ p [] [ text "A block quote." ] ] ] ] ]


    , testEq 249
        [ p [] [ text "Indented three spaces:" ]
        ]
        "   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote."
        [ ol [] [ li [] [ p [] [ text "A paragraph\nwith two lines." ], pre [] [ code [] [ text "indented code\n" ] ], blockquote [] [ p [] [ text "A block quote." ] ] ] ] ]


    , testEq 250
        [ p [] [ text "Four spaces indent gives a code block:" ]
        ]
        "    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote."
        [ pre [] [ code [] [ text "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.\n" ] ] ]


    , testEq 251
        [ ol [ start 5 ] [ li [] [text "Laziness. If a string of lines Ls constitute a list item with contents Bs, then the result of deleting some or all of the indentation from one or more lines in which the next non-whitespace character after the indentation is paragraph continuation text is a list item with the same contents and attributes. The unindented lines are called lazy continuation lines." ] ]
        , p [] [ text "Here is an example with lazy continuation lines:" ]
        ]
        "  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote."
        [ ol [] [ li [] [ p [] [ text "A paragraph\nwith two lines." ], pre [] [ code [] [ text "indented code\n" ] ], blockquote [] [ p [] [ text "A block quote." ] ] ] ] ]


    , testEq 252
        [ p [] [ text "Indentation can be partially deleted:" ]
        ]
        "  1.  A paragraph\n    with two lines."
        [ ol [] [ li [] [ text "A paragraph\nwith two lines." ] ] ]


    , testEq 253
        [ p [] [ text "These examples show how laziness can work in nested structures:" ]
        ]
        "> 1. > Blockquote\ncontinued here."
        [ blockquote [] [ ol [] [ li [] [ blockquote [] [ p [] [ text "Blockquote\ncontinued here." ] ] ] ] ] ]


    , testEq 254
        [ p [] [ text "" ]
        ]
        "> 1. > Blockquote\n> continued here."
        [ blockquote [] [ ol [] [ li [] [ blockquote [] [ p [] [ text "Blockquote\ncontinued here." ] ] ] ] ] ]


    , testEq 255
        [ ol [ start 6 ] [ li [] [text "That’s all. Nothing that is not counted as a list item by rules #1–5 counts as a list item." ] ]
        , p [] [ text "The rules for sublists follow from the general rules above. A sublist must be indented the same number of spaces a paragraph would need to be in order to be included in the list item." ]
        , p [] [ text "So, in this case we need two spaces indent:" ]
        ]
        "- foo\n  - bar\n    - baz\n      - boo"
        [ ul [] [ li [] [ text "foo", ul [] [ li [] [ text "bar", ul [] [ li [] [ text "baz", ul [] [ li [] [ text "boo" ] ] ] ] ] ] ] ] ]


    , testEq 256
        [ p [] [ text "One is not enough:" ]
        ]
        "- foo\n - bar\n  - baz\n   - boo"
        [ ul [] [ li [] [ text "foo" ], li [] [ text "bar" ], li [] [ text "baz" ], li [] [ text "boo" ] ] ]


    , testEq 257
        [ p [] [ text "Here we need four, because the list marker is wider:" ]
        ]
        "10) foo\n    - bar"
        [ ol [ start 10 ] [ li [] [ text "foo", ul [] [ li [] [ text "bar" ] ] ] ] ]


    , testEq 258
        [ p [] [ text "Three is not enough:" ]
        ]
        "10) foo\n   - bar"
        [ ol [ start 10 ] [ li [] [ text "foo" ] ], ul [] [ li [] [ text "bar" ] ] ]


    , testEq 259
        [ p [] [ text "A list may be the first block in a list item:" ]
        ]
        "- - foo"
        [ ul [] [ li [] [ ul [] [ li [] [ text "foo" ] ] ] ] ]


    , testEq 260
        []
        "1. - 2. foo"
        [ ol [] [ li [] [ ul [] [ li [] [ ol [ start 2 ] [ li [] [ text "foo" ] ] ] ] ] ] ]


    , testEq 261
        [ p [] [ text "A list item can contain a heading:" ]
        ]
        "- # Foo\n- Bar\n  ---\n  baz"
        [ ul [] [ li [] [ h1 [] [ text "Foo" ] ], li [] [ h2 [] [ text "Bar" ], text "baz" ] ] ]


    ]

