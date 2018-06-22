module Test.Spec exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Test.Helpers exposing (..)


run : List (Output msg)
run =
    [ testEq 1
        []
        "→foo→baz→→bim\n"
        [ pre []
            [ code []
                [ text "foo→baz→→bim" ]
            ]
        ]
    , testEq 2
        []
        "  →foo→baz→→bim\n"
        [ pre []
            [ code []
                [ text "foo→baz→→bim" ]
            ]
        ]
    , testEq 3
        []
        "    a→a\n    ὐ→a\n"
        [ pre []
            [ code []
                [ text "a→aὐ→a" ]
            ]
        ]
    , testEq 4
        []
        "  - foo\n\n→bar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , p []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 5
        []
        "- foo\n\n→→bar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , pre []
                    [ code []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 6
        []
        ">→→foo\n"
        [ blockquote []
            [ pre []
                [ code []
                    [ text "foo" ]
                ]
            ]
        ]
    , testEq 7
        []
        "-→→foo\n"
        [ ul []
            [ li []
                [ pre []
                    [ code []
                        [ text "foo" ]
                    ]
                ]
            ]
        ]
    , testEq 8
        []
        "    foo\n→bar\n"
        [ pre []
            [ code []
                [ text "foobar" ]
            ]
        ]
    , testEq 9
        []
        " - foo\n   - bar\n→ - baz\n"
        [ ul []
            [ li []
                [ text "foo"
                , ul []
                    [ li []
                        [ text "bar"
                        , ul []
                            [ li []
                                [ text "baz" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 10
        []
        "#→Foo\n"
        [ h1 []
            [ text "Foo" ]
        ]
    , testEq 11
        []
        "*→*→*→\n"
        [ hr []
            []
        ]
    , testEq 12
        []
        "- `one\n- two`\n"
        [ ul []
            [ li []
                [ text "`one" ]
            , li []
                [ text "two`" ]
            ]
        ]
    , testEq 13
        []
        "***\n---\n___\n"
        [ hr []
            []
        , text ""
        , hr []
            []
        , text ""
        , hr []
            []
        , text ""
        ]
    , testEq 14
        []
        "+++\n"
        [ p []
            [ text "+++" ]
        ]
    , testEq 15
        []
        "===\n"
        [ p []
            [ text "===" ]
        ]
    , testEq 16
        []
        "--\n**\n__\n"
        [ p []
            [ text "--**__" ]
        ]
    , testEq 17
        []
        " ***\n  ***\n   ***\n"
        [ hr []
            []
        , text ""
        , hr []
            []
        , text ""
        , hr []
            []
        , text ""
        ]
    , testEq 18
        []
        "    ***\n"
        [ pre []
            [ code []
                [ text "***" ]
            ]
        ]
    , testEq 19
        []
        "Foo\n    ***\n"
        [ p []
            [ text "Foo***" ]
        ]
    , testEq 20
        []
        "_____________________________________\n"
        [ hr []
            []
        , text ""
        ]
    , testEq 21
        []
        " - - -\n"
        [ hr []
            []
        , text ""
        ]
    , testEq 22
        []
        " **  * ** * ** * **\n"
        [ hr []
            []
        , text ""
        ]
    , testEq 23
        []
        "-     -      -      -\n"
        [ hr []
            []
        , text ""
        ]
    , testEq 24
        []
        "- - - -    \n"
        [ hr []
            []
        , text ""
        ]
    , testEq 25
        []
        "_ _ _ _ a\n\na------\n\n---a---\n"
        [ p []
            [ text "_ _ _ _ a" ]
        , p []
            [ text "a------" ]
        , p []
            [ text "---a---" ]
        ]
    , testEq 26
        []
        " *-*\n"
        [ p []
            [ em []
                [ text "-" ]
            ]
        ]
    , testEq 27
        []
        "- foo\n***\n- bar\n"
        [ ul []
            [ li []
                [ text "foo" ]
            ]
        , hr []
            []
        , ul []
            [ li []
                [ text "bar" ]
            ]
        ]
    , testEq 28
        []
        "Foo\n***\nbar\n"
        [ p []
            [ text "Foo" ]
        , hr []
            []
        , p []
            [ text "bar" ]
        ]
    , testEq 29
        []
        "Foo\n---\nbar\n"
        [ h2 []
            [ text "Foo" ]
        , p []
            [ text "bar" ]
        ]
    , testEq 30
        []
        "* Foo\n* * *\n* Bar\n"
        [ ul []
            [ li []
                [ text "Foo" ]
            ]
        , hr []
            []
        , ul []
            [ li []
                [ text "Bar" ]
            ]
        ]
    , testEq 31
        []
        "- Foo\n- * * *\n"
        [ ul []
            [ li []
                [ text "Foo" ]
            , li []
                [ hr []
                    []
                , text ""
                ]
            ]
        ]
    , testEq 32
        []
        "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n"
        [ h1 []
            [ text "foo" ]
        , h2 []
            [ text "foo" ]
        , h3 []
            [ text "foo" ]
        , h4 []
            [ text "foo" ]
        , h5 []
            [ text "foo" ]
        , h6 []
            [ text "foo" ]
        ]
    , testEq 33
        []
        "####### foo\n"
        [ p []
            [ text "####### foo" ]
        ]
    , testEq 34
        []
        "#5 bolt\n\n#hashtag\n"
        [ p []
            [ text "#5 bolt" ]
        , p []
            [ text "#hashtag" ]
        ]
    , testEq 35
        []
        "\\## foo\n"
        [ p []
            [ text "## foo" ]
        ]
    , testEq 36
        []
        "# foo *bar* \\*baz\\*\n"
        [ h1 []
            [ text "foo "
            , em []
                [ text "bar" ]
            , text "*baz*"
            ]
        ]
    , testEq 37
        []
        "#                  foo                     \n"
        [ h1 []
            [ text "foo" ]
        ]
    , testEq 38
        []
        " ### foo\n  ## foo\n   # foo\n"
        [ h3 []
            [ text "foo" ]
        , h2 []
            [ text "foo" ]
        , h1 []
            [ text "foo" ]
        ]
    , testEq 39
        []
        "    # foo\n"
        [ pre []
            [ code []
                [ text "# foo" ]
            ]
        ]
    , testEq 40
        []
        "foo\n    # bar\n"
        [ p []
            [ text "foo# bar" ]
        ]
    , testEq 41
        []
        "## foo ##\n  ###   bar    ###\n"
        [ h2 []
            [ text "foo" ]
        , h3 []
            [ text "bar" ]
        ]
    , testEq 42
        []
        "# foo ##################################\n##### foo ##\n"
        [ h1 []
            [ text "foo" ]
        , h5 []
            [ text "foo" ]
        ]
    , testEq 43
        []
        "### foo ###     \n"
        [ h3 []
            [ text "foo" ]
        ]
    , testEq 44
        []
        "### foo ### b\n"
        [ h3 []
            [ text "foo ### b" ]
        ]
    , testEq 45
        []
        "# foo#\n"
        [ h1 []
            [ text "foo#" ]
        ]
    , testEq 46
        []
        "### foo \\###\n## foo #\\##\n# foo \\#\n"
        [ h3 []
            [ text "foo ###" ]
        , h2 []
            [ text "foo ###" ]
        , h1 []
            [ text "foo #" ]
        ]
    , testEq 47
        []
        "****\n## foo\n****\n"
        [ hr []
            []
        , h2 []
            [ text "foo" ]
        , hr []
            []
        , text ""
        ]
    , testEq 48
        []
        "Foo bar\n# baz\nBar foo\n"
        [ p []
            [ text "Foo bar" ]
        , h1 []
            [ text "baz" ]
        , p []
            [ text "Bar foo" ]
        ]
    , testEq 49
        []
        "## \n#\n### ###\n"
        [ h2 []
            []
        , h1 []
            []
        , h3 []
            []
        ]
    , testEq 50
        []
        "Foo *bar*\n=========\n\nFoo *bar*\n---------\n"
        [ h1 []
            [ text "Foo "
            , em []
                [ text "bar" ]
            ]
        , h2 []
            [ text "Foo "
            , em []
                [ text "bar" ]
            ]
        ]
    , testEq 51
        []
        "Foo *bar\nbaz*\n====\n"
        [ h1 []
            [ text "Foo "
            , em []
                [ text "barbaz" ]
            ]
        ]
    , testEq 52
        []
        "Foo\n-------------------------\n\nFoo\n=\n"
        [ h2 []
            [ text "Foo" ]
        , h1 []
            [ text "Foo" ]
        ]
    , testEq 53
        []
        "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===\n"
        [ h2 []
            [ text "Foo" ]
        , h2 []
            [ text "Foo" ]
        , h1 []
            [ text "Foo" ]
        ]
    , testEq 54
        []
        "    Foo\n    ---\n\n    Foo\n---\n"
        [ pre []
            [ code []
                [ text "Foo---Foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 55
        []
        "Foo\n   ----      \n"
        [ h2 []
            [ text "Foo" ]
        ]
    , testEq 56
        []
        "Foo\n    ---\n"
        [ p []
            [ text "Foo---" ]
        ]
    , testEq 57
        []
        "Foo\n= =\n\nFoo\n--- -\n"
        [ p []
            [ text "Foo= =" ]
        , p []
            [ text "Foo" ]
        , hr []
            []
        , text ""
        ]
    , testEq 58
        []
        "Foo  \n-----\n"
        [ h2 []
            [ text "Foo" ]
        ]
    , testEq 59
        []
        "Foo\\\n----\n"
        [ h2 []
            [ text "Foo\\" ]
        ]
    , testEq 60
        []
        "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n"
        [ h2 []
            [ text "`Foo" ]
        , p []
            [ text "`" ]
        , h2 []
            [ text "<a title=\"a lot" ]
        , p []
            [ text "of dashes\"/>" ]
        ]
    , testEq 61
        []
        "> Foo\n---\n"
        [ blockquote []
            [ p []
                [ text "Foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 62
        []
        "> foo\nbar\n===\n"
        [ blockquote []
            [ p []
                [ text "foobar===" ]
            ]
        ]
    , testEq 63
        []
        "- Foo\n---\n"
        [ ul []
            [ li []
                [ text "Foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 64
        []
        "Foo\nBar\n---\n"
        [ h2 []
            [ text "FooBar" ]
        ]
    , testEq 65
        []
        "---\nFoo\n---\nBar\n---\nBaz\n"
        [ hr []
            []
        , h2 []
            [ text "Foo" ]
        , h2 []
            [ text "Bar" ]
        , p []
            [ text "Baz" ]
        ]
    , testEq 66
        []
        "\n====\n"
        [ p []
            [ text "====" ]
        ]
    , testEq 67
        []
        "---\n---\n"
        [ hr []
            []
        , text ""
        , hr []
            []
        , text ""
        ]
    , testEq 68
        []
        "- foo\n-----\n"
        [ ul []
            [ li []
                [ text "foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 69
        []
        "    foo\n---\n"
        [ pre []
            [ code []
                [ text "foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 70
        []
        "> foo\n-----\n"
        [ blockquote []
            [ p []
                [ text "foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 71
        []
        "\\> foo\n------\n"
        [ h2 []
            [ text "> foo" ]
        ]
    , testEq 72
        []
        "Foo\n\nbar\n---\nbaz\n"
        [ p []
            [ text "Foo" ]
        , h2 []
            [ text "bar" ]
        , p []
            [ text "baz" ]
        ]
    , testEq 73
        []
        "Foo\nbar\n\n---\n\nbaz\n"
        [ p []
            [ text "Foobar" ]
        , hr []
            []
        , p []
            [ text "baz" ]
        ]
    , testEq 74
        []
        "Foo\nbar\n* * *\nbaz\n"
        [ p []
            [ text "Foobar" ]
        , hr []
            []
        , p []
            [ text "baz" ]
        ]
    , testEq 75
        []
        "Foo\nbar\n\\---\nbaz\n"
        [ p []
            [ text "Foobar---baz" ]
        ]
    , testEq 76
        []
        "    a simple\n      indented code block\n"
        [ pre []
            [ code []
                [ text "a simple  indented code block" ]
            ]
        ]
    , testEq 77
        []
        "  - foo\n\n    bar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , p []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 78
        []
        "1.  foo\n\n    - bar\n"
        [ ol []
            [ li []
                [ p []
                    [ text "foo" ]
                , ul []
                    [ li []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 79
        []
        "    <a/>\n    *hi*\n\n    - one\n"
        [ pre []
            [ code []
                [ text "<a/>*hi*- one" ]
            ]
        ]
    , testEq 80
        []
        "    chunk1\n\n    chunk2\n  \n \n \n    chunk3\n"
        [ pre []
            [ code []
                [ text "chunk1chunk2chunk3" ]
            ]
        ]
    , testEq 81
        []
        "    chunk1\n      \n      chunk2\n"
        [ pre []
            [ code []
                [ text "chunk1    chunk2" ]
            ]
        ]
    , testEq 82
        []
        "Foo\n    bar\n\n"
        [ p []
            [ text "Foobar" ]
        ]
    , testEq 83
        []
        "    foo\nbar\n"
        [ pre []
            [ code []
                [ text "foo" ]
            ]
        , p []
            [ text "bar" ]
        ]
    , testEq 84
        []
        "# Heading\n    foo\nHeading\n------\n    foo\n----\n"
        [ h1 []
            [ text "Heading" ]
        , pre []
            [ code []
                [ text "foo" ]
            ]
        , h2 []
            [ text "Heading" ]
        , pre []
            [ code []
                [ text "foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 85
        []
        "        foo\n    bar\n"
        [ pre []
            [ code []
                [ text "foobar" ]
            ]
        ]
    , testEq 86
        []
        "\n    \n    foo\n    \n\n"
        [ pre []
            [ code []
                [ text "foo" ]
            ]
        ]
    , testEq 87
        []
        "    foo  \n"
        [ pre []
            [ code []
                [ text "foo  " ]
            ]
        ]
    , testEq 88
        []
        "```\n<\n >\n```\n"
        [ pre []
            [ code []
                [ text "< >" ]
            ]
        ]
    , testEq 89
        []
        "~~~\n<\n >\n~~~\n"
        [ pre []
            [ code []
                [ text "< >" ]
            ]
        ]
    , testEq 90
        []
        "```\naaa\n~~~\n```\n"
        [ pre []
            [ code []
                [ text "aaa~~~" ]
            ]
        ]
    , testEq 91
        []
        "~~~\naaa\n```\n~~~\n"
        [ pre []
            [ code []
                [ text "aaa```" ]
            ]
        ]
    , testEq 92
        []
        "````\naaa\n```\n``````\n"
        [ pre []
            [ code []
                [ text "aaa```" ]
            ]
        ]
    , testEq 93
        []
        "~~~~\naaa\n~~~\n~~~~\n"
        [ pre []
            [ code []
                [ text "aaa~~~" ]
            ]
        ]
    , testEq 94
        []
        "```\n"
        [ pre []
            [ code []
                []
            ]
        ]
    , testEq 95
        []
        "`````\n\n```\naaa\n"
        [ pre []
            [ code []
                [ text "```aaa" ]
            ]
        ]
    , testEq 96
        []
        "> ```\n> aaa\n\nbbb\n"
        [ blockquote []
            [ pre []
                [ code []
                    [ text "aaa" ]
                ]
            ]
        , p []
            [ text "bbb" ]
        ]
    , testEq 97
        []
        "```\n\n  \n```\n"
        [ pre []
            [ code []
                []
            ]
        ]
    , testEq 98
        []
        "```\n```\n"
        [ pre []
            [ code []
                []
            ]
        ]
    , testEq 99
        []
        " ```\n aaa\naaa\n```\n"
        [ pre []
            [ code []
                [ text "aaaaaa" ]
            ]
        ]
    , testEq 100
        []
        "  ```\naaa\n  aaa\naaa\n  ```\n"
        [ pre []
            [ code []
                [ text "aaaaaaaaa" ]
            ]
        ]
    , testEq 101
        []
        "   ```\n   aaa\n    aaa\n  aaa\n   ```\n"
        [ pre []
            [ code []
                [ text "aaa aaaaaa" ]
            ]
        ]
    , testEq 102
        []
        "    ```\n    aaa\n    ```\n"
        [ pre []
            [ code []
                [ text "```aaa```" ]
            ]
        ]
    , testEq 103
        []
        "```\naaa\n  ```\n"
        [ pre []
            [ code []
                [ text "aaa" ]
            ]
        ]
    , testEq 104
        []
        "   ```\naaa\n  ```\n"
        [ pre []
            [ code []
                [ text "aaa" ]
            ]
        ]
    , testEq 105
        []
        "```\naaa\n    ```\n"
        [ pre []
            [ code []
                [ text "aaa    ```" ]
            ]
        ]
    , testEq 106
        []
        "``` ```\naaa\n"
        [ p []
            [ code []
                []
            , text "aaa"
            ]
        ]
    , testEq 107
        []
        "~~~~~~\naaa\n~~~ ~~\n"
        [ pre []
            [ code []
                [ text "aaa~~~ ~~" ]
            ]
        ]
    , testEq 108
        []
        "foo\n```\nbar\n```\nbaz\n"
        [ p []
            [ text "foo" ]
        , pre []
            [ code []
                [ text "bar" ]
            ]
        , p []
            [ text "baz" ]
        ]
    , testEq 109
        []
        "foo\n---\n~~~\nbar\n~~~\n# baz\n"
        [ h2 []
            [ text "foo" ]
        , pre []
            [ code []
                [ text "bar" ]
            ]
        , h1 []
            [ text "baz" ]
        ]
    , testEq 110
        []
        "```ruby\ndef foo(x)\n  return 3\nend\n```\n"
        [ pre []
            [ code [ class "language-ruby" ]
                [ text "def foo(x)  return 3end" ]
            ]
        ]
    , testEq 111
        []
        "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~\n"
        [ pre []
            [ code [ class "language-ruby" ]
                [ text "def foo(x)  return 3end" ]
            ]
        ]
    , testEq 112
        []
        "````;\n````\n"
        [ pre []
            [ code [ class "language-;" ]
                []
            ]
        ]
    , testEq 113
        []
        "``` aa ```\nfoo\n"
        [ p []
            [ code []
                [ text "aa" ]
            , text "foo"
            ]
        ]
    , testEq 114
        []
        "```\n``` aaa\n```\n"
        [ pre []
            [ code []
                [ text "``` aaa" ]
            ]
        ]
    , testEq 115
        []
        "<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n\nokay.\n"
        []
    , testEq 116
        []
        " <div>\n  *hello*\n         <foo><a>\n"
        []
    , testEq 117
        []
        "</div>\n*foo*\n"
        []
    , testEq 118
        []
        "<DIV CLASS=\"foo\">\n\n*Markdown*\n\n</DIV>\n"
        []
    , testEq 119
        []
        "<div id=\"foo\"\n  class=\"bar\">\n</div>\n"
        []
    , testEq 120
        []
        "<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n"
        []
    , testEq 121
        []
        "<div>\n*foo*\n\n*bar*\n"
        []
    , testEq 122
        []
        "<div id=\"foo\"\n*hi*\n"
        []
    , testEq 123
        []
        "<div class\nfoo\n"
        []
    , testEq 124
        []
        "<div *???-&&&-<---\n*foo*\n"
        []
    , testEq 125
        []
        "<div><a href=\"bar\">*foo*</a></div>\n"
        []
    , testEq 126
        []
        "<table><tr><td>\nfoo\n</td></tr></table>\n"
        []
    , testEq 127
        []
        "<div></div>\n``` c\nint x = 33;\n```\n"
        []
    , testEq 128
        []
        "<a href=\"foo\">\n*bar*\n</a>\n"
        []
    , testEq 129
        []
        "<Warning>\n*bar*\n</Warning>\n"
        []
    , testEq 130
        []
        "<i class=\"foo\">\n*bar*\n</i>\n"
        []
    , testEq 131
        []
        "</ins>\n*bar*\n"
        []
    , testEq 132
        []
        "<del>\n*foo*\n</del>\n"
        []
    , testEq 133
        []
        "<del>\n\n*foo*\n\n</del>\n"
        []
    , testEq 134
        []
        "<del>*foo*</del>\n"
        []
    , testEq 135
        []
        "<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\nokay\n"
        []
    , testEq 136
        []
        "<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\nokay\n"
        []
    , testEq 137
        []
        "<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\nokay\n"
        []
    , testEq 138
        []
        "<style\n  type=\"text/css\">\n\nfoo\n"
        []
    , testEq 139
        []
        "> <div>\n> foo\n\nbar\n"
        []
    , testEq 140
        []
        "- <div>\n- foo\n"
        []
    , testEq 141
        []
        "<style>p{color:red;}</style>\n*foo*\n"
        []
    , testEq 142
        []
        "<!-- foo -->*bar*\n*baz*\n"
        []
    , testEq 143
        []
        "<script>\nfoo\n</script>1. *bar*\n"
        []
    , testEq 144
        []
        "<!-- Foo\n\nbar\n   baz -->\nokay\n"
        []
    , testEq 145
        []
        "<?php\n\n  echo '>';\n\n?>\nokay\n"
        []
    , testEq 146
        []
        "<!DOCTYPE html>\n"
        []
    , testEq 147
        []
        "<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\nokay\n"
        []
    , testEq 148
        []
        "  <!-- foo -->\n\n    <!-- foo -->\n"
        []
    , testEq 149
        []
        "  <div>\n\n    <div>\n"
        []
    , testEq 150
        []
        "Foo\n<div>\nbar\n</div>\n"
        []
    , testEq 151
        []
        "<div>\nbar\n</div>\n*foo*\n"
        []
    , testEq 152
        []
        "Foo\n<a href=\"bar\">\nbaz\n"
        []
    , testEq 153
        []
        "<div>\n\n*Emphasized* text.\n\n</div>\n"
        []
    , testEq 154
        []
        "<div>\n*Emphasized* text.\n</div>\n"
        []
    , testEq 155
        []
        "<table>\n\n<tr>\n\n<td>\nHi\n</td>\n\n</tr>\n\n</table>\n"
        []
    , testEq 156
        []
        "<table>\n\n  <tr>\n\n    <td>\n      Hi\n    </td>\n\n  </tr>\n\n</table>\n"
        []
    , testEq 157
        []
        "[foo]: /url \"title\"\n\n[foo]\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 158
        []
        "   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n"
        [ p []
            [ a [ href "/url", title "the title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 159
        []
        "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n"
        [ p []
            [ a [ href "my_(url)", title "title (with parens)" ]
                [ text "Foo*bar]" ]
            ]
        ]
    , testEq 160
        []
        "[Foo bar]:\n<my%20url>\n'title'\n\n[Foo bar]\n"
        [ p []
            [ a [ href "my%20url", title "title" ]
                [ text "Foo bar" ]
            ]
        ]
    , testEq 161
        []
        "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n"
        [ p []
            [ a [ href "/url", title "\ntitle\nline1\nline2\n" ]
                [ text "foo" ]
            ]
        ]
    , testEq 162
        []
        "[foo]: /url 'title\n\nwith blank line'\n\n[foo]\n"
        [ p []
            [ text "[foo]: /url 'title" ]
        , p []
            [ text "with blank line'" ]
        , p []
            [ text "[foo]" ]
        ]
    , testEq 163
        []
        "[foo]:\n/url\n\n[foo]\n"
        [ p []
            [ a [ href "/url" ]
                [ text "foo" ]
            ]
        ]
    , testEq 164
        []
        "[foo]:\n\n[foo]\n"
        [ p []
            [ text "[foo]:" ]
        , p []
            [ text "[foo]" ]
        ]
    , testEq 165
        []
        "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n"
        [ p []
            [ a [ href "/url%5Cbar*baz", title "foo\"bar\\baz" ]
                [ text "foo" ]
            ]
        ]
    , testEq 166
        []
        "[foo]\n\n[foo]: url\n"
        [ p []
            [ a [ href "url" ]
                [ text "foo" ]
            ]
        ]
    , testEq 167
        []
        "[foo]\n\n[foo]: first\n[foo]: second\n"
        [ p []
            [ a [ href "first" ]
                [ text "foo" ]
            ]
        ]
    , testEq 168
        []
        "[FOO]: /url\n\n[Foo]\n"
        [ p []
            [ a [ href "/url" ]
                [ text "Foo" ]
            ]
        ]
    , testEq 169
        []
        "[ΑΓΩ]: /φου\n\n[αγω]\n"
        [ p []
            [ a [ href "/%CF%86%CE%BF%CF%85" ]
                [ text "αγω" ]
            ]
        ]
    , testEq 170
        []
        "[foo]: /url\n"
        []
    , testEq 171
        []
        "[\nfoo\n]: /url\nbar\n"
        [ p []
            [ text "bar" ]
        ]
    , testEq 172
        []
        "[foo]: /url \"title\" ok\n"
        [ p []
            [ text "[foo]: /url \"title\" ok" ]
        ]
    , testEq 173
        []
        "[foo]: /url\n\"title\" ok\n"
        [ p []
            [ text "\"title\" ok" ]
        ]
    , testEq 174
        []
        "    [foo]: /url \"title\"\n\n[foo]\n"
        [ pre []
            [ code []
                [ text "[foo]: /url \"title\"" ]
            ]
        , p []
            [ text "[foo]" ]
        ]
    , testEq 175
        []
        "```\n[foo]: /url\n```\n\n[foo]\n"
        [ pre []
            [ code []
                [ text "[foo]: /url" ]
            ]
        , p []
            [ text "[foo]" ]
        ]
    , testEq 176
        []
        "Foo\n[bar]: /baz\n\n[bar]\n"
        [ p []
            [ text "Foo[bar]: /baz" ]
        , p []
            [ text "[bar]" ]
        ]
    , testEq 177
        []
        "# [Foo]\n[foo]: /url\n> bar\n"
        [ h1 []
            [ a [ href "/url" ]
                [ text "Foo" ]
            ]
        , blockquote []
            [ p []
                [ text "bar" ]
            ]
        ]
    , testEq 178
        []
        "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]\n"
        [ p []
            [ a [ href "/foo-url", title "foo" ]
                [ text "foo" ]
            , text ","
            , a [ href "/bar-url", title "bar" ]
                [ text "bar" ]
            , text ","
            , a [ href "/baz-url" ]
                [ text "baz" ]
            ]
        ]
    , testEq 179
        []
        "[foo]\n\n> [foo]: /url\n"
        [ p []
            [ a [ href "/url" ]
                [ text "foo" ]
            ]
        , blockquote []
            []
        ]
    , testEq 180
        []
        "aaa\n\nbbb\n"
        [ p []
            [ text "aaa" ]
        , p []
            [ text "bbb" ]
        ]
    , testEq 181
        []
        "aaa\nbbb\n\nccc\nddd\n"
        [ p []
            [ text "aaabbb" ]
        , p []
            [ text "cccddd" ]
        ]
    , testEq 182
        []
        "aaa\n\n\nbbb\n"
        [ p []
            [ text "aaa" ]
        , p []
            [ text "bbb" ]
        ]
    , testEq 183
        []
        "  aaa\n bbb\n"
        [ p []
            [ text "aaabbb" ]
        ]
    , testEq 184
        []
        "aaa\n             bbb\n                                       ccc\n"
        [ p []
            [ text "aaabbbccc" ]
        ]
    , testEq 185
        []
        "   aaa\nbbb\n"
        [ p []
            [ text "aaabbb" ]
        ]
    , testEq 186
        []
        "    aaa\nbbb\n"
        [ pre []
            [ code []
                [ text "aaa" ]
            ]
        , p []
            [ text "bbb" ]
        ]
    , testEq 187
        []
        "aaa     \nbbb     \n"
        [ p []
            [ text "aaa"
            , br []
                []
            , text "bbb"
            ]
        ]
    , testEq 188
        []
        "  \n\naaa\n  \n\n# aaa\n\n  \n"
        [ p []
            [ text "aaa" ]
        , h1 []
            [ text "aaa" ]
        ]
    , testEq 189
        []
        "> # Foo\n> bar\n> baz\n"
        [ blockquote []
            [ h1 []
                [ text "Foo" ]
            , p []
                [ text "barbaz" ]
            ]
        ]
    , testEq 190
        []
        "># Foo\n>bar\n> baz\n"
        [ blockquote []
            [ h1 []
                [ text "Foo" ]
            , p []
                [ text "barbaz" ]
            ]
        ]
    , testEq 191
        []
        "   > # Foo\n   > bar\n > baz\n"
        [ blockquote []
            [ h1 []
                [ text "Foo" ]
            , p []
                [ text "barbaz" ]
            ]
        ]
    , testEq 192
        []
        "    > # Foo\n    > bar\n    > baz\n"
        [ pre []
            [ code []
                [ text "> # Foo> bar> baz" ]
            ]
        ]
    , testEq 193
        []
        "> # Foo\n> bar\nbaz\n"
        [ blockquote []
            [ h1 []
                [ text "Foo" ]
            , p []
                [ text "barbaz" ]
            ]
        ]
    , testEq 194
        []
        "> bar\nbaz\n> foo\n"
        [ blockquote []
            [ p []
                [ text "barbazfoo" ]
            ]
        ]
    , testEq 195
        []
        "> foo\n---\n"
        [ blockquote []
            [ p []
                [ text "foo" ]
            ]
        , hr []
            []
        , text ""
        ]
    , testEq 196
        []
        "> - foo\n- bar\n"
        [ blockquote []
            [ ul []
                [ li []
                    [ text "foo" ]
                ]
            ]
        , ul []
            [ li []
                [ text "bar" ]
            ]
        ]
    , testEq 197
        []
        ">     foo\n    bar\n"
        [ blockquote []
            [ pre []
                [ code []
                    [ text "foo" ]
                ]
            ]
        , pre []
            [ code []
                [ text "bar" ]
            ]
        ]
    , testEq 198
        []
        "> ```\nfoo\n```\n"
        [ blockquote []
            [ pre []
                [ code []
                    []
                ]
            ]
        , p []
            [ text "foo" ]
        , pre []
            [ code []
                []
            ]
        ]
    , testEq 199
        []
        "> foo\n    - bar\n"
        [ blockquote []
            [ p []
                [ text "foo- bar" ]
            ]
        ]
    , testEq 200
        []
        ">\n"
        [ blockquote []
            []
        ]
    , testEq 201
        []
        ">\n>  \n> \n"
        [ blockquote []
            []
        ]
    , testEq 202
        []
        ">\n> foo\n>  \n"
        [ blockquote []
            [ p []
                [ text "foo" ]
            ]
        ]
    , testEq 203
        []
        "> foo\n\n> bar\n"
        [ blockquote []
            [ p []
                [ text "foo" ]
            ]
        , blockquote []
            [ p []
                [ text "bar" ]
            ]
        ]
    , testEq 204
        []
        "> foo\n> bar\n"
        [ blockquote []
            [ p []
                [ text "foobar" ]
            ]
        ]
    , testEq 205
        []
        "> foo\n>\n> bar\n"
        [ blockquote []
            [ p []
                [ text "foo" ]
            , p []
                [ text "bar" ]
            ]
        ]
    , testEq 206
        []
        "foo\n> bar\n"
        [ p []
            [ text "foo" ]
        , blockquote []
            [ p []
                [ text "bar" ]
            ]
        ]
    , testEq 207
        []
        "> aaa\n***\n> bbb\n"
        [ blockquote []
            [ p []
                [ text "aaa" ]
            ]
        , hr []
            []
        , blockquote []
            [ p []
                [ text "bbb" ]
            ]
        ]
    , testEq 208
        []
        "> bar\nbaz\n"
        [ blockquote []
            [ p []
                [ text "barbaz" ]
            ]
        ]
    , testEq 209
        []
        "> bar\n\nbaz\n"
        [ blockquote []
            [ p []
                [ text "bar" ]
            ]
        , p []
            [ text "baz" ]
        ]
    , testEq 210
        []
        "> bar\n>\nbaz\n"
        [ blockquote []
            [ p []
                [ text "bar" ]
            ]
        , p []
            [ text "baz" ]
        ]
    , testEq 211
        []
        "> > > foo\nbar\n"
        [ blockquote []
            [ blockquote []
                [ blockquote []
                    [ p []
                        [ text "foobar" ]
                    ]
                ]
            ]
        ]
    , testEq 212
        []
        ">>> foo\n> bar\n>>baz\n"
        [ blockquote []
            [ blockquote []
                [ blockquote []
                    [ p []
                        [ text "foobarbaz" ]
                    ]
                ]
            ]
        ]
    , testEq 213
        []
        ">     code\n\n>    not code\n"
        [ blockquote []
            [ pre []
                [ code []
                    [ text "code" ]
                ]
            ]
        , blockquote []
            [ p []
                [ text "not code" ]
            ]
        ]
    , testEq 214
        []
        "A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote.\n"
        [ p []
            [ text "A paragraphwith two lines." ]
        , pre []
            [ code []
                [ text "indented code" ]
            ]
        , blockquote []
            [ p []
                [ text "A block quote." ]
            ]
        ]
    , testEq 215
        []
        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.\n"
        [ ol []
            [ li []
                [ p []
                    [ text "A paragraphwith two lines." ]
                , pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , blockquote []
                    [ p []
                        [ text "A block quote." ]
                    ]
                ]
            ]
        ]
    , testEq 216
        []
        "- one\n\n two\n"
        [ ul []
            [ li []
                [ text "one" ]
            ]
        , p []
            [ text "two" ]
        ]
    , testEq 217
        []
        "- one\n\n  two\n"
        [ ul []
            [ li []
                [ p []
                    [ text "one" ]
                , p []
                    [ text "two" ]
                ]
            ]
        ]
    , testEq 218
        []
        " -    one\n\n     two\n"
        [ ul []
            [ li []
                [ text "one" ]
            ]
        , pre []
            [ code []
                [ text "two" ]
            ]
        ]
    , testEq 219
        []
        " -    one\n\n      two\n"
        [ ul []
            [ li []
                [ p []
                    [ text "one" ]
                , p []
                    [ text "two" ]
                ]
            ]
        ]
    , testEq 220
        []
        "   > > 1.  one\n>>\n>>     two\n"
        [ blockquote []
            [ blockquote []
                [ ol []
                    [ li []
                        [ p []
                            [ text "one" ]
                        , p []
                            [ text "two" ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 221
        []
        ">>- one\n>>\n  >  > two\n"
        [ blockquote []
            [ blockquote []
                [ ul []
                    [ li []
                        [ text "one" ]
                    ]
                , p []
                    [ text "two" ]
                ]
            ]
        ]
    , testEq 222
        []
        "-one\n\n2.two\n"
        [ p []
            [ text "-one" ]
        , p []
            [ text "2.two" ]
        ]
    , testEq 223
        []
        "- foo\n\n\n  bar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , p []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 224
        []
        "1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam\n"
        [ ol []
            [ li []
                [ p []
                    [ text "foo" ]
                , pre []
                    [ code []
                        [ text "bar" ]
                    ]
                , p []
                    [ text "baz" ]
                , blockquote []
                    [ p []
                        [ text "bam" ]
                    ]
                ]
            ]
        ]
    , testEq 225
        []
        "- Foo\n\n      bar\n\n\n      baz\n"
        [ ul []
            [ li []
                [ p []
                    [ text "Foo" ]
                , pre []
                    [ code []
                        [ text "barbaz" ]
                    ]
                ]
            ]
        ]
    , testEq 226
        []
        "123456789. ok\n"
        [ ol [ attribute "start" "123456789" ]
            [ li []
                [ text "ok" ]
            ]
        ]
    , testEq 227
        []
        "1234567890. not ok\n"
        [ p []
            [ text "1234567890. not ok" ]
        ]
    , testEq 228
        []
        "0. ok\n"
        [ ol [ attribute "start" "0" ]
            [ li []
                [ text "ok" ]
            ]
        ]
    , testEq 229
        []
        "003. ok\n"
        [ ol [ attribute "start" "3" ]
            [ li []
                [ text "ok" ]
            ]
        ]
    , testEq 230
        []
        "-1. not ok\n"
        [ p []
            [ text "-1. not ok" ]
        ]
    , testEq 231
        []
        "- foo\n\n      bar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , pre []
                    [ code []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 232
        []
        "  10.  foo\n\n           bar\n"
        [ ol [ attribute "start" "10" ]
            [ li []
                [ p []
                    [ text "foo" ]
                , pre []
                    [ code []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 233
        []
        "    indented code\n\nparagraph\n\n    more code\n"
        [ pre []
            [ code []
                [ text "indented code" ]
            ]
        , p []
            [ text "paragraph" ]
        , pre []
            [ code []
                [ text "more code" ]
            ]
        ]
    , testEq 234
        []
        "1.     indented code\n\n   paragraph\n\n       more code\n"
        [ ol []
            [ li []
                [ pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , p []
                    [ text "paragraph" ]
                , pre []
                    [ code []
                        [ text "more code" ]
                    ]
                ]
            ]
        ]
    , testEq 235
        []
        "1.      indented code\n\n   paragraph\n\n       more code\n"
        [ ol []
            [ li []
                [ pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , p []
                    [ text "paragraph" ]
                , pre []
                    [ code []
                        [ text "more code" ]
                    ]
                ]
            ]
        ]
    , testEq 236
        []
        "   foo\n\nbar\n"
        [ p []
            [ text "foo" ]
        , p []
            [ text "bar" ]
        ]
    , testEq 237
        []
        "-    foo\n\n  bar\n"
        [ ul []
            [ li []
                [ text "foo" ]
            ]
        , p []
            [ text "bar" ]
        ]
    , testEq 238
        []
        "-  foo\n\n   bar\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , p []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 239
        []
        "-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz\n"
        [ ul []
            [ li []
                [ text "foo" ]
            , li []
                [ pre []
                    [ code []
                        [ text "bar" ]
                    ]
                ]
            , li []
                [ pre []
                    [ code []
                        [ text "baz" ]
                    ]
                ]
            ]
        ]
    , testEq 240
        []
        "-   \n  foo\n"
        [ ul []
            [ li []
                [ text "foo" ]
            ]
        ]
    , testEq 241
        []
        "-\n\n  foo\n"
        [ ul []
            [ li []
                []
            ]
        , p []
            [ text "foo" ]
        ]
    , testEq 242
        []
        "- foo\n-\n- bar\n"
        [ ul []
            [ li []
                [ text "foo" ]
            , li []
                []
            , li []
                [ text "bar" ]
            ]
        ]
    , testEq 243
        []
        "- foo\n-   \n- bar\n"
        [ ul []
            [ li []
                [ text "foo" ]
            , li []
                []
            , li []
                [ text "bar" ]
            ]
        ]
    , testEq 244
        []
        "1. foo\n2.\n3. bar\n"
        [ ol []
            [ li []
                [ text "foo" ]
            , li []
                []
            , li []
                [ text "bar" ]
            ]
        ]
    , testEq 245
        []
        "*\n"
        [ ul []
            [ li []
                []
            ]
        ]
    , testEq 246
        []
        "foo\n*\n\nfoo\n1.\n"
        [ p []
            [ text "foo*" ]
        , p []
            [ text "foo1." ]
        ]
    , testEq 247
        []
        " 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote.\n"
        [ ol []
            [ li []
                [ p []
                    [ text "A paragraphwith two lines." ]
                , pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , blockquote []
                    [ p []
                        [ text "A block quote." ]
                    ]
                ]
            ]
        ]
    , testEq 248
        []
        "  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote.\n"
        [ ol []
            [ li []
                [ p []
                    [ text "A paragraphwith two lines." ]
                , pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , blockquote []
                    [ p []
                        [ text "A block quote." ]
                    ]
                ]
            ]
        ]
    , testEq 249
        []
        "   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote.\n"
        [ ol []
            [ li []
                [ p []
                    [ text "A paragraphwith two lines." ]
                , pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , blockquote []
                    [ p []
                        [ text "A block quote." ]
                    ]
                ]
            ]
        ]
    , testEq 250
        []
        "    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote.\n"
        [ pre []
            [ code []
                [ text "1.  A paragraph    with two lines.        indented code    > A block quote." ]
            ]
        ]
    , testEq 251
        []
        "  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote.\n"
        [ ol []
            [ li []
                [ p []
                    [ text "A paragraphwith two lines." ]
                , pre []
                    [ code []
                        [ text "indented code" ]
                    ]
                , blockquote []
                    [ p []
                        [ text "A block quote." ]
                    ]
                ]
            ]
        ]
    , testEq 252
        []
        "  1.  A paragraph\n    with two lines.\n"
        [ ol []
            [ li []
                [ text "A paragraphwith two lines." ]
            ]
        ]
    , testEq 253
        []
        "> 1. > Blockquote\ncontinued here.\n"
        [ blockquote []
            [ ol []
                [ li []
                    [ blockquote []
                        [ p []
                            [ text "Blockquotecontinued here." ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 254
        []
        "> 1. > Blockquote\n> continued here.\n"
        [ blockquote []
            [ ol []
                [ li []
                    [ blockquote []
                        [ p []
                            [ text "Blockquotecontinued here." ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 255
        []
        "- foo\n  - bar\n    - baz\n      - boo\n"
        [ ul []
            [ li []
                [ text "foo"
                , ul []
                    [ li []
                        [ text "bar"
                        , ul []
                            [ li []
                                [ text "baz"
                                , ul []
                                    [ li []
                                        [ text "boo" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 256
        []
        "- foo\n - bar\n  - baz\n   - boo\n"
        [ ul []
            [ li []
                [ text "foo" ]
            , li []
                [ text "bar" ]
            , li []
                [ text "baz" ]
            , li []
                [ text "boo" ]
            ]
        ]
    , testEq 257
        []
        "10) foo\n    - bar\n"
        [ ol [ attribute "start" "10" ]
            [ li []
                [ text "foo"
                , ul []
                    [ li []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 258
        []
        "10) foo\n   - bar\n"
        [ ol [ attribute "start" "10" ]
            [ li []
                [ text "foo" ]
            ]
        , ul []
            [ li []
                [ text "bar" ]
            ]
        ]
    , testEq 259
        []
        "- - foo\n"
        [ ul []
            [ li []
                [ ul []
                    [ li []
                        [ text "foo" ]
                    ]
                ]
            ]
        ]
    , testEq 260
        []
        "1. - 2. foo\n"
        [ ol []
            [ li []
                [ ul []
                    [ li []
                        [ ol [ attribute "start" "2" ]
                            [ li []
                                [ text "foo" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 261
        []
        "- # Foo\n- Bar\n  ---\n  baz\n"
        [ ul []
            [ li []
                [ h1 []
                    [ text "Foo" ]
                ]
            , li []
                [ h2 []
                    [ text "Bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 262
        []
        "- foo\n- bar\n+ baz\n"
        [ ul []
            [ li []
                [ text "foo" ]
            , li []
                [ text "bar" ]
            ]
        , ul []
            [ li []
                [ text "baz" ]
            ]
        ]
    , testEq 263
        []
        "1. foo\n2. bar\n3) baz\n"
        [ ol []
            [ li []
                [ text "foo" ]
            , li []
                [ text "bar" ]
            ]
        , ol [ attribute "start" "3" ]
            [ li []
                [ text "baz" ]
            ]
        ]
    , testEq 264
        []
        "Foo\n- bar\n- baz\n"
        [ p []
            [ text "Foo" ]
        , ul []
            [ li []
                [ text "bar" ]
            , li []
                [ text "baz" ]
            ]
        ]
    , testEq 265
        []
        "The number of windows in my house is\n14.  The number of doors is 6.\n"
        [ p []
            [ text "The number of windows in my house is14.  The number of doors is 6." ]
        ]
    , testEq 266
        []
        "The number of windows in my house is\n1.  The number of doors is 6.\n"
        [ p []
            [ text "The number of windows in my house is" ]
        , ol []
            [ li []
                [ text "The number of doors is 6." ]
            ]
        ]
    , testEq 267
        []
        "- foo\n\n- bar\n\n\n- baz\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                ]
            , li []
                [ p []
                    [ text "bar" ]
                ]
            , li []
                [ p []
                    [ text "baz" ]
                ]
            ]
        ]
    , testEq 268
        []
        "- foo\n  - bar\n    - baz\n\n\n      bim\n"
        [ ul []
            [ li []
                [ text "foo"
                , ul []
                    [ li []
                        [ text "bar"
                        , ul []
                            [ li []
                                [ p []
                                    [ text "baz" ]
                                , p []
                                    [ text "bim" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , testEq 269
        []
        "- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim\n"
        [ ul []
            [ li []
                [ text "foo" ]
            , li []
                [ text "bar" ]
            ]
        , ul []
            [ li []
                [ text "baz" ]
            , li []
                [ text "bim" ]
            ]
        ]
    , testEq 270
        []
        "-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , p []
                    [ text "notcode" ]
                ]
            , li []
                [ p []
                    [ text "foo" ]
                ]
            ]
        , pre []
            [ code []
                [ text "code" ]
            ]
        ]
    , testEq 271
        []
        "- a\n - b\n  - c\n   - d\n    - e\n   - f\n  - g\n - h\n- i\n"
        [ ul []
            [ li []
                [ text "a" ]
            , li []
                [ text "b" ]
            , li []
                [ text "c" ]
            , li []
                [ text "d" ]
            , li []
                [ text "e" ]
            , li []
                [ text "f" ]
            , li []
                [ text "g" ]
            , li []
                [ text "h" ]
            , li []
                [ text "i" ]
            ]
        ]
    , testEq 272
        []
        "1. a\n\n  2. b\n\n    3. c\n"
        [ ol []
            [ li []
                [ p []
                    [ text "a" ]
                ]
            , li []
                [ p []
                    [ text "b" ]
                ]
            , li []
                [ p []
                    [ text "c" ]
                ]
            ]
        ]
    , testEq 273
        []
        "- a\n- b\n\n- c\n"
        [ ul []
            [ li []
                [ p []
                    [ text "a" ]
                ]
            , li []
                [ p []
                    [ text "b" ]
                ]
            , li []
                [ p []
                    [ text "c" ]
                ]
            ]
        ]
    , testEq 274
        []
        "* a\n*\n\n* c\n"
        [ ul []
            [ li []
                [ p []
                    [ text "a" ]
                ]
            , li []
                []
            , li []
                [ p []
                    [ text "c" ]
                ]
            ]
        ]
    , testEq 275
        []
        "- a\n- b\n\n  c\n- d\n"
        [ ul []
            [ li []
                [ p []
                    [ text "a" ]
                ]
            , li []
                [ p []
                    [ text "b" ]
                , p []
                    [ text "c" ]
                ]
            , li []
                [ p []
                    [ text "d" ]
                ]
            ]
        ]
    , testEq 276
        []
        "- a\n- b\n\n  [ref]: /url\n- d\n"
        [ ul []
            [ li []
                [ p []
                    [ text "a" ]
                ]
            , li []
                [ p []
                    [ text "b" ]
                ]
            , li []
                [ p []
                    [ text "d" ]
                ]
            ]
        ]
    , testEq 277
        []
        "- a\n- ```\n  b\n\n\n  ```\n- c\n"
        [ ul []
            [ li []
                [ text "a" ]
            , li []
                [ pre []
                    [ code []
                        [ text "b" ]
                    ]
                ]
            , li []
                [ text "c" ]
            ]
        ]
    , testEq 278
        []
        "- a\n  - b\n\n    c\n- d\n"
        [ ul []
            [ li []
                [ text "a"
                , ul []
                    [ li []
                        [ p []
                            [ text "b" ]
                        , p []
                            [ text "c" ]
                        ]
                    ]
                ]
            , li []
                [ text "d" ]
            ]
        ]
    , testEq 279
        []
        "* a\n  > b\n  >\n* c\n"
        [ ul []
            [ li []
                [ text "a"
                , blockquote []
                    [ p []
                        [ text "b" ]
                    ]
                ]
            , li []
                [ text "c" ]
            ]
        ]
    , testEq 280
        []
        "- a\n  > b\n  ```\n  c\n  ```\n- d\n"
        [ ul []
            [ li []
                [ text "a"
                , blockquote []
                    [ p []
                        [ text "b" ]
                    ]
                , pre []
                    [ code []
                        [ text "c" ]
                    ]
                ]
            , li []
                [ text "d" ]
            ]
        ]
    , testEq 281
        []
        "- a\n"
        [ ul []
            [ li []
                [ text "a" ]
            ]
        ]
    , testEq 282
        []
        "- a\n  - b\n"
        [ ul []
            [ li []
                [ text "a"
                , ul []
                    [ li []
                        [ text "b" ]
                    ]
                ]
            ]
        ]
    , testEq 283
        []
        "1. ```\n   foo\n   ```\n\n   bar\n"
        [ ol []
            [ li []
                [ pre []
                    [ code []
                        [ text "foo" ]
                    ]
                , p []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 284
        []
        "* foo\n  * bar\n\n  baz\n"
        [ ul []
            [ li []
                [ p []
                    [ text "foo" ]
                , ul []
                    [ li []
                        [ text "bar" ]
                    ]
                , p []
                    [ text "baz" ]
                ]
            ]
        ]
    , testEq 285
        []
        "- a\n  - b\n  - c\n\n- d\n  - e\n  - f\n"
        [ ul []
            [ li []
                [ p []
                    [ text "a" ]
                , ul []
                    [ li []
                        [ text "b" ]
                    , li []
                        [ text "c" ]
                    ]
                ]
            , li []
                [ p []
                    [ text "d" ]
                , ul []
                    [ li []
                        [ text "e" ]
                    , li []
                        [ text "f" ]
                    ]
                ]
            ]
        ]
    , testEq 286
        []
        "`hi`lo`\n"
        [ p []
            [ code []
                [ text "hi" ]
            , text "lo`"
            ]
        ]
    , testEq 287
        []
        "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n"
        [ p []
            [ text "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" ]
        ]
    , testEq 288
        []
        "\\→\\A\\a\\ \\3\\φ\\«\n"
        [ p []
            [ text "\\→\\A\\a\\ \\3\\φ\\«" ]
        ]
    , testEq 289
        []
        "\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n"
        [ p []
            [ text "*not emphasized*<br/> not a tag[not a link](/foo)`not code`1. not a list* not a list# not a heading[foo]: /url \"not a reference\"" ]
        ]
    , testEq 290
        []
        "\\\\*emphasis*\n"
        [ p []
            [ text "\\"
            , em []
                [ text "emphasis" ]
            ]
        ]
    , testEq 291
        []
        "foo\\\nbar\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "bar"
            ]
        ]
    , testEq 292
        []
        "`` \\[\\` ``\n"
        [ p []
            [ code []
                [ text "\\[\\`" ]
            ]
        ]
    , testEq 293
        []
        "    \\[\\]\n"
        [ pre []
            [ code []
                [ text "\\[\\]" ]
            ]
        ]
    , testEq 294
        []
        "~~~\n\\[\\]\n~~~\n"
        [ pre []
            [ code []
                [ text "\\[\\]" ]
            ]
        ]
    , testEq 295
        []
        "<http://example.com?find=\\*>\n"
        [ p []
            [ a [ href "http://example.com?find=%5C*" ]
                [ text "http://example.com?find=\\*" ]
            ]
        ]
    , testEq 296
        []
        "<a href=\"/bar\\/)\">\n"
        []
    , testEq 297
        []
        "[foo](/bar\\* \"ti\\*tle\")\n"
        [ p []
            [ a [ href "/bar*", title "ti*tle" ]
                [ text "foo" ]
            ]
        ]
    , testEq 298
        []
        "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"\n"
        [ p []
            [ a [ href "/bar*", title "ti*tle" ]
                [ text "foo" ]
            ]
        ]
    , testEq 299
        []
        "``` foo\\+bar\nfoo\n```\n"
        [ pre []
            [ code [ class "language-foo+bar" ]
                [ text "foo" ]
            ]
        ]
    , testEq 300
        []
        "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n"
        [ p []
            [ text "& © Æ Ď¾ ℋ ⅆ∲ ≧̸" ]
        ]
    , testEq 301
        []
        "&#35; &#1234; &#992; &#98765432; &#0;\n"
        [ p []
            [ text "# Ӓ Ϡ � �" ]
        ]
    , testEq 302
        []
        "&#X22; &#XD06; &#xcab;\n"
        [ p []
            [ text "\" ആ ಫ" ]
        ]
    , testEq 303
        []
        "&nbsp &x; &#; &#x;\n&ThisIsNotDefined; &hi?;\n"
        [ p []
            [ text "&nbsp &x; &#; &#x;&ThisIsNotDefined; &hi?;" ]
        ]
    , testEq 304
        []
        "&copy\n"
        [ p []
            [ text "&copy" ]
        ]
    , testEq 305
        []
        "&MadeUpEntity;\n"
        [ p []
            [ text "&MadeUpEntity;" ]
        ]
    , testEq 306
        []
        "<a href=\"&ouml;&ouml;.html\">\n"
        []
    , testEq 307
        []
        "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n"
        [ p []
            [ a [ href "/f%C3%B6%C3%B6", title "föö" ]
                [ text "foo" ]
            ]
        ]
    , testEq 308
        []
        "[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n"
        [ p []
            [ a [ href "/f%C3%B6%C3%B6", title "föö" ]
                [ text "foo" ]
            ]
        ]
    , testEq 309
        []
        "``` f&ouml;&ouml;\nfoo\n```\n"
        [ pre []
            [ code [ class "language-föö" ]
                [ text "foo" ]
            ]
        ]
    , testEq 310
        []
        "`f&ouml;&ouml;`\n"
        [ p []
            [ code []
                [ text "f&ouml;&ouml;" ]
            ]
        ]
    , testEq 311
        []
        "    f&ouml;f&ouml;\n"
        [ pre []
            [ code []
                [ text "f&ouml;f&ouml;" ]
            ]
        ]
    , testEq 312
        []
        "`foo`\n"
        [ p []
            [ code []
                [ text "foo" ]
            ]
        ]
    , testEq 313
        []
        "`` foo ` bar  ``\n"
        [ p []
            [ code []
                [ text "foo ` bar" ]
            ]
        ]
    , testEq 314
        []
        "` `` `\n"
        [ p []
            [ code []
                [ text "``" ]
            ]
        ]
    , testEq 315
        []
        "``\nfoo\n``\n"
        [ p []
            [ code []
                [ text "foo" ]
            ]
        ]
    , testEq 316
        []
        "`foo   bar\n  baz`\n"
        [ p []
            [ code []
                [ text "foo bar baz" ]
            ]
        ]
    , testEq 317
        []
        "`a  b`\n"
        [ p []
            [ code []
                [ text "a  b" ]
            ]
        ]
    , testEq 318
        []
        "`foo `` bar`\n"
        [ p []
            [ code []
                [ text "foo `` bar" ]
            ]
        ]
    , testEq 319
        []
        "`foo\\`bar`\n"
        [ p []
            [ code []
                [ text "foo\\" ]
            , text "bar`"
            ]
        ]
    , testEq 320
        []
        "*foo`*`\n"
        [ p []
            [ text "*foo"
            , code []
                [ text "*" ]
            ]
        ]
    , testEq 321
        []
        "[not a `link](/foo`)\n"
        [ p []
            [ text "[not a "
            , code []
                [ text "link](/foo" ]
            , text ")"
            ]
        ]
    , testEq 322
        []
        "`<a href=\"`\">`\n"
        [ p []
            [ code []
                [ text "<a href=\"" ]
            , text "\">`"
            ]
        ]
    , testEq 323
        []
        "<a href=\"`\">`\n"
        []
    , testEq 324
        []
        "`<http://foo.bar.`baz>`\n"
        [ p []
            [ code []
                [ text "<http://foo.bar." ]
            , text "baz>`"
            ]
        ]
    , testEq 325
        []
        "<http://foo.bar.`baz>`\n"
        [ p []
            [ a [ href "http://foo.bar.%60baz" ]
                [ text "http://foo.bar.`baz" ]
            , text "`"
            ]
        ]
    , testEq 326
        []
        "```foo``\n"
        [ p []
            [ text "```foo``" ]
        ]
    , testEq 327
        []
        "`foo\n"
        [ p []
            [ text "`foo" ]
        ]
    , testEq 328
        []
        "*foo bar*\n"
        [ p []
            [ em []
                [ text "foo bar" ]
            ]
        ]
    , testEq 329
        []
        "a * foo bar*\n"
        [ p []
            [ text "a * foo bar*" ]
        ]
    , testEq 330
        []
        "a*\"foo\"*\n"
        [ p []
            [ text "a*\"foo\"*" ]
        ]
    , testEq 331
        []
        "* a *\n"
        [ p []
            [ text "* a *" ]
        ]
    , testEq 332
        []
        "foo*bar*\n"
        [ p []
            [ text "foo"
            , em []
                [ text "bar" ]
            ]
        ]
    , testEq 333
        []
        "5*6*78\n"
        [ p []
            [ text "5"
            , em []
                [ text "6" ]
            , text "78"
            ]
        ]
    , testEq 334
        []
        "_foo bar_\n"
        [ p []
            [ em []
                [ text "foo bar" ]
            ]
        ]
    , testEq 335
        []
        "_ foo bar_\n"
        [ p []
            [ text "_ foo bar_" ]
        ]
    , testEq 336
        []
        "a_\"foo\"_\n"
        [ p []
            [ text "a_\"foo\"_" ]
        ]
    , testEq 337
        []
        "foo_bar_\n"
        [ p []
            [ text "foo_bar_" ]
        ]
    , testEq 338
        []
        "5_6_78\n"
        [ p []
            [ text "5_6_78" ]
        ]
    , testEq 339
        []
        "пристаням_стремятся_\n"
        [ p []
            [ text "пристаням_стремятся_" ]
        ]
    , testEq 340
        []
        "aa_\"bb\"_cc\n"
        [ p []
            [ text "aa_\"bb\"_cc" ]
        ]
    , testEq 341
        []
        "foo-_(bar)_\n"
        [ p []
            [ text "foo-"
            , em []
                [ text "(bar)" ]
            ]
        ]
    , testEq 342
        []
        "_foo*\n"
        [ p []
            [ text "_foo*" ]
        ]
    , testEq 343
        []
        "*foo bar *\n"
        [ p []
            [ text "*foo bar *" ]
        ]
    , testEq 344
        []
        "*foo bar\n*\n"
        [ p []
            [ text "*foo bar*" ]
        ]
    , testEq 345
        []
        "*(*foo)\n"
        [ p []
            [ text "*(*foo)" ]
        ]
    , testEq 346
        []
        "*(*foo*)*\n"
        [ p []
            [ em []
                [ text "("
                , em []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]
    , testEq 347
        []
        "*foo*bar\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "bar"
            ]
        ]
    , testEq 348
        []
        "_foo bar _\n"
        [ p []
            [ text "_foo bar _" ]
        ]
    , testEq 349
        []
        "_(_foo)\n"
        [ p []
            [ text "_(_foo)" ]
        ]
    , testEq 350
        []
        "_(_foo_)_\n"
        [ p []
            [ em []
                [ text "("
                , em []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]
    , testEq 351
        []
        "_foo_bar\n"
        [ p []
            [ text "_foo_bar" ]
        ]
    , testEq 352
        []
        "_пристаням_стремятся\n"
        [ p []
            [ text "_пристаням_стремятся" ]
        ]
    , testEq 353
        []
        "_foo_bar_baz_\n"
        [ p []
            [ em []
                [ text "foo_bar_baz" ]
            ]
        ]
    , testEq 354
        []
        "_(bar)_.\n"
        [ p []
            [ em []
                [ text "(bar)" ]
            , text "."
            ]
        ]
    , testEq 355
        []
        "**foo bar**\n"
        [ p []
            [ strong []
                [ text "foo bar" ]
            ]
        ]
    , testEq 356
        []
        "** foo bar**\n"
        [ p []
            [ text "** foo bar**" ]
        ]
    , testEq 357
        []
        "a**\"foo\"**\n"
        [ p []
            [ text "a**\"foo\"**" ]
        ]
    , testEq 358
        []
        "foo**bar**\n"
        [ p []
            [ text "foo"
            , strong []
                [ text "bar" ]
            ]
        ]
    , testEq 359
        []
        "__foo bar__\n"
        [ p []
            [ strong []
                [ text "foo bar" ]
            ]
        ]
    , testEq 360
        []
        "__ foo bar__\n"
        [ p []
            [ text "__ foo bar__" ]
        ]
    , testEq 361
        []
        "__\nfoo bar__\n"
        [ p []
            [ text "__foo bar__" ]
        ]
    , testEq 362
        []
        "a__\"foo\"__\n"
        [ p []
            [ text "a__\"foo\"__" ]
        ]
    , testEq 363
        []
        "foo__bar__\n"
        [ p []
            [ text "foo__bar__" ]
        ]
    , testEq 364
        []
        "5__6__78\n"
        [ p []
            [ text "5__6__78" ]
        ]
    , testEq 365
        []
        "пристаням__стремятся__\n"
        [ p []
            [ text "пристаням__стремятся__" ]
        ]
    , testEq 366
        []
        "__foo, __bar__, baz__\n"
        [ p []
            [ strong []
                [ text "foo, "
                , strong []
                    [ text "bar" ]
                , text ", baz"
                ]
            ]
        ]
    , testEq 367
        []
        "foo-__(bar)__\n"
        [ p []
            [ text "foo-"
            , strong []
                [ text "(bar)" ]
            ]
        ]
    , testEq 368
        []
        "**foo bar **\n"
        [ p []
            [ text "**foo bar **" ]
        ]
    , testEq 369
        []
        "**(**foo)\n"
        [ p []
            [ text "**(**foo)" ]
        ]
    , testEq 370
        []
        "*(**foo**)*\n"
        [ p []
            [ em []
                [ text "("
                , strong []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]
    , testEq 371
        []
        "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**\n"
        [ p []
            [ strong []
                [ text "Gomphocarpus ("
                , em []
                    [ text "Gomphocarpus physocarpus" ]
                , text ", syn."
                , em []
                    [ text "Asclepias physocarpa" ]
                , text ")"
                ]
            ]
        ]
    , testEq 372
        []
        "**foo \"*bar*\" foo**\n"
        [ p []
            [ strong []
                [ text "foo \""
                , em []
                    [ text "bar" ]
                , text "\" foo"
                ]
            ]
        ]
    , testEq 373
        []
        "**foo**bar\n"
        [ p []
            [ strong []
                [ text "foo" ]
            , text "bar"
            ]
        ]
    , testEq 374
        []
        "__foo bar __\n"
        [ p []
            [ text "__foo bar __" ]
        ]
    , testEq 375
        []
        "__(__foo)\n"
        [ p []
            [ text "__(__foo)" ]
        ]
    , testEq 376
        []
        "_(__foo__)_\n"
        [ p []
            [ em []
                [ text "("
                , strong []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]
    , testEq 377
        []
        "__foo__bar\n"
        [ p []
            [ text "__foo__bar" ]
        ]
    , testEq 378
        []
        "__пристаням__стремятся\n"
        [ p []
            [ text "__пристаням__стремятся" ]
        ]
    , testEq 379
        []
        "__foo__bar__baz__\n"
        [ p []
            [ strong []
                [ text "foo__bar__baz" ]
            ]
        ]
    , testEq 380
        []
        "__(bar)__.\n"
        [ p []
            [ strong []
                [ text "(bar)" ]
            , text "."
            ]
        ]
    , testEq 381
        []
        "*foo [bar](/url)*\n"
        [ p []
            [ em []
                [ text "foo "
                , a [ href "/url" ]
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 382
        []
        "*foo\nbar*\n"
        [ p []
            [ em []
                [ text "foobar" ]
            ]
        ]
    , testEq 383
        []
        "_foo __bar__ baz_\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 384
        []
        "_foo _bar_ baz_\n"
        [ p []
            [ em []
                [ text "foo "
                , em []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 385
        []
        "__foo_ bar_\n"
        [ p []
            [ em []
                [ em []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]
    , testEq 386
        []
        "*foo *bar**\n"
        [ p []
            [ em []
                [ text "foo "
                , em []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 387
        []
        "*foo **bar** baz*\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 388
        []
        "*foo**bar**baz*\n"
        [ p []
            [ em []
                [ text "foo"
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 389
        []
        "***foo** bar*\n"
        [ p []
            [ em []
                [ strong []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]
    , testEq 390
        []
        "*foo **bar***\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 391
        []
        "*foo**bar***\n"
        [ p []
            [ em []
                [ text "foo"
                , strong []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 392
        []
        "*foo **bar *baz* bim** bop*\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar "
                    , em []
                        [ text "baz" ]
                    , text "bim"
                    ]
                , text "bop"
                ]
            ]
        ]
    , testEq 393
        []
        "*foo [*bar*](/url)*\n"
        [ p []
            [ em []
                [ text "foo "
                , a [ href "/url" ]
                    [ em []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 394
        []
        "** is not an empty emphasis\n"
        [ p []
            [ text "** is not an empty emphasis" ]
        ]
    , testEq 395
        []
        "**** is not an empty strong emphasis\n"
        [ p []
            [ text "**** is not an empty strong emphasis" ]
        ]
    , testEq 396
        []
        "**foo [bar](/url)**\n"
        [ p []
            [ strong []
                [ text "foo "
                , a [ href "/url" ]
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 397
        []
        "**foo\nbar**\n"
        [ p []
            [ strong []
                [ text "foobar" ]
            ]
        ]
    , testEq 398
        []
        "__foo _bar_ baz__\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 399
        []
        "__foo __bar__ baz__\n"
        [ p []
            [ strong []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 400
        []
        "____foo__ bar__\n"
        [ p []
            [ strong []
                [ strong []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]
    , testEq 401
        []
        "**foo **bar****\n"
        [ p []
            [ strong []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 402
        []
        "**foo *bar* baz**\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 403
        []
        "**foo*bar*baz**\n"
        [ p []
            [ strong []
                [ text "foo"
                , em []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]
    , testEq 404
        []
        "***foo* bar**\n"
        [ p []
            [ strong []
                [ em []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]
    , testEq 405
        []
        "**foo *bar***\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar" ]
                ]
            ]
        ]
    , testEq 406
        []
        "**foo *bar **baz**\nbim* bop**\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar "
                    , strong []
                        [ text "baz" ]
                    , text "bim"
                    ]
                , text "bop"
                ]
            ]
        ]
    , testEq 407
        []
        "**foo [*bar*](/url)**\n"
        [ p []
            [ strong []
                [ text "foo "
                , a [ href "/url" ]
                    [ em []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]
    , testEq 408
        []
        "__ is not an empty emphasis\n"
        [ p []
            [ text "__ is not an empty emphasis" ]
        ]
    , testEq 409
        []
        "____ is not an empty strong emphasis\n"
        [ p []
            [ text "____ is not an empty strong emphasis" ]
        ]
    , testEq 410
        []
        "foo ***\n"
        [ p []
            [ text "foo ***" ]
        ]
    , testEq 411
        []
        "foo *\\**\n"
        [ p []
            [ text "foo "
            , em []
                [ text "*" ]
            ]
        ]
    , testEq 412
        []
        "foo *_*\n"
        [ p []
            [ text "foo "
            , em []
                [ text "_" ]
            ]
        ]
    , testEq 413
        []
        "foo *****\n"
        [ p []
            [ text "foo *****" ]
        ]
    , testEq 414
        []
        "foo **\\***\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "*" ]
            ]
        ]
    , testEq 415
        []
        "foo **_**\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "_" ]
            ]
        ]
    , testEq 416
        []
        "**foo*\n"
        [ p []
            [ text "*"
            , em []
                [ text "foo" ]
            ]
        ]
    , testEq 417
        []
        "*foo**\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "*"
            ]
        ]
    , testEq 418
        []
        "***foo**\n"
        [ p []
            [ text "*"
            , strong []
                [ text "foo" ]
            ]
        ]
    , testEq 419
        []
        "****foo*\n"
        [ p []
            [ text "***"
            , em []
                [ text "foo" ]
            ]
        ]
    , testEq 420
        []
        "**foo***\n"
        [ p []
            [ strong []
                [ text "foo" ]
            , text "*"
            ]
        ]
    , testEq 421
        []
        "*foo****\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "***"
            ]
        ]
    , testEq 422
        []
        "foo ___\n"
        [ p []
            [ text "foo ___" ]
        ]
    , testEq 423
        []
        "foo _\\__\n"
        [ p []
            [ text "foo "
            , em []
                [ text "_" ]
            ]
        ]
    , testEq 424
        []
        "foo _*_\n"
        [ p []
            [ text "foo "
            , em []
                [ text "*" ]
            ]
        ]
    , testEq 425
        []
        "foo _____\n"
        [ p []
            [ text "foo _____" ]
        ]
    , testEq 426
        []
        "foo __\\___\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "_" ]
            ]
        ]
    , testEq 427
        []
        "foo __*__\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "*" ]
            ]
        ]
    , testEq 428
        []
        "__foo_\n"
        [ p []
            [ text "_"
            , em []
                [ text "foo" ]
            ]
        ]
    , testEq 429
        []
        "_foo__\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "_"
            ]
        ]
    , testEq 430
        []
        "___foo__\n"
        [ p []
            [ text "_"
            , strong []
                [ text "foo" ]
            ]
        ]
    , testEq 431
        []
        "____foo_\n"
        [ p []
            [ text "___"
            , em []
                [ text "foo" ]
            ]
        ]
    , testEq 432
        []
        "__foo___\n"
        [ p []
            [ strong []
                [ text "foo" ]
            , text "_"
            ]
        ]
    , testEq 433
        []
        "_foo____\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "___"
            ]
        ]
    , testEq 434
        []
        "**foo**\n"
        [ p []
            [ strong []
                [ text "foo" ]
            ]
        ]
    , testEq 435
        []
        "*_foo_*\n"
        [ p []
            [ em []
                [ em []
                    [ text "foo" ]
                ]
            ]
        ]
    , testEq 436
        []
        "__foo__\n"
        [ p []
            [ strong []
                [ text "foo" ]
            ]
        ]
    , testEq 437
        []
        "_*foo*_\n"
        [ p []
            [ em []
                [ em []
                    [ text "foo" ]
                ]
            ]
        ]
    , testEq 438
        []
        "****foo****\n"
        [ p []
            [ strong []
                [ strong []
                    [ text "foo" ]
                ]
            ]
        ]
    , testEq 439
        []
        "____foo____\n"
        [ p []
            [ strong []
                [ strong []
                    [ text "foo" ]
                ]
            ]
        ]
    , testEq 440
        []
        "******foo******\n"
        [ p []
            [ strong []
                [ strong []
                    [ strong []
                        [ text "foo" ]
                    ]
                ]
            ]
        ]
    , testEq 441
        []
        "***foo***\n"
        [ p []
            [ strong []
                [ em []
                    [ text "foo" ]
                ]
            ]
        ]
    , testEq 442
        []
        "_____foo_____\n"
        [ p []
            [ strong []
                [ strong []
                    [ em []
                        [ text "foo" ]
                    ]
                ]
            ]
        ]
    , testEq 443
        []
        "*foo _bar* baz_\n"
        [ p []
            [ em []
                [ text "foo _bar" ]
            , text "baz_"
            ]
        ]
    , testEq 444
        []
        "*foo __bar *baz bim__ bam*\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar *baz bim" ]
                , text "bam"
                ]
            ]
        ]
    , testEq 445
        []
        "**foo **bar baz**\n"
        [ p []
            [ text "**foo "
            , strong []
                [ text "bar baz" ]
            ]
        ]
    , testEq 446
        []
        "*foo *bar baz*\n"
        [ p []
            [ text "*foo "
            , em []
                [ text "bar baz" ]
            ]
        ]
    , testEq 447
        []
        "*[bar*](/url)\n"
        [ p []
            [ text "*"
            , a [ href "/url" ]
                [ text "bar*" ]
            ]
        ]
    , testEq 448
        []
        "_foo [bar_](/url)\n"
        [ p []
            [ text "_foo "
            , a [ href "/url" ]
                [ text "bar_" ]
            ]
        ]
    , testEq 449
        []
        "*<img src=\"foo\" title=\"*\"/>\n"
        [ p []
            [ text "*"
            , img [ src "foo", title "*" ]
                []
            ]
        ]
    , testEq 450
        []
        "**<a href=\"**\">\n"
        []
    , testEq 451
        []
        "__<a href=\"__\">\n"
        []
    , testEq 452
        []
        "*a `*`*\n"
        [ p []
            [ em []
                [ text "a "
                , code []
                    [ text "*" ]
                ]
            ]
        ]
    , testEq 453
        []
        "_a `_`_\n"
        [ p []
            [ em []
                [ text "a "
                , code []
                    [ text "_" ]
                ]
            ]
        ]
    , testEq 454
        []
        "**a<http://foo.bar/?q=**>\n"
        [ p []
            [ text "**a"
            , a [ href "http://foo.bar/?q=**" ]
                [ text "http://foo.bar/?q=**" ]
            ]
        ]
    , testEq 455
        []
        "__a<http://foo.bar/?q=__>\n"
        [ p []
            [ text "__a"
            , a [ href "http://foo.bar/?q=__" ]
                [ text "http://foo.bar/?q=__" ]
            ]
        ]
    , testEq 456
        []
        "[link](/uri \"title\")\n"
        [ p []
            [ a [ href "/uri", title "title" ]
                [ text "link" ]
            ]
        ]
    , testEq 457
        []
        "[link](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link" ]
            ]
        ]
    , testEq 458
        []
        "[link]()\n"
        [ p []
            [ a [ href "" ]
                [ text "link" ]
            ]
        ]
    , testEq 459
        []
        "[link](<>)\n"
        [ p []
            [ a [ href "" ]
                [ text "link" ]
            ]
        ]
    , testEq 460
        []
        "[link](/my uri)\n"
        [ p []
            [ text "[link](/my uri)" ]
        ]
    , testEq 461
        []
        "[link](</my uri>)\n"
        [ p []
            [ text "[link](</my uri>)" ]
        ]
    , testEq 462
        []
        "[link](foo\nbar)\n"
        [ p []
            [ text "[link](foobar)" ]
        ]
    , testEq 463
        []
        "[link](<foo\nbar>)\n"
        []
    , testEq 464
        []
        "[link](\\(foo\\))\n"
        [ p []
            [ a [ href "(foo)" ]
                [ text "link" ]
            ]
        ]
    , testEq 465
        []
        "[link](foo(and(bar)))\n"
        [ p []
            [ a [ href "foo(and(bar))" ]
                [ text "link" ]
            ]
        ]
    , testEq 466
        []
        "[link](foo\\(and\\(bar\\))\n"
        [ p []
            [ a [ href "foo(and(bar)" ]
                [ text "link" ]
            ]
        ]
    , testEq 467
        []
        "[link](<foo(and(bar)>)\n"
        [ p []
            [ a [ href "foo(and(bar)" ]
                [ text "link" ]
            ]
        ]
    , testEq 468
        []
        "[link](foo\\)\\:)\n"
        [ p []
            [ a [ href "foo):" ]
                [ text "link" ]
            ]
        ]
    , testEq 469
        []
        "[link](#fragment)\n\n[link](http://example.com#fragment)\n\n[link](http://example.com?foo=3#frag)\n"
        [ p []
            [ a [ href "#fragment" ]
                [ text "link" ]
            ]
        , p []
            [ a [ href "http://example.com#fragment" ]
                [ text "link" ]
            ]
        , p []
            [ a [ href "http://example.com?foo=3#frag" ]
                [ text "link" ]
            ]
        ]
    , testEq 470
        []
        "[link](foo\\bar)\n"
        [ p []
            [ a [ href "foo%5Cbar" ]
                [ text "link" ]
            ]
        ]
    , testEq 471
        []
        "[link](foo%20b&auml;)\n"
        [ p []
            [ a [ href "foo%20b%C3%A4" ]
                [ text "link" ]
            ]
        ]
    , testEq 472
        []
        "[link](\"title\")\n"
        [ p []
            [ a [ href "%22title%22" ]
                [ text "link" ]
            ]
        ]
    , testEq 473
        []
        "[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "link" ]
            , a [ href "/url", title "title" ]
                [ text "link" ]
            , a [ href "/url", title "title" ]
                [ text "link" ]
            ]
        ]
    , testEq 474
        []
        "[link](/url \"title \\\"&quot;\")\n"
        [ p []
            [ a [ href "/url", title "title \"\"" ]
                [ text "link" ]
            ]
        ]
    , testEq 475
        []
        "[link](/url \"title\")\n"
        [ p []
            [ a [ href "/url%C2%A0%22title%22" ]
                [ text "link" ]
            ]
        ]
    , testEq 476
        []
        "[link](/url \"title \"and\" title\")\n"
        [ p []
            [ text "[link](/url \"title \"and\" title\")" ]
        ]
    , testEq 477
        []
        "[link](/url 'title \"and\" title')\n"
        [ p []
            [ a [ href "/url", title "title \"and\" title" ]
                [ text "link" ]
            ]
        ]
    , testEq 478
        []
        "[link](   /uri\n  \"title\"  )\n"
        [ p []
            [ a [ href "/uri", title "title" ]
                [ text "link" ]
            ]
        ]
    , testEq 479
        []
        "[link] (/uri)\n"
        [ p []
            [ text "[link] (/uri)" ]
        ]
    , testEq 480
        []
        "[link [foo [bar]]](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [foo [bar]]" ]
            ]
        ]
    , testEq 481
        []
        "[link] bar](/uri)\n"
        [ p []
            [ text "[link] bar](/uri)" ]
        ]
    , testEq 482
        []
        "[link [bar](/uri)\n"
        [ p []
            [ text "[link "
            , a [ href "/uri" ]
                [ text "bar" ]
            ]
        ]
    , testEq 483
        []
        "[link \\[bar](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [bar" ]
            ]
        ]
    , testEq 484
        []
        "[link *foo **bar** `#`*](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link "
                , em []
                    [ text "foo "
                    , strong []
                        [ text "bar" ]
                    , code []
                        [ text "#" ]
                    ]
                ]
            ]
        ]
    , testEq 485
        []
        "[![moon](moon.jpg)](/uri)\n"
        [ p []
            [ a [ href "/uri" ]
                [ img [ alt "moon", src "moon.jpg" ]
                    []
                ]
            ]
        ]
    , testEq 486
        []
        "[foo [bar](/uri)](/uri)\n"
        [ p []
            [ text "[foo "
            , a [ href "/uri" ]
                [ text "bar" ]
            , text "](/uri)"
            ]
        ]
    , testEq 487
        []
        "[foo *[bar [baz](/uri)](/uri)*](/uri)\n"
        [ p []
            [ text "[foo "
            , em []
                [ text "[bar "
                , a [ href "/uri" ]
                    [ text "baz" ]
                , text "](/uri)"
                ]
            , text "](/uri)"
            ]
        ]
    , testEq 488
        []
        "![[[foo](uri1)](uri2)](uri3)\n"
        [ p []
            [ img [ alt "[foo](uri2)", src "uri3" ]
                []
            ]
        ]
    , testEq 489
        []
        "*[foo*](/uri)\n"
        [ p []
            [ text "*"
            , a [ href "/uri" ]
                [ text "foo*" ]
            ]
        ]
    , testEq 490
        []
        "[foo *bar](baz*)\n"
        [ p []
            [ a [ href "baz*" ]
                [ text "foo *bar" ]
            ]
        ]
    , testEq 491
        []
        "*foo [bar* baz]\n"
        [ p []
            [ em []
                [ text "foo [bar" ]
            , text "baz]"
            ]
        ]
    , testEq 492
        []
        "[foo <bar attr=\"](baz)\">\n"
        []
    , testEq 493
        []
        "[foo`](/uri)`\n"
        [ p []
            [ text "[foo"
            , code []
                [ text "](/uri)" ]
            ]
        ]
    , testEq 494
        []
        "[foo<http://example.com/?search=](uri)>\n"
        [ p []
            [ text "[foo"
            , a [ href "http://example.com/?search=%5D(uri)" ]
                [ text "http://example.com/?search=](uri)" ]
            ]
        ]
    , testEq 495
        []
        "[foo][bar]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 496
        []
        "[link [foo [bar]]][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [foo [bar]]" ]
            ]
        ]
    , testEq 497
        []
        "[link \\[bar][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link [bar" ]
            ]
        ]
    , testEq 498
        []
        "[link *foo **bar** `#`*][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "link "
                , em []
                    [ text "foo "
                    , strong []
                        [ text "bar" ]
                    , code []
                        [ text "#" ]
                    ]
                ]
            ]
        ]
    , testEq 499
        []
        "[![moon](moon.jpg)][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ img [ alt "moon", src "moon.jpg" ]
                    []
                ]
            ]
        ]
    , testEq 500
        []
        "[foo [bar](/uri)][ref]\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo "
            , a [ href "/uri" ]
                [ text "bar" ]
            , text "]"
            , a [ href "/uri" ]
                [ text "ref" ]
            ]
        ]
    , testEq 501
        []
        "[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo "
            , em []
                [ text "bar "
                , a [ href "/uri" ]
                    [ text "baz" ]
                ]
            , text "]"
            , a [ href "/uri" ]
                [ text "ref" ]
            ]
        ]
    , testEq 502
        []
        "*[foo*][ref]\n\n[ref]: /uri\n"
        [ p []
            [ text "*"
            , a [ href "/uri" ]
                [ text "foo*" ]
            ]
        ]
    , testEq 503
        []
        "[foo *bar][ref]\n\n[ref]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "foo *bar" ]
            ]
        ]
    , testEq 504
        []
        "[foo <bar attr=\"][ref]\">\n\n[ref]: /uri\n"
        []
    , testEq 505
        []
        "[foo`][ref]`\n\n[ref]: /uri\n"
        []
    , testEq 506
        []
        "[foo<http://example.com/?search=][ref]>\n\n[ref]: /uri\n"
        [ p []
            [ text "[foo"
            , a [ href "http://example.com/?search=%5D%5Bref%5D" ]
                [ text "http://example.com/?search=][ref]" ]
            ]
        ]
    , testEq 507
        []
        "[foo][BaR]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 508
        []
        "[Толпой][Толпой] is a Russian word.\n\n[ТОЛПОЙ]: /url\n"
        [ p []
            [ a [ href "/url" ]
                [ text "Толпой" ]
            , text "is a Russian word."
            ]
        ]
    , testEq 509
        []
        "[Foo\n  bar]: /url\n\n[Baz][Foo bar]\n"
        [ p []
            [ a [ href "/url" ]
                [ text "Baz" ]
            ]
        ]
    , testEq 510
        []
        "[foo] [bar]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ text "[foo] "
            , a [ href "/url", title "title" ]
                [ text "bar" ]
            ]
        ]
    , testEq 511
        []
        "[foo]\n[bar]\n\n[bar]: /url \"title\"\n"
        [ p []
            [ text "[foo]"
            , a [ href "/url", title "title" ]
                [ text "bar" ]
            ]
        ]
    , testEq 512
        []
        "[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]\n"
        [ p []
            [ a [ href "/url1" ]
                [ text "bar" ]
            ]
        ]
    , testEq 513
        []
        "[bar][foo\\!]\n\n[foo!]: /url\n"
        [ p []
            [ text "[bar][foo!]" ]
        ]
    , testEq 514
        []
        "[foo][ref[]\n\n[ref[]: /uri\n"
        [ p []
            [ text "[foo][ref[]" ]
        , p []
            [ text "[ref[]: /uri" ]
        ]
    , testEq 515
        []
        "[foo][ref[bar]]\n\n[ref[bar]]: /uri\n"
        [ p []
            [ text "[foo][ref[bar]]" ]
        , p []
            [ text "[ref[bar]]: /uri" ]
        ]
    , testEq 516
        []
        "[[[foo]]]\n\n[[[foo]]]: /url\n"
        [ p []
            [ text "[[[foo]]]" ]
        , p []
            [ text "[[[foo]]]: /url" ]
        ]
    , testEq 517
        []
        "[foo][ref\\[]\n\n[ref\\[]: /uri\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "foo" ]
            ]
        ]
    , testEq 518
        []
        "[bar\\\\]: /uri\n\n[bar\\\\]\n"
        [ p []
            [ a [ href "/uri" ]
                [ text "bar\\" ]
            ]
        ]
    , testEq 519
        []
        "[]\n\n[]: /uri\n"
        [ p []
            [ text "[]" ]
        , p []
            [ text "[]: /uri" ]
        ]
    , testEq 520
        []
        "[\n ]\n\n[\n ]: /uri\n"
        [ p []
            [ text "[]" ]
        , p []
            [ text "[]: /uri" ]
        ]
    , testEq 521
        []
        "[foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 522
        []
        "[*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ em []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]
    , testEq 523
        []
        "[Foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "Foo" ]
            ]
        ]
    , testEq 524
        []
        "[foo] \n[]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            , text "[]"
            ]
        ]
    , testEq 525
        []
        "[foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 526
        []
        "[*foo* bar]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ em []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]
    , testEq 527
        []
        "[[*foo* bar]]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ text "["
            , a [ href "/url", title "title" ]
                [ em []
                    [ text "foo" ]
                , text "bar"
                ]
            , text "]"
            ]
        ]
    , testEq 528
        []
        "[[bar [foo]\n\n[foo]: /url\n"
        [ p []
            [ text "[[bar "
            , a [ href "/url" ]
                [ text "foo" ]
            ]
        ]
    , testEq 529
        []
        "[Foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ a [ href "/url", title "title" ]
                [ text "Foo" ]
            ]
        ]
    , testEq 530
        []
        "[foo] bar\n\n[foo]: /url\n"
        [ p []
            [ a [ href "/url" ]
                [ text "foo" ]
            , text "bar"
            ]
        ]
    , testEq 531
        []
        "\\[foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ text "[foo]" ]
        ]
    , testEq 532
        []
        "[foo*]: /url\n\n*[foo*]\n"
        [ p []
            [ text "*"
            , a [ href "/url" ]
                [ text "foo*" ]
            ]
        ]
    , testEq 533
        []
        "[foo][bar]\n\n[foo]: /url1\n[bar]: /url2\n"
        [ p []
            [ a [ href "/url2" ]
                [ text "foo" ]
            ]
        ]
    , testEq 534
        []
        "[foo][]\n\n[foo]: /url1\n"
        [ p []
            [ a [ href "/url1" ]
                [ text "foo" ]
            ]
        ]
    , testEq 535
        []
        "[foo]()\n\n[foo]: /url1\n"
        [ p []
            [ a [ href "" ]
                [ text "foo" ]
            ]
        ]
    , testEq 536
        []
        "[foo](not a link)\n\n[foo]: /url1\n"
        [ p []
            [ a [ href "/url1" ]
                [ text "foo" ]
            , text "(not a link)"
            ]
        ]
    , testEq 537
        []
        "[foo][bar][baz]\n\n[baz]: /url\n"
        [ p []
            [ text "[foo]"
            , a [ href "/url" ]
                [ text "bar" ]
            ]
        ]
    , testEq 538
        []
        "[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2\n"
        [ p []
            [ a [ href "/url2" ]
                [ text "foo" ]
            , a [ href "/url1" ]
                [ text "baz" ]
            ]
        ]
    , testEq 539
        []
        "[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2\n"
        [ p []
            [ text "[foo]"
            , a [ href "/url1" ]
                [ text "bar" ]
            ]
        ]
    , testEq 540
        []
        "![foo](/url \"title\")\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 541
        []
        "![foo *bar*]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n"
        [ p []
            [ img [ alt "foo bar", src "train.jpg", title "train & tracks" ]
                []
            ]
        ]
    , testEq 542
        []
        "![foo ![bar](/url)](/url2)\n"
        [ p []
            [ img [ alt "foo bar", src "/url2" ]
                []
            ]
        ]
    , testEq 543
        []
        "![foo [bar](/url)](/url2)\n"
        [ p []
            [ img [ alt "foo bar", src "/url2" ]
                []
            ]
        ]
    , testEq 544
        []
        "![foo *bar*][]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n"
        [ p []
            [ img [ alt "foo bar", src "train.jpg", title "train & tracks" ]
                []
            ]
        ]
    , testEq 545
        []
        "![foo *bar*][foobar]\n\n[FOOBAR]: train.jpg \"train & tracks\"\n"
        [ p []
            [ img [ alt "foo bar", src "train.jpg", title "train & tracks" ]
                []
            ]
        ]
    , testEq 546
        []
        "![foo](train.jpg)\n"
        [ p []
            [ img [ alt "foo", src "train.jpg" ]
                []
            ]
        ]
    , testEq 547
        []
        "My ![foo bar](/path/to/train.jpg  \"title\"   )\n"
        [ p []
            [ text "My "
            , img [ alt "foo bar", src "/path/to/train.jpg", title "title" ]
                []
            ]
        ]
    , testEq 548
        []
        "![foo](<url>)\n"
        [ p []
            [ img [ alt "foo", src "url" ]
                []
            ]
        ]
    , testEq 549
        []
        "![](/url)\n"
        [ p []
            [ img [ alt "", src "/url" ]
                []
            ]
        ]
    , testEq 550
        []
        "![foo][bar]\n\n[bar]: /url\n"
        [ p []
            [ img [ alt "foo", src "/url" ]
                []
            ]
        ]
    , testEq 551
        []
        "![foo][bar]\n\n[BAR]: /url\n"
        [ p []
            [ img [ alt "foo", src "/url" ]
                []
            ]
        ]
    , testEq 552
        []
        "![foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 553
        []
        "![*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo bar", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 554
        []
        "![Foo][]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "Foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 555
        []
        "![foo] \n[]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            , text "[]"
            ]
        ]
    , testEq 556
        []
        "![foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 557
        []
        "![*foo* bar]\n\n[*foo* bar]: /url \"title\"\n"
        [ p []
            [ img [ alt "foo bar", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 558
        []
        "![[foo]]\n\n[[foo]]: /url \"title\"\n"
        [ p []
            [ text "![[foo]]" ]
        , p []
            [ text "[[foo]]: /url \"title\"" ]
        ]
    , testEq 559
        []
        "![Foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ img [ alt "Foo", src "/url", title "title" ]
                []
            ]
        ]
    , testEq 560
        []
        "\\!\\[foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ text "![foo]" ]
        ]
    , testEq 561
        []
        "\\![foo]\n\n[foo]: /url \"title\"\n"
        [ p []
            [ text "!"
            , a [ href "/url", title "title" ]
                [ text "foo" ]
            ]
        ]
    , testEq 562
        []
        "<http://foo.bar.baz>\n"
        [ p []
            [ a [ href "http://foo.bar.baz" ]
                [ text "http://foo.bar.baz" ]
            ]
        ]
    , testEq 563
        []
        "<http://foo.bar.baz/test?q=hello&id=22&boolean>\n"
        [ p []
            [ a [ href "http://foo.bar.baz/test?q=hello&id=22&boolean" ]
                [ text "http://foo.bar.baz/test?q=hello&id=22&boolean" ]
            ]
        ]
    , testEq 564
        []
        "<irc://foo.bar:2233/baz>\n"
        [ p []
            [ a [ href "irc://foo.bar:2233/baz" ]
                [ text "irc://foo.bar:2233/baz" ]
            ]
        ]
    , testEq 565
        []
        "<MAILTO:FOO@BAR.BAZ>\n"
        [ p []
            [ a [ href "MAILTO:FOO@BAR.BAZ" ]
                [ text "MAILTO:FOO@BAR.BAZ" ]
            ]
        ]
    , testEq 566
        []
        "<a+b+c:d>\n"
        [ p []
            [ a [ href "a+b+c:d" ]
                [ text "a+b+c:d" ]
            ]
        ]
    , testEq 567
        []
        "<made-up-scheme://foo,bar>\n"
        [ p []
            [ a [ href "made-up-scheme://foo,bar" ]
                [ text "made-up-scheme://foo,bar" ]
            ]
        ]
    , testEq 568
        []
        "<http://../>\n"
        [ p []
            [ a [ href "http://../" ]
                [ text "http://../" ]
            ]
        ]
    , testEq 569
        []
        "<localhost:5001/foo>\n"
        [ p []
            [ a [ href "localhost:5001/foo" ]
                [ text "localhost:5001/foo" ]
            ]
        ]
    , testEq 570
        []
        "<http://foo.bar/baz bim>\n"
        [ p []
            [ text "<http://foo.bar/baz bim>" ]
        ]
    , testEq 571
        []
        "<http://example.com/\\[\\>\n"
        [ p []
            [ a [ href "http://example.com/%5C%5B%5C" ]
                [ text "http://example.com/\\[\\" ]
            ]
        ]
    , testEq 572
        []
        "<foo@bar.example.com>\n"
        [ p []
            [ a [ href "mailto:foo@bar.example.com" ]
                [ text "foo@bar.example.com" ]
            ]
        ]
    , testEq 573
        []
        "<foo+special@Bar.baz-bar0.com>\n"
        [ p []
            [ a [ href "mailto:foo+special@Bar.baz-bar0.com" ]
                [ text "foo+special@Bar.baz-bar0.com" ]
            ]
        ]
    , testEq 574
        []
        "<foo\\+@bar.example.com>\n"
        [ p []
            [ text "<foo+@bar.example.com>" ]
        ]
    , testEq 575
        []
        "<>\n"
        [ p []
            [ text "<>" ]
        ]
    , testEq 576
        []
        "< http://foo.bar >\n"
        [ p []
            [ text "< http://foo.bar >" ]
        ]
    , testEq 577
        []
        "<m:abc>\n"
        [ p []
            [ text "<m:abc>" ]
        ]
    , testEq 578
        []
        "<foo.bar.baz>\n"
        [ p []
            [ text "<foo.bar.baz>" ]
        ]
    , testEq 579
        []
        "http://example.com\n"
        [ p []
            [ text "http://example.com" ]
        ]
    , testEq 580
        []
        "foo@bar.example.com\n"
        [ p []
            [ text "foo@bar.example.com" ]
        ]
    , testEq 581
        []
        "<a><bab><c2c>\n"
        []
    , testEq 582
        []
        "<a/><b2/>\n"
        []
    , testEq 583
        []
        "<a  /><b2\ndata=\"foo\" >\n"
        []
    , testEq 584
        []
        "<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n"
        []
    , testEq 585
        []
        "Foo <responsive-image src=\"foo.jpg\" />\n"
        [ p []
            [ text "Foo "
            , node "responsive-image"
                [ src "foo.jpg" ]
                []
            ]
        ]
    , testEq 586
        []
        "<33> <__>\n"
        [ p []
            [ text "<33> <__>" ]
        ]
    , testEq 587
        []
        "<a h*#ref=\"hi\">\n"
        [ p []
            [ text "<a h*#ref=\"hi\">" ]
        ]
    , testEq 588
        []
        "<a href=\"hi'> <a href=hi'>\n"
        [ p []
            [ text "<a href=\"hi'> <a href=hi'>" ]
        ]
    , testEq 589
        []
        "< a><\nfoo><bar/ >\n"
        [ p []
            [ text "< a><foo><bar/ >" ]
        ]
    , testEq 590
        []
        "<a href='bar'title=title>\n"
        [ p []
            [ text "<a href='bar'title=title>" ]
        ]
    , testEq 591
        []
        "</a></foo >\n"
        []
    , testEq 592
        []
        "</a href=\"foo\">\n"
        [ p []
            [ text "</a href=\"foo\">" ]
        ]
    , testEq 593
        []
        "foo <!-- this is a\ncomment - with hyphen -->\n"
        [ p []
            [ text "foo " ]
        ]
    , testEq 594
        []
        "foo <!-- not a comment -- two hyphens -->\n"
        [ p []
            [ text "foo <!-- not a comment -- two hyphens -->" ]
        ]
    , testEq 595
        []
        "foo <!--> foo -->\n\nfoo <!-- foo--->\n"
        [ p []
            [ text "foo <!--> foo -->" ]
        , p []
            [ text "foo <!-- foo--->" ]
        ]
    , testEq 596
        []
        "foo <?php echo $a; ?>\n"
        []
    , testEq 597
        []
        "foo <!ELEMENT br EMPTY>\n"
        []
    , testEq 598
        []
        "foo <![CDATA[>&<]]>\n"
        []
    , testEq 599
        []
        "foo <a href=\"&ouml;\">\n"
        []
    , testEq 600
        []
        "foo <a href=\"\\*\">\n"
        []
    , testEq 601
        []
        "<a href=\"\\\"\">\n"
        [ p []
            [ text "<a href=\"\"\">" ]
        ]
    , testEq 602
        []
        "foo  \nbaz\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "baz"
            ]
        ]
    , testEq 603
        []
        "foo\\\nbaz\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "baz"
            ]
        ]
    , testEq 604
        []
        "foo       \nbaz\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "baz"
            ]
        ]
    , testEq 605
        []
        "foo  \n     bar\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "bar"
            ]
        ]
    , testEq 606
        []
        "foo\\\n     bar\n"
        [ p []
            [ text "foo"
            , br []
                []
            , text "bar"
            ]
        ]
    , testEq 607
        []
        "*foo  \nbar*\n"
        [ p []
            [ em []
                [ text "foo"
                , br []
                    []
                , text "bar"
                ]
            ]
        ]
    , testEq 608
        []
        "*foo\\\nbar*\n"
        [ p []
            [ em []
                [ text "foo"
                , br []
                    []
                , text "bar"
                ]
            ]
        ]
    , testEq 609
        []
        "`code  \nspan`\n"
        [ p []
            [ code []
                [ text "code span" ]
            ]
        ]
    , testEq 610
        []
        "`code\\\nspan`\n"
        [ p []
            [ code []
                [ text "code\\ span" ]
            ]
        ]
    , testEq 611
        []
        "<a href=\"foo  \nbar\">\n"
        []
    , testEq 612
        []
        "<a href=\"foo\\\nbar\">\n"
        []
    , testEq 613
        []
        "foo\\\n"
        [ p []
            [ text "foo\\" ]
        ]
    , testEq 614
        []
        "foo  \n"
        [ p []
            [ text "foo" ]
        ]
    , testEq 615
        []
        "### foo\\\n"
        [ h3 []
            [ text "foo\\" ]
        ]
    , testEq 616
        []
        "### foo  \n"
        [ h3 []
            [ text "foo" ]
        ]
    , testEq 617
        []
        "foo\nbaz\n"
        [ p []
            [ text "foo\nbaz" ]
        ]
    , testEq 618
        []
        "foo \n baz\n"
        [ p []
            [ text "foo\nbaz" ]
        ]
    , testEq 619
        []
        "hello $.;'there\n"
        [ p []
            [ text "hello $.;'there" ]
        ]
    , testEq 620
        []
        "Foo χρῆν\n"
        [ p []
            [ text "Foo χρῆν" ]
        ]
    , testEq 621
        []
        "Multiple     spaces\n"
        [ p []
            [ text "Multiple     spaces" ]
        ]
    ]
