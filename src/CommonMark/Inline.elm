module CommonMark.Inline exposing (..)



import Html exposing (..)
import Regex exposing (Regex)


codeRegex : Regex
codeRegex =
    Regex.regex "(`+)\\s*([\\s\\S]*?[^`])\\s*\\1(?!`)" 


strongRegex : Regex
strongRegex =
    Regex.regex "__([\\s\\S]+?)__(?!_)|\\*\\*([\\s\\S]+?)\\*\\*(?!\\*)"


emRegex : Regex
emRegex =
    Regex.regex "\\b_((?:[^_]|__)+?)_\b|\\*((?:\\*\\*|[\\s\\S])+?)\\*(?!\\*)"


type Inline
    = Normal String
    | HardBreakLine
    | Code
    | Strong
    | Emphasis


type InlineMatch =
    InlineMatch
        { type_ : Inline
        , content : List InlineMatch
        , match : Regex.Match
        }


regexes : List ( Inline, Regex )
regexes =
    [ ( Code, codeRegex )
    , ( Strong, strongRegex )
    , ( Emphasis, emRegex )
    ]


findMatches : List ( Inline, Regex ) -> String -> List InlineMatch
findMatches regexes rawText =
    List.map (findMatch rawText) regexes
        |> List.concat
        |> List.sortBy
            (\(InlineMatch match) ->
                match.match.index
            )


findMatch : String -> ( Inline, Regex ) -> List InlineMatch
findMatch rawText ( type_, regex ) =
    Regex.find Regex.All regex rawText
        |> List.map
            (\match ->
                InlineMatch
                    { type_ = type_
                    , content = []
                    , match = match
                    }
            )


parseMatches : ( String, List InlineMatch, List InlineMatch ) -> List InlineMatch
parseMatches ( rawText, inlineMatches, ast ) =
    case inlineMatches of
        [] ->
            ast

        inlineMatch :: inlineMatchesTail ->
            parseMatch ( rawText, inlineMatch, ast )
                |> (,,) rawText inlineMatchesTail
                |> parseMatches
   

parseMatch : ( String, InlineMatch, List InlineMatch ) -> List InlineMatch
parseMatch ( rawText, InlineMatch inlineMatch, ast ) =
    case ast of
        [] ->
            if inlineMatch.match.index > 0 then
                [ InlineMatch
                    { inlineMatch
                        | content = [ normalInlineMatch inlineMatch.match.match ]
                    }
                , normalInlineMatch (String.left inlineMatch.match.index rawText)
                ]

            else
                [ newInlineMatch (InlineMatch inlineMatch)
                ]

        InlineMatch astHead :: astTail ->
            -- New InlineMatch
            if astHead.match.index + String.length astHead.match.match
                <= inlineMatch.match.index then
                    newInlineMatch (InlineMatch inlineMatch) :: ast

            -- Inside previous InlineMatch
            else if astHead.match.index < inlineMatch.match.index
                && astHead.match.index + String.length astHead.match.match
                    > inlineMatch.match.index + String.length inlineMatch.match.match then
                        ast

            -- Intersecting, so ignore
            else
                ast


newInlineMatch : InlineMatch -> InlineMatch
newInlineMatch (InlineMatch inlineMatch) =
    InlineMatch
        { inlineMatch
            | content = [ normalInlineMatch inlineMatch.match.match ]
        }


normalInlineMatch : String -> InlineMatch
normalInlineMatch str =
    InlineMatch
        { type_ = Normal str
        , content = []
        , match = { match = "", submatches = [], index = 0, number = 1 }
        }


toAST : String -> List InlineMatch
toAST rawText =
    findMatches regexes rawText
        |> (\inlineMatches ->
            parseMatches ( rawText, inlineMatches, [] ) )
        |> reverseASTs


reverseASTs : List InlineMatch -> List InlineMatch
reverseASTs =
    List.reverse
        >> List.map reverseContainedASTs


reverseContainedASTs : InlineMatch -> InlineMatch
reverseContainedASTs (InlineMatch inlineMatch) =
    InlineMatch
        { inlineMatch
            | content = reverseASTs inlineMatch.content
        }


inlineMatchesToHtml : List InlineMatch -> List (Html msg)
inlineMatchesToHtml =
    List.map inlineMatchToHtml
        >> List.concat


inlineMatchToHtml : InlineMatch -> List (Html msg)
inlineMatchToHtml (InlineMatch inlineMatch) =
    case inlineMatch.type_ of
        Normal text_ ->
            [ text text_ ]

        HardBreakLine ->
            [ br [] [] ]

        Code ->
            [ code [] (inlineMatchesToHtml inlineMatch.content) ]

        Strong ->
            [ strong [] (inlineMatchesToHtml inlineMatch.content) ]

        Emphasis ->
            [ em [] (inlineMatchesToHtml inlineMatch.content) ]


toHtml : String -> List (Html msg)
toHtml =
    toAST
        >> inlineMatchesToHtml


testSrt : String
testSrt = """
jhgjhg *Olá* heh*e __como \nestá* você__ `dfdfdf`
    jklhkjhh
    `dfdfdfdf` __fdgdfgdfg__
"""

main : Html msg
main =
    p [] (toHtml testSrt)
