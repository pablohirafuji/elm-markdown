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
    Regex.regex "\\b_((?:[^_]|__)+?)_\\b|\\*((?:\\*\\*|[\\s\\S])+?)\\*(?!\\*)"


hardBreakRegex : Regex
hardBreakRegex =
    Regex.regex " {2,}\\n|\\\\\\n"


codeSpaceReplaceRegex : Regex
codeSpaceReplaceRegex =
    Regex.regex " {2,}|\\n"


type Inline
    = Normal String
    | HardBreak
    | Code
    | Strong
    | Emphasis


-- Abstract Syntax Tree
type AST
    = NoMatch String
    | ASTMatch Match


type alias Match =
    { type_   : Inline
    , content : List AST
    , match   : String
    , start   : Int
    , end     : Int
    , rawText : String
    }


regexes : List ( Inline, Regex )
regexes =
    [ ( Code     , codeRegex )
    , ( Strong   , strongRegex )
    , ( Emphasis , emRegex )
    , ( HardBreak, hardBreakRegex )
    ]


findMatches : List ( Inline, Regex ) -> String -> List Match
findMatches regexes rawText =
    List.map (findMatch rawText) regexes
        |> List.concat
        |> List.sortBy .start


findMatch : String -> ( Inline, Regex ) -> List Match
findMatch rawText ( type_, regex ) =
    Regex.find Regex.All regex rawText
        |> List.map
            (\match ->
                { type_   = type_
                , content = []
                , match   = match.match
                , start   = match.index
                , end     = match.index + String.length match.match
                , rawText = matchToRawText type_ match.match
                }
            )


matchToRawText : Inline -> String -> String
matchToRawText type_ matchStr =
    case type_ of
        Strong ->
            String.slice 2 -2 matchStr

        Emphasis ->
            String.slice 1 -1 matchStr

        Code ->
            String.trim matchStr
                |> Regex.replace Regex.All codeSpaceReplaceRegex (\_ -> " ")

        _ ->
            matchStr


parseMatches : ( String, List Match, List AST ) -> List AST
parseMatches ( rawText, matches, ast ) =
    case matches of
        [] ->
            case ast of
                [] ->
                    if String.isEmpty rawText then
                        []

                    else
                        [ NoMatch rawText ]


                ASTMatch astHead :: _ ->
                    let
                        finalStr =
                            String.dropLeft astHead.end rawText

                    in
                        if String.isEmpty finalStr then
                            ast

                        else
                            NoMatch finalStr :: ast

                _ ->
                    ast


        match :: matchesTail ->
            parseMatch ( rawText, match, ast )
                |> (,,) rawText matchesTail
                |> parseMatches
   

parseMatch : ( String, Match, List AST ) -> List AST
parseMatch ( rawText, match, ast ) =
    case ast of
        [] ->
            if match.start > 0 then
                [ newASTMatch match
                , NoMatch (String.left (match.start) rawText)
                ]

            else
                [ newASTMatch match
                ]

        ASTMatch astHead :: astTail ->
            -- New Match
            if astHead.end == match.start then
                newASTMatch match
                    :: ast
            
            -- New Match and in between normal text
            else if astHead.end < match.start then
                newASTMatch match
                    :: NoMatch (String.slice astHead.end match.start rawText)
                    :: ast

            -- Inside previous Match
            else if astHead.start < match.start
                && astHead.end > match.end
                && astHead.type_ /= Code then
                    ASTMatch (addChild astHead match)
                        :: astTail

            -- Intersecting, so ignore
            else
                ast

        _ ->
            newASTMatch match
                :: ast


newASTMatch : Match -> AST
newASTMatch match =
    ASTMatch
        { match
            | content = [ NoMatch match.rawText ]
        }


addChild : Match -> Match -> Match
addChild parentMatch childMatch =
    let
        updtChildMatch =
            { childMatch
                | start = childMatch.start - parentMatch.start
                , end = childMatch.end - parentMatch.end
            }

        updtParentMatch parsedASTs =
            { parentMatch
                | content =
                    parseMatches
                        ( parentMatch.rawText
                        , [ updtChildMatch ]
                        , parsedASTs
                        )
            }

    in
        case parentMatch.content of
            NoMatch _ :: [] ->
                updtParentMatch []

            _ ->
                updtParentMatch parentMatch.content


toAST : String -> List AST
toAST rawText =
    let trimmedText = String.trim rawText
    in findMatches regexes trimmedText
            |> (\matches ->
                parseMatches ( trimmedText, matches, [] ) )
            |> reverseASTs


reverseASTs : List AST -> List AST
reverseASTs =
    List.reverse
        >> List.map reverseASTContent


reverseASTContent : AST -> AST
reverseASTContent ast =
    case ast of
        ASTMatch match ->
            ASTMatch
                { match | content = reverseASTs match.content }

        _ ->
            ast


astsToHtml : List AST -> List (Html msg)
astsToHtml =
    List.map astToHtml
        >> List.concat


astToHtml : AST -> List (Html msg)
astToHtml ast =
    case ast of
        NoMatch str ->
            [ text str ]

        ASTMatch match ->
            case match.type_ of
                Normal text_ ->
                    [ text text_ ]

                HardBreak ->
                    [ br [] [] ]

                Code ->
                    [ code [] [ text match.rawText ] ]

                Strong ->
                    [ strong [] (astsToHtml match.content) ]

                Emphasis ->
                    [ em [] (astsToHtml match.content) ]


toHtml : String -> List (Html msg)
toHtml =
    toAST
        >> astsToHtml


testSrt : String
testSrt = """
Pablo **Gustavo `Daiji`** *da __Costa* Hirafuji__ `código com  *negrito*`  
meio __negrito__ `final`.
"""

main : Html msg
main =
    div []
        [ p [] (toHtml testSrt)
        , p [] [ text (toString <| toAST testSrt ) ]
        ]

