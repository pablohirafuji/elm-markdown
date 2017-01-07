module CommonMark.Inline exposing (..)



import Html exposing (..)
import Html.Attributes exposing (href, title)
--import Http exposing (encodeUri)
import Regex exposing (Regex)



hardBreakRegex : Regex
hardBreakRegex =
    Regex.regex " {2,}\\n|\\\\\\n"


type Inline
    = HardBreak
    | Code
    | Emphasis Int -- Length
    | Link ( String, Maybe String) -- ( Url, Maybe Title )


-- Abstract Syntax Tree
type AST
    = NoMatch String
    | ASTMatch Match


type alias Match =
    { type_   : Inline
    , content : List AST
    , start   : Int
    , end     : Int
    , rawText : String
    , text    : String
    }


regexes : List ( Inline, Regex )
regexes =
    [ ( HardBreak, hardBreakRegex )
    ]


findMatches : List ( Inline, Regex ) -> String -> List Match
findMatches regexes rawText =
    List.map (findMatch rawText) regexes
        |> List.concat


findMatch : String -> ( Inline, Regex ) -> List Match
findMatch rawText ( type_, regex ) =
    Regex.find Regex.All regex rawText
        |> List.map (baseMatch type_)


baseMatch : Inline -> Regex.Match -> Match
baseMatch type_ regexMatch =
    { type_   = type_
    , content = []
    , start   = regexMatch.index
    , end     = regexMatch.index + String.length regexMatch.match
    , rawText = regexMatch.match
    , text    = regexMatch.match
    }


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
                && astHead.end > match.end then
                    ASTMatch (addChild astHead match)
                        :: astTail

            -- Overlaping previous Match
            else
                ast

        _ ->
            newASTMatch match
                :: ast


newASTMatch : Match -> AST
newASTMatch match =
    ASTMatch
        { match
            | content = [ NoMatch match.text ]
        }


addChild : Match -> Match -> Match
addChild parentMatch childMatch =
    let
        reduction =
            case parentMatch.type_ of
                Emphasis length ->
                    parentMatch.start + length

                _ ->
                    parentMatch.start


        updtChildMatch =
            { childMatch
                | start = childMatch.start - reduction
                , end = childMatch.end - reduction
            }

        updtParentMatch parsedASTs =
            { parentMatch
                | content =
                    parseMatches
                        ( parentMatch.text
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
    let
        trimmedText = String.trim rawText
    
    in
        findMatches regexes trimmedText
            |> \matches ->
                    .matches (lexer (initLexerModel trimmedText))
                        ++ matches
            |> List.sortBy .start
            |> \matches -> parseMatches ( trimmedText, matches, [] )
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
                HardBreak ->
                    [ br [] [] ]

                Code ->
                    [ code [] [ text match.text ] ]

                Emphasis length ->
                    case length of
                        1 -> [ em [] (astsToHtml match.content) ]
                        2 -> [ strong [] (astsToHtml match.content) ]
                        _ -> [ strong [] [ em [] (astsToHtml match.content) ] ]

                Link ( url, maybeTitle ) ->
                    case maybeTitle of
                        Just title_ ->
                            [ a [ href url, title title_ ]
                                [ text match.text ]
                            ]

                        Nothing ->
                            [ a [ href url ]
                                [ text match.text ]
                            ]


toHtml : String -> List (Html msg)
toHtml =
    toAST
        >> astsToHtml


-- Scanner


lexer : LexerModel -> LexerModel
lexer model =
    case String.uncons model.remainText of
        Nothing ->
            model

        Just ( char, remainTextTail ) ->
            let
                noOpModel =
                    { model
                        | remainText = remainTextTail
                        , lastChar = Just char
                        , index = model.index + 1
                    }

            in
                if char == '*' || char == '_' then
                    emphasisTagFound model
                        |> Result.withDefault noOpModel
                        |> lexer

                else if char == '`' then
                    codeTagFound model
                        |> Maybe.withDefault noOpModel
                        |> lexer

                else if char == '[' then
                    linkTagFound model
                        |> Maybe.withDefault noOpModel
                        |> lexer

                else
                    lexer noOpModel


type alias LexerModel =
    { rawText : String
    , remainText : String
    , lastChar : Maybe Char
    , tokens : List Token
    , index : Int
    , matches : List Match
    }


initLexerModel : String -> LexerModel
initLexerModel rawText  =
    { rawText = rawText
    , remainText = rawText
    , lastChar = Nothing
    , tokens = []
    , index = 0
    , matches = []
    }


type alias Token =
    { index : Int
    , length : Int
    , meaning : Meaning
    --, state : State -- Desnecessário?
    }


type Meaning
    = EmphasisTag Char
    | LinkOpen
    | ImageOpen



-- Code Span


codeTagFound : LexerModel -> Maybe LexerModel
codeTagFound model =
    let
        openRegex : Regex
        openRegex =
            Regex.regex "^(`+)"


        closeRegex : Int -> Regex
        closeRegex length =
            Regex.regex ("^([\\s\\S]*?[^`])(`{" ++ toString length ++ "})([^`]|$)")


        extractOpenTagLength : String -> Maybe Int
        extractOpenTagLength =
            Regex.find (Regex.AtMost 1) openRegex
                >> List.head
                >> Maybe.map (.match >> String.length)


        formatCode : String -> String
        formatCode code =
            String.trim code
                |> Regex.replace Regex.All
                    (Regex.regex "[\\s]*\\n+[\\s]*| {2,}")
                    (\_ -> " ")


        toMatch : ( String, String ) -> Match
        toMatch ( code, tag ) =
            let
                rawText = tag ++ code ++ tag

            in
                { type_   = Code
                , content = []
                , start   = model.index
                , end     = model.index + String.length rawText
                , rawText = rawText
                , text    = formatCode code
                }


        verifyCloseTag : String -> Int -> LexerModel
        verifyCloseTag remainText tagLength =
            let
                remainTextWithoutOpenTag : String
                remainTextWithoutOpenTag =
                    String.dropLeft tagLength remainText


                noMatchLexerModel : LexerModel
                noMatchLexerModel =
                    { model
                        | remainText = remainTextWithoutOpenTag
                        , index = model.index + tagLength
                        , lastChar = Just '`'
                    }


                maybeCloseRegexMatch : String -> Maybe Regex.Match
                maybeCloseRegexMatch =
                    Regex.find (Regex.AtMost 1) (closeRegex tagLength)
                        >> List.head


                extractCodeAndCloseTag : Regex.Match -> LexerModel
                extractCodeAndCloseTag regexMatch =
                    case regexMatch.submatches of
                        Just code :: Just closeTag :: _ ->
                            ( code, closeTag )
                                |> toMatch
                                |> updateLexerModel model

                        _ ->
                            noMatchLexerModel


            in
                remainTextWithoutOpenTag
                    |> maybeCloseRegexMatch
                    |> Maybe.map extractCodeAndCloseTag
                    |> Maybe.withDefault noMatchLexerModel


    in
        model.remainText
            |> extractOpenTagLength
            |> Maybe.map (verifyCloseTag model.remainText)



----- Link


type alias LinkMatch =
    { matchLength : Int
    , inside : String
    , url : String
    , maybeTitle : Maybe String
    }


insideRegex : String
insideRegex =
    "[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*"


titleRegex : String
titleRegex =
    "(?:\\s+(?:'([^'\\\\]*(?:\\\\.[^'\\\\]*)*)'|\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?"


hrefRegex : String
hrefRegex =
    "\\s*(?:<([^<>\\s]*)>|([^\\s\\(\\)\\\\]*(?:\\\\.[^\\(\\)\\\\]*)*))" ++ titleRegex ++ "\\s*"


extractLinkRegex : Regex.Match -> Maybe LinkMatch
extractLinkRegex regexMatch =
    case regexMatch.submatches of
        Just rawText
            :: maybeRawUrlAB -- with angle brackets: <http://url.com>
            :: maybeRawUrlW -- without angle brackets : http://url.com
            :: maybeTitleSQ -- with single quotes: 'title'
            :: maybeTitleDQ -- with double quotes: "title"
            :: maybeTitleP -- with parenthesis: (title)
            :: _ ->
                let
                    maybeRawUrl : Maybe String
                    maybeRawUrl =
                        returnFirstJust [ maybeRawUrlAB, maybeRawUrlW ]


                    toReturn : String -> LinkMatch
                    toReturn rawUrl =
                        { matchLength = String.length regexMatch.match
                        , inside = rawText
                        , url = rawUrl
                        , maybeTitle =
                            returnFirstJust
                                [ maybeTitleSQ
                                , maybeTitleDQ
                                , maybeTitleP
                                ]
                        }

                in
                    maybeRawUrl
                        |> Maybe.map toReturn
                    

        _ ->
            Nothing


-- TODO code backtick have precedence over link - how to do?
linkTagFound : LexerModel -> Maybe LexerModel
linkTagFound model =
    let
        linkRegex : Regex
        linkRegex =
            Regex.regex ("^\\[(" ++ insideRegex ++ ")\\]\\(" ++ hrefRegex ++ "\\)")


        refLinkRegex : Regex
        refLinkRegex =
            Regex.regex ("^\\[(" ++ insideRegex ++ ")\\]\\s*\\[([^\\]]*)\\]")


        linkRegexToMatch : LinkMatch -> Match
        linkRegexToMatch { matchLength, inside, url, maybeTitle } =
            { type_   = Link ( url, maybeTitle ) 
            , content = []
            , start   = model.index
            , end     = model.index + matchLength
            , rawText = inside
            , text    = inside
            }


        applyLinkRegex : String -> Maybe Match
        applyLinkRegex =
            Regex.find (Regex.AtMost 1) linkRegex
                >> List.head
                >> Maybe.map (Debug.log "linkMatchRegex")
                >> Maybe.andThen extractLinkRegex
                >> Maybe.map linkRegexToMatch
                -->> Maybe.map (Debug.log "linkMatch")


        extractRefLinkRegex : String -> Maybe Regex.Match
        extractRefLinkRegex =
            Regex.find (Regex.AtMost 1) refLinkRegex
                >> List.head
                -->> Maybe.map (Debug.log "refLink")


    in
        model.remainText
            |> applyLinkRegex
            |> Maybe.map (updateLexerModel model)



-- Emphasis


emphasisTagFound : LexerModel -> Result String LexerModel
emphasisTagFound model =
    let
        emSequenceRegex : Regex
        emSequenceRegex =
            Regex.regex "^([\\*_]+)([^\\*_])?"


        ( maybeEmSequence, maybeNextString) =
            Regex.find (Regex.AtMost 1) emSequenceRegex model.remainText
                |> regexMatchToTuple


        isLeftSpace =
            model.lastChar
                |> Maybe.map (containSpace << String.fromChar)
                |> Maybe.withDefault True


        isLeftPuntuaction =
            model.lastChar
                |> Maybe.map (containPuntuaction << String.fromChar)
                |> Maybe.withDefault False


        isRightSpace =
            maybeNextString
                |> Maybe.map containSpace
                |> Maybe.withDefault True


        isRightPuntuaction =
            maybeNextString
                |> Maybe.map containPuntuaction
                |> Maybe.withDefault False


        leftFringeRank =
            if isLeftSpace then 0
            else if isLeftPuntuaction then 1
            else 2


        rightFringeRank =
            if isRightSpace then 0
            else if isRightPuntuaction then 1
            else 2

    in
        case maybeEmSequence of
            Just emSequence ->
                let
                    emSequenceLength =
                        String.length emSequence


                    remainText =
                        model.remainText
                            |> String.dropLeft emSequenceLength


                    lastChar =
                        String.reverse emSequence
                            |> String.uncons
                            |> Maybe.map Tuple.first


                    char =
                        String.uncons emSequence
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault '*'


                    index =
                        model.index + emSequenceLength


                    emToken =
                        { index = model.index
                        , length = emSequenceLength
                        , meaning = EmphasisTag char
                        }


                    updtModel =
                        { model
                            | remainText = remainText
                            , lastChar = lastChar
                            , index = index
                        }
                -- Separar em lista ["***", "___"]
                -- Mapear para EmphasisTag Char
                -- Adicionar em tokens
                in
                    if leftFringeRank == rightFringeRank then
                        Result.Ok updtModel

                    -- Opening tag
                    else if leftFringeRank < rightFringeRank then
                        Result.Ok
                            { updtModel
                                | tokens = emToken :: model.tokens
                            }

                    -- CLosing tag
                    else
                        case retrieveToken emToken model.tokens of
                            Just ( openToken, closeToken, updtTokens ) ->
                                Result.Ok
                                    { updtModel
                                        | tokens = updtTokens
                                        , matches =
                                            tokenToMatch
                                                model.rawText
                                                openToken
                                                closeToken
                                                    :: model.matches
                                    }

                            Nothing ->
                                Result.Ok updtModel


            Nothing ->
                Result.Err "Invalid emphasis sequence"
                    |> Debug.log "emphasisTagFound"



retrieveToken : Token -> List Token -> Maybe ( Token, Token, List Token )
retrieveToken token tokens =
    case tokens of
        [] ->
            Nothing

        tokensHead :: tokensTail ->
            if tokensHead.meaning == token.meaning then
                let
                    remainLenght =
                        tokensHead.length - token.length

                in
                    if remainLenght == 0 then
                        Just ( tokensHead, token, tokensTail )

                    else if remainLenght > 0 then
                        Just
                            ( { tokensHead
                                | index = tokensHead.index + remainLenght
                                , length = tokensHead.length - remainLenght
                              }
                            , token
                            , { tokensHead | length = remainLenght }
                                :: tokensTail
                            )

                    else -- fechar outros abertos
                        Just
                            ( tokensHead
                            , { token
                                | index = token.index - remainLenght
                                , length = token.length + remainLenght
                              }
                            , tokensTail
                            )

            else
                retrieveToken token tokensTail


tokenToMatch : String -> Token -> Token -> Match
tokenToMatch rawText openToken closeToken =
    let
        start = openToken.index
        end = closeToken.index + closeToken.length
        textStart = openToken.index + openToken.length
        textEnd = closeToken.index

    in
        { type_   = Emphasis openToken.length
        , content = []
        , start   = start
        , end     = end
        , rawText = String.slice start end rawText -- É necessário??
        , text = String.slice textStart textEnd rawText
        }



-- Helpers


containSpace : String -> Bool
containSpace str =
    Regex.contains
        (Regex.regex "\\s")
        str


containPuntuaction : String -> Bool
containPuntuaction str =
    Regex.contains
        (Regex.regex "[!-#%-\\*,-/:;\\?@\\[-\\]_\\{\\}]")
        str


regexMatchToTuple : List Regex.Match -> ( Maybe String, Maybe String )
regexMatchToTuple matches =
    case matches of
        match :: _ ->
            case match.submatches of
                maybeEmSequence :: maybeNextChar :: _ ->
                    ( maybeEmSequence, maybeNextChar )

                _ ->
                    ( Nothing, Nothing )

        _ ->
            ( Nothing, Nothing )


returnFirstJust : List (Maybe a) -> Maybe a
returnFirstJust maybes =
    let
        process : Maybe a -> Maybe a -> Maybe a
        process a maybeFound =
            case maybeFound of
                Just found -> Just found
                Nothing -> a

    in
        List.foldl process Nothing maybes


updateLexerModel : LexerModel -> Match -> LexerModel
updateLexerModel model match =
    { model
        | remainText =
            String.dropLeft (match.end - match.start) model.remainText
        , index = match.end
        , matches = match :: model.matches
        , lastChar =
            model.rawText
                |> String.reverse
                |> String.uncons
                |> Maybe.map Tuple.first
    }


testSrt : String
testSrt = """
***Pablo* Gustavo** `Daiji` *da Costa* Hirafuji `código com  *negrito*`  
meio __negrito__ `final`*fsgdfgf* .
"""

main : Html msg
main =
    div []
        [ p [] (toHtml testSrt)
        , p [] [ text (toString <| toAST testSrt ) ]
        , p []
            [ text
                <| toString
                <| lexer
                <| initLexerModel testSrt
            ]
        ]
