module CommonMark.Inline exposing (..)


import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (href, title, alt, src)
import Http exposing (encodeUri)
import Regex exposing (Regex)



-- Abstract Syntax Tree


type AST
    = NoMatch String
    | ASTMatch Match


type Match =
    Match
        { type_   : Inline
        , content : List AST
        , start   : Int
        , end     : Int
        , rawText : String
        , text    : String
        , matches : List Match
        }


type Inline
    = HardBreak
    | Code
    | Emphasis Int -- Tag length
    | Link ( String, Maybe String) -- ( Url, Maybe Title )
    | Image ( String, Maybe String) -- ( Src, Maybe Title )


regexes : List ( Inline, Regex )
regexes =
    [ ( HardBreak, hardBreakRegex )
    ]


hardBreakRegex : Regex
hardBreakRegex =
    Regex.regex " {2,}\\n|\\\\\\n"


whiteSpaceChars : String
whiteSpaceChars =
    " \\t\\f\\v\\r\\n"


cleanWhitespaces : String -> String
cleanWhitespaces =
    String.trim
        >> Regex.replace Regex.All
            (Regex.regex ("[" ++ whiteSpaceChars ++ "]+"))
            (\_ -> " ")


escapableRegex : Regex
escapableRegex =
    Regex.regex "(\\\\+)([!\"#$%&\\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-])"


replaceEscapable : String -> String
replaceEscapable =
    Regex.replace Regex.All escapableRegex
        (\regexMatch ->
            case regexMatch.submatches of
                Just backslashes :: Just escapedStr :: _ ->
                    String.repeat
                        (floor (toFloat (String.length backslashes) / 2))
                        "\\" ++ escapedStr

                _ ->
                    regexMatch.match
        )


-- to decode the following chars: ;,/?:@&=+$#%
decodeUrlRegex : Regex
decodeUrlRegex =
    Regex.regex "%(?:3B|2C|2F|3F|3A|40|26|3D|2B|24|23|25)"


encodeUrl : String -> String
encodeUrl =
    Http.encodeUri
        >> Regex.replace Regex.All decodeUrlRegex
            (\match ->
                Http.decodeUri match.match
                    |> Maybe.withDefault match.match
            )


toAST : References -> String -> List AST
toAST refs rawText =
    let
        trimmedText = String.trim rawText
    
    in
        findMatches refs trimmedText
            |> List.sortBy .start
            |> \matches -> parseMatches ( trimmedText, matches, [] )
            |> reverseASTs


findMatches : References -> String -> List Match
findMatches refs rawText =
    findRegexesMatches regexes rawText
        |> \matches ->
                .matches (lexer (initLexerModel refs rawText))
                    ++ matches


findRegexesMatches : List ( Inline, Regex ) -> String -> List Match
findRegexesMatches regexes rawText =
    List.map (findRegexMatch rawText) regexes
        |> List.concat


findRegexMatch : String -> ( Inline, Regex ) -> List Match
findRegexMatch rawText ( type_, regex ) =
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
                    -- No text to parse
                    if String.isEmpty rawText then
                        []

                    -- No match found
                    else
                        [ NoMatch rawText ]


                -- Add final unmatched string
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
            -- Add initial unmatched string
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
            
            -- New Match and add in between unmatched string
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


        updtParentMatch asts =
            { parentMatch
                | content = asts
            }

        --updtParentMatch parsedASTs =
        --    { parentMatch
        --        | content =
        --            parseMatches
        --                ( parentMatch.text
        --                , [ updtChildMatch ]
        --                , parsedASTs
        --                )
        --    }

    in
        case parentMatch.content of
            NoMatch _ :: [] ->
                updtParentMatch [ updtChildMatch ]

            _ ->
                updtParentMatch
                    (parentMatch.content ++ [ updtChildMatch ])



-- Lexer


type alias LexerModel =
    { rawText : String
    , remainText : String
    , lastChar : Maybe Char
    , isEscaped : Bool
    , tokens : List Token
    , index : Int
    , matches : List Match
    , refs : References
    }


initLexerModel : References -> String -> LexerModel
initLexerModel refs rawText  =
    { rawText = rawText
    , remainText = rawText
    , lastChar = Nothing
    , isEscaped = False
    , tokens = []
    , index = 0
    , matches = []
    , refs = refs
    }


type alias Token =
    { index : Int
    , length : Int
    , meaning : Meaning
    }


type Meaning
    = EmphasisTag Char
    | LinkOpen
    | ImageOpen


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
                        , isEscaped = False
                    }

            in
                if model.isEscaped then
                    lexer noOpModel

                else
                    if char == '*' || char == '_' then
                        emphasisTagFound model
                            |> Maybe.withDefault noOpModel
                            |> lexer

                    else if char == '`' then
                        codeTagFound model
                            |> Maybe.withDefault noOpModel
                            |> lexer

                    else if char == '[' then
                        linkTagFound model
                            |> Maybe.withDefault noOpModel
                            |> lexer

                    else if char == '!' then
                        imageTagFound model
                            |> Maybe.withDefault noOpModel
                            |> lexer

                    else if char == '<' then
                        autoLinkTagFound model
                            |> Maybe.withDefault noOpModel
                            |> lexer

                    else if char == '\\' then
                        lexer { noOpModel | isEscaped = True }

                    else
                        lexer noOpModel



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
                , text    = cleanWhitespaces code
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


type alias References =
    Dict String ( String, Maybe String ) -- Label ( Url, Maybe Title )


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
    "(?:[" ++ whiteSpaceChars ++ "]+(?:'([^'\\\\]*(?:\\\\.[^'\\\\]*)*)'|\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?"


hrefRegex : String
hrefRegex =
    "\\s*(?:<([^<>"
    ++ whiteSpaceChars ++ "]*)>|([^"
    ++ whiteSpaceChars ++ "\\(\\)\\\\]*(?:\\\\.[^\\(\\)\\\\]*)*))"


urlTitleRegex : String
urlTitleRegex =
    "\\[(" ++ insideRegex
        ++ ")\\]\\("
        ++ hrefRegex
        ++ titleRegex
        ++ "\\s*\\)"


refRegex : String
refRegex =
    "\\[(" ++ insideRegex
        ++ ")\\](?:\\[\\s*("
        ++ insideRegex
        ++ ")\\s*\\])?"


linkRegex : Regex
linkRegex =
    Regex.regex ("^" ++ urlTitleRegex)


refLinkRegex : Regex
refLinkRegex =
    Regex.regex ("^" ++ refRegex)


extractUrlTitleRegex : Regex.Match -> Maybe LinkMatch
extractUrlTitleRegex regexMatch =
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


extractRefRegex : References -> Regex.Match -> Maybe LinkMatch
extractRefRegex refs regexMatch =
    case regexMatch.submatches of
        Just label :: maybeLabel :: _ ->
            let
                refLabel : String
                refLabel =
                    case maybeLabel of
                        Nothing -> label
                        Just "" -> label
                        Just ref -> ref


                maybeRefItem : Maybe ( String, Maybe String )
                maybeRefItem =
                    Dict.get (prepareRefLabel refLabel) refs


                toReturn : ( String, Maybe String ) -> LinkMatch
                toReturn ( rawUrl, maybeTitle ) =
                    { matchLength = String.length regexMatch.match
                    , inside = label
                    , url = rawUrl
                    , maybeTitle = maybeTitle
                    }

            in
                maybeRefItem
                    |> Maybe.map toReturn
                    

        _ ->
            Nothing


prepareRefLabel : String -> String
prepareRefLabel =
    cleanWhitespaces
        >> String.toLower


-- TODO code backtick have precedence over link - how to do?
-- TODO: reference labels corta todos os espaços e quebra de linha
        -- String.tim >> Regex.replace
-- TODO: Parsar título quando imprimir
linkTagFound : LexerModel -> Maybe LexerModel
linkTagFound model =
    let
        linkMatchToMatch : LexerModel -> LinkMatch -> Match
        linkMatchToMatch model { matchLength, inside, url, maybeTitle } =
            { type_   = Link
                            ( encodeUrl (replaceEscapable url)
                            , Maybe.map replaceEscapable maybeTitle ) 
            , content = []
            , start   = model.index
            , end     = model.index + matchLength
            , rawText = inside
            , text    = replaceEscapable inside
            }


        applyLinkRegex : String -> Maybe LinkMatch
        applyLinkRegex =
            Regex.find (Regex.AtMost 1) linkRegex
                >> List.head
                >> Maybe.andThen extractUrlTitleRegex


        applyRefLinkRegex : String -> Maybe LinkMatch
        applyRefLinkRegex =
            Regex.find (Regex.AtMost 1) refLinkRegex
                >> List.head
                >> Maybe.andThen (extractRefRegex model.refs)


    in
        model.remainText
            |> applyLinkRegex
            |> ifNothing (applyRefLinkRegex model.remainText)
            |> Maybe.map (linkMatchToMatch model)
            |> Maybe.map (updateLexerModel model)



-- Image


imageRegex : Regex
imageRegex =
    Regex.regex ("^!" ++ urlTitleRegex)


refImageRegex : Regex
refImageRegex =
    Regex.regex ("^!" ++ refRegex)


imageTagFound : LexerModel -> Maybe LexerModel
imageTagFound model =
    let
        linkMatchToMatch : LinkMatch -> Match
        linkMatchToMatch { matchLength, inside, url, maybeTitle } =
            { type_   = Image
                            ( replaceEscapable url
                            , Maybe.map replaceEscapable maybeTitle )
            , content = []
            , start   = model.index
            , end     = model.index + matchLength
            , rawText = inside
            , text    = replaceEscapable inside
            }


        applyImageRegex : String -> Maybe LinkMatch
        applyImageRegex =
            Regex.find (Regex.AtMost 1) imageRegex
                >> List.head
                >> Maybe.andThen extractUrlTitleRegex


        applyRefImageRegex : String -> Maybe LinkMatch
        applyRefImageRegex =
            Regex.find (Regex.AtMost 1) refImageRegex
                >> List.head
                >> Maybe.andThen (extractRefRegex model.refs)


    in
        model.remainText
            |> applyImageRegex
            |> ifNothing (applyRefImageRegex model.remainText)
            |> Maybe.map linkMatchToMatch
            |> Maybe.map (updateLexerModel model)



-- Autolink


-- From http://spec.commonmark.org/dingus/commonmark.js
emailAutoLinkRegex : Regex
emailAutoLinkRegex =
    Regex.regex "^<([a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>"


-- From http://spec.commonmark.org/dingus/commonmark.js
autoLinkRegex : Regex
autoLinkRegex =
    Regex.regex "^<([A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*)>"


autoLinkTagFound : LexerModel -> Maybe LexerModel
autoLinkTagFound model =
    let
        linkMatchToMatch : LexerModel -> LinkMatch -> Match
        linkMatchToMatch model { matchLength, inside, url, maybeTitle } =
            { type_   = Link ( url, maybeTitle ) 
            , content = []
            , start   = model.index
            , end     = model.index + matchLength
            , rawText = inside
            , text    = inside
            }

        extractRegex : Regex.Match -> Maybe LinkMatch
        extractRegex regexMatch =
            regexMatch.submatches
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.map
                    (\url ->
                        { matchLength =
                            String.length regexMatch.match
                        , inside = url
                        , url = encodeUrl url
                        , maybeTitle = Nothing
                        }
                    )
            

        applyEmailAutoLinkRegex : String -> Maybe LinkMatch
        applyEmailAutoLinkRegex =
            Regex.find (Regex.AtMost 1) emailAutoLinkRegex
                >> List.head
                >> Maybe.andThen extractRegex
                >> Maybe.map
                    (\linkMatch ->
                        { linkMatch
                            | url = "mailto:" ++ linkMatch.url
                        }
                    )


        applyAutoLinkRegex : String -> Maybe LinkMatch
        applyAutoLinkRegex =
            Regex.find (Regex.AtMost 1) autoLinkRegex
                >> List.head
                >> Maybe.andThen extractRegex


    in
        model.remainText
            |> applyEmailAutoLinkRegex
            |> ifNothing (applyAutoLinkRegex model.remainText)
            |> Maybe.map (linkMatchToMatch model)
            |> Maybe.map (updateLexerModel model)



-- Emphasis


emphasisTagFound : LexerModel -> Maybe LexerModel
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


        processEmSequence : String -> LexerModel
        processEmSequence emSequence =
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
            -- Separar em lista ["***", "___"] Testr regex split (.)\1+
            -- Mapear para EmphasisTag Char
            -- Adicionar em tokens
            in
                if leftFringeRank == rightFringeRank then
                    updtModel
                    -- Se rightFringeRank /= 0 pode ser fechamento
                    -- Se não for, é abertura

                -- Opening tag
                else if leftFringeRank < rightFringeRank then
                    { updtModel
                        | tokens = emToken :: model.tokens
                    }

                -- CLosing tag
                else
                    case retrieveToken emToken model.tokens of
                        Just ( openToken, closeToken, updtTokens ) ->
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
                            updtModel


    in
        maybeEmSequence
            |> Maybe.map processEmSequence


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


ifNothing : Maybe a -> Maybe a -> Maybe a
ifNothing maybe maybe_ =
    if maybe_ == Nothing then
        maybe

    else
        maybe_


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



-- Finalization


reverseASTs : List AST -> List AST
reverseASTs =
    List.reverse
        >> List.map reverseASTContent


reverseASTContent : AST -> AST
reverseASTContent ast =
    case ast of
        ASTMatch match ->
            case match.type_ of
                Link _ ->
                    ASTMatch
                        { match
                            | content = toAST Dict.empty match.text
                        }

                _ ->
                    ASTMatch
                        { match
                            | content = reverseASTs match.content
                        }

        NoMatch rawText ->
            NoMatch (replaceEscapable rawText)


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
                                (astsToHtml match.content)
                            ]

                        Nothing ->
                            [ a [ href url ]
                                (astsToHtml match.content)
                            ]

                Image ( url, maybeTitle ) ->
                    case maybeTitle of
                        Just title_ ->
                            [ img
                                [ alt match.text
                                , src url
                                , title title_
                                ] []
                            ]

                        Nothing ->
                            [ img
                                [ alt match.text
                                , src url
                                ] []
                            ]
