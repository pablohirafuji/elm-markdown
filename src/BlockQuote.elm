module BlockQuote exposing (..)


import Regex exposing (Regex)
import Html exposing (Html, blockquote)



-- Model


type alias Line = String


regex : Regex
regex =
    Regex.regex "^ {0,3}(?:>[ ]?)(.*)$"


fromMatch : Regex.Match -> Maybe Line
fromMatch match =
    match.submatches
        |> List.head
        |> Maybe.withDefault Nothing


view : List ( Html msg ) -> Html msg
view =
    blockquote []


{-

BlockQuote acaba quando:
    BlockQuote :: BlockQuote
    BlockQuote :: BlockQuote [ blankLine ]
    BlockQuote :: TextLine (Só pode ser parágrafo ou adicionar ao último parágrafo)
    BlockQuote :: _ Acaba


type alias Block =
    List ( Type, ( List Line, List Block ) )

Fazer blankBlock para fazer os cases
    case BlockQuote :: BlankBlock
    case BlockQuote :: UndefinedBlock
        se último bloco do blocquote for Paragraph ou UndefinedBlock


=========================================


    = BlankLine String
    | TextLine String
    | ATXHeadingLine Int String
    | SetextHeadingLine Int String
    | ThematicBreakLine
    | BlockQuoteLine String
    | ListLine ListState String -- Ordered Number -> Delimiter -> RawText
    --| ListLine Lists.Block
    | CodeLine Code.Block


Lines that Always interrupt
    Thematic breaksLine
    Fenced code line
    ATX headings


Interrupt per Block
    Blockquote
        ListLine
        BlankLine (Last inside block or next block)

    ListLine
        BlockQuoteLine

    Paragraph
        BlankLine


Not interrupt
    ListLine/BlockQuote
        Setext headings
        Indented code blocks


MaybeInterrupt
    ListLine/BlockQuote
        TextLine (When last is Paragraph)


[ ( Block, Set Block - InterruptedByLines - Always interrupt ++ ) ]

if interrupt then
    if List.member containerBlocks then
        parsa o conteúdo do bloco
    else
        passa para o próximo


type Block
    = LeafBlock
    | ContainerBlock

containerBlocks 



=========================================

Fazer retornar Maybe (List Block) já atualizado com
o parágrafo, se não volta Nothing

-}

