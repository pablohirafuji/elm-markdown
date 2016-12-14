module Block exposing (Block)


type Block
    = Blank String
    | ThematicBreak
    | Paragraph (List String)
    | Heading Int (List String)
    | Code CodeInfo (List String)
    | BlockQuote (List Block)
    | List ListInfo (List Block)