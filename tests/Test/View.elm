module Test.View exposing (..)

--import Test.HTMLBlocks

import Html exposing (..)
import Html.Attributes exposing (checked, href, style, type_)
import Html.Events exposing (onCheck)
import Test.ATXHeading
import Test.BlankLine
import Test.BlockQuote
import Test.FencedCode
import Test.Helpers exposing (Output)
import Test.IndentedCode
import Test.Initial
import Test.Inline.Autolinks
import Test.Inline.Code
import Test.Inline.Custom
import Test.Inline.EmphasisStrong
import Test.Inline.Entity
import Test.Inline.Escape
import Test.Inline.Images
import Test.Inline.LineBreak
import Test.Inline.Link
import Test.Inline.RawHtml
import Test.LinkReferenceDefinition
import Test.List
import Test.ListItem
import Test.Paragraph
import Test.SetextHeading
import Test.ThematicBreak



-- Model


type alias Model =
    { showFailed : Bool
    , showSucceed : Bool
    }


initModel : Model
initModel =
    { showFailed = True
    , showSucceed = False
    }



-- Msg/Update


type Msg
    = ShowFailed Bool
    | ShowSucceed Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowFailed bool ->
            { model | showFailed = bool }

        ShowSucceed bool ->
            { model | showSucceed = bool }



-- Based on http://spec.commonmark.org/0.27/


tests : List ( String, List (Output msg) )
tests =
    [ ( "1-4 Initial examples", Test.Initial.run )
    , ( "4.1 Thematic breaks", Test.ThematicBreak.run )
    , ( "4.2 ATX headings", Test.ATXHeading.run )
    , ( "4.3 Setext headings", Test.SetextHeading.run )
    , ( "4.4 Indented code blocks", Test.IndentedCode.run )
    , ( "4.5 Fenced code blocks", Test.FencedCode.run )

    --, ( "4.6 HTML Blocks", Test.HTMLBlock.run )
    , ( "4.7 Link reference definitions", Test.LinkReferenceDefinition.run )
    , ( "4.8 Paragraphs", Test.Paragraph.run )
    , ( "4.9 Blank lines", Test.BlankLine.run )
    , ( "5.1 Block quotes", Test.BlockQuote.run )
    , ( "5.2 List Items", Test.ListItem.run )
    , ( "5.3 Lists", Test.List.run )
    , ( "6.1 Backslash escapes", Test.Inline.Escape.run )
    , ( "6.2 Entity and numeric character references", Test.Inline.Entity.run )
    , ( "6.3 Code spans", Test.Inline.Code.run )
    , ( "6.4 Emphasis and strong emphasis", Test.Inline.EmphasisStrong.run )
    , ( "6.5 Links", Test.Inline.Link.run )
    , ( "6.6 Images", Test.Inline.Images.run )
    , ( "6.7 Autolinks", Test.Inline.Autolinks.run )
    , ( "6.8 Raw HTML", Test.Inline.RawHtml.run )
    , ( "6.9-11 Line breaks and Textual content", Test.Inline.LineBreak.run )
    , ( "Customs", Test.Inline.Custom.run )
    ]


view : Model -> Html Msg
view model =
    div [] <|
        [ h1 []
            [ text <|
                "Tests ("
                    ++ String.fromInt (successTestCount tests)
                    ++ "/"
                    ++ String.fromInt (totalTestCount tests)
                    ++ ")"
            ]
        , p []
            [ text "Based on "
            , a [ href "http://spec.commonmark.org/0.27/" ]
                [ text "CommonMark Spec" ]
            , text "."
            ]
        , label []
            [ input
                [ type_ "checkbox"
                , onCheck ShowFailed
                , checked model.showFailed
                ]
                []
            , text "Show Failed"
            ]
        , label []
            [ input
                [ type_ "checkbox"
                , onCheck ShowSucceed
                , checked model.showSucceed
                ]
                []
            , text "Show Succeed"
            ]
        ]
            ++ showTests model tests


totalTestCount : List ( String, List (Output msg) ) -> Int
totalTestCount allTests =
    let
        sumTests : ( String, List (Output msg) ) -> Int -> Int
        sumTests ( _, outputs ) count =
            List.length outputs
                |> (+) count
    in
    List.foldl sumTests 0 allTests


successTestCount : List ( String, List (Output msg) ) -> Int
successTestCount allTests =
    let
        sumSuccessTests : ( String, List (Output msg) ) -> Int -> Int
        sumSuccessTests ( _, outputs ) count =
            List.filter
                (\ { result, expectedResult } ->
                    result == expectedResult
                )
                outputs
                |> List.length
                |> (+) count
    in
    List.foldl sumSuccessTests 0 allTests


outputToResult : Output msg -> Result (Html msg) (Html msg)
outputToResult { result, expectedResult } =
    if result == expectedResult then
        Result.Ok (div [] result)
    else
        Result.Err (div [] result)




showTests : Model -> List ( String, List (Output Msg) ) -> List (Html Msg)
showTests model allTests =
        List.map (showTest model) allTests


showTest : Model -> ( String, List (Output msg) ) -> Html msg
showTest model ( testTitle, outputs ) =
    let
        passed : Int
        passed =
            successTestCount [ ( testTitle, outputs ) ]

        bgStyle : List (Html.Attribute msg)
        bgStyle =
            [ style "background-color" bgColor ]

        bgColor : String
        bgColor =
            if List.length outputs == passed then
                "#90EE90"

            else
                "#EEB4B4"
    in
    details [] <|
        [ summary [] <|
            [ text (testTitle ++ " ")
            , span bgStyle
                [ text <|
                    "("
                        ++ String.fromInt passed
                        ++ "/"
                        ++ String.fromInt (List.length outputs)
                        ++ ")"
                ]
            ]
        , ul [] (List.map (testView model) outputs)
        ]


testView : Model -> Output msg -> Html msg
testView model output =
    case outputToResult output of
        Result.Ok html ->
            if model.showSucceed then
                li [] [ html ]

            else
                text ""

        Result.Err html ->
            if model.showFailed then
                li [] [ html ]

            else
                text ""
