module UI.Button.Score exposing (..)

import Bets.Types exposing (Score)
import Bets.Types.Score as S
import Element exposing (Element, centerX, centerY, height, px, spacing, text, width)
import Element.Events exposing (onClick)
import UI.Style exposing (ButtonSemantics)


type ScoreButton
    = SR ( Int, Int )
    | ZR


type ScoreButtonX
    = SB Int ( Int, Int ) String
    | ZB Int


scoreString : Int -> Int -> String
scoreString h a =
    List.foldr (++) "" [ " ", String.fromInt h, "-", String.fromInt a, " " ]


displayScore : Maybe Score -> Element.Element msg
displayScore mScore =
    let
        txt =
            case mScore of
                Just score ->
                    S.asString score

                Nothing ->
                    " _-_ "
    in
    Element.el (UI.Style.score [ width (px 50), centerY ]) (Element.text txt)


viewKeyboard : msg -> (Int -> Int -> msg) -> Element msg
viewKeyboard noop matchToMsg =
    let
        toButton s =
            scoreButton UI.Style.Potential s noop matchToMsg

        toRow scoreList =
            Element.row (UI.Style.scoreRow [ centerX, spacing 2, centerY ])
                (List.map toButton scoreList)
    in
    Element.column (UI.Style.scoreColumn [ centerX, spacing 2 ])
        (List.map toRow scores)


scoreButton : UI.Style.ButtonSemantics -> ScoreButtonX -> msg -> (Int -> Int -> msg) -> Element msg
scoreButton sem sb noop createMsg =
    -- scoreButton c matchID sb =
    let
        ( msg, txt ) =
            case sb of
                ZB _ ->
                    ( noop, "" )

                SB _ ( home, away ) t ->
                    ( createMsg home away, t )
    in
    scoreButton_ sem msg txt


scoreButton_ : ButtonSemantics -> msg -> String -> Element msg
scoreButton_ semantics msg buttonText =
    let
        w =
            px 48
                |> width

        h =
            px 28
                |> height

        buttonLayout =
            UI.Style.scoreButton semantics [ w, h, onClick msg, centerX, centerY, height (px 26) ]
    in
    Element.row buttonLayout [ text buttonText ]


row00 : List ScoreButton
row00 =
    [ SR ( 3, 0 )
    , SR ( 2, 0 )
    , SR ( 1, 0 )
    , SR ( 0, 0 )
    , SR ( 0, 1 )
    , SR ( 0, 2 )
    , SR ( 0, 3 )
    ]


row01 : List ScoreButton
row01 =
    [ SR ( 6, 0 )
    , SR ( 5, 0 )
    , SR ( 4, 0 )
    , ZR
    , SR ( 0, 4 )
    , SR ( 0, 5 )
    , SR ( 0, 6 )
    ]


row10 : List ScoreButton
row10 =
    [ SR ( 4, 1 )
    , SR ( 3, 1 )
    , SR ( 2, 1 )
    , SR ( 1, 1 )
    , SR ( 1, 2 )
    , SR ( 1, 3 )
    , SR ( 1, 4 )
    ]


row11 : List ScoreButton
row11 =
    [ SR ( 7, 1 )
    , SR ( 6, 1 )
    , SR ( 5, 1 )
    , ZR
    , SR ( 1, 5 )
    , SR ( 1, 6 )
    , SR ( 1, 7 )
    ]


row20 : List ScoreButton
row20 =
    [ SR ( 5, 2 )
    , SR ( 4, 2 )
    , SR ( 3, 2 )
    , SR ( 2, 2 )
    , SR ( 2, 3 )
    , SR ( 2, 4 )
    , SR ( 2, 5 )
    ]


row3 : List ScoreButton
row3 =
    [ SR ( 6, 3 )
    , SR ( 5, 3 )
    , SR ( 4, 3 )
    , SR ( 3, 3 )
    , SR ( 3, 4 )
    , SR ( 3, 5 )
    , SR ( 3, 6 )
    ]


row4 : List ScoreButton
row4 =
    [ SR ( 5, 4 )
    , SR ( 4, 4 )
    , SR ( 4, 5 )
    ]


row5 : List ScoreButton
row5 =
    [ SR ( 5, 5 )
    ]


scores : List (List ScoreButtonX)
scores =
    [ row00
    , row01
    , row10
    , row11
    , row20
    , row3
    , row4
    , row5
    ]
        |> List.map indexedScores


indexedScores : List ScoreButton -> List ScoreButtonX
indexedScores scoreList =
    let
        mkScoreButtonX s =
            case s of
                ( i, SR ( h, a ) ) ->
                    SB i ( h, a ) (scoreString h a)

                ( i, ZR ) ->
                    ZB i
    in
    scoreList
        |> List.indexedMap Tuple.pair
        |> List.map mkScoreButtonX
