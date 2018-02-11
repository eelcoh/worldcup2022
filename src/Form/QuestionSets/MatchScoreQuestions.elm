module Form.QuestionSets.MatchScoreQuestions exposing (Msg, update, view)

import Bets.Types exposing (Bet, Answer, AnswerID, AnswerT(..), Answers, Group, Team, Match, Score)
import Bets.Types.Match as M
import Bets.Types.Score as S
import Bets.Types.Group as G
import Bets.Bet exposing (setMatchScore)
import Form.QuestionSets.Types exposing (Model, ChangeCursor(..), updateCursor)
import UI.Grid as UI exposing (container, row, Color(..), Size(..), Align(..))
import UI.Team exposing (viewTeam, viewTeamLarge)
import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (id, value, placeholder, class)
import String


type Msg
    = UpdateHome Answer Int
    | UpdateAway Answer Int
    | Update Answer Int Int
    | SelectMatch AnswerID
    | NoOp


update : Msg -> Model -> Bet -> ( Bet, Model, Cmd Msg )
update action model bet =
    case action of
        UpdateHome answer h ->
            ( setMatchScore bet answer ( Just h, Nothing ), updateCursor model bet Dont, Cmd.none )

        UpdateAway answer a ->
            ( setMatchScore bet answer ( Nothing, Just a ), updateCursor model bet Implicit, Cmd.none )

        Update answer h a ->
            ( setMatchScore bet answer ( Just h, Just a ), updateCursor model bet Implicit, Cmd.none )

        SelectMatch answerId ->
            ( bet, updateCursor model bet (Explicit answerId), Cmd.none )

        NoOp ->
            ( bet, model, Cmd.none )


view : Model -> Bet -> Maybe Answer -> Answers -> Html Msg
view model bet mAnswer answers =
    case mAnswer of
        Just (( answerId, AnswerGroupMatch g match mScore points ) as answer) ->
            div []
                [ displayHeader g
                , introduction
                , displayMatches model.cursor answers
                , viewInput model answer (M.homeTeam match) (M.awayTeam match) mScore
                , viewKeyboard_ model answer
                ]

        _ ->
            div [] []


displayHeader : Group -> Html Msg
displayHeader grp =
    h1 [] [ text ("Voorspel de uitslagen in group " ++ (G.toString grp)) ]


introduction : Html Msg
introduction =
    p [] [ text "Voorspel de uitslagen door op de knop met de gewenste score te klikken. Voor een juiste uitslag krijg je 3 punten. Heb je enkel de toto goed levert je dat 1 punt op." ]


row0 : List ( Int, Int )
row0 =
    [ ( 5, 0 )
    , ( 4, 0 )
    , ( 3, 0 )
    , ( 2, 0 )
    , ( 1, 0 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 0, 2 )
    , ( 0, 3 )
    , ( 0, 4 )
    , ( 0, 5 )
    ]


row1 : List ( Int, Int )
row1 =
    [ ( 5, 1 )
    , ( 4, 1 )
    , ( 3, 1 )
    , ( 2, 1 )
    , ( 1, 1 )
    , ( 1, 2 )
    , ( 1, 3 )
    , ( 1, 4 )
    , ( 1, 5 )
    ]


row2 : List ( Int, Int )
row2 =
    [ ( 5, 2 )
    , ( 4, 2 )
    , ( 3, 2 )
    , ( 2, 2 )
    , ( 2, 3 )
    , ( 2, 4 )
    , ( 2, 5 )
    ]


row3 : List ( Int, Int )
row3 =
    [ ( 5, 3 )
    , ( 4, 3 )
    , ( 3, 3 )
    , ( 3, 4 )
    , ( 3, 5 )
    ]


row4 : List ( Int, Int )
row4 =
    [ ( 5, 4 )
    , ( 4, 4 )
    , ( 4, 5 )
    ]


row5 : List ( Int, Int )
row5 =
    [ ( 5, 5 )
    ]


scores : List (List ( Int, ( Int, Int, String ) ))
scores =
    [ row0
    , row1
    , row2
    , row3
    , row4
    , row5
    ]
        |> List.map indexedScores


indexedScores : List ( Int, Int ) -> List ( Int, ( Int, Int, String ) )
indexedScores scoreList =
    scoreList
        |> List.map (\( h, a ) -> ( h, a, (scoreString h a) ))
        |> List.indexedMap (,)


viewInput : Model -> Answer -> Maybe Team -> Maybe Team -> Maybe Score -> Html Msg
viewInput model answer homeTeam awayTeam mScore =
    let
        makeAction act val =
            case (String.toInt val) of
                Ok v ->
                    act v

                Err _ ->
                    NoOp

        inputField v act =
            div [ class "inp" ]
                [ input
                    [ value v
                    , Html.Events.onInput (makeAction act)
                    ]
                    []
                ]

        extractScore extractor =
            mScore
                |> Maybe.andThen extractor
                |> Maybe.map toString
                |> Maybe.withDefault ""

        homeInput =
            inputField (extractScore S.homeScore) (UpdateHome answer)

        awayInput =
            inputField (extractScore S.awayScore) (UpdateAway answer)

        homeBadge =
            UI.button2 M Active [ (id "home") ] [ viewTeamLarge homeTeam ]

        awayBadge =
            UI.button2 M Active [ (id "away") ] [ viewTeamLarge awayTeam ]
    in
        container Center
            [ class "score-input center container" ]
            [ homeBadge
            , homeInput
            , awayInput
            , awayBadge
            ]


viewKeyboard_ : Model -> Answer -> Html Msg
viewKeyboard_ model answer =
    let
        toButton ( _, ( h, a, t ) ) =
            scoreButton_ Potential answer h a t

        toRow scoreList =
            row Center [] (List.map toButton scoreList)
    in
        container Center [] (List.map toRow scores)


scoreButton_ : Color -> Answer -> Int -> Int -> String -> Html Msg
scoreButton_ c answer home away t =
    let
        handler =
            onClick (Update answer home away)
    in
        UI.scoreButton c [ handler ] [ span [] [ text t ] ]



{-
   scoreButton : Msg -> Int -> Button.Instance Mdl Msg
   scoreButton action i =
     Button.instance i MDL Button.flat (Button.model True)
       [ Button.fwdClick action ]
-}


displayMatches : AnswerID -> Answers -> Html Msg
displayMatches cursor answers =
    let
        display =
            displayMatch cursor
    in
        container Justified [] (List.map display answers)


displayMatch : AnswerID -> Answer -> Html Msg
displayMatch cursor ( answerId, answer ) =
    let
        colors =
            if cursor == answerId then
                Active
            else
                Potential

        disp match mScore =
            let
                c =
                    case ( (cursor == answerId), (S.isComplete mScore) ) of
                        ( True, _ ) ->
                            3

                        ( _, True ) ->
                            12

                        ( _, False ) ->
                            13

                handler =
                    onClick (SelectMatch answerId)
            in
                UI.score L
                    colors
                    [ handler ]
                    [ viewTeam (M.homeTeam match)
                    , displayScore mScore
                    , viewTeam (M.awayTeam match)
                    ]
    in
        case answer of
            AnswerGroupMatch g match mScore _ ->
                disp match mScore

            _ ->
                p [] []


scoreString : a -> b -> String
scoreString h a =
    List.foldr (++) "" [ " ", (toString h), "-", (toString a), " " ]


displayScore : Maybe Score -> Html Msg
displayScore mScore =
    let
        contents =
            case mScore of
                Just score ->
                    (text (S.asString score))

                Nothing ->
                    text " _-_ "
    in
        UI.txt [ "dash" ] L Irrelevant [] [ contents ]
