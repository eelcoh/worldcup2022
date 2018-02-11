module Form.Questions.MatchScore exposing (Msg, update, view)

import Bets.Types exposing (Bet, Answer, AnswerID, AnswerT(..), Group, Team, Match, Score)
import Bets.Types.Match as M
import Bets.Types.Score as S
import Bets.Bet exposing (setMatchScore)
import UI.Team exposing (viewTeam)
import UI.Grid as UI exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Form.Questions.Types exposing (QState)


type Msg
    = UpdateHome Answer Int
    | UpdateAway Answer Int
    | Update Answer Int Int
    | SelectMatch AnswerID


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        UpdateHome answer h ->
            let
                newBet =
                    setMatchScore bet answer ( Just h, Nothing )
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )

        UpdateAway answer a ->
            let
                newBet =
                    setMatchScore bet answer ( Nothing, Just a )
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )

        Update answer h a ->
            let
                newBet =
                    setMatchScore bet answer ( Just h, Just a )
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )

        SelectMatch answerId ->
            ( bet, { qState | next = Just answerId }, Cmd.none )


view : Bet -> QState -> Html Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId

        v match mScore answer =
            div []
                [ viewTeam (M.homeTeam match)
                , displayScore mScore
                , viewTeam (M.awayTeam match)
                , viewKeyboard answer
                ]
    in
        case mAnswer of
            Just (( answerId, AnswerGroupMatch g match mScore points ) as answer) ->
                div []
                    [ displayMatches g bet answerId
                    , v match mScore answer
                    ]

            _ ->
                p [] []


viewKeyboard : Answer -> Html Msg
viewKeyboard answer =
    div []
        [ UI.button XS Active [ onClick (Update answer 1 0) ] [ text "1 - 0" ]
        , UI.button XS Active [ onClick (Update answer 2 0) ] [ text "2 - 0" ]
        , UI.button XS Active [ onClick (Update answer 3 0) ] [ text "3 - 0" ]
        , UI.button XS Active [ onClick (Update answer 4 0) ] [ text "4 - 0" ]
        , UI.button XS Active [ onClick (Update answer 5 0) ] [ text "5 - 0" ]
        , UI.button XS Active [ onClick (Update answer 6 0) ] [ text "6 - 0" ]
        , UI.button XS Active [ onClick (UpdateHome answer 6) ] [ text "6 - _" ]
        , UI.button XS Active [ onClick (UpdateAway answer 5) ] [ text "_ - 5" ]
        ]


displayMatches : Group -> Bet -> AnswerID -> Html Msg
displayMatches grp bet answerId =
    let
        answers =
            Bets.Bet.findGroupMatchAnswers grp bet
    in
        div [] (List.map displayMatch answers)


displayMatch : Answer -> Html Msg
displayMatch ( answerId, answer ) =
    case answer of
        AnswerGroupMatch g match mScore _ ->
            div [ onClick (SelectMatch answerId) ]
                [ viewTeam (M.homeTeam match)
                , displayScore mScore
                , viewTeam (M.awayTeam match)
                ]

        _ ->
            p [] []


displayScore : Maybe Score -> Html Msg
displayScore mScore =
    case mScore of
        Just score ->
            span []
                [ span [] [ text (asString (S.homeScore score)) ]
                , span [] [ text " - " ]
                , span [] [ text (asString (S.awayScore score)) ]
                ]

        Nothing ->
            span []
                [ span [] [ text " - " ]
                ]


asString : Maybe Int -> String
asString mi =
    case mi of
        Nothing ->
            "_"

        Just i ->
            toString i
