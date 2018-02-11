module Form.Question
    exposing
        ( view
        , update
        , Msg
        )

import Html exposing (Html, p, map)
import Bets.Types exposing (Bet, Answer, AnswerT(..), AnswerID, Group, Round)
import Form.Questions.GroupPosition
import Form.Questions.MatchScore
import Form.Questions.MatchWinner
import Form.Questions.Participant
import Form.Questions.Bracket
import Form.Questions.GroupBestThirds
import Form.Questions.Topscorer
import Form.Questions.Types exposing (..)


-- import Form.Cards exposing (..)


type Msg
    = GroupPositionAnswer Form.Questions.GroupPosition.Msg
    | GroupBestThirdsAnswer Form.Questions.GroupBestThirds.Msg
    | MatchScoreAnswer Form.Questions.MatchScore.Msg
    | MatchWinnerAnswer Form.Questions.MatchWinner.Msg
    | BracketAnswer Form.Questions.Bracket.Msg
    | TopscorerAnswer Form.Questions.Topscorer.Msg
    | ParticipantAnswer Form.Questions.Participant.Msg


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        GroupPositionAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.GroupPosition.update act bet qState
            in
                ( newBet, newQState, Cmd.map (GroupPositionAnswer) fx )

        GroupBestThirdsAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.GroupBestThirds.update act bet qState
            in
                ( newBet, newQState, Cmd.map (GroupBestThirdsAnswer) fx )

        MatchScoreAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.MatchScore.update act bet qState
            in
                ( newBet, newQState, Cmd.map (MatchScoreAnswer) fx )

        MatchWinnerAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.MatchWinner.update act bet qState
            in
                ( newBet, newQState, Cmd.map (MatchWinnerAnswer) fx )

        BracketAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Bracket.update act bet qState
            in
                ( newBet, newQState, Cmd.map (BracketAnswer) fx )

        TopscorerAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Topscorer.update act bet qState
            in
                ( newBet, newQState, Cmd.map (TopscorerAnswer) fx )

        ParticipantAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Participant.update act bet qState
            in
                ( newBet, newQState, Cmd.map (ParticipantAnswer) fx )


view : Bet -> QState -> Html Msg
view bet qState =
    case qState.questionType of
        QMatchScore ->
            map MatchScoreAnswer (Form.Questions.MatchScore.view bet qState)

        QMatchWinner ->
            map MatchWinnerAnswer (Form.Questions.MatchWinner.view bet qState)

        QGroupPosition ->
            map GroupPositionAnswer (Form.Questions.GroupPosition.view bet qState)

        QGroupBestThirds ->
            map GroupBestThirdsAnswer (Form.Questions.GroupBestThirds.view bet qState)

        QTopscorer ->
            map TopscorerAnswer (Form.Questions.Topscorer.view bet qState)

        QBracket ->
            map BracketAnswer (Form.Questions.Bracket.view bet qState)

        QParticipant ->
            map ParticipantAnswer (Form.Questions.Participant.view bet qState)
