module Form.Question
    exposing
        ( view
        , update
        , Msg
        )

import Bets.Types exposing (Bet, Answer, AnswerT(..), AnswerID, Group, Round)
import UI.Style
import Element
import Form.Questions.Participant
import Form.Questions.Bracket
import Form.Questions.GroupBestThirds
import Form.Questions.Topscorer
import Form.Questions.Types exposing (..)


-- import Form.Cards exposing (..)


type Msg
    = GroupBestThirdsAnswer Form.Questions.GroupBestThirds.Msg
    | BracketAnswer Form.Questions.Bracket.Msg
    | TopscorerAnswer Form.Questions.Topscorer.Msg
    | ParticipantAnswer Form.Questions.Participant.Msg


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        GroupBestThirdsAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.GroupBestThirds.update act bet qState
            in
                ( newBet, newQState, Cmd.map (GroupBestThirdsAnswer) fx )

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


view : Bet -> QState -> Element.Element UI.Style.Style variation Msg
view bet qState =
    case qState.questionType of
        QGroupBestThirds ->
            Element.map GroupBestThirdsAnswer (Form.Questions.GroupBestThirds.view bet qState)

        QTopscorer ->
            Element.map TopscorerAnswer (Form.Questions.Topscorer.view bet qState)

        QBracket ->
            Element.map BracketAnswer (Form.Questions.Bracket.view bet qState)

        QParticipant ->
            Element.map ParticipantAnswer (Form.Questions.Participant.view bet qState)



--
