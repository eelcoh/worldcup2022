module Form.Question exposing
    ( Msg
    , update
    , view
    )

import Bets.Types exposing (AnswerT(..), Bet)
import Element
import Form.Questions.Bracket
import Form.Questions.Participant
import Form.Questions.Topscorer
import Form.Questions.Types exposing (QState, QuestionType(..))


type Msg
    = BracketAnswer Form.Questions.Bracket.Msg
    | TopscorerAnswer Form.Questions.Topscorer.Msg
    | ParticipantAnswer Form.Questions.Participant.Msg


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        BracketAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Bracket.update act bet qState
            in
            ( newBet, newQState, Cmd.map BracketAnswer fx )

        TopscorerAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Topscorer.update act bet qState
            in
            ( newBet, newQState, Cmd.map TopscorerAnswer fx )

        ParticipantAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Participant.update act bet qState
            in
            ( newBet, newQState, Cmd.map ParticipantAnswer fx )


view : Bet -> QState -> Element.Element Msg
view bet qState =
    case qState.questionType of
        QTopscorer ->
            Element.map TopscorerAnswer (Form.Questions.Topscorer.view bet qState)

        QBracket _ ->
            Element.map BracketAnswer (Form.Questions.Bracket.view bet qState)

        QParticipant ->
            Element.map ParticipantAnswer (Form.Questions.Participant.view bet qState)



--
