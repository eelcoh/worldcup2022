module Form.Question exposing
    ( Msg
    , update
    , view
    )

import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Group, Round)
import Element
import Form.Questions.Bracket
import Form.Questions.GroupBestThirds
import Form.Questions.Participant
import Form.Questions.Topscorer
import Form.Questions.Types exposing (..)
import UI.Style



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
            ( newBet, newQState, Cmd.map GroupBestThirdsAnswer fx )

        BracketAnswer act ->
            let
                ( newBet, newQState, fx ) =
                    Form.Questions.Bracket.update (Debug.log "action" act) bet (Debug.log "Question.elm - update - incoming qState" qState)
            in
            ( newBet, Debug.log "Question.elm - update - new qState" newQState, Cmd.map BracketAnswer fx )

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
        QGroupBestThirds ->
            Element.map GroupBestThirdsAnswer (Form.Questions.GroupBestThirds.view bet qState)

        QTopscorer ->
            Element.map TopscorerAnswer (Form.Questions.Topscorer.view bet qState)

        QBracket bracketState ->
            let
                br =
                    Debug.log "Question.elm - view - bracketState" bracketState
            in
            Element.map BracketAnswer (Form.Questions.Bracket.view bet (Debug.log "Question.elm - view - qState" qState))

        QParticipant ->
            Element.map ParticipantAnswer (Form.Questions.Participant.view bet qState)



--
