module Form.Questions.Types exposing (..)

import Bets.Types exposing (Bet, AnswerID)
import Bets.Types.Answer
import Bets.Bet


type alias QState =
    Model


type alias Model =
    { questionType : QuestionType
    , answerId : AnswerID
    , next : Maybe AnswerID
    }


type QuestionType
    = QBracket
    | QGroupBestThirds
    | QParticipant
    | QTopscorer


question : QuestionType -> AnswerID -> QState
question questionType answerId =
    Model questionType answerId Nothing


qBracket : AnswerID -> QState
qBracket answerId =
    question QBracket answerId


qGroupBestThirds : AnswerID -> QState
qGroupBestThirds answerId =
    question QGroupBestThirds answerId


qParticipant : AnswerID -> QState
qParticipant answerId =
    question QParticipant answerId


qTopscorer : AnswerID -> QState
qTopscorer answerId =
    question QTopscorer answerId


isComplete : Bet -> QState -> Bool
isComplete bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId
    in
        case mAnswer of
            Just answer ->
                Bets.Types.Answer.isComplete answer

            Nothing ->
                False


display : Bet -> QState -> String
display bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId
    in
        case mAnswer of
            Just answer ->
                Bets.Types.Answer.summary answer

            Nothing ->
                ""
