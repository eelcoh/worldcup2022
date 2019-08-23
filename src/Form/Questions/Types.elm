module Form.Questions.Types exposing (Angle, BracketState(..), Model, QState, QuestionType(..), display, isComplete, qBracket, qParticipant, qTopscorer, question)

import Bets.Bet
import Bets.Types exposing (AnswerID, Bet, Candidate, Slot)
import Bets.Types.Answer


type alias QState =
    Model


type alias Model =
    { questionType : QuestionType
    , answerId : AnswerID
    , next : Maybe AnswerID
    }


type alias Angle =
    Float


type BracketState
    = ShowMatches
    | ShowSecondRoundSelection Slot Candidate Angle Angle


type QuestionType
    = QBracket BracketState
    | QParticipant
    | QTopscorer


question : QuestionType -> AnswerID -> QState
question questionType answerId =
    Model questionType answerId Nothing


qBracket : AnswerID -> QState
qBracket answerId =
    question (QBracket ShowMatches) answerId


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
