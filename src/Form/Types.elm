module Form.Types
    exposing
        ( Card(..)
        , Info(..)
        , Model
        , Msg(..)
        , FormInfoMsg(..)
        , SubmitMsgType(..)
        , SubmitState(..)
        )

import Bets.Types exposing (AnswerID, Bet, Group, Round)
import Form.Question
import Form.QuestionSet
import Form.QuestionSets.Types as QuestionSets
import Form.Questions.Types as Questions
import Html exposing (Html)
import Http


type Card
    = IntroCard Info
    | QuestionCard Questions.Model
    | QuestionSetCard QuestionSets.Model
    | SubmitCard


type Info
    = Intro
    | FirstRoundIntro
    | GroupIntro Group
    | GroupStandingsIntro Group
    | BestThirdIntro
    | BracketIntro
    | BracketView
    | BracketRoundIntro Round
    | TopscorerIntro
    | AboutYouIntro
    | ThankYou


type alias Cursor =
    Int


type alias Page =
    Int


type alias State msg =
    Model msg


type SubmitState
    = Clean
    | Dirty
    | Sent
    | Done
    | Error
    | Reset


type alias Model msg =
    { cards : List Card
    , bet : Bet
    , idx : Page
    , card : Html msg
    , formId : Maybe String
    , submitted : SubmitState
    }


type SubmitMsgType
    = PlaceBet
    | BetPlaced (Result Http.Error Bet)
    | NoOp
    | Again


type FormInfoMsg
    = NoOps


type Msg
    = NavigateTo Page
    | Answered Page Form.Question.Msg
    | InfoMsg FormInfoMsg
    | QuestionSetMsg Page Form.QuestionSet.Msg
    | SubmitMsg SubmitMsgType
