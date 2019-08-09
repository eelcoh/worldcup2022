module Form.Types exposing
    ( Card(..)
    , Flags
    , FormInfoMsg(..)
    , Info(..)
    , InputState(..)
    , Model
    , Msg(..)
    , Page
    )

import Bets.Types exposing (AnswerID, Bet, Group, Round)
import Browser
import Browser.Navigation as Navigation
import Form.Question
import Form.QuestionSet
import Form.QuestionSets.Types as QuestionSets
import Form.Questions.Types as Questions
import Html exposing (Html)
import Http
import RemoteData exposing (WebData)
import Url


type alias Flags =
    { formId : Maybe String
    }


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


type InputState
    = Clean
    | Dirty


type alias Model msg =
    { cards : List Card
    , bet : Bet
    , savedBet : WebData Bet
    , idx : Page
    , card : Html msg
    , formId : Maybe String
    , betState : InputState
    , navKey : Navigation.Key
    }



-- type SubmitMsgType
--     = PlaceBet
--     | BetPlaced (Result Http.Error Bet)
--     | NoOp
--     | Again


type FormInfoMsg
    = NoOps


type Msg
    = NavigateTo Page
    | Answered Page Form.Question.Msg
    | InfoMsg FormInfoMsg
    | QuestionSetMsg Page Form.QuestionSet.Msg
    | SubmitMsg
    | SubmittedBet (WebData Bet)
    | NoOp
    | Restart
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | ScreenResize Int Int
