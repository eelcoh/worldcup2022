module Form.Types exposing
    ( Card(..)
    , Flags
    , FormInfoMsg(..)
    , Info(..)
    , InputState(..)
    , Model
    , Msg(..)
    , Page
    , ScreenSize
    )

import Bets.Types exposing (Bet, Group, Round)
import Browser
import Browser.Navigation as Navigation
import Form.Bracket.Types as Bracket
import Form.GroupMatches.Types as GroupMatches
import Form.Participant.Types as Participant
import Form.Topscorer.Types as Topscorer
import Html exposing (Html)
import RemoteData exposing (WebData)
import Url


type alias Flags =
    { formId : Maybe String
    , width : Int
    , height : Int
    }


type Card
    = IntroCard Info
      -- | QuestionCard Questions.Model
    | GroupMatchesCard GroupMatches.State
    | BracketCard Bracket.State
    | TopscorerCard
    | ParticipantCard
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


type alias Page =
    Int


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
    , screenSize : ScreenSize
    }


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type FormInfoMsg
    = NoOps


type Msg
    = NavigateTo Page
      -- | Answered Page Form.Question.Msg
    | InfoMsg FormInfoMsg
      -- | QuestionSetMsg Page Form.QuestionSet.Msg
    | GroupMatchMsg Group GroupMatches.Msg
    | BracketMsg Bracket.Msg
    | TopscorerMsg Topscorer.Msg
    | ParticipantMsg Participant.Msg
    | SubmitMsg
    | SubmittedBet (WebData Bet)
    | NoOp
    | Restart
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | ScreenResize Int Int
