module Types exposing
    ( ActivitiesModel
    , Activity(..)
    , ActivityMeta
    , App(..)
    , Card(..)
    , Comment
    , Flags
    , FormInfoMsg(..)
    , Info(..)
    , InputState(..)
    , Model
    , Msg(..)
    , Page
    , Post
    , Token(..)
    , init
    , initComment
    , initPost
    )

import Bets.Init
import Bets.Types exposing (Bet, Group(..), Round(..))
import Browser
import Browser.Navigation as Navigation
import Form.Bracket.Types as Bracket
import Form.GroupMatches.Types as GroupMatches
import Form.Participant.Types as Participant
import Form.Screen as Screen
import Form.Topscorer.Types as Topscorer
import Html exposing (Html, div)
import RemoteData exposing (RemoteData(..), WebData)
import Time
import Url


type App
    = Home
    | Blog
    | Form



-- | Ranking
-- | RankingDetailsView
-- | Bets String
-- | Login
-- | Results
-- | EditMatchResult
-- | KOResults
-- | TSResults


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


init : Maybe String -> Screen.Size -> Navigation.Key -> Model Msg
init formId sz navKey =
    { cards = initCards sz
    , bet = Bets.Init.bet
    , savedBet = NotAsked
    , idx = 0
    , card = div [] []
    , formId = formId
    , betState = Clean
    , navKey = navKey
    , screen = sz
    , activities = activitiesInit
    , app = Home
    , token = NotAsked
    , timeZone = Time.utc
    }


type alias Model msg =
    { cards : List Card
    , bet : Bet
    , savedBet : WebData Bet
    , idx : Page
    , card : Html msg
    , formId : Maybe String
    , betState : InputState
    , navKey : Navigation.Key
    , screen : Screen.Size
    , activities : ActivitiesModel msg
    , app : App
    , token : WebData Token
    , timeZone : Time.Zone
    }


type FormInfoMsg
    = NoOps


type Msg
    = NavigateTo Page
    | SetApp App
      -- | Answered Page Form.Question.Msg
    | FoundTimeZone Time.Zone
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
      --
    | RefreshActivities
    | FetchedActivities (WebData (List Activity))
    | FetchedBet (WebData Bet)
    | SetCommentMsg String
    | SetCommentAuthor String
    | SaveComment
    | SavedComment (WebData (List Activity))
    | HideCommentInput
    | ShowCommentInput
    | SetPostTitle String
    | SetPostPassphrase String
    | SetPostMsg String
    | SetPostAuthor String
    | SavePost
    | SavedPost (WebData (List Activity))
    | HidePostInput
    | ShowPostInput


initCards : Screen.Size -> List Card
initCards sz =
    [ IntroCard Intro
    , GroupMatchesCard <| GroupMatches.init A "m01"
    , GroupMatchesCard <| GroupMatches.init B "m03"
    , GroupMatchesCard <| GroupMatches.init C "m05"
    , GroupMatchesCard <| GroupMatches.init D "m07"
    , GroupMatchesCard <| GroupMatches.init E "m10"
    , GroupMatchesCard <| GroupMatches.init F "m11"
    , BracketCard <| Bracket.init sz
    , TopscorerCard
    , ParticipantCard
    , SubmitCard
    ]



-- Activities


type alias ActivitiesModel msg =
    { activities : WebData (List Activity)
    , comment : Comment
    , post : Post
    , contents : Html msg
    , showComment : Bool
    , showPost : Bool
    }


activitiesInit : ActivitiesModel Msg
activitiesInit =
    { activities = NotAsked
    , comment = initComment
    , post = initPost
    , contents = div [] []
    , showComment = True
    , showPost = False
    }


initComment : Comment
initComment =
    { author = "", msg = "" }


initPost : Post
initPost =
    { author = "", title = "", msg = "", passphrase = "" }


type alias Name =
    String


type alias Author =
    String


type alias Title =
    String


type alias Message =
    String


type alias UUID =
    String


type alias ActivityMeta =
    { date : Time.Posix
    , active : Bool
    , uuid : String
    }


type Activity
    = ANewBet ActivityMeta Name UUID
    | AComment ActivityMeta Author Message
    | APost ActivityMeta Author Title Message
    | ANewRanking ActivityMeta


type alias Comment =
    { author : String
    , msg : String
    }


type alias Post =
    { author : String
    , title : String
    , msg : String
    , passphrase : String
    }


type Token
    = Token String
