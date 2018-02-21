module Form.Init exposing (init, initModel)

import Html exposing (div)
import Bets.Init
import Bets.Types exposing (Group(..), Round(..))
import Form.Types exposing (..)
import Form.Questions.Types as Q
import Form.QuestionSets.Types as QSet


init : Flags -> ( Model Msg, Cmd Msg )
init flags =
    ( { initModel | formId = flags.formId }, Cmd.none )


initModel : Model Msg
initModel =
    { cards = cards
    , bet = Bets.Init.bet
    , idx = 0
    , card = div [] []
    , formId = Nothing
    , submitted = Clean
    }


cards : List Card
cards =
    [ IntroCard Intro
    , QuestionSetCard <| QSet.matchScoreQuestions A "m01"
    , QuestionSetCard <| QSet.matchScoreQuestions B "m03"
    , QuestionSetCard <| QSet.matchScoreQuestions C "m05"
    , QuestionSetCard <| QSet.matchScoreQuestions D "m07"
    , QuestionSetCard <| QSet.matchScoreQuestions E "m10"
    , QuestionSetCard <| QSet.matchScoreQuestions F "m11"
    , QuestionSetCard <| QSet.matchScoreQuestions G "m13"
    , QuestionSetCard <| QSet.matchScoreQuestions H "m16"
    , QuestionSetCard <| QSet.groupPositionQuestions A "wa"
    , QuestionSetCard <| QSet.groupPositionQuestions B "wb"
    , QuestionSetCard <| QSet.groupPositionQuestions C "wc"
    , QuestionSetCard <| QSet.groupPositionQuestions D "wd"
    , QuestionSetCard <| QSet.groupPositionQuestions E "we"
    , QuestionSetCard <| QSet.groupPositionQuestions F "wf"
    , QuestionSetCard <| QSet.groupPositionQuestions G "wg"
    , QuestionSetCard <| QSet.groupPositionQuestions H "wh"
    , QuestionCard <| Q.qBracket "br"
    , QuestionCard <| Q.qTopscorer "ts"
    , QuestionCard <| Q.qParticipant "me"
    , SubmitCard
    ]
