module Form.Init exposing (cards)

import Bets.Types exposing (Group(..), Round(..))
import Form.QuestionSets.Types as QSet
import Form.Questions.Types as Q
import Form.Types exposing (..)




cards : List Card
cards =
    [ IntroCard Intro
    , QuestionSetCard <| QSet.matchScoreQuestions A "m01"
    , QuestionSetCard <| QSet.matchScoreQuestions B "m03"
    , QuestionSetCard <| QSet.matchScoreQuestions C "m05"
    , QuestionSetCard <| QSet.matchScoreQuestions D "m07"
    , QuestionSetCard <| QSet.matchScoreQuestions E "m10"
    , QuestionSetCard <| QSet.matchScoreQuestions F "m11"
    , QuestionCard <| Q.qBracket "br"
    , QuestionCard <| Q.qTopscorer "ts"
    , QuestionCard <| Q.qParticipant "me"
    , SubmitCard
    ]
