module Form.Init exposing (cards)

import Bets.Types exposing (Group(..), Round(..))
import Form.Bracket.Types as Bracket
import Form.GroupMatches.Types as GroupMatches
import Form.Types exposing (Card(..), Info(..))


cards : List Card
cards =
    [ IntroCard Intro
    , GroupMatchesCard <| GroupMatches.init A "m01"
    , GroupMatchesCard <| GroupMatches.init B "m03"
    , GroupMatchesCard <| GroupMatches.init C "m05"
    , GroupMatchesCard <| GroupMatches.init D "m07"
    , GroupMatchesCard <| GroupMatches.init E "m10"
    , GroupMatchesCard <| GroupMatches.init F "m11"
    , BracketCard Bracket.init
    , TopscorerCard
    , ParticipantCard
    , SubmitCard
    ]
