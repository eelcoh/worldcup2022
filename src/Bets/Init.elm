module Bets.Init exposing (bet)

import Bets.Init.Euro2020.Bet exposing (answers)
import Bets.Types exposing (..)
import Bets.Types.Participant as P
import Bets.Types.Team exposing (team)


bet : Bet
bet =
    { answers = answers
    , betId = Nothing
    , uuid = Nothing
    , active = True
    }
