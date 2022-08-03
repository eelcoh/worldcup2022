module Bets.Init exposing (bet)

import Bets.Init.WorldCup2022.Bet exposing (answers)
import Bets.Types exposing (Bet)
import Bets.Types.Participant as Participant


bet : Bet
bet =
    { answers = answers
    , betId = Nothing
    , uuid = Nothing
    , active = True
    , participant = Participant.init
    }
