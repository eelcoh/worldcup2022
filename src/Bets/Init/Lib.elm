module Bets.Init.Lib exposing (answerGroupMatch)

import Bets.Types exposing (..)
import Bets.Types.DateTime as DateTime
import Bets.Types.Draw as Draw
import Bets.Types.Participant as P
import Bets.Types.Team exposing (team)
import Tuple exposing (pair)


answerGroupMatch : DrawID -> Group -> Match -> Answer
answerGroupMatch drawID group match =
    let
        points =
            Nothing

        score =
            Nothing

        question =
            AnswerGroupMatch group match score points
    in
    pair drawID question
