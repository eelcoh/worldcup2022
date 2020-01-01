module Bets.Init.Lib exposing (answerGroupMatch)

import Bets.Types exposing (Answer, AnswerT(..), DrawID, Group, Match)
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
