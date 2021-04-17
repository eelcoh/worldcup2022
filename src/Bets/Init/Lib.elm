module Bets.Init.Lib exposing (answerBracket, answerGroupMatch, answerTopscorer)

import Bets.Types exposing (Answer(..), Bracket, DrawID, Group, GroupMatch(..), Match, Points, Topscorer)
import Tuple exposing (pair)


answer : a -> Points -> Answer a
answer val points =
    Answer val points


answerBracket : Bracket -> Answer Bracket
answerBracket bracket =
    answer bracket Nothing


answerTopscorer : Answer Topscorer
answerTopscorer =
    answer ( Nothing, Nothing ) Nothing


answerGroupMatch : DrawID -> Group -> Match -> ( DrawID, Answer GroupMatch )
answerGroupMatch drawID group match =
    let
        points =
            Nothing

        score =
            Nothing

        m =
            GroupMatch group match score

        question =
            answer m points
    in
    pair drawID question
