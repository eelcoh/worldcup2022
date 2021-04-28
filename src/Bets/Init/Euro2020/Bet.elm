module Bets.Init.Euro2020.Bet exposing (answers)

import Bets.Init.Euro2020.Tournament exposing (bracket, matches)
import Bets.Init.Lib as Init
import Bets.Types exposing (Answer(..), AnswerGroupMatches, Answers, Group(..), GroupMatch(..))
import Time exposing (Month(..))


answers : Answers
answers =
    { matches = initMatches
    , bracket = Init.answerBracket bracket
    , topscorer = Init.answerTopscorer
    }


initMatches : AnswerGroupMatches
initMatches =
    [ Init.answerGroupMatch "m01" A matches.m01
    , Init.answerGroupMatch "m02" A matches.m02
    , Init.answerGroupMatch "m14" A matches.m14
    , Init.answerGroupMatch "m13" A matches.m13
    , Init.answerGroupMatch "m26" A matches.m26
    , Init.answerGroupMatch "m25" A matches.m25
    , Init.answerGroupMatch "m03" B matches.m03
    , Init.answerGroupMatch "m04" B matches.m04
    , Init.answerGroupMatch "m15" B matches.m15
    , Init.answerGroupMatch "m16" B matches.m16
    , Init.answerGroupMatch "m28" B matches.m28
    , Init.answerGroupMatch "m27" B matches.m27
    , Init.answerGroupMatch "m05" C matches.m05
    , Init.answerGroupMatch "m06" C matches.m06
    , Init.answerGroupMatch "m17" C matches.m17
    , Init.answerGroupMatch "m18" C matches.m18
    , Init.answerGroupMatch "m29" C matches.m29
    , Init.answerGroupMatch "m30" C matches.m30
    , Init.answerGroupMatch "m07" D matches.m07
    , Init.answerGroupMatch "m08" D matches.m08
    , Init.answerGroupMatch "m20" D matches.m20
    , Init.answerGroupMatch "m19" D matches.m19
    , Init.answerGroupMatch "m32" D matches.m32
    , Init.answerGroupMatch "m31" D matches.m31
    , Init.answerGroupMatch "m09" E matches.m09
    , Init.answerGroupMatch "m10" E matches.m10
    , Init.answerGroupMatch "m22" E matches.m22
    , Init.answerGroupMatch "m21" E matches.m21
    , Init.answerGroupMatch "m33" E matches.m33
    , Init.answerGroupMatch "m34" E matches.m34
    , Init.answerGroupMatch "m11" F matches.m11
    , Init.answerGroupMatch "m12" F matches.m12
    , Init.answerGroupMatch "m24" F matches.m24
    , Init.answerGroupMatch "m23" F matches.m23
    , Init.answerGroupMatch "m36" F matches.m36
    , Init.answerGroupMatch "m35" F matches.m35
    ]
