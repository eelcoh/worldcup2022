module Bets.Init.WorldCup2022.Bet exposing (answers)

import Bets.Init.Lib as Init
import Bets.Init.WorldCup2022.Tournament exposing (bracket, matches)
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
    , Init.answerGroupMatch "m18" A matches.m18
    , Init.answerGroupMatch "m19" A matches.m19
    , Init.answerGroupMatch "m36" A matches.m36
    , Init.answerGroupMatch "m35" A matches.m35

    --
    , Init.answerGroupMatch "m03" B matches.m03
    , Init.answerGroupMatch "m04" B matches.m04
    , Init.answerGroupMatch "m20" B matches.m20
    , Init.answerGroupMatch "m17" B matches.m17
    , Init.answerGroupMatch "m34" B matches.m34
    , Init.answerGroupMatch "m33" B matches.m33

    --
    , Init.answerGroupMatch "m08" C matches.m08
    , Init.answerGroupMatch "m07" C matches.m07
    , Init.answerGroupMatch "m22" C matches.m22
    , Init.answerGroupMatch "m24" C matches.m24
    , Init.answerGroupMatch "m39" C matches.m39
    , Init.answerGroupMatch "m40" C matches.m40

    --
    , Init.answerGroupMatch "m05" D matches.m05
    , Init.answerGroupMatch "m06" D matches.m06
    , Init.answerGroupMatch "m21" D matches.m21
    , Init.answerGroupMatch "m23" D matches.m23
    , Init.answerGroupMatch "m37" D matches.m37
    , Init.answerGroupMatch "m38" D matches.m38

    --
    , Init.answerGroupMatch "m10" E matches.m10
    , Init.answerGroupMatch "m11" E matches.m11
    , Init.answerGroupMatch "m25" E matches.m25
    , Init.answerGroupMatch "m28" E matches.m28
    , Init.answerGroupMatch "m43" E matches.m43
    , Init.answerGroupMatch "m44" E matches.m44

    --
    , Init.answerGroupMatch "m12" F matches.m12
    , Init.answerGroupMatch "m09" F matches.m09
    , Init.answerGroupMatch "m27" F matches.m27
    , Init.answerGroupMatch "m26" F matches.m26
    , Init.answerGroupMatch "m42" F matches.m42
    , Init.answerGroupMatch "m41" F matches.m41

    --
    , Init.answerGroupMatch "m13" G matches.m13
    , Init.answerGroupMatch "m16" G matches.m16
    , Init.answerGroupMatch "m29" G matches.m29
    , Init.answerGroupMatch "m31" G matches.m31
    , Init.answerGroupMatch "m47" G matches.m47
    , Init.answerGroupMatch "m48" G matches.m48

    --
    , Init.answerGroupMatch "m14" H matches.m14
    , Init.answerGroupMatch "m15" H matches.m15
    , Init.answerGroupMatch "m30" H matches.m30
    , Init.answerGroupMatch "m32" H matches.m32
    , Init.answerGroupMatch "m45" H matches.m45
    , Init.answerGroupMatch "m46" H matches.m46
    ]
