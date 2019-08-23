module Bets.Init.Euro2020.Bet exposing (answers)

import Bets.Init.Euro2020.Tournament exposing (bracket, matches)
import Bets.Init.Lib exposing (answerGroupMatch)
import Bets.Types exposing (..)
import Bets.Types.DateTime exposing (date, time)
import Bets.Types.Draw as Draw
import Bets.Types.Participant as P
import Bets.Types.Team exposing (team)
import Time exposing (Month(..))
import Tuple exposing (pair)


answers : Answers
answers =
    let
        m =
            matches
    in
    -- group A
    [ answerGroupMatch "m01" A m.m01
    , answerGroupMatch "m02" A m.m02
    , answerGroupMatch "m14" A m.m14
    , answerGroupMatch "m13" A m.m13
    , answerGroupMatch "m26" A m.m26
    , answerGroupMatch "m25" A m.m25
    , answerGroupMatch "m03" B m.m03
    , answerGroupMatch "m04" B m.m04
    , answerGroupMatch "m15" B m.m15
    , answerGroupMatch "m16" B m.m16
    , answerGroupMatch "m28" B m.m28
    , answerGroupMatch "m27" B m.m27
    , answerGroupMatch "m05" C m.m05
    , answerGroupMatch "m06" C m.m06
    , answerGroupMatch "m17" C m.m17
    , answerGroupMatch "m18" C m.m18
    , answerGroupMatch "m29" C m.m29
    , answerGroupMatch "m30" C m.m30
    , answerGroupMatch "m07" D m.m07
    , answerGroupMatch "m08" D m.m08
    , answerGroupMatch "m20" D m.m20
    , answerGroupMatch "m19" D m.m19
    , answerGroupMatch "m32" D m.m32
    , answerGroupMatch "m31" D m.m31
    , answerGroupMatch "m09" E m.m09
    , answerGroupMatch "m10" E m.m10
    , answerGroupMatch "m22" E m.m22
    , answerGroupMatch "m21" E m.m21
    , answerGroupMatch "m33" E m.m33
    , answerGroupMatch "m34" E m.m34
    , answerGroupMatch "m12" F m.m12
    , answerGroupMatch "m11" F m.m11
    , answerGroupMatch "m24" F m.m24
    , answerGroupMatch "m23" F m.m23
    , answerGroupMatch "m36" F m.m36
    , answerGroupMatch "m35" F m.m35

    -- bracket
    , pair "br" (AnswerBracket bracket Nothing)

    -- topscorer
    , pair "ts" (AnswerTopscorer ( Nothing, Nothing ) Nothing)

    -- participant
    , pair "me" (AnswerParticipant P.init)
    ]
