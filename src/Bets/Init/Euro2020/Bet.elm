module Bets.Init.Euro2020.Bet exposing (answers)

import Bets.Init.Euro2020.Tournament exposing (bracket, matches)
import Bets.Types exposing (Answer(..), AnswerGroupMatch, AnswerGroupMatches, Answers, Group(..), GroupMatch(..), Match)
import Time exposing (Month(..))
import Tuple exposing (pair)


answers : Answers
answers =
    { matches = initMatches
    , bracket = Answer bracket Nothing
    , topscorer = Answer ( Nothing, Nothing ) Nothing
    }


initAnswerGroupMatch : Group -> Match -> AnswerGroupMatch
initAnswerGroupMatch groupId match =
    Answer (GroupMatch groupId match Nothing) Nothing


initMatches : AnswerGroupMatches
initMatches =
    [ pair "m01" (initAnswerGroupMatch A matches.m01)
    , pair "m02" (initAnswerGroupMatch A matches.m02)
    , pair "m14" (initAnswerGroupMatch A matches.m14)
    , pair "m13" (initAnswerGroupMatch A matches.m13)
    , pair "m26" (initAnswerGroupMatch A matches.m26)
    , pair "m25" (initAnswerGroupMatch A matches.m25)
    , pair "m03" (initAnswerGroupMatch B matches.m03)
    , pair "m04" (initAnswerGroupMatch B matches.m04)
    , pair "m15" (initAnswerGroupMatch B matches.m15)
    , pair "m16" (initAnswerGroupMatch B matches.m16)
    , pair "m28" (initAnswerGroupMatch B matches.m28)
    , pair "m27" (initAnswerGroupMatch B matches.m27)
    , pair "m05" (initAnswerGroupMatch C matches.m05)
    , pair "m06" (initAnswerGroupMatch C matches.m06)
    , pair "m17" (initAnswerGroupMatch C matches.m17)
    , pair "m18" (initAnswerGroupMatch C matches.m18)
    , pair "m29" (initAnswerGroupMatch C matches.m29)
    , pair "m30" (initAnswerGroupMatch C matches.m30)
    , pair "m07" (initAnswerGroupMatch D matches.m07)
    , pair "m08" (initAnswerGroupMatch D matches.m08)
    , pair "m20" (initAnswerGroupMatch D matches.m20)
    , pair "m19" (initAnswerGroupMatch D matches.m19)
    , pair "m32" (initAnswerGroupMatch D matches.m32)
    , pair "m31" (initAnswerGroupMatch D matches.m31)
    , pair "m09" (initAnswerGroupMatch E matches.m09)
    , pair "m10" (initAnswerGroupMatch E matches.m10)
    , pair "m22" (initAnswerGroupMatch E matches.m22)
    , pair "m21" (initAnswerGroupMatch E matches.m21)
    , pair "m33" (initAnswerGroupMatch E matches.m33)
    , pair "m34" (initAnswerGroupMatch E matches.m34)
    , pair "m12" (initAnswerGroupMatch F matches.m12)
    , pair "m11" (initAnswerGroupMatch F matches.m11)
    , pair "m24" (initAnswerGroupMatch F matches.m24)
    , pair "m23" (initAnswerGroupMatch F matches.m23)
    , pair "m36" (initAnswerGroupMatch F matches.m36)
    , pair "m35" (initAnswerGroupMatch F matches.m35)
    ]
