module Bets.Init.Lib exposing (answerGroupMatch, answerMatchWinnerInit, answerSecondRound)

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


answerSecondRound : DrawID -> SecondRoundCandidate -> Answer
answerSecondRound drawID candidate =
    let
        draw =
            Draw.init drawID

        question =
            AnswerSecondRound candidate draw Nothing
    in
    pair drawID question


answerMatchWinnerInit : Round -> Draw -> Draw -> Date -> Time -> Stadium -> Maybe DrawID -> AnswerT
answerMatchWinnerInit round home away date time stadium mNextId =
    let
        dt =
            DateTime.toPosix date time

        points =
            Nothing

        team =
            Nothing

        match =
            Match home away dt stadium
    in
    AnswerMatchWinner round match mNextId team points
