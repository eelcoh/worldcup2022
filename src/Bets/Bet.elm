module Bets.Bet exposing
    ( cleanThirds
    , decode
    , decodeBet
    ,  encode
       -- , findAllGroupMatchAnswers

    ,  findGroupMatchAnswers
       -- , getAnswer

    , getBracket
    , getTopscorer
    , isComplete
    , setMatchScore
    , setParticipant
    , setQualifier
    , setTopscorer
    , setWinner
    )

import Bets.Json.Encode exposing (mIntEnc, mStrEnc)
import Bets.Types exposing (Answer(..), AnswerGroupMatches, Bet, Bracket, Group, MatchID, Participant, Qualifier, Score, Slot, Topscorer, Winner)
import Bets.Types.Answer.Bracket as Bracket
import Bets.Types.Answer.GroupMatches as GroupMatches
import Bets.Types.Answer.Topscorer as Topscorer
import Bets.Types.Answers as A
import Bets.Types.Participant as Participant
import Bool.Extra as Bool
import Json.Decode exposing (Decoder, field, maybe)
import Json.Encode



-- getAnswer : Bet -> AnswerID -> Maybe Answer
-- getAnswer bet answerId =
--     Bets.Types.Answers.getAnswer bet.answers answerId
-- candidates : Bet -> Answer -> Candidates
-- findAllGroupMatchAnswers bet =
--     Bets.Types.Answers.findAllGroupMatchAnswers bet.answers


findGroupMatchAnswers : Group -> Bet -> AnswerGroupMatches
findGroupMatchAnswers group bet =
    GroupMatches.findGroupMatchAnswers group bet.answers.matches


isComplete : Bet -> Bool
isComplete bet =
    Bool.all
        [ GroupMatches.isComplete bet.answers.matches
        , Topscorer.isComplete bet.answers.topscorer
        , Bracket.isComplete bet.answers.bracket
        , Participant.isComplete bet.participant
        ]



-- newBet : Bet -> Answers -> Bet
-- newBet bet newAnswers =
--     { bet | answers = newAnswers }


setWinner : Bet -> Slot -> Winner -> Bet
setWinner bet slot winner =
    { bet | answers = A.setWinner bet.answers slot winner }


setQualifier : Bet -> Slot -> Group -> Qualifier -> Bet
setQualifier bet slot grp qualifier =
    { bet | answers = A.setQualifier bet.answers slot grp qualifier }


cleanThirds : Bet -> Group -> Bet
cleanThirds bet grp =
    { bet | answers = A.cleanThirds bet.answers grp }


setMatchScore : Bet -> MatchID -> Score -> Bet
setMatchScore bet matchID score =
    { bet | answers = A.setScore bet.answers matchID score }


setParticipant : Bet -> Participant -> Bet
setParticipant bet participant =
    { bet | participant = participant }


setTopscorer : Bet -> Topscorer -> Bet
setTopscorer bet ts =
    { bet | answers = A.setTopscorer bet.answers ts }


getTopscorer : Bet -> Topscorer
getTopscorer bet =
    let
        extract (Answer topscorer _) =
            topscorer
    in
    extract bet.answers.topscorer


getBracket : Bet -> Bracket
getBracket bet =
    let
        extract (Answer bracket _) =
            bracket
    in
    extract bet.answers.bracket


encode : Bet -> Json.Encode.Value
encode bet =
    let
        betObject =
            Json.Encode.object
                [ ( "answers", A.encode bet.answers )
                , ( "betId", mStrEnc bet.betId )
                , ( "uuid", mStrEnc bet.uuid )
                , ( "active", Json.Encode.bool bet.active )
                , ( "participant", Participant.encode bet.participant )
                ]
    in
    Json.Encode.object
        [ ( "bet", betObject ) ]


type alias IncomingBet =
    { bet : Bet }


decode : Decoder Bet
decode =
    Json.Decode.map
        (\x -> x.bet)
        decodeIncoming


decodeIncoming : Decoder IncomingBet
decodeIncoming =
    Json.Decode.map
        IncomingBet
        (field "bet" decodeBet)


decodeBet : Decoder Bet
decodeBet =
    Json.Decode.map5 Bet
        (field "answers" A.decode)
        (field "betId" (maybe Json.Decode.string))
        (field "uuid" (maybe Json.Decode.string))
        (field "active" Json.Decode.bool)
        (field "participant" Participant.decode)
