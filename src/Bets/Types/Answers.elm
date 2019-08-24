module Bets.Types.Answers exposing
    ( decode
    , encode
    , findAllGroupMatchAnswers
    , findGroupMatchAnswers
    , getAnswer
    , setMatchScore
    , setParticipant
    , setQualifier
    , setTopscorer
    , setWinner
    )

import Bets.Types exposing (..)
import Bets.Types.Answer
import Bets.Types.Bracket as B
import Bets.Types.Match exposing (..)
import Bets.Types.Round exposing (isSameOrANextRound, nextRound)
import Bets.Types.Score as S
import Dict
import Json.Decode exposing (Decoder)
import Json.Encode
import Tuple



-- EXPOSED


getAnswer : Answers -> AnswerID -> Maybe Answer
getAnswer answers answerID =
    List.filter (\( id, _ ) -> id == answerID) answers
        |> List.head



{-
   Set the score for a groupmatch question.
-}


setMatchScore : Answers -> Answer -> Score -> Answers
setMatchScore answers ( answerID, answer ) score =
    let
        newAnswer =
            case answer of
                AnswerGroupMatch grp match s points ->
                    let
                        newScore =
                            Maybe.map (S.merge score) s
                    in
                    Tuple.pair answerID (AnswerGroupMatch grp match newScore points)

                _ ->
                    Tuple.pair answerID answer
    in
    setAnswer newAnswer answers



{-
   Set the topscorer answer.
-}


setTopscorer : Answers -> Answer -> Topscorer -> Answers
setTopscorer answers ( answerID, answer ) topscorer =
    let
        newAnswer =
            case answer of
                AnswerTopscorer _ points ->
                    Tuple.pair answerID (AnswerTopscorer topscorer points)

                _ ->
                    Tuple.pair answerID answer
    in
    setAnswer newAnswer answers



{-
   Set the participant data.
-}


setParticipant : Answers -> Answer -> Participant -> Answers
setParticipant answers ( answerID, answer ) participant =
    let
        newAnswer =
            case answer of
                AnswerParticipant _ ->
                    Tuple.pair answerID (AnswerParticipant participant)

                _ ->
                    Tuple.pair answerID answer
    in
    setAnswer newAnswer answers



-- ******* --
-- PRIVATE - OTHER FUNCTIONS --
-- Generic
{-
   Updates an answer in the Answers. Maps all answers, but only updates
   the answer if it has the same id.
-}


updateAnswer : Answer -> Answer -> Answer
updateAnswer newAnswer currentAnswer =
    if Tuple.first newAnswer == Tuple.first currentAnswer then
        newAnswer

    else
        currentAnswer


setAnswer : Answer -> Answers -> Answers
setAnswer answer answers =
    List.map (updateAnswer answer) answers



-- Bracket functions
{-
   Remove team from a bracket answer
   - Removes the team from the match if it is in
   - Removes the team a selected match winner if it was selected
   Only removes the team from this answer if the round of the match
   is smaller larger than or equal to the round given in the input.

   Does NOT recursively remove the team from next rounds, assumes this
   function is applied as part of a map over the answers answers:

     List.map (unsetTeamAnswer round team) answers

-}


unsetTeamAnswer : List Team -> Round -> Answer -> Answer
unsetTeamAnswer teams rnd answer =
    case answer of
        ( answerID, AnswerBracket bracket points ) ->
            let
                newBracket =
                    List.map Just teams
                        |> List.foldr (\b a -> B.unsetQualifier a b) bracket

                newAnswer =
                    ( answerID, AnswerBracket newBracket points )
            in
            newAnswer

        _ ->
            answer


unsetTeamsMatch : Match -> List Team -> Match
unsetTeamsMatch match teams =
    List.foldr (\b a -> unsetTeamMatch a b) match teams


unsetTeams : Answers -> Maybe Round -> List Team -> Answers
unsetTeams answers mRound teams =
    case mRound of
        Nothing ->
            answers

        Just r ->
            List.map (unsetTeamAnswer teams r) answers


setDrawMatchWinner : Draw -> Answer -> Answer
setDrawMatchWinner (( drawID, mTeam ) as draw) ( answerID, answer ) =
    let
        nextTeam mTm match =
            case mTm of
                Nothing ->
                    Nothing

                Just t ->
                    if List.member t (teamsInMatch match) then
                        Just t

                    else
                        Nothing
    in
    case answer of
        AnswerBracket bracket points ->
            let
                newBracket =
                    B.set bracket drawID mTeam

                newAnswer =
                    AnswerBracket newBracket points
            in
            Tuple.pair answerID newAnswer

        _ ->
            Tuple.pair answerID answer



{-
   Toplevel function for assigning a team to a slot in the bracket.
   This maps over all answers. One of these will be updated.
-}


setDraw : Draw -> Answers -> Answers
setDraw draw answers =
    List.map (setDrawMatchWinner draw) answers


setDraws : List Draw -> Answers -> Answers
setDraws draws answers =
    case draws of
        [] ->
            answers

        draw :: rest ->
            let
                newAnswers =
                    setDraw draw answers
            in
            setDraws rest newAnswers


setNext : NextID -> Team -> Answers -> Answers
setNext nextID team answers =
    case nextID of
        Just drawID ->
            setDraw ( drawID, Just team ) answers

        _ ->
            answers


updateMatchWinner : DrawID -> Maybe Team -> Answers -> Answers
updateMatchWinner drawID mTeam answers =
    setDraw ( drawID, mTeam ) answers


updateBracket : DrawID -> Maybe Team -> Answers -> Answers
updateBracket drawID mTeam answers =
    setDraw ( drawID, mTeam ) answers


setWinner : Answers -> Answer -> Slot -> Winner -> Answers
setWinner answers ( aid, answer ) slot homeOrAway =
    case answer of
        AnswerBracket bracket points ->
            let
                newBracket =
                    B.proceed bracket slot homeOrAway

                newAnswer =
                    Tuple.pair aid (AnswerBracket newBracket points)

                newAnswers =
                    setAnswer newAnswer answers
            in
            newAnswers

        _ ->
            answers


setQualifier : Answers -> Answer -> Slot -> Qualifier -> Answers
setQualifier answers ( aid, answer ) slot qualifier =
    case answer of
        AnswerBracket bracket points ->
            let
                newBracket =
                    B.unsetQualifier bracket qualifier
                        |> setSlot

                setSlot br =
                    B.set br slot qualifier

                newAnswer =
                    Tuple.pair aid (AnswerBracket newBracket points)

                newAnswers =
                    setAnswer newAnswer answers
            in
            newAnswers

        _ ->
            answers


isMatchScoreAnswer : Answer -> Bool
isMatchScoreAnswer ( _, answer ) =
    case answer of
        AnswerGroupMatch _ _ _ _ ->
            True

        _ ->
            False


isGroupMatchScoreAnswer : Group -> Answer -> Bool
isGroupMatchScoreAnswer grp ( _, answer ) =
    case answer of
        AnswerGroupMatch g _ _ _ ->
            g == grp

        _ ->
            False


findGroupAnswers : (Answer -> Bool) -> Answers -> Answers
findGroupAnswers fltr answers =
    List.filter fltr answers


findGroupMatchAnswers : Group -> Answers -> Answers
findGroupMatchAnswers grp answers =
    findGroupAnswers (isGroupMatchScoreAnswer grp) answers


findAllGroupMatchAnswers : Answers -> Answers
findAllGroupMatchAnswers answers =
    findGroupAnswers isMatchScoreAnswer answers


encode : Answers -> Json.Encode.Value
encode answers =
    let
        d =
            List.map Bets.Types.Answer.encodeAnswer answers
    in
    Json.Encode.object d


decode : Decoder Answers
decode =
    Json.Decode.dict Bets.Types.Answer.decode
        |> Json.Decode.map Dict.toList
