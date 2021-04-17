module Bets.Types.Answer.GroupMatches exposing
    ( decode
    , encode
    , findGroupMatchAnswers
    , getAnswer
    , isComplete
    , isCompleteGroup
    , setScore
    )

import Bets.Types exposing (Answer(..), AnswerGroupMatch, AnswerGroupMatches, Group(..), MatchID, Score)
import Bets.Types.Answer.GroupMatch as GroupMatch exposing (isMatchForGroup)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode



-- EXPOSED


getAnswer : AnswerGroupMatches -> MatchID -> Maybe ( MatchID, AnswerGroupMatch )
getAnswer matches matchID =
    List.filter (\( mID, _ ) -> mID == matchID) matches
        |> List.head



{-
   Set the score for a groupmatch question.
-}


setScore : AnswerGroupMatches -> MatchID -> Score -> AnswerGroupMatches
setScore matches setMatchID score =
    let
        set ( matchID, groupMatch ) =
            if matchID == setMatchID then
                ( matchID, GroupMatch.setScore groupMatch score )

            else
                ( matchID, groupMatch )
    in
    List.map set matches


isComplete : AnswerGroupMatches -> Bool
isComplete matches =
    List.map Tuple.second matches
        |> List.all GroupMatch.isComplete


isCompleteGroup : Group -> AnswerGroupMatches -> Bool
isCompleteGroup grp matches =
    findGroupMatchAnswers grp matches
        |> isComplete



-- ******* --
-- PRIVATE - OTHER FUNCTIONS --
-- Generic
{-
   Updates an answer in the Answers. Maps all answers, but only updates
   the answer if it has the same id.
-}


findGroupMatchAnswers : Group -> AnswerGroupMatches -> AnswerGroupMatches
findGroupMatchAnswers grp matches =
    List.filter (Tuple.second >> isMatchForGroup grp) matches


encode : AnswerGroupMatches -> Json.Encode.Value
encode matches =
    List.map (Tuple.mapSecond GroupMatch.encode) matches
        |> Json.Encode.object


decode : Decoder AnswerGroupMatches
decode =
    Decode.keyValuePairs GroupMatch.decode
