module Bets.Types.Answer.GroupMatch exposing
    ( decode
    , encode
    , isComplete
    , isMatchForGroup
    , setScore
    , summary
    )

import Bets.Types exposing (Answer(..), AnswerGroupMatch, Group, GroupMatch(..), Match, Points, Score)
import Bets.Types.Group
import Bets.Types.Match
import Bets.Types.Points
import Bets.Types.Score
import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


setScore : AnswerGroupMatch -> Score -> AnswerGroupMatch
setScore (Answer (GroupMatch group match mScore) points) nScore =
    let
        newScore =
            case Maybe.map (\oScore -> Bets.Types.Score.merge oScore nScore) mScore of
                Just s ->
                    Just s

                Nothing ->
                    Just nScore
    in
    Answer (GroupMatch group match newScore) points


isComplete : AnswerGroupMatch -> Bool
isComplete (Answer (GroupMatch _ _ mScore) _) =
    Bets.Types.Score.isComplete mScore


isMatchForGroup : Group -> AnswerGroupMatch -> Bool
isMatchForGroup groupRequested (Answer (GroupMatch group _ _) _) =
    groupRequested == group


summary : AnswerGroupMatch -> String
summary (Answer (GroupMatch group _ _) _) =
    "Wedstrijden " ++ Bets.Types.Group.toString group


encode : AnswerGroupMatch -> Json.Encode.Value
encode (Answer (GroupMatch group match mScore) points) =
    Json.Encode.object
        [ ( "group", Bets.Types.Group.encode group )
        , ( "match", Bets.Types.Match.encode match )
        , ( "score", Bets.Types.Score.encodeMaybe mScore )
        , ( "points", Bets.Types.Points.encode points )
        ]


constructor : Group -> Match -> Maybe Score -> Points -> AnswerGroupMatch
constructor group match mScore points =
    Answer (GroupMatch group match mScore) points


decode : Decoder AnswerGroupMatch
decode =
    Decode.succeed constructor
        |> required "group" Bets.Types.Group.decode
        |> required "match" Bets.Types.Match.decode
        |> required "score" (nullable Bets.Types.Score.decode)
        |> required "points" Bets.Types.Points.decode
