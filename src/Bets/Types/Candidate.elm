module Bets.Types.Candidate exposing (decode, encode, get, sort, toSortable)

import Bets.Init.WorldCup2022.Tournament as Tournament
import Bets.Types exposing (Bracket(..), Candidate(..), Group, Team)
import Bets.Types.Group as Group
import Json.Decode exposing (Decoder, andThen, field)
import Json.Decode.Extra exposing (fromResult)
import Json.Encode


get : Candidate -> List ( Group, Team )
get position =
    let
        teams g =
            Tournament.initTeamData
                |> List.filter (\t -> Group.equal t.group g)
                |> List.map .team
                |> List.map (Tuple.pair g)
    in
    case position of
        FirstPlace grp ->
            teams grp

        SecondPlace grp ->
            teams grp

        BestThirdFrom grps ->
            List.concatMap teams grps


sort : List Candidate -> List Candidate
sort candidates =
    List.sortBy toSortable candidates


toSortable : Candidate -> String
toSortable c =
    case c of
        FirstPlace g ->
            Group.toString g ++ "1"

        SecondPlace g ->
            Group.toString g ++ "2"

        BestThirdFrom thirds ->
            List.map Group.toString thirds
                |> String.concat
                |> (++) "z3"


encode : Candidate -> Json.Encode.Value
encode spot =
    case spot of
        FirstPlace grp ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "first-place" )
                , ( "groups", Json.Encode.list Group.encode [ grp ] )
                ]

        SecondPlace grp ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "second-place" )
                , ( "groups", Json.Encode.list Group.encode [ grp ] )
                ]

        BestThirdFrom grps ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "best-thirds-from" )
                , ( "groups", Json.Encode.list Group.encode grps )
                ]


decode : Decoder Candidate
decode =
    field "candidate-type" Json.Decode.string
        |> andThen decodeAnswerTDetails


decodeAnswerTDetails : String -> Decoder Candidate
decodeAnswerTDetails s =
    let
        makeCandidate constructor grps =
            List.head grps
                |> Maybe.map constructor
                |> Result.fromMaybe "Multiple groups for single group position"

        makeBestThirds grps =
            Result.Ok (BestThirdFrom grps)

        decode_ s_ =
            case s_ of
                "first-place" ->
                    Json.Decode.map (makeCandidate FirstPlace)
                        (field "groups" (Json.Decode.list Group.decode))

                "second-place" ->
                    Json.Decode.map (makeCandidate SecondPlace)
                        (field "groups" (Json.Decode.list Group.decode))

                "best-thirds-from" ->
                    Json.Decode.map makeBestThirds
                        (field "groups" (Json.Decode.list Group.decode))

                _ ->
                    Json.Decode.succeed (Result.Err "unknown candidate type")
    in
    decode_ s
        |> Json.Decode.andThen fromResult
