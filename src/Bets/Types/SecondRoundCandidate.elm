module Bets.Types.SecondRoundCandidate exposing (decode, encode, get)

import Bets.Init.Euro2020.Tournament as Tournament
import Bets.Types exposing (Bracket(..), Group, SecondRoundCandidate(..), Slot, Team)
import Bets.Types.Group as Group
import Json.Decode exposing (Decoder, andThen, field, map2, map4, maybe)
import Json.Encode


get : SecondRoundCandidate -> List ( Group, Team )
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


encode : SecondRoundCandidate -> Json.Encode.Value
encode spot =
    case spot of
        FirstPlace grp ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "first-place" )
                , ( "group", Group.encode grp )
                ]

        SecondPlace grp ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "second-place" )
                , ( "group", Group.encode grp )
                ]

        BestThirdFrom grps ->
            Json.Encode.object
                [ ( "candidate-type", Json.Encode.string "best-thirds-from" )
                , ( "groups", Json.Encode.list Group.encode grps )
                ]


decode : Decoder SecondRoundCandidate
decode =
    field "candidate-type" Json.Decode.string
        |> andThen decodeAnswerTDetails


decodeAnswerTDetails : String -> Decoder SecondRoundCandidate
decodeAnswerTDetails s =
    case s of
        "first-place" ->
            Json.Decode.map FirstPlace
                (field "group" Group.decode)

        "second-place" ->
            Json.Decode.map SecondPlace
                (field "group" Group.decode)

        "best-thirds-from" ->
            Json.Decode.map BestThirdFrom
                (field "group" (Json.Decode.list Group.decode))

        _ ->
            Json.Decode.fail "unknown type of candidate"
