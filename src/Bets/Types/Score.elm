module Bets.Types.Score exposing (asString, awayScore, decode, encode, encodeMaybe, homeScore, isComplete, merge)

import Bets.Json.Encode exposing (mIntEnc)
import Bets.Types exposing (Score)
import Json.Decode exposing (Decoder, index, map2, maybe)
import Json.Encode


homeScore : Score -> Maybe Int
homeScore ( mInt, _ ) =
    mInt


awayScore : Score -> Maybe Int
awayScore ( _, mInt ) =
    mInt



-- Json handling


score : Maybe Int -> Maybe Int -> Score
score h a =
    ( h, a )


merge : Score -> Score -> Score
merge oldScore newScore =
    let
        ( newHome, newAway ) =
            newScore

        ( oldHome, _ ) =
            oldScore

        home =
            case newHome of
                Just _ ->
                    newHome

                Nothing ->
                    oldHome

        away =
            case newAway of
                Just _ ->
                    newAway

                Nothing ->
                    oldHome

        score_ =
            ( home, away )
    in
    score_


asString : Score -> String
asString ( mH, mA ) =
    let
        str mInt =
            Maybe.map String.fromInt mInt |> Maybe.withDefault "_"
    in
    List.foldr (++) "" [ str mH, "-", str mA ]


isComplete : Maybe Score -> Bool
isComplete mScore =
    case mScore of
        Just ( Just _, Just _ ) ->
            True

        _ ->
            False


decode : Decoder Score
decode =
    map2 score
        (index 0 (maybe Json.Decode.int))
        (index 1 (maybe Json.Decode.int))


encodeMaybe : Maybe Score -> Json.Encode.Value
encodeMaybe mScore =
    case mScore of
        Nothing ->
            Json.Encode.null

        Just s ->
            encode s


encode : Score -> Json.Encode.Value
encode ( mHome, mAway ) =
    Json.Encode.list mIntEnc
        [ mHome
        , mAway
        ]
