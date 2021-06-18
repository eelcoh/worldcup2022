module Bets.Types.Round exposing
    ( compare
    , decode
    , encode
    , fromInt
    , fromRoman
    , isSameOrANextRound
    , nextRound
    , toInt
    , toString
    )

import Bets.Types exposing (Round(..))
import Json.Decode exposing (Decoder)
import Json.Encode



-- MODEL


fromRoman : String -> Round
fromRoman r =
    case String.toUpper r of
        "I" ->
            I

        "II" ->
            II

        "III" ->
            III

        "IV" ->
            IV

        "V" ->
            V

        _ ->
            VI


toRoman : Round -> String
toRoman r =
    case r of
        I ->
            "I"

        II ->
            "II"

        III ->
            "III"

        IV ->
            "IV"

        V ->
            "V"

        VI ->
            " VI"


toInt : Round -> Int
toInt r =
    case r of
        I ->
            1

        II ->
            2

        III ->
            3

        IV ->
            4

        V ->
            5

        VI ->
            6


fromInt : Int -> Round
fromInt i =
    case i of
        1 ->
            I

        2 ->
            II

        3 ->
            III

        4 ->
            IV

        5 ->
            V

        _ ->
            VI


toString : Round -> String
toString =
    toInt >> String.fromInt


isSameOrANextRound : Round -> Round -> Bool
isSameOrANextRound r1 r2 =
    toInt r1 >= toInt r2


nextRound : Round -> Maybe Round
nextRound r =
    case r of
        I ->
            Just II

        II ->
            Just III

        III ->
            Just IV

        IV ->
            Just V

        V ->
            Just VI

        VI ->
            Nothing


compare : Round -> Round -> Order
compare r1 r2 =
    Basics.compare (toInt r1) (toInt r2)


encode : Round -> Json.Encode.Value
encode r =
    Json.Encode.int (toInt r)



{-
   decodeRound : Int -> Decoder Round
   decodeRound i =
     succeed (toRound i)

   decode : Decoder Round
   decode =
     "round" := Json.Decode.int `andThen` decodeRound
-}


decode : Decoder Round
decode =
    Json.Decode.int
        |> Json.Decode.map fromInt
