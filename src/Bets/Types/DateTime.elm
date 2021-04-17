module Bets.Types.DateTime exposing (date, decode, encode, time, toPosix)

import Bets.Types exposing (Date, Time)
import Json.Decode exposing (Decoder)
import Json.Encode
import Time
import Time.Extra as TimeX
import TimeZone


encode : Time.Posix -> Json.Encode.Value
encode t =
    Json.Encode.int (Time.posixToMillis t)


decode : Decoder Time.Posix
decode =
    Json.Decode.int
        |> Json.Decode.map Time.millisToPosix


toPosix : Date -> Time -> Time.Posix
toPosix { year, month, day } { hour, minutes } =
    TimeX.Parts year month day hour minutes 0 0
        |> TimeX.partsToPosix cet


date : Int -> Time.Month -> Int -> Date
date year month day =
    Date year month day


time : Int -> Int -> Time
time hour minute =
    Time hour minute


cet : Time.Zone
cet =
    TimeZone.europe__amsterdam ()
