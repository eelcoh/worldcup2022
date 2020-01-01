module Bets.Types.Stadium exposing (decode, encode)

import Bets.Types exposing (Stadium)
import Json.Decode exposing (Decoder, field)
import Json.Encode


decode : Decoder Stadium
decode =
    Json.Decode.map2 Stadium
        (field "stadium" Json.Decode.string)
        (field "town" Json.Decode.string)


encode : Stadium -> Json.Encode.Value
encode { stadium, town } =
    Json.Encode.object
        [ ( "stadium", Json.Encode.string stadium )
        , ( "town", Json.Encode.string town )
        ]
