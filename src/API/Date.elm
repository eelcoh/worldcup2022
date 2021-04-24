module API.Date exposing (..)

import Json.Decode exposing (Decoder)
import Time


decode : Decoder Time.Posix
decode =
    Json.Decode.int
        |> Json.Decode.map Time.millisToPosix
