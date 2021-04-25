module Bets.Types.HasQualified exposing (..)

import Bets.Types exposing (HasQualified(..))
import Json.Decode exposing (Decoder)
import Json.Encode


encode : HasQualified -> Json.Encode.Value
encode hasQ =
    Json.Encode.string (toString hasQ)


decode : Decoder HasQualified
decode =
    Json.Decode.string
        |> Json.Decode.map fromString


toString : HasQualified -> String
toString hasQ =
    case hasQ of
        TBD ->
            "TBD"

        In ->
            "In"

        Out ->
            "Out"


fromString : String -> HasQualified
fromString hasQStr =
    case hasQStr of
        "TBD" ->
            TBD

        "In" ->
            In

        "Out" ->
            Out

        _ ->
            TBD
