module Bets.Types.Group exposing (decode, encode, toGroup, toString)

import Bets.Types exposing (Group(..))
import Json.Decode exposing (Decoder)
import Json.Encode


toString : Group -> String
toString grp =
    case grp of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        H ->
            "H"


toGroup : String -> Group
toGroup s =
    case s of
        "A" ->
            A

        "B" ->
            B

        "C" ->
            C

        "D" ->
            D

        "E" ->
            E

        "F" ->
            F

        "G" ->
            G

        _ ->
            H


encode : Group -> Json.Encode.Value
encode grp =
    Json.Encode.string (toString grp)


decode : Decoder Group
decode =
    Json.Decode.string
        |> Json.Decode.map toGroup
