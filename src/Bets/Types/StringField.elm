module Bets.Types.StringField exposing (..)

import Bets.Types exposing (StringField(..))
import Email
import Json.Decode exposing (Decoder)
import Json.Encode


value : StringField -> String
value sf =
    case sf of
        Initial s ->
            s

        Changed s ->
            s

        Error s ->
            s


isValid : StringField -> Bool
isValid sf =
    case sf of
        Changed _ ->
            True

        Error _ ->
            False

        Initial _ ->
            False


emailIsValid : StringField -> Bool
emailIsValid sf =
    case sf of
        Changed s ->
            Email.isValid (String.trim s)

        Error _ ->
            False

        Initial _ ->
            False


decode : Decoder (Maybe String) -> Decoder StringField
decode =
    let
        mapper m =
            case m of
                Just s ->
                    Changed s

                Nothing ->
                    Error ""
    in
    Json.Decode.map mapper


encode : StringField -> Json.Encode.Value
encode sf =
    case sf of
        Changed str ->
            Json.Encode.string (String.trim str)

        Error _ ->
            Json.Encode.null

        Initial str ->
            Json.Encode.string (String.trim str)
