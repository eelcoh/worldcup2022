module Bets.Types.Participant exposing
    ( decode
    , encode
    , init
    , isComplete
    , setAddress
    , setEmail
    , setHowyouknowus
    , setName
    , setPhoneNumber
    )

import Bets.Json.Encode exposing (mStrEnc)
import Bets.Types exposing (Participant, StringField(..))
import Bets.Types.StringField as StringField
import Bool.Extra
import Email
import Json.Decode exposing (Decoder, field, maybe)
import Json.Encode
import Maybe.Extra as M


init : Participant
init =
    Participant initial initial initial initial initial initial


initial =
    Initial ""


setField s =
    if s == "" then
        Error s

    else
        Changed s


setEmailField s =
    if s == "" then
        Error s

    else if Email.isValid s then
        Changed s

    else
        Error s


setName : Participant -> String -> Participant
setName participant name =
    { participant | name = setField name }


setAddress : Participant -> String -> Participant
setAddress participant address =
    { participant | address = setField address }


setPhoneNumber : Participant -> String -> Participant
setPhoneNumber participant phone =
    { participant | phone = setField phone }


setEmail : Participant -> String -> Participant
setEmail participant email =
    { participant | email = setEmailField email }


setHowyouknowus : Participant -> String -> Participant
setHowyouknowus participant howyouknowus =
    { participant | howyouknowus = setField howyouknowus }


isComplete : Participant -> Bool
isComplete participant =
    let
        emailValid =
            StringField.emailIsValid participant.email
    in
    List.map StringField.isValid [ participant.name, participant.address, participant.phone, participant.howyouknowus ]
        |> (::) (StringField.emailIsValid participant.email)
        |> Bool.Extra.all


decode : Decoder Participant
decode =
    let
        decoder s =
            field s Json.Decode.string
                |> maybe
                |> StringField.decode
    in
    Json.Decode.map6 Participant
        (decoder "name")
        (decoder "address")
        (decoder "residence")
        (decoder "phone")
        (decoder "email")
        (decoder "howyouknowus")


encode : Participant -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ( "name", StringField.encode p.name )
        , ( "address", StringField.encode p.address )
        , ( "residence", StringField.encode p.residence )
        , ( "phone", StringField.encode p.phone )
        , ( "email", StringField.encode p.email )
        , ( "howyouknowus", StringField.encode p.howyouknowus )
        ]
