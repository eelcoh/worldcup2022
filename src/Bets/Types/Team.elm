module Bets.Types.Team exposing
    ( decode
    , display
    , displayFull
    , encode
    , encodeMaybe
    , equal
    , flag
    , flagUrl
    , isComplete
    , log
    , mdisplay
    , mdisplayFull
    , mdisplayID
    , team
    )

import Bets.Types exposing (Group(..), Team, TeamID)
import Html exposing (..)
import Html.Attributes exposing (src)
import Json.Decode exposing (Decoder)
import Json.Encode
import Maybe.Extra as M


team : TeamID -> String -> Team
team teamID teamName =
    Team teamID teamName


display : Team -> String
display t =
    t.teamID


displayFull : Team -> String
displayFull t =
    t.teamName


equal : Team -> Team -> Bool
equal t1 t2 =
    (t1.teamID == t2.teamID) && (t1.teamName == t2.teamName)


mdisplayFull : Maybe Team -> String
mdisplayFull mteam =
    Maybe.map displayFull mteam
        |> Maybe.withDefault ""


mdisplay : Maybe Team -> String
mdisplay mteam =
    Maybe.map display mteam
        |> Maybe.withDefault ""


mdisplayID : Maybe Team -> String
mdisplayID mteam =
    Maybe.map .teamID mteam
        |> Maybe.withDefault "..."


log : Team -> String
log t =
    t.teamName


flag : Maybe Team -> Html msg
flag mteam =
    let
        uri t =
            flagUri 2 ++ t.teamID ++ ".png"

        default =
            team "xyz" ""
    in
    case mteam of
        Nothing ->
            img [ src (uri default) ] []

        Just t ->
            img [ src (uri t) ] []


flagUrlRound teamID =
    case teamID of
        "SUI" ->
            Just "assets/svg/switzerland.svg"

        "TUR" ->
            Just "assets/svg/turkey.svg"

        "ITA" ->
            Just "assets/svg/italy.svg"

        "WAL" ->
            Just "assets/svg/wales.svg"

        "DEN" ->
            Just "assets/svg/denmark.svg"

        "FIN" ->
            Just "assets/svg/finland.svg"

        "BEL" ->
            Just "assets/svg/belgium.svg"

        "RUS" ->
            Just "assets/svg/russia.svg"

        "NED" ->
            Just "assets/svg/netherlands.svg"

        "UKR" ->
            Just "assets/svg/ukraine.svg"

        "AUT" ->
            Just "assets/svg/austria.svg"

        "PLA" ->
            Just "assets/svg/playoff.svg"

        "ENG" ->
            Just "assets/svg/england.svg"

        "CRO" ->
            Just "assets/svg/croatia.svg"

        "PAD" ->
            Just "assets/svg/playoff.svg"

        "CZE" ->
            Just "assets/svg/czech-republic.svg"

        "ESP" ->
            Just "assets/svg/spain.svg"

        "SWE" ->
            Just "assets/svg/sweden.svg"

        "POL" ->
            Just "assets/svg/republic-of-poland.svg"

        "PAC" ->
            Just "assets/svg/playoff.svg"

        "PAF" ->
            Just "assets/svg/playoff.svg"

        "POR" ->
            Just "assets/svg/portugal.svg"

        "FRA" ->
            Just "assets/svg/france.svg"

        "GER" ->
            Just "assets/svg/germany.svg"

        _ ->
            Nothing


flagUrl : Maybe Team -> String
flagUrl mteam =
    let
        uri t =
            Maybe.withDefault
                (flagUri 2 ++ t.teamID ++ ".png")
                (flagUrlRound t.teamID)

        default =
            team "xyz" ""
    in
    case mteam of
        Nothing ->
            uri default

        Just t ->
            uri t


flagUri : Int -> String
flagUri size =
    if (size > 5) || (size < 0) then
        "http://img.fifa.com/images/flags/3/"

    else
        "http://img.fifa.com/images/flags/" ++ String.fromInt size ++ "/"


isComplete : Maybe Team -> Bool
isComplete mTeam =
    M.isJust mTeam


decode : Decoder Team
decode =
    Json.Decode.map2 Team
        (Json.Decode.field "teamID" Json.Decode.string)
        (Json.Decode.field "teamName" Json.Decode.string)


encodeMaybe : Maybe Team -> Json.Encode.Value
encodeMaybe mTeam =
    case mTeam of
        Just t ->
            encode t

        Nothing ->
            Json.Encode.null


encode : Team -> Json.Encode.Value
encode t =
    Json.Encode.object
        [ ( "teamID", Json.Encode.string t.teamID )
        , ( "teamName", Json.Encode.string t.teamName )
        ]
