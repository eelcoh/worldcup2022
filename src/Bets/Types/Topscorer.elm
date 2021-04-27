module Bets.Types.Topscorer exposing
    ( decode
    , encode
    , equal
    , getPlayer
    , getTeam
    , isComplete
    , setPlayer
    , setTeam
    )

import Bets.Json.Encode exposing (mStrEnc)
import Bets.Types exposing (Player, Team, Topscorer)
import Bets.Types.Team as Team
import Json.Decode exposing (Decoder, field, maybe)
import Json.Encode
import Maybe.Extra as M
import Tuple exposing (first, second)


getTeam : Topscorer -> Maybe Team
getTeam ts =
    second ts



{-
   Sets or toggles the team. As team changes, topscorer player is always cleared.
-}


setTeam : Topscorer -> Team -> Topscorer
setTeam ts team =
    case getTeam ts of
        Nothing ->
            ( Nothing, Just team )

        Just t ->
            if t == team then
                ( Nothing, Nothing )

            else
                ( Nothing, Just team )


getPlayer : Topscorer -> Maybe Player
getPlayer ts =
    first ts


setPlayer : Topscorer -> Player -> Topscorer
setPlayer ( mPlayer, mTeam ) player =
    case mPlayer of
        Nothing ->
            ( Just player, mTeam )

        -- we should actually check whether that is possible. Someday.
        Just p ->
            if p == player then
                ( Nothing, mTeam )

            else
                ( Just player, mTeam )


topscorer : Maybe String -> Maybe Team -> Topscorer
topscorer mName mTeam =
    ( mName, mTeam )


isComplete : Topscorer -> Bool
isComplete ( mName, mTeam ) =
    M.isJust mName && M.isJust mTeam


encode : Topscorer -> Json.Encode.Value
encode ( mName, mTeam ) =
    Json.Encode.object
        [ ( "name", mStrEnc mName )
        , ( "team", Team.encodeMaybe mTeam )
        ]


decode : Decoder Topscorer
decode =
    Json.Decode.map
        (\x -> topscorer x.name x.team)
        decodeTSObj


type alias TopscorerObject =
    { name : Maybe String
    , team : Maybe Team
    }


decodeTSObj : Decoder TopscorerObject
decodeTSObj =
    Json.Decode.map2
        TopscorerObject
        (field "name" (maybe Json.Decode.string))
        (field "team" (maybe Team.decode))


equal : Topscorer -> Topscorer -> Bool
equal ( ts1, t1 ) ( ts2, t2 ) =
    let
        teamEqual =
            Maybe.map2 Team.equal t1 t2

        topscorerEqual =
            Maybe.map2 (==) ts1 ts2
    in
    Maybe.map2 (&&) teamEqual topscorerEqual
        |> Maybe.withDefault False
