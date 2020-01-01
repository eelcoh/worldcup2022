module Bets.Types.Draw exposing (decode, encode, init, isComplete, team)

import Bets.Types exposing (Draw, DrawID, Team)
import Bets.Types.Team as T
import Json.Decode exposing (Decoder, map2, maybe)
import Json.Encode


team : Draw -> Maybe Team
team ( _, mTeam ) =
    mTeam


draw : DrawID -> Maybe Team -> Draw
draw drawId mTeam =
    ( drawId, mTeam )


init : DrawID -> Draw
init drawId =
    ( drawId, Nothing )


isComplete : Draw -> Bool
isComplete ( _, mTeam ) =
    T.isComplete mTeam


decode : Decoder Draw
decode =
    map2 draw
        (Json.Decode.field "draw" Json.Decode.string)
        (Json.Decode.field "team" (maybe T.decode))


encode : Draw -> Json.Encode.Value
encode ( drawId, mTeam ) =
    Json.Encode.object
        [ ( "draw", Json.Encode.string drawId )
        , ( "team", T.encodeMaybe mTeam )
        ]
