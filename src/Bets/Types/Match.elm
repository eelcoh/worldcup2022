module Bets.Types.Match exposing
    ( awayTeam
    , decode
    , encode
    , homeTeam
    , isComplete
    , match
    , setTeamMatch
    , teamsInMatch
    , unsetTeamMatch
    )

import Bets.Types exposing (Date, Draw, Match(..), MatchID, Stadium, Team, Time)
import Bets.Types.DateTime as DateTime
import Bets.Types.Draw
import Bets.Types.Stadium
import Json.Decode exposing (Decoder)
import Json.Encode


homeTeam : Match -> Maybe Team
homeTeam (Match _ d _ _ _) =
    Bets.Types.Draw.team d


awayTeam : Match -> Maybe Team
awayTeam (Match _ _ d _ _) =
    Bets.Types.Draw.team d


match : MatchID -> Draw -> Draw -> Date -> Time -> Stadium -> Match
match matchID home away date time stadium =
    let
        dt =
            DateTime.toPosix date time
    in
    Match matchID home away dt stadium



{-
   Get the list of teams in a match.
-}


teamsInMatch : Match -> List Team
teamsInMatch (Match _ ( _, mHome ) ( _, mAway ) _ _) =
    List.filterMap identity [ mHome, mAway ]


isComplete : Match -> Maybe Team -> Bool
isComplete m mTeam =
    case mTeam of
        Just t ->
            let
                teams =
                    teamsInMatch m
            in
            (List.length teams == 2) && List.member t teams

        Nothing ->
            False



{-
   Sets the draw (slot) of a match if the drawId is the same.
-}


setTeamMatch : Match -> Draw -> Match
setTeamMatch (Match matchID home away dt stadium) ( drawId, mTeam ) =
    let
        updateDraw (( dId, _ ) as draw) =
            if dId == drawId then
                ( drawId, mTeam )

            else
                draw

        newHome =
            updateDraw home

        newAway =
            updateDraw away
    in
    Match matchID newHome newAway dt stadium



{-
   Takes a match, and a team, and removes the team if it is participant.
   Keeps everything else in tact.
-}


unsetTeamMatch : Match -> Team -> Match
unsetTeamMatch (Match matchID home away dt stadium) team =
    let
        cleanDraw (( drawId, mTeam ) as draw) =
            case mTeam of
                Just t ->
                    if team == t then
                        ( drawId, Nothing )

                    else
                        draw

                _ ->
                    draw

        newHome =
            cleanDraw home

        newAway =
            cleanDraw away
    in
    Match matchID newHome newAway dt stadium


encode : Match -> Json.Encode.Value
encode (Match matchID home away dt stadium) =
    Json.Encode.object
        [ ( "matchID", Json.Encode.string matchID )
        , ( "home", Bets.Types.Draw.encode home )
        , ( "away", Bets.Types.Draw.encode away )
        , ( "time", DateTime.encode dt )
        , ( "stadium", Bets.Types.Stadium.encode stadium )
        ]


decode : Decoder Match
decode =
    Json.Decode.map5 Match
        (Json.Decode.field "matchID" Json.Decode.string)
        (Json.Decode.field "home" Bets.Types.Draw.decode)
        (Json.Decode.field "away" Bets.Types.Draw.decode)
        (Json.Decode.field "time" DateTime.decode)
        (Json.Decode.field "stadium" Bets.Types.Stadium.decode)
