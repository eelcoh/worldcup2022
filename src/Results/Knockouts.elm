module Results.Knockouts exposing (..)

import Bets.Types exposing (HasQualified(..), Round(..), Team)
import Bets.Types.Bracket
import Bets.Types.HasQualified as HasQualified
import Bets.Types.Round as Round exposing (isSameOrANextRound)
import Bets.Types.Team
import Element exposing (alignLeft, alignRight, column, height, padding, paddingXY, px, row, spacing, spacingXY, width)
import Http
import Json.Decode exposing (Decoder, andThen, field, keyValuePairs, maybe)
import Json.Encode
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Web exposing (defaultConfig)
import Tuple
import Types exposing (Access(..), Activity(..), DataStatus(..), KnockoutsResults, Model, Msg(..), Qualified(..), TeamRounds, Token(..))
import UI.Button
import UI.Style
import UI.Team
import UI.Text


fetchKnockoutsResults : Cmd Msg
fetchKnockoutsResults =
    Web.get "/bets/results/knockouts/" FetchedKnockoutsResults decode


inititaliseKnockoutsResults : Token -> Cmd Msg
inititaliseKnockoutsResults (Token token) =
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }

        url =
            "/bets/results/knockouts/initial/"

        json =
            Json.Encode.object []
    in
    Web.postWithConfig config url FetchedKnockoutsResults decode json


storeKnockoutsResults : Token -> KnockoutsResults -> Cmd Msg
storeKnockoutsResults (Token token) results =
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }

        url =
            "/bets/results/knockouts/"

        json =
            encode results
    in
    Web.postWithConfig config url StoredKnockoutsResults decode json



-- update


update : Round -> HasQualified -> Team -> WebData KnockoutsResults -> WebData KnockoutsResults
update round qualified team results =
    case results of
        Success results_ ->
            updateKnockoutsResults round qualified team results_
                |> RemoteData.succeed

        _ ->
            results


updateKnockoutsResults : Round -> HasQualified -> Team -> KnockoutsResults -> KnockoutsResults
updateKnockoutsResults round qualified team results =
    let
        teamResults =
            List.map (updateTeamRounds round qualified team) results.teams
    in
    { results | teams = teamResults }


updateTeamRounds : Round -> HasQualified -> Team -> ( String, TeamRounds ) -> ( String, TeamRounds )
updateTeamRounds round qualified team ( teamID, teamRounds ) =
    if team.teamID == teamID then
        let
            roundsQualified =
                updateRoundsQualified round qualified teamRounds.roundsQualified
        in
        ( teamID, { teamRounds | roundsQualified = roundsQualified } )

    else
        ( teamID, teamRounds )


updateRoundsQualified : Round -> HasQualified -> List ( Round, HasQualified ) -> List ( Round, HasQualified )
updateRoundsQualified round qualified teamRounds =
    List.map (updateRoundQualified round qualified) teamRounds


updateRoundQualified : Round -> HasQualified -> ( Round, HasQualified ) -> ( Round, HasQualified )
updateRoundQualified round qualified ( r, q ) =
    let
        cmp =
            Round.compare round r
    in
    case ( qualified, cmp ) of
        ( In, LT ) ->
            ( r, TBD )

        ( In, GT ) ->
            ( r, In )

        ( TBD, LT ) ->
            ( r, TBD )

        ( TBD, GT ) ->
            ( r, q )

        ( Out, LT ) ->
            ( r, Out )

        ( Out, GT ) ->
            ( r, q )

        ( _, EQ ) ->
            ( r, qualified )



-- View


view : Model Msg -> Element.Element Msg
view model =
    let
        auth =
            case model.token of
                Success _ ->
                    Authorised

                _ ->
                    Unauthorised

        items =
            case ( auth, model.knockoutsResults ) of
                ( Authorised, Fresh (Success results) ) ->
                    [ viewKnockoutsResults auth results
                    , UI.Button.pill UI.Style.Inactive UpdateKnockoutsResults "Update"
                    , UI.Button.pill UI.Style.Potential InitialiseKnockoutsResults "Initialiseer"
                    ]

                ( Authorised, Filthy (Success results) ) ->
                    [ viewKnockoutsResults auth results
                    , UI.Button.pill UI.Style.Active UpdateKnockoutsResults "Update"
                    , UI.Button.pill UI.Style.Potential InitialiseKnockoutsResults "Initialiseer"
                    ]

                ( Authorised, err ) ->
                    let
                        e =
                            Debug.toString err
                    in
                    [ Element.text e
                    , UI.Button.pill UI.Style.Inactive UpdateKnockoutsResults "Update"
                    , UI.Button.pill UI.Style.Potential InitialiseKnockoutsResults "Initialiseer"
                    ]

                ( Unauthorised, Fresh (Success results) ) ->
                    [ viewKnockoutsResults auth results ]

                ( _, _ ) ->
                    [ Element.text "..." ]
    in
    Element.column
        [ spacingXY 0 14 ]
        items



-- {teams : List (TeamId, { team: Team, roundsQualified : List (Round, HasQualified)})}


viewKnockoutsResults : Access -> KnockoutsResults -> Element.Element Msg
viewKnockoutsResults auth results =
    let
        viewTeamRounds ( _, teamRounds ) =
            viewKnockoutsPerTeam teamRounds
    in
    Element.column
        [ spacingXY 0 20 ]
        (List.map (Tuple.second >> viewKnockoutsPerTeam) results.teams)


viewKnockoutsPerTeam : TeamRounds -> Element.Element Msg
viewKnockoutsPerTeam { team, roundsQualified } =
    let
        teamBtn =
            UI.Team.viewTeam (Just team)

        roundButtons =
            List.map (viewRoundButtons team) roundsQualified
    in
    Element.row
        [ padding 20, spacing 20 ]
        (teamBtn :: roundButtons)


viewRoundButtons : Team -> ( Round, HasQualified ) -> Element.Element Msg
viewRoundButtons team ( rnd, qualified ) =
    let
        succeeded =
            ChangeQualify rnd In team

        failed =
            ChangeQualify rnd Out team

        unknown =
            ChangeQualify rnd TBD team

        btnSemantic q =
            let
                h =
                    toHasQualified qualified
            in
            if q == h then
                UI.Style.Selected

            else
                UI.Style.Potential

        toHasQualified q =
            case q of
                In ->
                    Did

                Out ->
                    DidNot

                TBD ->
                    NotYet
    in
    Element.column
        [ spacing 20 ]
        [ Element.el [ width (px 40) ] (UI.Text.simpleText (Round.toString rnd))
        , UI.Button.pill (btnSemantic Did) succeeded "In"
        , UI.Button.pill (btnSemantic NotYet) unknown "TBD"
        , UI.Button.pill (btnSemantic DidNot) failed "Out"
        ]


mkTeamButton : Types.Qualified -> (Team -> Msg) -> Team -> Element.Element Msg
mkTeamButton qualified msg team =
    let
        c =
            case qualified of
                Did ->
                    UI.Style.Selected

                DidNot ->
                    UI.Style.Wrong

                _ ->
                    UI.Style.Potential

        act =
            msg team
    in
    UI.Button.teamButton c act team


encode : KnockoutsResults -> Json.Encode.Value
encode results =
    Json.Encode.object
        [ ( "teams", Json.Encode.object (List.map encodeTeamQ results.teams) )
        ]



-- Json.Encode.object
--     [ ( "teams", Json.Encode.object (List.map encodeTeamQ results.teams) )
--     ]
-- encodeTeamQs : List ( String, TeamRounds ) -> Json.Encode.Value
-- encodeTeamQs ( teamId, teamrounds ) =
--     Json.Encode.list encodeTeamQ


encodeTeamQ : ( String, TeamRounds ) -> ( String, Json.Encode.Value )
encodeTeamQ ( teamId, teamrounds ) =
    ( teamId, encodeTeamRounds teamrounds )


encodeTeamRounds : TeamRounds -> Json.Encode.Value
encodeTeamRounds teamrounds =
    Json.Encode.object
        [ ( "team", Bets.Types.Team.encode teamrounds.team )
        , ( "roundsQualified", encodeRoundSQualified teamrounds.roundsQualified )
        ]


encodeRoundSQualified : List ( Round, HasQualified ) -> Json.Encode.Value
encodeRoundSQualified teamrounds =
    List.map roundQualifiedToString teamrounds
        |> Json.Encode.object


roundQualifiedToString : ( Round, HasQualified ) -> ( String, Json.Encode.Value )
roundQualifiedToString ( r, rQ ) =
    ( String.fromInt <| Round.toInt r, HasQualified.encode rQ )



-- encodeKnockouts : Knockouts -> Json.Encode.Value
-- encodeKnockouts kos =
--     Json.Encode.object
--         [ ( "teamsIn", teamsEncode kos.teamsIn )
--         , ( "teamsOut", teamsEncode kos.teamsOut )
--         ]
-- teamsEncode : List Team -> Json.Encode.Value
-- teamsEncode teams =
--     List.map Bets.Types.Team.encode teams
--         |> Json.Encode.list


decode : Decoder KnockoutsResults
decode =
    Json.Decode.map KnockoutsResults
        (field "teams" (Json.Decode.keyValuePairs decodeTeamRounds))


decodeTeamRounds : Decoder TeamRounds
decodeTeamRounds =
    let
        roundFromString r =
            String.toInt r
                |> Maybe.withDefault 6
                |> Round.fromInt

        decoder_ =
            keyValuePairs HasQualified.decode
                |> Json.Decode.map (List.map (\( a, b ) -> ( roundFromString a, b )))
    in
    Json.Decode.map2 TeamRounds
        (field "team" Bets.Types.Team.decode)
        (field "roundsQualified" decoder_)



-- (field "roundsQualified" decodeRoundsQualified)
-- decodeRoundsQualified : Decoder (List ( Round, HasQualified ))
-- decodeRoundsQualified =
--     Json.Decode.map2 Tuple.pair
--         (Json.Decode.index 0 Round.decode)
--         (Json.Decode.index 0 HasQualified.decode)
--         |> Json.Decode.list
-- let
--     Json.Decode.list
-- in
-- Json.Decode.keyValuePairs HasQualified.decode
--     |> Json.Decode.map mkRoundsQualified
-- mkRoundsQualified : List ( Int, HasQualified ) -> List ( Round, HasQualified )
-- mkRoundsQualified list =
--     List.filterMap parseRoundQualifiedPair list
-- parseRoundQualifiedPair : ( Int, HasQualified ) -> Maybe ( Round, HasQualified )
-- parseRoundQualifiedPair ( rStr, hasQ ) =
--     Round.fromInt rStr
--         |> Maybe.map (\r -> ( r, hasQ ))
-- decoder : Decoder (List (Foo, Bar))
-- decoder =
--   keyValuePairs barDecoder
--     |> Decode.map parseKeys
-- NORMAL FUNCTIONS BELOW
-- parseKeys : List (String, Bar) -> List (Foo, Bar)
-- parseKeys list =
--   List.filterMap parseKVPair list
-- parseKVPair : (String, Bar) -> Maybe (Foo, Bar)
-- parseKVPair (str, value) =
--   parseKey str
--     |> Maybe.map (\key -> (key, value))
-- parseKey : String -> Maybe Foo
-- parseKey str =
--   case str of
--     "afoo" -> Just AFoo
--     "bfoo" -> Just BFoo
--     _ -> Nothing
-- Decoder (List (String, Bar)) -> Decoder (List (Foo, Bar))
-- decodeRoundQualified : Decoder ( Round, HasQualified )
-- decodeRoundQualified =
--     decodeRound
--         |> Json.Decode.andThen decodeRoundHasQualified
-- decodeRoundHasQualified : Round -> Decoder ( Round, HasQualified )
-- decodeRoundHasQualified r =
--     Bets.Types.Bracket.decodeHasQualified
--         |> Json.Decode.andThen (decodeRoundHasQualified2 r)
-- decodeRoundHasQualified2 : Round -> HasQualified -> Decoder ( Round, HasQualified )
-- decodeRoundHasQualified2 r q =
--     Json.Decode.succeed ( r, q )
-- decodeRound : String -> Decoder Round
-- decodeRound str =
--     case str of
--         "I" ->
--             Json.Decode.succeed I
--         "II" ->
--             Json.Decode.succeed II
--         "III" ->
--             Json.Decode.succeed III
--         "IV" ->
--             Json.Decode.succeed IV
--         "V" ->
--             Json.Decode.succeed V
--         "VI" ->
--             Json.Decode.succeed VI
--         s ->
--             Json.Decode.fail ("Expected a Round, got : " ++ s)
