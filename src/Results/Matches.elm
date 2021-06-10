module Results.Matches exposing (..)

import Authentication
import Bets.Types exposing (Match, Score)
import Bets.Types.Score as S
import Bets.Types.Team
import Element exposing (centerX, height, padding, paddingXY, px, spacing, spacingXY, width)
import Element.Border as Border
import Element.Events as Events
import Http
import Json.Decode exposing (Decoder, andThen, field, maybe)
import Json.Encode
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web exposing (defaultConfig)
import Types exposing (Access(..), Activity(..), MatchResult, MatchResults, Model, Msg(..), Token(..))
import UI.Button
import UI.Button.Score
import UI.Color
import UI.Font
import UI.Style exposing (ButtonSemantics(..))
import UI.Team
import UI.Text


fetchMatchResults : Cmd Msg
fetchMatchResults =
    Web.get "/bets/results/matches/" FetchedMatchResults decode


updateMatchResults : Token -> MatchResult -> Cmd Msg
updateMatchResults (Token token) match =
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }

        url =
            "/bets/results/matches/" ++ match.match

        json =
            encodeMatchResult match
    in
    Web.putWithConfig config url StoredMatchResult decodeMatchResult json


initialise : Token -> Cmd Msg
initialise (Token token) =
    -- Web.post "/bets/ranking/initial/" FetchedRanking decode Json.Encode.null
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }
    in
    Web.postWithConfig config "/bets/results/matches/initial/" FetchedMatchResults decode Json.Encode.null


view : Model Msg -> Element.Element Msg
view model =
    let
        access =
            Authentication.isAuthorised model

        hdr =
            UI.Text.displayHeader "Resultaten Wedstrijden"

        mainView =
            case model.matchResults of
                Success results ->
                    displayMatches access results.results

                NotAsked ->
                    Element.text "nog niet opgevraagd"

                Loading ->
                    Element.text "aan het ophalen..."

                Failure e ->
                    Element.column
                        []
                        [ UI.Text.error "oei, oei, oei, daar ging iets niet helemaal goed"
                        ]

        initialiseButton =
            case access of
                Authorised ->
                    UI.Button.pill Wrong InitialiseMatchResults "initialiseer"

                Unauthorised ->
                    Element.none
    in
    Element.column
        [ paddingXY 0 20 ]
        [ hdr
        , mainView
        , initialiseButton
        ]


displayMatches : Access -> List MatchResult -> Element.Element Msg
displayMatches access matches =
    Element.wrappedRow
        [ padding 10, spacingXY 20 40, centerX ]
        (List.map (displayMatch access) matches)


displayMatch : Access -> MatchResult -> Element.Element Msg
displayMatch access match =
    let
        pts =
            case match.score of
                Just _ ->
                    Just 3

                Nothing ->
                    Nothing

        handler =
            case access of
                Authorised ->
                    Events.onClick (EditMatch match)

                Unauthorised ->
                    Events.onClick NoOp

        home =
            UI.Team.viewTeam <| Just match.homeTeam

        away =
            UI.Team.viewTeam <| Just match.awayTeam

        sc =
            displayScore match.score

        pointsStyle =
            case pts of
                Just 0 ->
                    [ Border.color UI.Color.red, Border.solid ]

                Just 1 ->
                    [ Border.color UI.Color.green, Border.dashed ]

                Just 3 ->
                    [ Border.color UI.Color.green, Border.solid ]

                _ ->
                    [ Border.color UI.Color.grey, Border.solid ]

        semantics =
            case pts of
                Just 3 ->
                    UI.Style.Right

                Just 1 ->
                    UI.Style.Active

                Just 0 ->
                    UI.Style.Wrong

                _ ->
                    UI.Style.Perhaps

        style =
            UI.Style.matchRow semantics
                [ handler
                , UI.Font.button
                , Element.centerY
                , width (px 160)
                , height (px 100)
                , Border.width 5
                ]

        css =
            pointsStyle ++ [ Border.width 5, handler, paddingXY 10 5, spacing 7, width (px 150), height (px 70) ]
    in
    Element.row
        style
        [ home, sc, away ]



-- edit


edit : Model Msg -> Element.Element Msg
edit model =
    let
        access =
            Authentication.isAuthorised model

        items =
            case ( access, model.matchResult ) of
                ( Authorised, Success match ) ->
                    [ displayMatch Unauthorised match
                    , viewKeyboard match
                    , UI.Button.pill UI.Style.Active (CancelMatchResult match) "Wissen"
                    ]

                ( Authorised, Failure e ) ->
                    [ UI.Text.error "(Basics.toString e)"
                    ]

                ( Authorised, NotAsked ) ->
                    [ UI.Text.error "geen wedstrijd geselecteerd" ]

                _ ->
                    [ UI.Text.error "dit is niet de bedoeling" ]
    in
    Element.column [ paddingXY 0 20 ] items


viewKeyboard : MatchResult -> Element.Element Msg
viewKeyboard match =
    let
        toMsg h a =
            Types.UpdateMatchResult { match | score = score_ h a }

        score_ home away =
            mkScore home away
    in
    UI.Button.Score.viewKeyboard NoOp toMsg


mkScore h a =
    Just ( Just h, Just a )



-- json


encodeMatchResult : MatchResult -> Json.Encode.Value
encodeMatchResult match =
    let
        ( homeScore, awayScore, isSet ) =
            case match.score of
                Just ( Just h, Just a ) ->
                    ( h, a, True )

                Just ( Just h, Nothing ) ->
                    ( h, 0, False )

                Just ( Nothing, Just a ) ->
                    ( 0, a, False )

                Just ( Nothing, Nothing ) ->
                    ( 0, 0, False )

                Nothing ->
                    ( 0, 0, False )
    in
    Json.Encode.object
        [ ( "matchResultId", Json.Encode.string match.matchResultId )
        , ( "match", Json.Encode.string match.match )
        , ( "homeTeam", Bets.Types.Team.encode match.homeTeam )
        , ( "awayTeam", Bets.Types.Team.encode match.awayTeam )
        , ( "homeScore", Json.Encode.int homeScore )
        , ( "awayScore", Json.Encode.int awayScore )
        , ( "isSet", Json.Encode.bool isSet )
        ]


decode : Decoder MatchResults
decode =
    Json.Decode.map MatchResults
        (field "matchResults" (Json.Decode.list decodeMatchResult))


decodeMatchResult : Decoder MatchResult
decodeMatchResult =
    field "isSet" Json.Decode.bool
        |> andThen decodeScore
        |> andThen decodeRest


decodeScore : Bool -> Decoder (Maybe ( Maybe Int, Maybe Int ))
decodeScore isSet =
    if isSet then
        Json.Decode.map2 mkScore
            (field "homeScore" Json.Decode.int)
            (field "awayScore" Json.Decode.int)

    else
        Json.Decode.map2 (\_ _ -> Nothing)
            (field "homeScore" Json.Decode.int)
            (field "awayScore" Json.Decode.int)


decodeRest : Maybe Bets.Types.Score -> Decoder MatchResult
decodeRest score =
    Json.Decode.map4 (\mResId m h a -> MatchResult mResId m h a score)
        (field "matchResultId" Json.Decode.string)
        (field "match" Json.Decode.string)
        (field "homeTeam" Bets.Types.Team.decode)
        (field "awayTeam" Bets.Types.Team.decode)



--


displayScore : Maybe Score -> Element.Element Msg
displayScore mScore =
    let
        txt =
            case mScore of
                Just score ->
                    S.asString score

                Nothing ->
                    " _-_ "
    in
    Element.el (UI.Style.score [ width (px 50) ]) (Element.text txt)
