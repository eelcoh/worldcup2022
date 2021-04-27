module Results.Ranking exposing (fetchRanking, fetchRankingDetails, recreate, view, viewDetails)

import API.Date as Date
import Bets.Bet
import Bets.View
import Date
import Element exposing (alignRight, column, fill, paddingEach, paddingXY, px, width)
import Element.Events as Events
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web exposing (defaultConfig)
import Types exposing (Activity(..), Model, Msg(..), RankingDetails, RankingGroup, RankingSummary, RankingSummaryLine, RoundScore, Token(..))
import UI.Button
import UI.Style
import UI.Text


fetchRanking : Cmd Msg
fetchRanking =
    Web.get "/bets/ranking/" FetchedRanking decode


fetchRankingDetails : String -> Cmd Msg
fetchRankingDetails uuid =
    Web.get ("/bets/ranking/" ++ uuid) FetchedRankingDetails decodeRankingDetails


recreate : Token -> Cmd Msg
recreate (Token token) =
    -- Web.post "/bets/ranking/initial/" FetchedRanking decode Json.Encode.null
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }
    in
    Web.postWithConfig config "/bets/ranking/initial/" FetchedRanking decode Json.Encode.null


view : Model Msg -> Element.Element Msg
view model =
    let
        items =
            case model.token of
                Success _ ->
                    [ adminBox model
                    , viewRankingGroups model
                    ]

                _ ->
                    [ viewRankingGroups model ]
    in
    Element.column
        []
        items


adminBox : Model Msg -> Element.Element Msg
adminBox _ =
    UI.Button.pill UI.Style.Active RecreateRanking "recreate"


viewRankingGroups : Model Msg -> Element.Element Msg
viewRankingGroups model =
    case model.ranking of
        Success ranking ->
            let
                header =
                    viewRankingHeader

                rank =
                    List.map viewRankingGroup ranking.summary

                datetxt =
                    Element.el
                        [ alignRight, paddingXY 0 10 ]
                        (Element.text ("bijgewerkt op " ++ UI.Text.dateText model.timeZone ranking.time))

                column =
                    (header :: rank) ++ [ datetxt ]
            in
            Element.column
                [ pad 0 0 50 0 ]
                column

        NotAsked ->
            Element.text "nog niet opgevraagd"

        Loading ->
            Element.text "aan het ophalen..."

        Failure _ ->
            UI.Text.error "oei, daar ging iets niet helemaal goed"


viewRankingHeader : Element.Element msg
viewRankingHeader =
    Element.row
        [ paddingXY 0 5 ]
        [ Element.el [ width (px 40), pad 0 10 0 0 ] (Element.text "#")
        , Element.el [ width fill, pad 0 10 0 10 ] (Element.text "Naam")
        , Element.el [ width (px 100), pad 0 20 0 10 ] (Element.text "Punten")
        ]


viewRankingGroup : RankingGroup -> Element.Element Msg
viewRankingGroup grp =
    Element.row
        [ paddingXY 0 5 ]
        [ Element.el [ width (px 40), pad 0 10 0 0 ] (Element.text (String.fromInt grp.pos))
        , viewRankingLines grp.bets
        , Element.el [ width (px 55), pad 0 20 0 10 ] (Element.text (String.fromInt grp.total))
        ]


viewRankingLines : List RankingSummaryLine -> Element.Element Msg
viewRankingLines lines =
    Element.column
        [ width fill, pad 0 10 4 10 ]
        (List.map viewRankingLine lines)


viewRankingLine : RankingSummaryLine -> Element.Element Msg
viewRankingLine line =
    let
        click =
            Events.onClick (ViewRankingDetails line.uuid)
    in
    Element.el [ click ] (Element.text line.name)


viewDetails : Model Msg -> Element.Element Msg
viewDetails model =
    case model.rankingDetails of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure err ->
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success details ->
            Bets.View.view details.bet model.screen


decode : Decoder RankingSummary
decode =
    Json.Decode.map2 RankingSummary
        (field "summary" (Json.Decode.list decodeRankingRankingGroup))
        (field "time" Date.decode)


decodeRankingRankingGroup : Decoder RankingGroup
decodeRankingRankingGroup =
    Json.Decode.map3 RankingGroup
        (field "pos" Json.Decode.int)
        (field "bets" (Json.Decode.list decodeRankingSummaryLine))
        (field "total" Json.Decode.int)


decodeRankingSummaryLine : Decoder RankingSummaryLine
decodeRankingSummaryLine =
    Json.Decode.map5 RankingSummaryLine
        (field "name" Json.Decode.string)
        (field "rounds" (Json.Decode.list decodeRoundScore))
        (field "topscorer" Json.Decode.int)
        (field "total" Json.Decode.int)
        (field "uuid" Json.Decode.string)


decodeRoundScore : Decoder RoundScore
decodeRoundScore =
    Json.Decode.map2 RoundScore
        (field "round" Json.Decode.string)
        (field "points" Json.Decode.int)


decodeRankingDetails : Decoder RankingDetails
decodeRankingDetails =
    Json.Decode.map6 RankingDetails
        (field "name" Json.Decode.string)
        (field "rounds" (Json.Decode.list decodeRoundScore))
        (field "topscorer" Json.Decode.int)
        (field "total" Json.Decode.int)
        (field "uuid" Json.Decode.string)
        (field "bet" Bets.Bet.decode)



--


pad : Int -> Int -> Int -> Int -> Element.Attribute msg
pad top right bottom left =
    paddingEach
        { top = top
        , right = right
        , bottom = bottom
        , left = left
        }
