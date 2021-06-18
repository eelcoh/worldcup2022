module Results.Bets exposing (..)

import Bets.Bet
import Element exposing (paddingXY, px, spaceEvenly, width)
import Html.Attributes exposing (name)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Web exposing (defaultConfig)
import Types exposing (BetSummaryLine, Model, Msg(..), Toggle(..), Token(..), UUID)
import UI.Button
import UI.Style
import UI.Text exposing (simpleText, style)


fetchBets : Cmd Msg
fetchBets =
    Web.get "/bets/" FetchedBets decode


toggleActive : Token -> UUID -> Toggle -> Cmd Msg
toggleActive (Token token) uid toggle =
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }

        path =
            case toggle of
                ToggleActive ->
                    "/bets/activate/" ++ uid

                ToggleInactive ->
                    "/bets/deactivate/" ++ uid
    in
    Web.postWithConfig config path ToggledBetActiveFlag Bets.Bet.decodeBet Json.Encode.null


view : Model Msg -> Element.Element Msg
view model =
    let
        mBets =
            case model.token of
                Success (Token _) ->
                    case model.bets of
                        Success bs ->
                            Just bs

                        _ ->
                            Nothing

                _ ->
                    case model.bets of
                        Success bs ->
                            Just (List.filter (\b -> b.active) bs)

                        _ ->
                            Nothing

        mkLines bets =
            List.map (viewSummaryLine model.token) bets

        items =
            Maybe.withDefault []
                (Maybe.map mkLines mBets)
                |> Element.column [ paddingXY 0 20 ]

        numberOf =
            Maybe.withDefault 0
                (Maybe.map List.length mBets)
                |> String.fromInt

        header =
            UI.Text.displayHeader (numberOf ++ " Inzendingen")
    in
    Element.column
        [ paddingXY 0 20 ]
        [ header
        , items
        ]


viewSummaryLine : WebData Token -> BetSummaryLine -> Element.Element Msg
viewSummaryLine token bet =
    case token of
        Success _ ->
            viewAdminRow bet.name bet.active bet.uuid

        _ ->
            viewRow bet.name


viewAdminRow : String -> Bool -> UUID -> Element.Element Msg
viewAdminRow name active uid =
    let
        ( btnText, btnSemantics, toggle ) =
            if active then
                ( "active", UI.Style.Right, ToggleInactive )

            else
                ( "inactive", UI.Style.Wrong, ToggleActive )
    in
    Element.row
        [ paddingXY 0 20, spaceEvenly, width (px 300) ]
        [ Element.link UI.Text.style { url = "#inzendingen/" ++ uid, label = Element.text name }
        , UI.Button.pill btnSemantics (ToggleBetActiveFlag uid toggle) btnText
        ]


viewRow : String -> Element.Element Msg
viewRow name =
    Element.row
        [ paddingXY 0 20 ]
        [ simpleText name ]


decode : Decoder (List BetSummaryLine)
decode =
    Json.Decode.list decodeLine


decodeLine : Decoder BetSummaryLine
decodeLine =
    Json.Decode.map3 BetSummaryLine
        (field "name" Json.Decode.string)
        (field "active" Json.Decode.bool)
        (field "uuid" Json.Decode.string)
