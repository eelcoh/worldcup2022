module Form.Info
    exposing
        ( Msg
        , view
        , update
        )

import Form.Types exposing (Info(..), FormInfoMsg(..))
import Html exposing (div, text, h1, Html, section, p, ul, li)
import Bets.Types exposing (Bet)


type alias Msg =
    FormInfoMsg


update : Msg -> Bet -> ( Bet, Cmd Msg )
update act bet =
    case act of
        NoOps ->
            ( bet, Cmd.none )


view : Info -> Html Msg
view info =
    let
        cardContents =
            case info of
                Intro ->
                    section []
                        [ h1 [] [ text "Hier is de voetbalpool weer!" ]
                        , p [] [ text "Welkom op het formulier voor de voetbalpool. Vul achtereenvolgens de volgende vragen in:" ]
                        , ul []
                            [ li [] [ text "Uitslagen van de wedstrijden voor iedere poule." ]
                            , li [] [ text "De landen die de volgende ronde halen. Het is wat ingewikkeld. Eerst moet je de nummers 1 2 en 3 in de eindstand van een poule voorspellen. De nummers 1 en 2 gaan door. Van de zes nummers 3 gaan er maar vier door. Die moet je ook nog even voorspellen." ]
                            , li [] [ text "Klik vervolgens het schema volledig bij elkaar." ]
                            , li [] [ text "Selecteer je topscorer." ]
                            , li [] [ text "En vertel ons wie je bent" ]
                            ]
                        , p []
                            [ text "Als voorgaande jaren is de inleg "
                            , Html.b [] [ text "vijf euro" ]
                            , text ", en de verdeling 50%, 30% en 20% voor de winnaar, nummer 2 en nummer 3. Bij gelijke stand wordt de opbrengst gedeeld."
                            ]
                        ]

                FirstRoundIntro ->
                    div [] [ text "Hello FirstRoundIntro" ]

                GroupIntro group ->
                    div [] [ text ("Hello GroupIntro " ++ (toString group)) ]

                GroupStandingsIntro group ->
                    div [] [ text ("Hello GroupStandingsIntro " ++ (toString group)) ]

                BestThirdIntro ->
                    div [] [ text "Hello BestThirdIntro" ]

                BracketIntro ->
                    div [] [ h1 [] [ text "Hello BracketIntro" ] ]

                BracketView ->
                    div [] [ text "Hello BracketView" ]

                BracketRoundIntro round ->
                    div [] [ text ("Hello BracketRoundIntro " ++ (toString round)) ]

                TopscorerIntro ->
                    div [] [ text "Hello TopscorerIntro" ]

                AboutYouIntro ->
                    div [] [ text "Hello AboutYouIntro" ]

                ThankYou ->
                    div [] [ text "Hello ThankYou" ]
    in
        div []
            [ cardContents ]
