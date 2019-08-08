module Form.Info exposing
    ( Msg
    , update
    , view
    )

import Bets.Types exposing (Bet)
import Bets.Types.Group as Group
import Bets.Types.Round as Round
import Element
import Element.Attributes exposing (height, padding, paddingBottom, paddingTop, px, spacing, width)
import Form.Types exposing (FormInfoMsg(..), Info(..))
import UI.Style
import UI.Text


type alias Msg =
    FormInfoMsg


update : Msg -> Bet -> ( Bet, Cmd Msg )
update act bet =
    case act of
        NoOps ->
            ( bet, Cmd.none )


view : Info -> Element.Element UI.Style.Style variation msg
view info =
    let
        cardContents =
            case info of
                Intro ->
                    Element.column UI.Style.None [] introduction

                FirstRoundIntro ->
                    UI.Text.simpleText "Hello FirstRoundIntro"

                GroupIntro group ->
                    UI.Text.simpleText ("Hello GroupIntro " ++ Group.toString group)

                GroupStandingsIntro group ->
                    UI.Text.simpleText ("Hello GroupStandingsIntro " ++ Group.toString group)

                BestThirdIntro ->
                    UI.Text.simpleText "Hello BestThirdIntro"

                BracketIntro ->
                    UI.Text.displayHeader "Hello BracketIntro"

                BracketView ->
                    UI.Text.simpleText "Hello BracketView"

                BracketRoundIntro round_ ->
                    UI.Text.simpleText ("Hello BracketRoundIntro " ++ Round.toString round_)

                TopscorerIntro ->
                    UI.Text.simpleText "Hello TopscorerIntro"

                AboutYouIntro ->
                    UI.Text.simpleText "Hello AboutYouIntro"

                ThankYou ->
                    UI.Text.simpleText "Hello ThankYou"
    in
    cardContents


introduction : List (Element.Element UI.Style.Style variation msg)
introduction =
    [ UI.Text.displayHeader "Hier is de voetbalpool weer!"
    , Element.paragraph UI.Style.Introduction
        [ width (px 600), spacing 7 ]
        [ Element.text "Welkom op het formulier voor de voetbalpool. Vul achtereenvolgens de volgende vragen in:" ]
    , Element.column UI.Style.None
        [ width (px 600), spacing 7, paddingTop 10, paddingBottom 10 ]
        [ UI.Text.bulletText "Uitslagen van de wedstrijden voor iedere poule."
        , UI.Text.bulletText "De landen die de volgende ronde halen. Het is wat ingewikkeld. Eerst moet je de nummers 1 2 en 3 in de eindstand van een poule voorspellen. De nummers 1 en 2 gaan door. Van de zes nummers 3 gaan er maar vier door. Die moet je ook nog even voorspellen."
        , UI.Text.bulletText "Klik vervolgens het schema volledig bij elkaar."
        , UI.Text.bulletText "Selecteer je topscorer."
        , UI.Text.bulletText "En vertel ons wie je bent"
        ]
    , Element.paragraph UI.Style.Introduction
        [ width (px 600), spacing 7 ]
        [ UI.Text.simpleText "Als voorgaande jaren is de inleg "
        , UI.Text.boldText "vijf euro"
        , UI.Text.simpleText ", en de verdeling 50%, 30% en 20% voor de winnaar, nummer 2 en nummer 3. Bij gelijke stand wordt de opbrengst gedeeld."
        ]
    ]
