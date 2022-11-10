module Form.Info exposing
    ( Msg
    , update
    , view
    )

import Bets.Types exposing (Bet)
import Bets.Types.Group as Group
import Bets.Types.Round as Round
import Element exposing (fill, spacing, width)
import Types exposing (FormInfoMsg(..), Info(..))
import UI.Page exposing (page)
import UI.Style
import UI.Text


type alias Msg =
    FormInfoMsg


update : Msg -> Bet -> ( Bet, Cmd Msg )
update act bet =
    case act of
        NoOps ->
            ( bet, Cmd.none )


view : Info -> Element.Element Msg
view info =
    let
        cardContents =
            case info of
                Intro ->
                    page "introduction" introduction

                FirstRoundIntro ->
                    Element.text "Hello FirstRoundIntro"

                GroupIntro group ->
                    Element.text ("Hello GroupIntro " ++ Group.toString group)

                GroupStandingsIntro group ->
                    Element.text ("Hello GroupStandingsIntro " ++ Group.toString group)

                BestThirdIntro ->
                    Element.text "Hello BestThirdIntro"

                BracketIntro ->
                    UI.Text.displayHeader "Hello BracketIntro"

                BracketView ->
                    Element.text "Hello BracketView"

                BracketRoundIntro round_ ->
                    Element.text ("Hello BracketRoundIntro " ++ Round.toString round_)

                TopscorerIntro ->
                    Element.text "Hello TopscorerIntro"

                AboutYouIntro ->
                    Element.text "Hello AboutYouIntro"

                ThankYou ->
                    Element.text "Hello ThankYou"
    in
    cardContents


introduction : List (Element.Element Msg)
introduction =
    [ UI.Text.displayHeader "Hier is de voetbalpool weer!"
    , Element.paragraph (UI.Style.introduction [ spacing 16 ])
        [ Element.text "Welkom op het formulier voor de voetbalpool. Vul achtereenvolgens de volgende vragen in:" ]
    , Element.column (UI.Style.introduction [ spacing 16 ])
        [ UI.Text.bulletText "Uitslagen van de wedstrijden voor iedere poule."
        , UI.Text.bulletText "De landen die de volgende ronde halen. De nummers 1 en 2 gaan van een poule gaan door. "
        , UI.Text.bulletText "Klik vervolgens het schema volledig bij elkaar."
        , UI.Text.bulletText "Selecteer je topscorer."
        , UI.Text.bulletText "En vertel ons wie je bent"
        ]
    , Element.paragraph (UI.Style.introduction [])
        [ Element.text "Als voorgaande jaren is de inleg "
        , UI.Text.boldText "vijf euro"
        , Element.text ", en de verdeling 50%, 30% en 20% voor de winnaar, nummer 2 en nummer 3. Bij gelijke stand wordt de opbrengst gedeeld."
        ]
    , attribution
    ]


attribution : Element.Element msg
attribution =
    Element.paragraph (UI.Style.attribution [ spacing 7 ])
        [ Element.text "Icons made by "
        , Element.link []
            { url = "https://www.flaticon.com/authors/freepik"
            , label = Element.text "Freepik"
            }
        , Element.text " from "
        , Element.link []
            { url = "https://www.flaticon.com/"
            , label = Element.text "www.flaticon.com."
            }
        ]
