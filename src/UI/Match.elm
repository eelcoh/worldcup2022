module UI.Match exposing (..)

import Bets.Types exposing (Match, Score)
import Bets.Types.Match as Match
import Element exposing (centerX, centerY, height, padding, paddingXY, px, spacing, width)
import Element.Border as Border
import Types exposing (Msg(..))
import UI.Button.Score exposing (displayScore)
import UI.Color as Color
import UI.Font
import UI.Style
import UI.Team


display : Match -> Maybe Score -> Element.Attribute msg -> UI.Style.ButtonSemantics -> Element.Element msg
display match_ mScore handler semantics =
    let
        home =
            UI.Team.viewTeam (Match.homeTeam match_)

        away =
            UI.Team.viewTeam (Match.awayTeam match_)

        sc =
            displayScore mScore

        style =
            UI.Style.matchRow semantics
                [ handler
                , UI.Font.button
                , Element.centerY
                , width (px 160)
                , height (px 100)

                -- , spacing 5
                -- , centerY
                -- , centerX
                -- , padding 0
                -- , width (px 160)
                -- , height (px 100)
                ]
    in
    Element.row style
        [ home, sc, away ]



--             , Border.color Color.panel
--             , Element.spacing 20
--             , Element.padding 20
--             , Border.rounded 5
--             , UI.Font.button
--             , Element.centerY
--             , centerY
--             , paddingXY 10 5
--             , width (px 180)
--             , height (px 100)
--             ]
--   [ Background.color Color.panel
--             , Font.color Color.primaryText
--             , Font.size (scaled 1)
--             , Font.center
--             , Element.pointer
--             , UI.Font.match
--             , Border.color border
--             , Border.width 1
--             , Border.rounded 10
--             ]
