module UI.Button exposing (button, maybeTeamBadge, maybeTeamButton, pill, scoreButton, submit, teamButton)

import Bets.Types
import Element exposing (..)
import Element.Attributes exposing (center, fill, height, padding, percent, px, verticalCenter, width)
import Element.Events exposing (onClick)
import UI.Grid exposing (Size(..))
import UI.Style exposing (ButtonSemantics, ScoreButtonSemantics, Style)
import UI.Team


pill : ButtonSemantics -> msg -> String -> Element Style variation msg
pill semantics msg buttonText =
    let
        buttonLayout =
            [ height (px 36), onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Score [ padding 10 ] (text buttonText)
    in
    Element.column (UI.Style.Button semantics) buttonLayout [ textElement ]


submit : ButtonSemantics -> msg -> String -> Element Style variation msg
submit semantics msg buttonText =
    let
        buttonLayout =
            [ height (px 76), width (px 150), onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Score [ padding 10 ] (text buttonText)
    in
    Element.column (UI.Style.Button semantics) buttonLayout [ textElement ]


button : Size -> ButtonSemantics -> msg -> String -> Element Style variation msg
button sz semantics msg buttonText =
    let
        ( w, h ) =
            case sz of
                XXL ->
                    ( width (percent 100), height (px 250) )

                XL ->
                    ( width (percent 40), height (px 76) )

                L ->
                    ( width (percent 30), height (px 76) )

                M ->
                    ( width (percent 24), height (px 76) )

                S ->
                    ( width (percent 16.66666), height (px 150) )

                XS ->
                    ( width (percent 10), height (px 76) )

                XXS ->
                    ( width (percent 10), height (px 40) )

                XXXS ->
                    ( width fill, height (px 40) )

        buttonLayout =
            [ w, h, onClick msg, Element.Attributes.center, Element.Attributes.verticalCenter ]
    in
    el (UI.Style.Button semantics) buttonLayout (text buttonText)


scoreButton : ScoreButtonSemantics -> msg -> String -> Element Style variation msg
scoreButton semantics msg buttonText =
    let
        w =
            px 48
                |> width

        h =
            px 28
                |> height

        buttonLayout =
            [ w, h, onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.Score [] (text buttonText)
    in
    Element.column (UI.Style.ScoreButton semantics) buttonLayout [ textElement ]


teamButton :
    UI.Style.TeamButtonSemantics
    -> msg
    -> Bets.Types.Team
    -> Element Style variation msg
teamButton semantics msg team =
    maybeTeamButton semantics msg (Just team)


maybeTeamButton :
    UI.Style.TeamButtonSemantics
    -> msg
    -> Maybe Bets.Types.Team
    -> Element Style variation msg
maybeTeamButton semantics msg team =
    let
        w =
            width (px 64)

        h =
            height (px 76)

        buttonLayout =
            [ w, h, onClick msg, center, verticalCenter ]

        textElement =
            Element.el UI.Style.TeamName [] (UI.Team.viewTeamEl team)
    in
    Element.column (UI.Style.TeamButton semantics) buttonLayout [ textElement ]


maybeTeamBadge :
    UI.Style.TeamButtonSemantics
    -> Maybe Bets.Types.Team
    -> Element Style variation msg
maybeTeamBadge semantics team =
    let
        w =
            width (px 64)

        h =
            height (px 76)

        buttonLayout =
            [ w, h, center, verticalCenter ]

        textElement =
            Element.el UI.Style.TeamName [] (UI.Team.viewTeamEl team)
    in
    Element.column (UI.Style.TeamButton semantics) buttonLayout [ textElement ]
