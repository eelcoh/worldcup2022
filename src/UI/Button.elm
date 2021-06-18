module UI.Button exposing (Size(..), button, maybeTeamBadge, maybeTeamBadgeSmall, maybeTeamBadgeVerySmall, maybeTeamButton, navlink, pill, submit, teamButton)

import Bets.Types
import Element exposing (Element, centerX, centerY, fill, height, padding, px, text, width)
import Element.Events exposing (onClick)
import UI.Style as Style exposing (ButtonSemantics)
import UI.Team


type Size
    = XXL
    | XL
    | L
    | M
    | S
    | XS
    | XXS
    | XXXS


navlink : ButtonSemantics -> String -> String -> Element msg
navlink semantics link linkText =
    let
        linkStyle =
            Style.button semantics [ padding 3, height (px 30), centerX, centerY ]
    in
    Element.link linkStyle { url = link, label = Element.text linkText }


pill : ButtonSemantics -> msg -> String -> Element msg
pill semantics msg buttonText =
    let
        buttonLayout =
            Style.button semantics [ padding 3, height (px 30), onClick msg, centerY ]
    in
    Element.column buttonLayout [ Element.el [] (text buttonText) ]


submit : ButtonSemantics -> msg -> String -> Element msg
submit semantics msg buttonText =
    let
        buttonLayout =
            Style.button semantics [ padding 10, height (px 76), width (px 150), onClick msg, centerX, centerY ]
    in
    Element.column buttonLayout [ Element.el [ centerX, centerY ] (text buttonText) ]


button : Size -> ButtonSemantics -> msg -> String -> Element msg
button sz semantics msg buttonText =
    let
        ( w, h ) =
            case sz of
                XXL ->
                    ( width (px 100), height (px 250) )

                XL ->
                    ( width (px 40), height (px 76) )

                L ->
                    ( width (px 30), height (px 76) )

                M ->
                    ( width (px 24), height (px 76) )

                S ->
                    ( width (px 16), height (px 150) )

                XS ->
                    ( width (px 10), height (px 76) )

                XXS ->
                    ( width (px 10), height (px 40) )

                XXXS ->
                    ( width fill, height (px 40) )

        buttonLayout =
            Style.button semantics [ w, h, onClick msg, centerX, centerY ]
    in
    Element.column buttonLayout [ text buttonText ]


teamButton : ButtonSemantics -> msg -> Bets.Types.Team -> Element msg
teamButton semantics msg team =
    Element.el
        [ centerX
        , Element.spaceEvenly
        ]
        (maybeTeamButton semantics msg (Just team))


maybeTeamButton : ButtonSemantics -> msg -> Maybe Bets.Types.Team -> Element msg
maybeTeamButton semantics msg team =
    let
        buttonLayout =
            Style.teamBadge semantics [ onClick msg, centerX, centerY, padding 10 ]
    in
    Element.column buttonLayout [ UI.Team.viewTeam team ]


maybeTeamBadge : ButtonSemantics -> Maybe Bets.Types.Team -> Element msg
maybeTeamBadge semantics team =
    let
        w =
            width (px 64)

        h =
            height (px 76)

        buttonLayout =
            Style.teamButton semantics [ w, h, centerX, centerY ]
    in
    Element.column buttonLayout [ UI.Team.viewTeam team ]


maybeTeamBadgeSmall : ButtonSemantics -> Maybe Bets.Types.Team -> Element msg
maybeTeamBadgeSmall semantics team =
    let
        w =
            width (px 64)

        h =
            height (px 76)

        buttonLayout =
            Style.teamButton semantics [ w, h, centerX, centerY ]
    in
    Element.column buttonLayout [ UI.Team.viewTeamSmall team ]


maybeTeamBadgeVerySmall : ButtonSemantics -> Maybe Bets.Types.Team -> Element msg
maybeTeamBadgeVerySmall semantics team =
    let
        w =
            width (px 32)

        h =
            height (px 38)

        buttonLayout =
            Style.teamBadgeVerySmall semantics [ w, h, centerX, centerY ]
    in
    Element.column buttonLayout [ UI.Team.viewTeamVerySmall team ]
