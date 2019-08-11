module UI.Team exposing (viewTeam, viewTeamFull)

import Bets.Types exposing (Team)
import Bets.Types.Team as T
import Element exposing (Element, column, el, fill, height, image, layout, padding, px, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Color as Color
import UI.Font exposing (scaled)
import UI.Style as Style


viewTeam : Maybe Team -> Element msg
viewTeam mTeam =
    let
        teamNameTxt =
            Maybe.map T.display mTeam
                |> Maybe.withDefault "..."

        flagUrl =
            T.flagUrl mTeam

        img =
            { src = flagUrl
            , description =
                teamNameTxt
            }
    in
    column teamBox
        [ row [ Element.centerX ]
            [ image [] img
            ]
        , row teamName
            [ Element.text teamNameTxt ]
        ]


viewTeamFull : Maybe Team -> Element msg
viewTeamFull team =
    let
        teamNameTxt =
            Maybe.map T.displayFull team
                |> Maybe.withDefault "..."

        img =
            { src = T.flagUrl team
            , description =
                teamNameTxt
            }

        w =
            Element.width (px 150)

        h =
            Element.height (px 82)
    in
    column teamBox
        [ row [ Element.centerX ]
            [ image [] img
            ]
        , row teamName
            [ Element.text teamNameTxt ]
        ]


teamBox =
    [ Element.spaceEvenly
    , height (px 45)
    , width (px 34)
    , Background.color Color.primaryDark
    , Font.color Color.primaryText
    , Font.size (UI.Font.scaled 1)
    , Font.center
    ]


teamName =
    [ Font.center
    , Font.size (UI.Font.scaled 1)
    , Font.center
    ]
