module UI.Score exposing (viewScore)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Bets.Types exposing (Team)
import Bets.Types.Team as T
import UI.Grid


view : Maybe Team -> Maybe Score -> Maybe Team -> Html
view home mScore away =
    div scoreBlock


viewTeam : Maybe Team -> Html
viewTeam team =
    div [ teamStyle ]
        [ span [ flagStyle ] [ T.flag team ]
        , Html.br [] []
        , span [ teamNameStyle ] [ text (T.mdisplay team) ]
        ]


scoreStyle =
    style
        [ ( "width", "33%" )
        , ( "float", "left" )
        , ( "text-align", "center" )
        ]


teamStyle =
    style
        [ ( "width", "33%" )
        , ( "float", "left" )
        , ( "text-align", "center" )
        ]


flagStyle =
    style
        [ ( "width", "100%" )
        ]


teamNameStyle =
    style
        [ ( "width", "100%" )
        ]
