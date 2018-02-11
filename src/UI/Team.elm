module UI.Team exposing (viewTeam, viewTeamLarge)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)
import Bets.Types exposing (Team)
import Bets.Types.Team as T


viewTeam : Maybe Team -> Html msg
viewTeam team =
    div [ class "team" ]
        [ span [ class "flag" ] [ T.flag team ]
        , Html.br [] []
        , span [ class "team-name" ] [ text (T.mdisplay team) ]
        ]


viewTeamLarge : Maybe Team -> Html msg
viewTeamLarge team =
    div [ class "team" ]
        [ span [ class "flag" ] [ T.flag team ]
        , Html.br [] []
        , span [ class "team-name-lg" ] [ text (T.mdisplayFull team) ]
        ]
