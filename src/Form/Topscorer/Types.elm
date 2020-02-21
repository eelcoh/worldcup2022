module Form.Topscorer.Types exposing (..)

import Bets.Types exposing (Team)


type Msg
    = SelectTeam Team
    | SelectPlayer String


type IsSelected
    = Selected
    | NotSelected
