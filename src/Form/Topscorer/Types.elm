module Form.Topscorer.Types exposing (IsSelected(..), Msg(..))

import Bets.Types exposing (Team)


type Msg
    = SelectTeam Team
    | SelectPlayer String


type IsSelected
    = Selected
    | NotSelected
