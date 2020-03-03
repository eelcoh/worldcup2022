module Form.Bracket.Types exposing (Angle, IsWinner(..), Msg(..), State(..), init)

import Bets.Types exposing (Bracket(..), Candidate(..), CurrentSlot(..), HasQualified(..), Slot, Team, Winner(..))


type Msg
    = SetWinner Slot Winner
    | SetQualifier Slot Candidate
    | SetSlot Slot Team
    | CloseQualifierView
    | OpenQualifierView


type alias Angle =
    Float


type IsWinner
    = Yes
    | No
    | Undecided


init : State
init =
    ShowMatches


type State
    = ShowMatches
    | ShowSecondRoundSelection Slot Candidate
