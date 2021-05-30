module Form.Bracket.Types exposing (Angle, BracketState(..), IsWinner(..), Msg(..), State, createState, init, initialKnockouts, initialQualifierView)

import Bets.Types exposing (Bracket(..), Candidate(..), CurrentSlot(..), Group(..), HasQualified(..), Slot, Team, Winner(..))
import UI.Screen as Screen


type Msg
    = SetWinner Slot Winner
    | SetQualifier Slot Candidate
    | SetSlot Slot Candidate Group Team
    | CloseQualifierView
    | OpenQualifierView


type alias Angle =
    Float


type IsWinner
    = Yes
    | No
    | Undecided


init : Screen.Size -> State
init sz =
    State sz <| ShowSecondRoundSelection "WA" (FirstPlace A)


initialKnockouts : Screen.Size -> State
initialKnockouts sz =
    State sz ShowMatches


initialQualifierView : State -> State
initialQualifierView { screen } =
    init screen


createState : Screen.Size -> BracketState -> State
createState sz bs =
    State sz bs


type BracketState
    = ShowMatches
    | ShowSecondRoundSelection Slot Candidate


type alias State =
    { screen : Screen.Size
    , bracketState : BracketState
    }
