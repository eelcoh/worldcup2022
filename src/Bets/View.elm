module Bets.View exposing (..)

import Bets.Types exposing (Bet)
import Element exposing (Element)
import Types exposing (Msg)
import UI.Screen as Screen


view : Bet -> Screen.Size -> Element Msg
view _ _ =
    Element.text "Bets"
