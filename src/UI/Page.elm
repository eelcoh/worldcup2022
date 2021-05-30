module UI.Page exposing (..)

import Element exposing (centerX, fill, paddingXY, spacing, width)
import UI.Screen
import UI.Style


page : String -> List (Element.Element msg) -> Element.Element msg
page name elements =
    Element.column
        (UI.Style.page [ centerX, spacing 20, paddingXY 20 0, UI.Screen.className name, width fill ])
        elements
