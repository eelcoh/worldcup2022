module UI.Color.RGB exposing
    ( background
    , black
    , green
    , grey
    , orange
    , panel
    , potential
    , primary
    , primaryDark
    , primaryLight
    , primaryText
    , red
    , right
    , secondary
    , secondaryDark
    , secondaryLight
    , secondaryText
    , selected
    , toRGB
    , white
    , wrong
    )

import Element exposing (Color)
import UI.Color as Color


red : String
red =
    toRGB Color.red


black : String
black =
    toRGB Color.black


grey : String
grey =
    toRGB Color.grey


potential : String
potential =
    white



-- browngrey : String
-- browngrey =
--     rgb255 215 204 200


white : String
white =
    toRGB Color.white


orange : String
orange =
    toRGB Color.orange


green : String
green =
    toRGB Color.green


right : String
right =
    green


wrong : String
wrong =
    red


selected : String
selected =
    black


primary : String
primary =
    toRGB Color.primary


primaryDark : String
primaryDark =
    black


primaryLight : String
primaryLight =
    toRGB Color.primaryLight


secondary : String
secondary =
    toRGB Color.secondary


secondaryDark : String
secondaryDark =
    toRGB Color.secondaryDark


secondaryLight : String
secondaryLight =
    orange


primaryText : String
primaryText =
    white


secondaryText : String
secondaryText =
    orange



-- rgb255 0x4E 0x34 0x2E
-- rgb255 0 150 136


background : String
background =
    toRGB Color.background


panel : String
panel =
    black


toRGB : Color -> String
toRGB clr =
    Element.toRgb clr
        |> (\c -> [ c.red, c.green, c.blue, c.alpha ])
        |> List.map ((*) 255 >> round >> String.fromInt)
        |> String.join ", "
        |> (\v -> "RGB(" ++ v ++ ")")
