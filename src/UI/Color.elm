module UI.Color exposing
    ( asHex
    , background
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
    , white
    , wrong
    )

import Element exposing (Color, rgb255)
import Hex


asHex : Color -> String
asHex clr =
    let
        color =
            Element.toRgb clr

        to255 f =
            (255 * f)
                |> Basics.round
                |> Basics.modBy 255
                |> Hex.toString

        r =
            to255 color.red

        g =
            to255 color.green

        b =
            to255 color.blue

        a =
            to255 color.alpha
    in
    "#" ++ r ++ g ++ b


red : Color
red =
    rgb255 255 0 0


black : Color
black =
    rgb255 0x24 0x24 0x24


grey : Color
grey =
    rgb255 96 125 139


potential : Color
potential =
    white



-- browngrey : Color
-- browngrey =
--     rgb255 215 204 200


white : Color
white =
    rgb255 0xE7 0xE7 0xE7


orange : Color
orange =
    rgb255 0xF7 0x7F 0x21


green : Color
green =
    rgb255 0x42 0x9F 0x59


right : Color
right =
    green


wrong : Color
wrong =
    red


selected : Color
selected =
    black


primary : Color
primary =
    rgb255 0x21 0x21 0x21


primaryDark : Color
primaryDark =
    black


primaryLight : Color
primaryLight =
    rgb255 0x48 0x48 0x48


secondary : Color
secondary =
    rgb255 0xB0 0xBE 0xC5


secondaryDark : Color
secondaryDark =
    rgb255 0x80 0x8E 0x95


secondaryLight : Color
secondaryLight =
    orange


primaryText : Color
primaryText =
    white


secondaryText : Color
secondaryText =
    orange



-- rgb255 0x4E 0x34 0x2E
-- rgb255 0 150 136


background : Color
background =
    rgb255 0x32 0x32 0x32


panel : Color
panel =
    black
