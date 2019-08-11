module UI.Font exposing (asap, button, input, lora, match, roboto, scaled, score, team)

import Element
import Element.Font as Font


scaled : Int -> Int
scaled s =
    s
        |> Element.modular 16 1.25
        |> Basics.round


roboto =
    Font.family
        [ Font.typeface "Roboto Mono"
        ]


asap =
    Font.family
        [ Font.typeface "Asap"
        ]


lora =
    Font.family
        [ Font.typeface "Lora"
        ]


button =
    roboto


score =
    roboto


match =
    roboto


team =
    roboto


input =
    asap
