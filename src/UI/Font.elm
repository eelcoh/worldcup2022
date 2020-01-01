module UI.Font exposing (asap, button, input, lora, match, roboto, scaled, score, team)

import Element
import Element.Font as Font


scaled : Int -> Int
scaled s =
    s
        |> Element.modular 16 1.25
        |> Basics.round


roboto : Element.Attribute msg
roboto =
    Font.family
        [ Font.typeface "Roboto Mono"
        ]


asap : Element.Attribute msg
asap =
    Font.family
        [ Font.typeface "Asap"
        ]


lora : Element.Attribute msg
lora =
    Font.family
        [ Font.typeface "Lora"
        ]


button : Element.Attribute msg
button =
    roboto


score : Element.Attribute msg
score =
    roboto


match : Element.Attribute msg
match =
    roboto


team : Element.Attribute msg
team =
    roboto


input : Element.Attribute msg
input =
    asap
