module UI.Screen exposing (Device(..), Size, className, device, elementId, maxWidth, size, width)

import Element
import Html.Attributes exposing (property)
import Json.Encode as Encode


type alias Size =
    { width : Float
    , height : Float
    }


type Device
    = Phone
    | Computer


size : Int -> Int -> Size
size w h =
    Size (toFloat w) (toFloat h)


maxWidth : Size -> Int
maxWidth screen =
    round <| (80 * screen.width) / 100


device : Size -> Device
device screen =
    if screen.width < 500 then
        Phone

    else
        Computer


width : Size -> Element.Attribute msg
width screen =
    Element.width
        (Element.fill
            |> Element.maximum (maxWidth screen)
        )


elementId : String -> Element.Attribute msg
elementId identifier =
    Element.htmlAttribute <| property "id" (Encode.string identifier)


className : String -> Element.Attribute msg
className identifier =
    Element.htmlAttribute <| property "className" (Encode.string identifier)
