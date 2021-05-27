module UI.Screen exposing (Size, className, elementId, maxWidth, size, width)

import Element
import Html.Attributes exposing (property)
import Json.Encode as Encode


type alias Size =
    { width : Float
    , height : Float
    }


size : Int -> Int -> Size
size w h =
    Size (toFloat w) (toFloat h)


maxWidth : Size -> Int
maxWidth screen =
    round <| (80 * screen.width) / 100


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
