module UI.Screen exposing (Size, maxWidth, size)


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
