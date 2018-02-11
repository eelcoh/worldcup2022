module UI.Style
    exposing
        ( std
        , small
        , smallBlock
        , grid
        , Colour(..)
        , Cell
        )

import Array
import Html exposing (Html, text, Attribute)
import Material.Color as Color
import Material.Grid as Grid exposing (cell)
import Material.Style as S exposing (css, styled, attribute)


type alias Cell =
    Grid.Cell


type alias Style =
    S.Style


type Colour
    = Active
    | Inactive
    | Wrong
    | Right
    | Unknown
    | Selected


colour : Colour -> Style
colour c =
    case c of
        Active ->
            color 1

        Inactive ->
            color 2

        Wrong ->
            color 3

        Right ->
            color 4

        Unknown ->
            color 5

        Selected ->
            color 6


grid =
    Grid.grid


color : Int -> Style
color k =
    Array.get ((k + 0) % Array.length Color.hues) Color.hues
        |> Maybe.withDefault Color.Teal
        |> flip Color.color Color.S500
        |> Color.background


mycell : Int -> List Style -> List Html -> Cell
mycell k styling =
    cell <| List.concat [ stijle k, styling ]


smallBlock : Attribute -> Colour -> List ( String, String ) -> List Html -> Cell
smallBlock handler c styles contents =
    let
        blockStyles =
            List.append styles [ ( "margin", "0 auto" ), ( "width", "100%" ), ( "align", "center" ), ( "cursor", "pointer" ) ]
                |> toStyle
    in
        small
            [ Grid.size Grid.All 2, colour c, attribute handler ]
            [ S.div blockStyles contents
            ]


small : List Style -> List Html -> Cell
small =
    mycell 100


std : List Style -> List Html -> Cell
std =
    mycell 400


stijle : Int -> List Style
stijle h =
    [ css "text-sizing" "border-box"
    , css "background-color" "#BDBDBD"
    , css "height" (toString h ++ "px")
    , css "padding" "8px"
      --, css "padding" "4px"
    , css "color" "white"
    , css "overflow" "hidden"
    , css "cursor" "pointer"
    ]


toStyle : List ( String, String ) -> List Style
toStyle stylz =
    List.map (\( k, v ) -> css k v) stylz
