module UI.Card
    exposing
        ( Card
        , card
        , update
        , view
        , Action
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- import Task

import Time exposing (second)
import Html.Animation as Anim
import Html.Animation.Properties exposing (..)


type alias Card =
    { contents : CardContent
    , style : Anim.Animation
    }


card : Html -> Card
card contents =
    { contents = contents
    , animationStyle = initStyle
    }


initStyle =
    Anim.init
        [ Top -350.0 Px
        , Opacity 0.0
        ]


type Action
    = Show
    | Hide
    | Animate Anim.Action



-- update


onCard =
    Anim.forwardTo
        Animate
        .style
        (\w style -> { w | style = style })



-- style setter


update : Msg -> Card -> ( Card, Cmd Msg )
update msg card =
    case msg of
        Show ->
            Anim.animate
                |> Anim.props
                    [ Top (Anim.to 0) Px
                    , Opacity (Anim.to 1)
                    ]
                |> onCard card

        Hide ->
            Anim.animate
                |> Anim.props
                    [ Top (Anim.to 0) Px
                    , Opacity (Anim.to 0)
                    ]
                |> onCard card

        Animate msg ->
            onCard card msg


view : Card -> Html -> Html Msg
view card contents =
    let
        triggerStyle =
            [ ( "position", "absolute" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "350px" )
            , ( "height", "100%" )
              --, ("background-color", "#AAA")
            , ( "border", "2px dashed #AAA" )
            ]
    in
        div
            [ style triggerStyle
            , onMouseEnter address Show
            ]
            [ h1 [ style [ ( "padding", "25px" ) ] ]
                [ text "Hover here to see menu!" ]
            , div []
                [ button [ onClick Show ] [ text "show" ]
                , button [ onClick Hide ] [ text "hide" ]
                ]
            , viewCard address card contents
            ]


viewCard : Address Action -> Card -> Html -> Html
viewCard address card contents =
    let
        cardStyle =
            [ ( "position", "absolute" )
            , ( "top", "-2px" )
            , ( "margin-left", "-2px" )
            , ( "padding", "25px" )
            , ( "width", "300px" )
            , ( "height", "100%" )
            , ( "background-color", "rgb(aa,aa,aa)" )
            , ( "color", "white" )
            , ( "border", "2px solid rgb(58,40,69)" )
            ]
    in
        div [ style (cardStyle ++ (Anim.render card.style)) ]
            [ h1 [] [ text "Hidden Menu" ]
            , contents
            , button [ onClick address Hide ] [ text "hide" ]
            ]
