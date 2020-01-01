module UI.Style exposing
    ( ButtonSemantics(..)
    , activeMatch
    , body
    , bullet
    , button
    , buttonActive
    , buttonInactive
    , buttonIrrelevant
    , buttonPerhaps
    , buttonPotential
    , buttonRight
    , buttonSelected
    , buttonWrong
    , emphasis
    , error
    , flag
    , flagImage
    , groupBadge
    , groupPosition
    , groupPositions
    , header1
    , header2
    , introduction
    , link
    , matchRow
    , matches
    , menu
    , none
    , page
    , score
    , scoreButton
    , scoreButtonSBPotential
    , scoreButtonSBSelected
    , scoreColumn
    , scoreInput
    , scoreRow
    , teamButton
    , teamButtonTBPotential
    , teamButtonTBSelected
    , teamName
    , text
    , textInput
    , wrapper
    )

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import UI.Color as Color
import UI.Font exposing (scaled)


type ButtonSemantics
    = Active
    | Inactive
    | Wrong
    | Right
    | Perhaps
    | Irrelevant
    | Potential
    | Selected


scoreButton : ButtonSemantics -> List (Element.Attribute msg) -> List (Element.Attribute msg)
scoreButton semantics attrs =
    case semantics of
        Potential ->
            scoreButtonSBPotential ++ attrs

        Selected ->
            scoreButtonSBSelected ++ attrs

        _ ->
            buttonWrong ++ attrs


teamButton : ButtonSemantics -> List (Element.Attribute msg) -> List (Element.Attribute msg)
teamButton semantics attrs =
    case semantics of
        Potential ->
            teamButtonTBPotential ++ attrs

        Selected ->
            teamButtonTBSelected ++ attrs

        _ ->
            buttonWrong ++ attrs


button : ButtonSemantics -> List (Element.Attribute msg) -> List (Element.Attribute msg)
button semantics attrs =
    case semantics of
        Active ->
            buttonActive ++ attrs

        Inactive ->
            buttonInactive ++ attrs

        Wrong ->
            buttonWrong ++ attrs

        Right ->
            buttonRight ++ attrs

        Perhaps ->
            buttonPerhaps ++ attrs

        Irrelevant ->
            buttonIrrelevant ++ attrs

        Potential ->
            buttonPotential ++ attrs

        Selected ->
            buttonSelected ++ attrs


header1 : List (Element.Attribute msg) -> List (Element.Attribute msg)
header1 attrs =
    attrs
        ++ [ Background.color Color.primary
           , Font.size (scaled 3)
           , Font.color Color.primaryText
           , UI.Font.asap
           , Font.italic
           ]


header2 : List (Element.Attribute msg) -> List (Element.Attribute msg)
header2 attrs =
    attrs
        ++ [ UI.Font.asap
           , Font.italic
           , Font.size (scaled 2)
           , Font.color Color.secondaryText
           ]


body : List (Element.Attribute msg) -> List (Element.Attribute msg)
body _ =
    []


menu : List (Element.Attribute msg) -> List (Element.Attribute msg)
menu attrs =
    attrs
        ++ [ Background.color Color.primary
           , Font.size (scaled 1)
           , Font.color Color.primaryText
           , UI.Font.asap
           ]


text : List (Element.Attribute msg) -> List (Element.Attribute msg)
text attrs =
    attrs
        ++ [ UI.Font.lora
           , Font.italic
           , Font.size (scaled 1)
           , Element.spacing 2
           ]


introduction : List (Element.Attribute msg) -> List (Element.Attribute msg)
introduction attrs =
    attrs
        ++ [ UI.Font.lora
           , Font.italic
           , Font.size (scaled 1)
           , Element.spacing 2
           ]


error : List (Element.Attribute msg) -> List (Element.Attribute msg)
error attrs =
    attrs
        ++ [ Background.color Color.secondaryLight
           , Border.width 1
           , Border.color Color.red
           , Font.color Color.red
           , Font.size (scaled 1)
           , Element.spacing 2
           , UI.Font.lora
           ]


page : List (Element.Attribute msg) -> List (Element.Attribute msg)
page attrs =
    attrs
        ++ []


buttonActive : List (Element.Attribute msg)
buttonActive =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Border.width 1
    , Element.pointer
    , Element.mouseOver
        [ Background.color Color.secondaryLight
        , Font.color Color.secondaryText
        ]
    , UI.Font.button
    ]


buttonInactive : List (Element.Attribute msg)
buttonInactive =
    [ Background.color Color.secondaryDark
    , Font.color Color.secondaryText
    , Border.width 1
    , Element.spacing 10
    , Element.htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
    , UI.Font.button
    ]


buttonWrong : List (Element.Attribute msg)
buttonWrong =
    [ Font.color Color.wrong
    , Border.width 1
    , Element.pointer
    , UI.Font.button
    ]


buttonRight : List (Element.Attribute msg)
buttonRight =
    [ Font.color Color.secondaryText
    , Border.width 1
    , Element.pointer
    , UI.Font.button
    ]


buttonPerhaps : List (Element.Attribute msg)
buttonPerhaps =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Border.width 1
    , Element.pointer
    , UI.Font.button
    ]


buttonIrrelevant : List (Element.Attribute msg)
buttonIrrelevant =
    [ Border.color Color.secondaryLight
    , Border.width 1
    , Font.color Color.secondaryText
    , Element.pointer
    , UI.Font.button
    ]


buttonPotential : List (Element.Attribute msg)
buttonPotential =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Border.color Color.secondary
    , Border.width 1
    , Element.pointer
    , Element.mouseOver
        [ Border.color Color.secondaryLight
        , Background.color Color.primaryDark
        , Font.color Color.primaryText
        ]
    , UI.Font.button
    ]


buttonSelected : List (Element.Attribute msg)
buttonSelected =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Border.width 1
    , Border.color Color.primary
    , Element.pointer
    , Element.mouseOver
        [ Background.color Color.primaryDark
        , Font.color Color.primaryText
        , Border.color Color.orange
        ]
    , UI.Font.button
    ]


scoreButtonSBPotential : List (Element.Attribute msg)
scoreButtonSBPotential =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Border.width 1
    , Border.color Color.secondary
    , Element.spacing 10
    , Font.center
    , Font.size (scaled 1)
    , Element.pointer
    , UI.Font.button
    ]


scoreButtonSBSelected : List (Element.Attribute msg)
scoreButtonSBSelected =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Border.width 1
    , Border.color Color.secondary
    , Element.spacing 10
    , Font.center
    , Font.size (scaled 1)
    , Element.pointer
    , UI.Font.button
    ]


teamButtonTBPotential : List (Element.Attribute msg)
teamButtonTBPotential =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Border.width 1
    , Border.color Color.secondaryLight
    , Element.spacing 10
    , Font.center
    , Font.size (scaled 1)
    , Element.pointer
    , UI.Font.button
    ]


teamButtonTBSelected : List (Element.Attribute msg)
teamButtonTBSelected =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Border.width 1
    , Border.color Color.primary
    , Element.spacing 10
    , Font.center
    , Font.size (scaled 1)
    , Element.pointer
    , UI.Font.button
    ]


flag : List (Element.Attribute msg) -> List (Element.Attribute msg)
flag attrs =
    attrs
        ++ []


flagImage : List (Element.Attribute msg) -> List (Element.Attribute msg)
flagImage attrs =
    attrs
        ++ []


teamName : List (Element.Attribute msg) -> List (Element.Attribute msg)
teamName attrs =
    attrs
        ++ [ Font.center
           ]


scoreRow : List (Element.Attribute msg) -> List (Element.Attribute msg)
scoreRow attrs =
    attrs
        ++ []


scoreColumn : List (Element.Attribute msg) -> List (Element.Attribute msg)
scoreColumn attrs =
    attrs
        ++ []


scoreInput : List (Element.Attribute msg) -> List (Element.Attribute msg)
scoreInput attrs =
    attrs
        ++ [ Border.width 2
           , Border.color Color.secondaryLight
           , UI.Font.score
           ]


score : List (Element.Attribute msg) -> List (Element.Attribute msg)
score attrs =
    attrs
        ++ [ Font.center
           , UI.Font.score
           ]


matches : List (Element.Attribute msg) -> List (Element.Attribute msg)
matches attrs =
    attrs
        ++ []


activeMatch : List (Element.Attribute msg) -> List (Element.Attribute msg)
activeMatch attrs =
    attrs
        ++ []


matchRow : ButtonSemantics -> List (Element.Attribute msg) -> List (Element.Attribute msg)
matchRow semantics attrs =
    let
        base =
            case semantics of
                Active ->
                    matchRowActive

                Selected ->
                    matchRowSelected

                Potential ->
                    matchRowPotential

                _ ->
                    matchRowPotential
    in
    attrs ++ base


matchRowActive : List (Element.Attribute msg)
matchRowActive =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Font.size (scaled 1)
    , Font.center
    , Element.pointer
    , UI.Font.match
    ]


matchRowSelected : List (Element.Attribute msg)
matchRowSelected =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Font.size (scaled 1)
    , Font.center
    , Element.pointer
    , UI.Font.match
    ]


matchRowPotential : List (Element.Attribute msg)
matchRowPotential =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Font.size (scaled 1)
    , Font.center
    , Element.pointer
    , UI.Font.match
    ]


emphasis : List (Element.Attribute msg) -> List (Element.Attribute msg)
emphasis attrs =
    attrs
        ++ [ Font.color Color.orange
           , Font.extraBold
           ]


wrapper : List (Element.Attribute msg) -> List (Element.Attribute msg)
wrapper attrs =
    attrs


groupBadge : List (Element.Attribute msg) -> List (Element.Attribute msg)
groupBadge attrs =
    attrs


groupPosition : List (Element.Attribute msg) -> List (Element.Attribute msg)
groupPosition attrs =
    attrs


groupPositions : List (Element.Attribute msg) -> List (Element.Attribute msg)
groupPositions attrs =
    attrs


none : List (Element.Attribute msg) -> List (Element.Attribute msg)
none attrs =
    attrs


bullet : List (Element.Attribute msg) -> List (Element.Attribute msg)
bullet attrs =
    Background.color Color.orange :: attrs


textInput : List (Element.Attribute msg) -> List (Element.Attribute msg)
textInput attrs =
    attrs
        ++ [ Border.width 2
           , Border.color Color.secondaryLight
           , UI.Font.input
           ]


link : List (Element.Attribute msg) -> List (Element.Attribute msg)
link attrs =
    attrs
        ++ [ Font.color Color.orange
           , Element.pointer
           ]
