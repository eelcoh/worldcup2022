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
import Element.Events as Events
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


scoreButton semantics attrs =
    case semantics of
        Potential ->
            scoreButtonSBPotential ++ attrs

        Selected ->
            scoreButtonSBSelected ++ attrs

        _ ->
            buttonWrong ++ attrs


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


header1 attrs =
    attrs
        ++ [ Background.color Color.primary
           , Font.size (scaled 3)
           , Font.color Color.primaryText
           , UI.Font.asap
           , Font.italic
           ]


header2 attrs =
    attrs
        ++ [ UI.Font.asap
           , Font.italic
           , Font.size (scaled 2)
           , Font.color Color.secondaryText
           ]


body attrs =
    []


menu attrs =
    attrs
        ++ [ Background.color Color.primary
           , Font.size (scaled 1)
           , Font.color Color.primaryText
           , UI.Font.asap
           ]


text attrs =
    attrs
        ++ [ UI.Font.lora
           , Font.italic
           , Font.size (scaled 1)
           , Element.spacing 2
           ]


introduction attrs =
    attrs
        ++ [ UI.Font.lora
           , Font.italic
           , Font.size (scaled 1)
           , Element.spacing 2
           ]


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


page attrs =
    attrs
        ++ []


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


buttonInactive =
    [ Background.color Color.secondaryDark
    , Font.color Color.secondaryText
    , Border.width 1
    , Element.spacing 10
    , Element.htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
    , UI.Font.button
    ]


buttonWrong =
    [ Font.color Color.wrong
    , Border.width 1
    , Element.pointer
    , UI.Font.button
    ]


buttonRight =
    [ Font.color Color.secondaryText
    , Border.width 1
    , Element.pointer
    , UI.Font.button
    ]


buttonPerhaps =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Border.width 1
    , Element.pointer
    , UI.Font.button
    ]


buttonIrrelevant =
    [ Border.color Color.secondaryLight
    , Border.width 1
    , Font.color Color.secondaryText
    , Element.pointer
    , UI.Font.button
    ]


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


flag attrs =
    attrs
        ++ []


flagImage attrs =
    attrs
        ++ []


teamName attrs =
    attrs
        ++ [ Font.center
           ]


scoreRow attrs =
    attrs
        ++ []


scoreColumn attrs =
    attrs
        ++ []


scoreInput attrs =
    attrs
        ++ [ Border.width 2
           , Border.color Color.secondaryLight
           , UI.Font.score
           ]


score attrs =
    attrs
        ++ [ Font.center
           , UI.Font.score
           ]


matches attrs =
    attrs
        ++ []


activeMatch attrs =
    attrs
        ++ []


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


matchRowActive =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Font.size (scaled 1)
    , Font.center
    , Element.pointer
    , UI.Font.match
    ]


matchRowSelected =
    [ Background.color Color.secondaryLight
    , Font.color Color.secondaryText
    , Font.size (scaled 1)
    , Font.center
    , Element.pointer
    , UI.Font.match
    ]


matchRowPotential =
    [ Background.color Color.secondary
    , Font.color Color.secondaryText
    , Font.size (scaled 1)
    , Font.center
    , Element.pointer
    , UI.Font.match
    ]


emphasis attrs =
    attrs
        ++ [ Font.color Color.orange
           , Font.extraBold
           ]


wrapper attrs =
    attrs


groupBadge attrs =
    attrs


groupPosition attrs =
    attrs


groupPositions attrs =
    attrs


none attrs =
    attrs


bullet attrs =
    [ Background.color Color.orange ] ++ attrs


textInput attrs =
    attrs
        ++ [ Border.width 2
           , Border.color Color.secondaryLight
           , UI.Font.input
           ]


link attrs =
    attrs
        ++ [ Font.color Color.orange
           , Element.pointer
           ]
