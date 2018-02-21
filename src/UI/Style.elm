module UI.Style exposing (..)

import Color exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Scale as Scale


type Style
    = Button ButtonSemantics
    | ScoreButton ScoreButtonSemantics
    | TeamButton TeamButtonSemantics
    | MatchRow ButtonSemantics
    | Matches
    | ActiveMatch
    | TeamNameFull
    | Flag
    | FlagImage
    | TeamName
    | TeamBox
    | Score
    | ScoreRow
    | ScoreColumn
    | ScoreInput
    | Introduction
    | Header1
    | Page
    | Emphasis
    | Wrapper
    | GroupBadge
    | GroupPosition
    | GroupPositions
    | None
    | Bullet
    | TextInput
    | Link


type ScoreButtonSemantics
    = SBPotential
    | SBSelected


type TeamButtonSemantics
    = TBPotential
    | TBSelected
    | TBInactive


type ButtonSemantics
    = Active
    | Inactive
    | Wrong
    | Right
    | Perhaps
    | Irrelevant
    | Potential
    | Selected


grey : Color
grey =
    rgb 96 125 139


potential : Color
potential =
    white


browngrey : Color
browngrey =
    rgb 215 204 200


white : Color
white =
    rgb 255 255 255


orange : Color
orange =
    rgb 230 74 25


selected : Color
selected =
    black



-- rgb 0 150 136


scale : Int -> Float
scale =
    Scale.modular 16 1.618


stylesheet : StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ style Header1
            [ Font.size (scale 2)
            , Color.text orange
            , Font.typeface
                [ Font.font "medium-content-sans-serif-font"
                ]
            ]
        , style Introduction
            [ Font.size (scale 1)
            , Font.lineHeight 1.4
            , Font.typeface
                [ Font.font "medium-content-serif-font"
                ]
            ]
        , style Page
            []
        , style (Button Active)
            [ Color.background selected
            , Color.text white
            , hover
                [ cursor "pointer"
                , Color.background orange
                , Color.text white
                ]
            ]
        , style (Button Inactive)
            [ Color.background <| rgb 121 85 72
            , Color.text white
            , Font.lineHeight 1.0
            , hover [ cursor "not-allowed" ]
            ]
        , style (Button Wrong)
            [ Color.background <| rgb 233 30 99
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Right)
            [ Color.background selected
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Perhaps)
            [ Color.background selected
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Irrelevant)
            [ Color.background <| rgb 200 200 200
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Potential)
            [ Color.background potential
            , Color.text browngrey
            , Border.all 2
            , Color.border browngrey
            , hover
                [ cursor "pointer"
                , Border.all 2
                , Border.solid
                , Color.border black
                , Color.background white
                , Color.text black
                ]
            ]
        , style (Button Selected)
            [ Color.background selected
            , Color.text white
            , Border.all 2
            , Color.border selected
            , hover
                [ cursor "pointer"
                , Color.background orange
                , Color.text black
                , Color.border orange
                ]
            ]
        , style (ScoreButton SBPotential)
            [ Color.background potential
            , Color.text white
            , Border.all 2
            , Color.border selected
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style (ScoreButton SBSelected)
            [ Color.background selected
            , Color.text white
            , Border.all 2
            , Color.border selected
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style (TeamButton TBPotential)
            [ Color.background potential
            , Color.text grey
            , Border.all 2
            , Color.border potential
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style (TeamButton TBSelected)
            [ Color.background selected
            , Color.text white
            , Border.all 2
            , Color.border selected
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style Flag
            []
        , style FlagImage
            []
        , style TeamName
            [ Font.center
            ]
        , style TeamBox
            []
        , style ScoreRow
            []
        , style ScoreColumn
            []
        , style ScoreInput
            []
        , style Score
            [ Font.center
            ]
        , style Matches
            []
        , style ActiveMatch
            []
        , style (MatchRow Active)
            [ Color.background orange
            , Color.text potential
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            ]
        , style (MatchRow Selected)
            [ Color.background <| rgb 0 150 136
            , Color.text potential
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            ]
        , style (MatchRow Potential)
            [ Color.background selected
            , Color.text white
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            ]
        , style TeamNameFull
            [ Font.size (scale 1)
            , Font.center
            ]
        , style TeamBox
            [ Color.background orange
            , Color.text white
            , Font.size (scale 1)
            , Font.center
            ]
        , style Emphasis
            [ Color.text orange
            , Font.weight 700
            ]
        , style Wrapper
            []
        , style GroupBadge
            []
        , style GroupPosition
            []
        , style GroupPositions
            []
        , style None
            []
        , style Bullet
            [ Color.background orange ]
        , style TextInput
            []
        , style Link
            [ Color.text orange
            , hover [ cursor "pointer" ]
            ]
        ]
