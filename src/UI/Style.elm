module UI.Style exposing (..)

import Color exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Scale as Scale
import UI.Color exposing (..)


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
    | Text
    | Menu
    | Header1
    | Header2
    | Error
    | ErrorText
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



-- grey : Color
-- grey =
--     rgb 96 125 139
-- potential : Color
-- potential =
--     UI.Color.white
-- browngrey : Color
-- browngrey =
--     rgb 215 204 200
-- UI.Color.white : Color
-- UI.Color.white =
--     rgb 255 255 255
-- grey : Color
-- grey =
--     rgb 96 125 139
-- potential : Color
-- potential =
--     UI.Color.white
-- browngrey : Color
-- browngrey =
--     rgb 215 204 200
-- UI.Color.white : Color
-- UI.Color.white =
--     rgb 255 255 255
-- UI.Color.orange : Color
-- UI.Color.orange =
--     rgb 230 74 25
-- selected : Color
-- selected =
--     black
-- primary : Color
-- primary =
--     rgb 0x00 0x60 0x64
-- primaryDark : Color
-- primaryDark =
--     rgb 0x00 0x36 0x3A
-- primaryLight : Color
-- primaryLight =
--     rgb 0x42 0x8E 0x92
-- secondary : Color
-- secondary =
--     rgb 0xBC 0xAA 0xA4
-- secondaryDark : Color
-- secondaryDark =
--     rgb 0x8C 0x7B 0x75
-- secondaryLight : Color
-- secondaryLight =
--     rgb 0xEF 0xDC 0xD5
-- primaryText : Color
-- primaryText =
--     rgb 0xFF 0xFF 0xFF
-- secondaryText : Color
-- secondaryText =
--     rgb 0 0 0
-- rgb 0 150 136


scale : Int -> Float
scale =
    Scale.modular 16 1.618


stylesheet : StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ style Header1
            [ Font.size (scale 5)
            , Color.text primaryText
            , Color.background primary
            , Font.typeface
                [ Font.font "Asap"
                , Font.cursive
                ]
            ]
        , style Header2
            [ Font.size 28
            , Color.text secondaryText
            , Font.typeface
                [ Font.font "Asap"
                , Font.cursive
                ]
            ]
        , style Menu
            [ Font.size 20
            , Color.text primaryText
            , Color.background primary
            , Font.typeface
                [ Font.font "Asap"
                , Font.sansSerif
                ]
            ]
        , style Error
            [ Color.text red
            , Color.background secondaryLight
            , Border.all 1
            , Color.border red
            , Font.size 20
            , Font.lineHeight 1.4
            , Font.typeface
                [ Font.font "Lora"
                ]
            ]
        , style Text
            [ Font.size 20
            , Font.lineHeight 1.6
            , Font.typeface
                [ Font.font "Lora"
                ]
            ]
        , style Introduction
            [ Font.size 20
            , Font.lineHeight 1.4
            , Font.typeface
                [ Font.font "Lora"
                ]
            ]
        , style Page
            []
        , style (Button Active)
            [ Color.background secondary
            , Color.text secondaryText
            , Border.all 1
            , hover
                [ cursor "pointer"
                , Color.background secondaryLight
                , Color.text secondaryText
                ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Inactive)
            [ Color.background secondaryDark
            , Color.text secondaryText
            , Border.all 1
            , Font.lineHeight 1.0
            , hover [ cursor "not-allowed" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Wrong)
            [ Color.text wrong
            , Border.all 1
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Right)
            [ Color.text secondaryText
            , Border.all 1
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Perhaps)
            [ Color.background secondary
            , Color.text secondaryText
            , Border.all 1
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Irrelevant)
            [ Color.border secondaryLight
            , Border.all 1
            , Color.text secondaryText
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Potential)
            [ Color.background secondary
            , Color.text secondaryText
            , Color.border secondary
            , Border.all 1
            , hover
                [ cursor "pointer"
                , Color.border secondaryLight
                , Color.background primaryDark
                , Color.text primaryText
                ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (Button Selected)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 1
            , Color.border UI.Color.primary
            , hover
                [ cursor "pointer"
                , Color.background primaryDark
                , Color.text primaryText
                , Color.border UI.Color.orange
                ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (ScoreButton SBPotential)
            [ Color.background secondary
            , Color.text secondaryText
            , Border.all 1
            , Color.border secondary
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (ScoreButton SBSelected)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 1
            , Color.border secondary
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (TeamButton TBPotential)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 1
            , Color.border secondaryLight
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (TeamButton TBSelected)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Border.all 1
            , Color.border UI.Color.primary
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
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
            [ Border.all 2
            , Color.border secondaryLight
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style Score
            [ Font.center
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style Matches
            []
        , style ActiveMatch
            []
        , style (MatchRow Active)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (MatchRow Selected)
            [ Color.background secondaryLight
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style (MatchRow Potential)
            [ Color.background secondary
            , Color.text secondaryText
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style TeamNameFull
            [ Font.size 20
            , Font.center
            , Font.typeface
                [ Font.font "Roboto Mono"
                ]
            ]
        , style TeamBox
            [ Color.background primaryDark
            , Color.text primaryText
            , Font.size (scale 1)
            , Font.center
            ]
        , style Emphasis
            [ Color.text UI.Color.orange
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
            [ Color.background UI.Color.orange ]
        , style TextInput
            [ Border.all 2
            , Color.border secondaryLight
            , Font.typeface
                [ Font.font "Asap"
                ]
            ]
        , style Link
            [ Color.text UI.Color.orange
            , hover [ cursor "pointer" ]
            ]
        ]
