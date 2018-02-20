module UI.Style exposing (..)

import Color exposing (..)
import Style exposing (..)
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


brownGrey : Color
brownGrey =
    rgb 215 204 200


white : Color
white =
    rgb 255 255 255


orange : Color
orange =
    rgb 230 74 25


green : Color
green =
    rgb 0 150 136


scale : Int -> Float
scale =
    Scale.modular 16 1.618


stylesheet : StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ style Header1
            [ Font.size (scale 2)
            , Color.text orange
            ]
        , style Introduction
            [ Font.size (scale 1)
            ]
        , style Page
            []
        , style (Button Active)
            [ Color.background green
            , Color.text white
            , hover [ cursor "pointer" ]
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
            [ Color.background green
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Perhaps)
            [ Color.background green
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Irrelevant)
            [ Color.background <| rgb 200 200 200
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Potential)
            [ Color.background brownGrey
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (Button Selected)
            [ Color.background <| rgb 0 150 136
            , Color.text white
            , hover [ cursor "pointer" ]
            ]
        , style (ScoreButton SBPotential)
            [ Color.background brownGrey
            , Color.text white
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style (ScoreButton SBSelected)
            [ Color.background green
            , Color.text white
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style (TeamButton TBPotential)
            [ Color.background brownGrey
            , Color.text grey
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            , hover [ cursor "pointer" ]
            ]
        , style (TeamButton TBSelected)
            [ Color.background green
            , Color.text white
            , Font.lineHeight 1.0
            , Font.center
            , Font.size 15
            ]
        , style (TeamButton TBSelected)
            [ Color.background grey
            , Color.text white
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
            , Color.text brownGrey
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            ]
        , style (MatchRow Selected)
            [ Color.background <| rgb 0 150 136
            , Color.text brownGrey
            , Font.size (scale 1)
            , Font.center
            , hover [ cursor "pointer" ]
            ]
        , style (MatchRow Potential)
            [ Color.background green
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



{-


   .xxl {
       height: 250px; width: 100%}
   .xl {
       height: 76px; width: 40%}
   .l {
       height: 76px; width: 30%}
   .m {
       height: 76px; width: 24%}
   .s {
       height: 150px; width: 16.66666%}
   .xs {
       height: 76px; width: 10%}

   .xxs {
       height: 40px; width: auto; line-height: 1.0;}

   .xxxs {
       height: 30px;
       width: auto;
       line-height: 1.0;
       font-size: 0.750rem;
       margin: 2px;
       padding: 10px;
     }

   .scoreButton {
       height: 28px; width: 48px; line-height: 1.0;}

   .scoreButton > span {
     display: block;
     margin: auto;
     text-align: center;
   }

   .teamButtonBracket {
     flex-grow: 1;
     flex: none;
     align-self: flex-start;
     padding: 5px 1px;
     margin: 1px
   }

-}
