module Form.Questions.Bracket exposing (Msg, update, view)

import Arc2d exposing (Arc2d)
import Basics.Extra exposing (inDegrees)
import Bets.Bet exposing (setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Bracket(..), Qualifier, Slot, Team, Winner(..))
import Bets.Types.Bracket as B
import Element exposing (alignRight, centerX, height, paddingXY, px, spaceEvenly, spacing, width)
import Form.Questions.Types exposing (QState)
import Geometry.Svg as Geo
import Html exposing (..)
import List.Extra as Extra
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.PathD exposing (Point, Segment(..), pathD)
import UI.Button
import UI.Style
import UI.Text


type Msg
    = SetWinner AnswerID Slot Winner


type IsWinner
    = Yes
    | No
    | Undecided


isWinner : Winner -> Winner -> IsWinner
isWinner bracketWinner homeOrAway =
    case bracketWinner of
        None ->
            Undecided

        _ ->
            if homeOrAway == bracketWinner then
                Yes

            else
                No


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        SetWinner answerId slot homeOrAway ->
            let
                mAnswer =
                    Bets.Bet.getAnswer bet answerId

                newBet =
                    case mAnswer of
                        Just answer ->
                            Bets.Bet.setWinner bet answer slot homeOrAway

                        Nothing ->
                            bet
            in
            ( newBet, { qState | next = Nothing }, Cmd.none )


view : Bet -> QState -> Element.Element Msg
view bet qQState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qQState.answerId

        header =
            UI.Text.displayHeader "Klik je een weg door het schema"

        introtext =
            """Dit is het schema voor de tweede ronde en verder. In het midden staat de finale,
         daarboven en onder de ronden die daaraan voorafgaan. Voor de juiste kwartfinalisten
         krijg je 4 punten. Halve finalisten leveren 7 punten op, finalisten 10 punten en de
         kampioen 13 punten."""

        introduction =
            Element.paragraph [] [ UI.Text.simpleText introtext ]
    in
    case mAnswer of
        Just (( answerId, AnswerBracket bracket _ ) as answer) ->
            Element.column
                []
                [ header
                , introduction
                , viewBracket bet answer bracket
                , Element.el [ width (px 600), height (px 600) ]
                    (Element.html <|
                        Svg.svg
                            [ Attributes.width "600"
                            , Attributes.height "600"
                            , Attributes.viewBox "0 0 600 600"
                            ]
                            (arcs 250 16 ++ arcs 200 16 ++ arcs 150 8 ++ arcs 100 4 ++ arcs 50 2)
                    )
                ]

        _ ->
            Element.none


viewBracket : Bet -> Answer -> Bracket -> Element.Element Msg
viewBracket bet answer bracket =
    {-
       mn37 = MatchNode "m37" None tnra tnrc -- "2016/06/15 15:00" saintetienne (Just "W37")
       mn38 = MatchNode "m38" None tnwb tnt2 -- "2016/06/15 15:00" paris (Just "W38")
       mn39 = MatchNode "m39" None tnwd tnt4 -- "2016/06/15 15:00" lens (Just "W39")
       mn40 = MatchNode "m40" None tnwa tnt1 -- "2016/06/15 15:00" lyon (Just "W40")
       mn41 = MatchNode "m41" None tnwc tnt3 -- "2016/06/15 15:00" lille (Just "W41")
       mn42 = MatchNode "m42" None tnwf tnre -- "2016/06/15 15:00" toulouse (Just "W42")
       mn43 = MatchNode "m43" None tnwe tnrd -- "2016/06/15 15:00" saintdenis (Just "W43")
       mn44 = MatchNode "m44" None tnrb tnrf -- "2016/06/15 15:00" nice (Just "W44")

       mn45 = MatchNode "m45" None mn37 mn39 -- "2016/06/15 15:00" marseille (Just "W45")
       mn46 = MatchNode "m46" None mn38 mn42 --  "2016/06/15 15:00" lille (Just "W46")
       mn47 = MatchNode "m47" None mn41 mn43 -- "2016/06/15 15:00" bordeaux (Just "W47")
       mn48 = MatchNode "m48" None mn40 mn44 -- "2016/06/15 15:00" saintdenis (Just "W48")

       mn49 = MatchNode "m49" None mn45 mn46 -- "2016/06/15 15:00" lyon (Just "W49")
       mn50 = MatchNode "m50" None mn47 mn48 -- "2016/06/15 15:00" marseille (Just "W50")

       mn51 = MatchNode "m51" None mn49 mn50 -- "2016/06/15 15:00" saintdenis Nothing
    -}
    let
        v mb =
            viewMatchWinner bet answer mb

        final =
            B.get bracket "m64"

        m49 =
            v <| B.get bracket "m49"

        m50 =
            v <| B.get bracket "m50"

        m51 =
            v <| B.get bracket "m51"

        m52 =
            v <| B.get bracket "m52"

        m53 =
            v <| B.get bracket "m53"

        m54 =
            v <| B.get bracket "m54"

        m55 =
            v <| B.get bracket "m55"

        m56 =
            v <| B.get bracket "m56"

        m57 =
            v <| B.get bracket "m57"

        m58 =
            v <| B.get bracket "m58"

        m59 =
            v <| B.get bracket "m59"

        m60 =
            v <| B.get bracket "m60"

        m61 =
            v <| B.get bracket "m61"

        m62 =
            v <| B.get bracket "m62"

        m64 =
            v <| B.get bracket "m64"

        champion =
            mkButtonChamp final
    in
    Element.column
        [ spacing 10, width (px 600) ]
        [ Element.row [ spaceEvenly ] [ m49, m50, m53, m54 ]
        , Element.row [ spaceEvenly, paddingXY 76 0 ] [ m57, m58 ]
        , Element.row [ centerX ] [ m61 ]
        , Element.row [ alignRight, spacing 44 ] [ m64, champion ]
        , Element.row [ centerX ] [ m62 ]
        , Element.row [ spaceEvenly, paddingXY 76 0 ] [ m59, m60 ]
        , Element.row [ spaceEvenly ] [ m51, m52, m55, m56 ]
        ]


viewMatchWinner : a -> ( AnswerID, a2 ) -> Maybe Bracket -> Element.Element Msg
viewMatchWinner bet answer mBracket =
    case mBracket of
        Just (MatchNode slot winner home away rd _) ->
            let
                homeButton =
                    mkButton answer HomeTeam slot (isWinner winner HomeTeam) home

                awayButton =
                    mkButton answer AwayTeam slot (isWinner winner AwayTeam) away

                dash =
                    text " - "
            in
            Element.row [ spacing 7 ] [ homeButton, awayButton ]

        _ ->
            Element.none


mkButton : ( AnswerID, a2 ) -> Winner -> Slot -> IsWinner -> Bracket -> Element.Element Msg
mkButton answer wnnr slot isSelected bracket =
    let
        s =
            case isSelected of
                Yes ->
                    UI.Style.Selected

                No ->
                    UI.Style.Potential

                Undecided ->
                    UI.Style.Potential

        answerId =
            Tuple.first answer

        msg =
            SetWinner answerId slot wnnr

        attrs =
            []

        team =
            B.qualifier bracket
    in
    UI.Button.maybeTeamButton s msg team


mkButtonChamp : Maybe Bracket -> Element.Element Msg
mkButtonChamp mBracket =
    let
        mTeam =
            mBracket
                |> Maybe.andThen B.winner

        s =
            case mTeam of
                Just t ->
                    UI.Style.Selected

                Nothing ->
                    UI.Style.Potential

        attrs =
            []
    in
    UI.Button.maybeTeamBadge s mTeam


arcs : Float -> Int -> List (Svg Msg)
arcs radius segments =
    let
        segmentLength =
            360
                / toFloat segments

        starts : List Float
        starts =
            List.range 0 (segments - 1)
                |> List.map (toFloat >> (*) segmentLength)

        ends =
            List.map ((+) segmentLength) starts

        reds =
            Extra.cycle segments [ "#d3d2e6" ]

        blues =
            Extra.cycle segments [ "#f2f2f2" ]

        clrs =
            Extra.interweave reds blues

        uncurry f ( a, b, c ) =
            f a b c
    in
    Extra.zip3 starts ends clrs
        |> List.map (uncurry (describeArc 300 300 radius))



-- arc : Int -> Float -> Float -> String -> Svg Msg
-- arc radius segmentLength startAngle clr =
--     Geo.arc2d
--         [ Attributes.stroke clr
--         , Attributes.strokeWidth "48"
--         , Attributes.fill "#fff"
--         ]
--         (Arc2d.with
--             { centerPoint =
--                 Point2d.fromCoordinates ( 400, 400 )
--             , radius = toFloat radius
--             , startAngle = degrees startAngle
--             , sweptAngle = degrees segmentLength
--             }
--         )
-- type Leaf
--     = Home
--     | Away
--     | Entry
-- segment : Round -> Int -> Leaf -> String -> Svg Msg
-- segment rnd pos leaf name =
--     let
--         ( ring, maxPos ) =
--             case rnd of
--                 I ->
--                     ( 5, 16 )
--                 II ->
--                     ( 4, 16 )
--                 III ->
--                     ( 3, 8 )
--                 IV ->
--                     ( 2, 4 )
--                 V ->
--                     ( 1, 2 )
--                 VI ->
--                     ( 0, 1 )
--     in
--     TypedSVG.path
{-

   cosinerule
       r^2 = R^2 + R^2 - 2.R.R cos y

       2 . R^2 . cos y = 2 . R^2 - r^2

       cos y = (2 . R^2 - r^2) / (2 . R^2)

       radians = acos (2 . R^2 - r^2) / (2 . R^2)
-}


calculateAngle r1 r2 r3 =
    -- r1^2 = r2^2 + r3^2 - 2.r2.r3.cos alpha
    -- alpha = acos (r2^2 + r3^2 - r1^2) / (2 . r2 . r3)
    acos ((r2 ^ 2 + r3 ^ 2 - r1 ^ 2) / (2 * r2 * r3))


roundedBorderAngle borderRadius mainRadius =
    --acos ((2 * (mainRadius ^ 2)) - (borderRadius ^ 2)) / (2 * (mainRadius ^ 2))
    calculateAngle borderRadius mainRadius mainRadius


polarToCartesian centerX centerY radius angleInDegrees =
    let
        angleInRadians =
            (angleInDegrees - 90) * pi / 180.0
    in
    ( centerX + (radius * cos angleInRadians)
    , centerY + (radius * sin angleInRadians)
    )


describeArc x y innerRadius startAngle endAngle clr =
    let
        borderRadius =
            10

        startInner =
            polarToCartesian x y innerRadius startAngle

        endInner =
            polarToCartesian x y innerRadius endAngle

        gamma =
            calculateAngle borderRadius (outerRadius - borderRadius) (outerRadius - borderRadius)
                |> inDegrees

        innerRadiusXY =
            ( innerRadius, innerRadius )

        outerRadius =
            innerRadius + 40

        outerRadiusXY =
            ( outerRadius, outerRadius )

        startOuter =
            polarToCartesian x y outerRadius endAngle

        endOuter =
            polarToCartesian x y outerRadius (Debug.log "startAngle" startAngle + Debug.log "gamma" gamma)

        ( bX, bY ) =
            polarToCartesian x y (outerRadius - borderRadius) (startAngle + gamma)

        beta =
            let
                beta_ =
                    calculateAngle (outerRadius - borderRadius) borderRadius (outerRadius - borderRadius)
                        |> inDegrees
            in
            180 - beta_

        endBorder =
            polarToCartesian bX bY borderRadius (Debug.log "s+g-b" (startAngle + gamma - Debug.log "beta" beta))

        -- polarToCartesian x y outerRadius (startAngle + gamma)
        borderRadiusXY =
            ( borderRadius, borderRadius )

        -- startRoundedBorder =
        --     polarToCartesian centerRoundedBorderX centerRoundedBorderY borderRadius (1 - gamma)
        largeArcFlagInner =
            if endAngle - startAngle <= 180 then
                False

            else
                True

        largeArcFlagOuter =
            if startAngle - endAngle <= 180 then
                False

            else
                True

        xAxisRotation =
            0

        sweepFlag =
            False
    in
    Svg.path
        [ Attributes.d <|
            pathD
                [ M startInner
                , A innerRadiusXY 0 False (not largeArcFlagInner) endInner
                , L startOuter
                , A outerRadiusXY 0 False largeArcFlagOuter endOuter
                , A borderRadiusXY 0 False False endBorder
                , L startInner
                , Z
                ]
        , Attributes.fill clr
        ]
        []


describeArc2 x y innerRadius startAngle endAngle clr =
    let
        startInner =
            polarToCartesian x y innerRadius endAngle

        endInner =
            polarToCartesian x y innerRadius startAngle

        endRoundedBorder =
            polarToCartesian x y (outerRadius - borderRadius) endAngle

        gamma =
            calculateAngle borderRadius (outerRadius - borderRadius) (outerRadius - borderRadius)

        ( centerRoundedBorderX, centerRoundedBorderY ) =
            polarToCartesian x y (innerRadius + borderRadius) (endAngle + gamma)

        startRoundedBorder =
            polarToCartesian centerRoundedBorderX centerRoundedBorderY borderRadius (1 - gamma)

        innerRadiusXY =
            ( innerRadius, innerRadius )

        outerRadius =
            innerRadius + 40

        outerRadiusXY =
            ( outerRadius, outerRadius )

        startOuter =
            polarToCartesian x y outerRadius endAngle

        endOuter =
            polarToCartesian x y outerRadius (startAngle + gamma)

        borderRadius =
            5

        borderRadiusXY =
            ( borderRadius, borderRadius )

        largeArcFlagInner =
            if endAngle - startAngle <= 180 then
                False

            else
                True

        largeArcFlagOuter =
            if startAngle - endAngle <= 180 then
                False

            else
                True

        xAxisRotation =
            0

        sweepFlag =
            False
    in
    Svg.path
        [ Attributes.d <|
            pathD
                [ M startInner
                , A innerRadiusXY 0 False largeArcFlagInner endInner
                , L endOuter
                , A outerRadiusXY 0 False (not largeArcFlagOuter) startRoundedBorder
                , A borderRadiusXY 0 False False endRoundedBorder
                , L startInner
                , Z
                ]
        , Attributes.fill clr
        ]
        []



-- Svg.path
--     [ Attributes.d <|
--         pathD
--             [ M startInner
--             , A innerRadiusXY 0 False largeArcFlagInner endInner
--             , L endOuter
--             , A outerRadiusXY 0 False (not largeArcFlagOuter) startOuter
--             , L startInner
--             , Z
--             ]
--     , Attributes.fill clr
--     ]
--     []
-- String.join " "
--     [ "M"
--     , String.fromFloat startInner.x
--     , String.fromFloat startInner.y
--     , "A"
--     , String.fromFloat innerRadius
--     , String.fromFloat innerRadius
--     , String.fromFloat xAxisRotation
--     , String.fromInt largeArcFlag
--     , String.fromInt sweepFlag
--     , String.fromFloat endInner.x
--     , String.fromFloat endInner.y
--     , "L"
--     , String.fromFloat endOuter.x
--     , String.fromFloat endOuter.y
--     , "A"
--     , String.fromFloat outerRadius
--     , String.fromFloat outerRadius
--     , String.fromFloat xAxisRotation
--     , String.fromInt largeArcFlag
--     , String.fromInt sweepFlag
--     , String.fromFloat startOuter.x
--     , String.fromFloat startOuter.y
--     , "L"
--     , String.fromFloat startInner.x
--     , String.fromFloat startOuter.x
--     ]
--     |> Svg.d
