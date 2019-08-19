module Form.Questions.Bracket exposing (Msg, update, view)

import Arc2d exposing (Arc2d)
import Basics.Extra exposing (inDegrees)
import Bets.Bet exposing (setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Bracket(..), Qualifier, Slot, Team, Winner(..))
import Bets.Types.Bracket as B
import Bets.Types.Team as T
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
                , viewRings bet answer bracket

                -- , viewBracket bet answer bracket
                , Element.el [ width (px 365), height (px 365) ]
                    (Element.html <|
                        Svg.svg
                            [ Attributes.width "365"
                            , Attributes.height "365"
                            , Attributes.viewBox "0 0 365 365"
                            ]
                            (arcs 5 16 ++ arcs 4 16 ++ arcs 3 8 ++ arcs 2 4 ++ arcs 1 2)
                    )
                ]

        _ ->
            Element.none



-- viewBracket : Bet -> Answer -> Bracket -> Element.Element Msg
-- viewBracket bet answer bracket =
--     let
--         v mb =
--             viewMatchWinner bet answer mb
--         final =
--             B.get bracket "m51"
--         m37 =
--             v <| B.get bracket "m37"
--         m38 =
--             v <| B.get bracket "m38"
--         m39 =
--             v <| B.get bracket "m39"
--         m40 =
--             v <| B.get bracket "m40"
--         m41 =
--             v <| B.get bracket "m41"
--         m42 =
--             v <| B.get bracket "m42"
--         m43 =
--             v <| B.get bracket "m43"
--         m44 =
--             v <| B.get bracket "m44"
--         -- quarter finals
--         m45 =
--             v <| B.get bracket "m45"
--         m46 =
--             v <| B.get bracket "m46"
--         m47 =
--             v <| B.get bracket "m47"
--         m48 =
--             v <| B.get bracket "m48"
--         -- semi final
--         m49 =
--             v <| B.get bracket "m49"
--         m50 =
--             v <|
--                 B.get bracket "m50"
--         -- final
--         m51 =
--             v <| B.get bracket "m51"
--         champion =
--             mkButtonChamp final
--     in
--     Element.column
--         [ spacing 10, width (px 600) ]
--         [ Element.row [ spaceEvenly ] [ m41, m42, m37, m39 ]
--         , Element.row [ spaceEvenly, paddingXY 76 0 ] [ m45, m46 ]
--         , Element.row [ centerX ] [ m49 ]
--         , Element.row [ alignRight, spacing 44 ] [ m51, champion ]
--         , Element.row [ centerX ] [ m50 ]
--         , Element.row [ spaceEvenly, paddingXY 76 0 ] [ m47, m48 ]
--         , Element.row [ spaceEvenly ] [ m38, m40, m43, m44 ]
--         ]
-- viewMatchWinner : a -> ( AnswerID, a2 ) -> Maybe Bracket -> Element.Element Msg
-- viewMatchWinner bet answer mBracket =
--     case mBracket of
--         Just (MatchNode slot winner home away rd _) ->
--             let
--                 homeButton =
--                     mkButton answer HomeTeam slot (isWinner winner HomeTeam) home
--                 awayButton =
--                     mkButton answer AwayTeam slot (isWinner winner AwayTeam) away
--                 dash =
--                     text " - "
--             in
--             Element.row [ spacing 7 ] [ homeButton, awayButton ]
--         _ ->
--             Element.none


viewRings : Bet -> Answer -> Bracket -> Element.Element Msg
viewRings bet answer bracket =
    let
        v mb =
            viewRingMatch bet answer mb

        final =
            B.get bracket "m51"

        m37 =
            v <| B.get bracket "m37"

        m38 =
            v <| B.get bracket "m38"

        m39 =
            v <| B.get bracket "m39"

        m40 =
            v <| B.get bracket "m40"

        m41 =
            v <| B.get bracket "m41"

        m42 =
            v <| B.get bracket "m42"

        m43 =
            v <| B.get bracket "m43"

        m44 =
            v <| B.get bracket "m44"

        -- quarter finals
        m45 =
            v <| B.get bracket "m45"

        m46 =
            v <| B.get bracket "m46"

        m47 =
            v <| B.get bracket "m47"

        m48 =
            v <| B.get bracket "m48"

        -- semi final
        m49 =
            v <| B.get bracket "m49"

        m50 =
            v <|
                B.get bracket "m50"

        -- final
        m51 =
            v <| B.get bracket "m51"

        -- champion =
        --     mkButtonChamp final
        applyValue a fs =
            List.concatMap (\f -> f a) fs

        fn1 ( a, b ) ( c, d ) =
            ( a + c, b )

        -- List.map (Tuple.pair segmentAngleSize) matches
        mkRingData : Float -> Float -> List (Float -> Float -> Float -> Svg Msg) -> Svg Msg
        mkRingData ring angle ms =
            List.map (\_ -> angle) ms
                |> Extra.scanl (+) 0
                |> Extra.zip ms
                |> List.map (\( a, b ) -> ( b + 90, a ))
                |> List.map (\( angleStart, f ) -> f ring angle angleStart)
                |> Svg.g []

        ring4 =
            mkRingData 4 (360.0 / 8.0) [ m41, m42, m37, m39, m38, m40, m43, m44 ]

        ring3 =
            mkRingData 3 (360.0 / 4.0) [ m45, m46, m47, m48 ]

        ring2 =
            mkRingData 2 (360.0 / 2.0) [ m49, m50 ]

        ring1 =
            mkRingData 1 (360.0 / 1.0) [ m51 ]

        rings =
            [ ring4
            , ring3
            , ring2
            , ring1
            ]
    in
    Element.el [ width (px 365), height (px 365) ]
        (Element.html <|
            Svg.svg
                [ Attributes.width "365"
                , Attributes.height "365"
                , Attributes.viewBox "0 0 365 365"
                ]
                rings
        )



-- viewMatchRing : Bet -> Answer -> (Maybe Bracket) -> Float -> Float ->List (Svg Msg)
-- viewMatchRing bet answer  matches segmentAngleSize segmentAngleSize  =
--     let
--     in
--         List.concatMap (uncurry (viewRingMatch bet answer ring segmentAngleSize)) matchesAndAngles


viewRingMatch : Bet -> Answer -> Maybe Bracket -> Float -> Float -> Float -> Svg Msg
viewRingMatch bet answer mBracket ring segmentAngleSize matchAngle =
    case mBracket of
        Just (MatchNode slot winner home away rd _) ->
            let
                awayStartAngle =
                    matchAngle + (segmentAngleSize / 2)

                awayEndAngle =
                    matchAngle + segmentAngleSize

                centerMatchAngle =
                    awayStartAngle

                homeLeaf =
                    Leaf ring matchAngle awayStartAngle "#cecece" HomeLeaf slot

                awayLeaf =
                    Leaf ring awayStartAngle awayEndAngle "#ececec" AwayLeaf slot

                dash =
                    text " - "

                moveOut =
                    if round ring < 2 then
                        moveDiagonal2 ring (Debug.log "center angle" centerMatchAngle) 0

                    else
                        moveDiagonal2 ring (Debug.log "center angle" centerMatchAngle) 2
            in
            List.concat
                [ viewLeaf answer HomeTeam slot (isWinner winner HomeTeam) home homeLeaf
                , viewLeaf answer AwayTeam slot (isWinner winner AwayTeam) away awayLeaf
                ]
                |> Svg.g []

        _ ->
            Svg.g [] []


viewLeaf : ( AnswerID, a2 ) -> Winner -> Slot -> IsWinner -> Bracket -> Leaf -> List (Svg Msg)
viewLeaf answer wnnr slot isSelected bracket leaf =
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

        team =
            B.qualifier bracket
    in
    mkLeaf s msg team leaf


leafToString : Leaf -> String
leafToString { ring, startAngle, endAngle, leafType } =
    let
        attrs =
            [ String.fromInt <| round ring
            , lt
            , String.fromInt <| round startAngle
            , String.fromInt <| round endAngle
            ]

        lt =
            case leafType of
                HomeLeaf ->
                    "home"

                AwayLeaf ->
                    "away"
    in
    String.join " " attrs



-- mkButton : ( AnswerID, a2 ) -> Winner -> Slot -> IsWinner -> Bracket -> Element.Element Msg
-- mkButton answer wnnr slot isSelected bracket =
--     let
--         s =
--             case isSelected of
--                 Yes ->
--                     UI.Style.Selected
--                 No ->
--                     UI.Style.Potential
--                 Undecided ->
--                     UI.Style.Potential
--         answerId =
--             Tuple.first answer
--         msg =
--             SetWinner answerId slot wnnr
--         attrs =
--             []
--         team =
--             B.qualifier bracket
--     in
--     UI.Button.maybeTeamButton s msg team
-- mkButtonChamp : Maybe Bracket -> Element.Element Msg
-- mkButtonChamp mBracket =
--     let
--         mTeam =
--             mBracket
--                 |> Maybe.andThen B.winner
--         s =
--             case mTeam of
--                 Just t ->
--                     UI.Style.Selected
--                 Nothing ->
--                     UI.Style.Potential
--         attrs =
--             []
--     in
--     UI.Button.maybeTeamBadge s mTeam


arcs : Float -> Int -> List (Svg Msg)
arcs ring segments =
    let
        segmentLength =
            360
                / toFloat segments

        starts : List Float
        starts =
            List.range 0 (segments - 1)
                |> List.map (toFloat >> (*) segmentLength >> (+) 270)

        ends =
            List.map ((+) segmentLength) starts

        clrs =
            Extra.cycle segments [ "#d3d2e6", "#d3d2e6", "#f2f2f2", "#f2f2f2" ]

        apply ( f, a ) =
            f a

        homeLeaf ( s, e, c ) =
            Leaf ring s e c HomeLeaf c

        awayLeaf ( s, e, c ) =
            Leaf ring s e c AwayLeaf c

        consLeafs =
            Extra.cycle segments [ homeLeaf, awayLeaf ]

        leafs =
            Extra.zip3 starts ends clrs
                |> Extra.zip consLeafs
                |> List.map apply

        renderLeafs =
            List.map describeLeaf leafs

        renderTexts =
            List.map (setText Nothing) leafs
    in
    renderLeafs ++ renderTexts


mkLeaf s msg team leaf =
    [ describeLeaf leaf, setText team leaf ]



{- cosinerule
   r^2 = R^2 + R^2 - 2.R.R cos y

   2 . R^2 . cos y = 2 . R^2 - r^2

   cos y = (2 . R^2 - r^2) / (2 . R^2)

   radians = acos (2 . R^2 - r^2) / (2 . R^2)
-}


type alias Leaf =
    { ring : Float
    , startAngle : Float
    , endAngle : Float
    , clr : String
    , leafType : LeafType
    , team : String
    }


type LeafType
    = HomeLeaf
    | AwayLeaf


config =
    { x = 170
    , y = 170
    , borderRadius = 5
    , ringHeight = 25
    , ringSpacing = 5
    }



-- describeLeaf2 : Leaf -> Svg.Svg Msg
-- describeLeaf2 ({ ring, startAngle, endAngle, clr, leafType } as leaf) =
--     let
--         x =
--             config.x
--         y =
--             config.y
--         borderRadius =
--             config.borderRadius
--         innerRadius =
--             ringRadius ring
--         innerRadiusXY =
--             ( innerRadius, innerRadius )
--         outerRadius =
--             innerRadius + config.ringHeight
--         outerRadiusXY =
--             ( outerRadius, outerRadius )
--         upperBorderRadiusXY =
--             ( borderRadius, borderRadius )
--         lowerBorderRadiusXY =
--             ( borderRadius, borderRadius )
--         gamma =
--             calculateAngle borderRadius (outerRadius - borderRadius) (outerRadius - borderRadius)
--         lowerGamma =
--             calculateAngle borderRadius (innerRadius + borderRadius) (innerRadius + borderRadius)
--         upperBeta =
--             let
--                 beta_ =
--                     calculateAngle (outerRadius - borderRadius) borderRadius (outerRadius - borderRadius)
--             in
--             case leafType of
--                 HomeLeaf ->
--                     180 - beta_
--                 AwayLeaf ->
--                     180 - beta_
--         lowerBeta =
--             let
--                 beta_ =
--                     calculateAngle (innerRadius + borderRadius) borderRadius (innerRadius + borderRadius)
--             in
--             case leafType of
--                 HomeLeaf ->
--                     180 - beta_
--                 AwayLeaf ->
--                     180 - beta_
--         largeArcFlagInner =
--             if endAngle - startAngle <= 180 then
--                 False
--             else
--                 True
--         sweepFlag =
--             if startAngle - endAngle <= 180 then
--                 False
--             else
--                 True
--         largeArcFlagOuter =
--             False
--         xAxisRotation =
--             0
--         startInner =
--             polarToCartesian x y innerRadius startAngle
--         endInner =
--             polarToCartesian x y innerRadius endAngle
--         startOuter =
--             case leafType of
--                 HomeLeaf ->
--                     polarToCartesian x y outerRadius endAngle
--                 AwayLeaf ->
--                     polarToCartesian x y outerRadius (endAngle - gamma)
--         endOuter =
--             case leafType of
--                 HomeLeaf ->
--                     polarToCartesian x y outerRadius (startAngle + gamma)
--                 AwayLeaf ->
--                     polarToCartesian x y outerRadius startAngle
--         ( bX, bY ) =
--             case leafType of
--                 HomeLeaf ->
--                     polarToCartesian x y (outerRadius - borderRadius) (startAngle + gamma)
--                 AwayLeaf ->
--                     polarToCartesian x y (outerRadius - borderRadius) (endAngle - gamma)
--         upperBorderEnd =
--             case leafType of
--                 HomeLeaf ->
--                     -- polarToCartesian bX bY borderRadius (Debug.log "s+g-b" (startAngle + gamma - Debug.log "upperBeta" upperBeta))
--                     polarToCartesian bX bY borderRadius (startAngle + gamma - upperBeta)
--                 AwayLeaf ->
--                     polarToCartesian bX bY borderRadius (endAngle - gamma + upperBeta)
--         segmentPath =
--             case leafType of
--                 HomeLeaf ->
--                     [ M startInner
--                     , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
--                     , L startOuter
--                     , A outerRadiusXY xAxisRotation False sweepFlag endOuter
--                     , A upperBorderRadiusXY xAxisRotation False False upperBorderEnd
--                     , L startInner
--                     , Z
--                     ]
--                 AwayLeaf ->
--                     [ M startInner
--                     , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
--                     , L upperBorderEnd
--                     , A upperBorderRadiusXY xAxisRotation False sweepFlag startOuter
--                     , A outerRadiusXY xAxisRotation False False endOuter
--                     , L startInner
--                     , Z
--                     ]
--         -- [ M endInner
--         -- , L startOuter
--         -- , A outerRadiusXY xAxisRotation False sweepFlag endOuter
--         -- , A upperBorderRadiusXY xAxisRotation False False upperBorderEnd
--         -- , L lowerBorderStart
--         -- , A lowerBorderRadiusXY xAxisRotation False False startInner
--         -- , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
--         -- ]
--         logging =
--             Debug.log "leaf: " (leafToString leaf)
--     in
--     Svg.path
--         [ Attributes.d <|
--             pathD segmentPath
--         , Attributes.fill clr
--         , Attributes.stroke "#34fg23"
--         ]
--         []


describeLeaf : Leaf -> Svg.Svg Msg
describeLeaf ({ ring, startAngle, endAngle, clr, leafType } as leaf) =
    let
        x =
            config.x

        y =
            config.y

        borderRadius =
            config.borderRadius

        endInner =
            case leafType of
                HomeLeaf ->
                    polarToCartesian x y innerRadius endAngle

                AwayLeaf ->
                    polarToCartesian x y innerRadius startAngle

        innerRadius =
            ringRadius ring

        outerRadius =
            innerRadius + config.ringHeight

        innerRadiusXY =
            ( innerRadius, innerRadius )

        outerRadiusXY =
            ( outerRadius, outerRadius )

        borderRadiusXY =
            ( borderRadius, borderRadius )

        xAxisRotation =
            0

        startOuter =
            case leafType of
                HomeLeaf ->
                    polarToCartesian x y outerRadius endAngle

                AwayLeaf ->
                    polarToCartesian x y outerRadius startAngle

        endOuter =
            case leafType of
                HomeLeaf ->
                    polarToCartesian x y outerRadius (startAngle + gamma)

                AwayLeaf ->
                    polarToCartesian x y outerRadius (endAngle - gamma)

        gamma =
            calculateAngle borderRadius (outerRadius - borderRadius) (outerRadius - borderRadius)

        ( upperBorderX, upperBorderY ) =
            case leafType of
                HomeLeaf ->
                    polarToCartesian x y (outerRadius - borderRadius) (startAngle + gamma)

                AwayLeaf ->
                    polarToCartesian x y (outerRadius - borderRadius) (endAngle - gamma)

        upperBorderEnd =
            case leafType of
                HomeLeaf ->
                    polarToCartesian upperBorderX upperBorderY borderRadius (startAngle + gamma - upperBeta)

                AwayLeaf ->
                    polarToCartesian upperBorderX upperBorderY borderRadius (endAngle - gamma + upperBeta)

        upperBeta =
            let
                beta_ =
                    calculateAngle (outerRadius - borderRadius) borderRadius (outerRadius - borderRadius)
            in
            case leafType of
                HomeLeaf ->
                    180 - beta_

                AwayLeaf ->
                    180 - beta_

        ( lowerBorderX, lowerBorderY ) =
            case leafType of
                HomeLeaf ->
                    polarToCartesian x y (innerRadius + borderRadius) (startAngle + gamma)

                AwayLeaf ->
                    polarToCartesian x y (innerRadius + borderRadius) (endAngle - gamma)

        lowerBorderStart =
            case leafType of
                HomeLeaf ->
                    polarToCartesian lowerBorderX lowerBorderY borderRadius (startAngle + gamma - upperBeta)

                AwayLeaf ->
                    polarToCartesian lowerBorderX lowerBorderY borderRadius (endAngle - gamma + upperBeta)

        lowerBorderEnd =
            case leafType of
                HomeLeaf ->
                    polarToCartesian lowerBorderX lowerBorderY borderRadius (startAngle + 180 + gamma)

                AwayLeaf ->
                    polarToCartesian lowerBorderX lowerBorderY borderRadius (endAngle + 180 - gamma)

        sweepFlag =
            if startAngle - endAngle <= 180 then
                case leafType of
                    HomeLeaf ->
                        False

                    AwayLeaf ->
                        True

            else
                case leafType of
                    HomeLeaf ->
                        True

                    AwayLeaf ->
                        False

        segmentPath =
            [ M endInner
            , L startOuter
            , A outerRadiusXY xAxisRotation False sweepFlag endOuter
            , A borderRadiusXY xAxisRotation False sweepFlag upperBorderEnd
            , L lowerBorderStart
            , A borderRadiusXY xAxisRotation False sweepFlag lowerBorderEnd
            , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
            ]

        logging =
            Debug.log "leaf: " (leafToString leaf)
    in
    Svg.path
        [ Attributes.d <|
            pathD segmentPath
        , Attributes.fill clr
        , Attributes.stroke "#34fg23"
        ]
        []


setText : Qualifier -> Leaf -> Svg.Svg Msg
setText qualifier { ring, startAngle, endAngle, team } =
    let
        teamId =
            T.mdisplayID qualifier

        x =
            config.x

        y =
            config.y

        innerRadius =
            ring * config.ringHeight + ((ring - 1) * config.ringSpacing)

        radius =
            6 + innerRadius

        radiusXY =
            ( radius, radius )

        startXY =
            polarToCartesian x y radius startAngle

        endXY =
            polarToCartesian x y radius endAngle

        sweepFlag =
            if startAngle - endAngle <= 180 then
                False

            else
                True

        pathId =
            "tp-" ++ String.fromInt (Basics.round startAngle) ++ "-" ++ String.fromInt (Basics.round radius)

        textPath =
            Svg.path
                [ Attributes.d <|
                    pathD
                        [ M startXY
                        , A radiusXY 0 False (not sweepFlag) endXY
                        ]
                , Attributes.stroke "transparent"
                , Attributes.fill "none"
                , Attributes.id pathId
                ]
                []

        textActual =
            Svg.text_
                [ Attributes.fill "red"
                , Attributes.fontFamily "Roboto Mono"
                , Attributes.fontSize "18"
                , Attributes.textAnchor "middle"
                ]
                [ Svg.textPath
                    [ Attributes.xlinkHref ("#" ++ pathId)
                    , Attributes.startOffset "50%"
                    ]
                    [ Svg.text (String.right 3 teamId) ]
                ]
    in
    Svg.g []
        [ textPath
        , textActual
        ]



-- SVG Helpers


calculateAngle r1 r2 r3 =
    -- cosinerule:
    -- r1^2 = r2^2 + r3^2 - 2.r2.r3.cos alpha
    -- alpha = acos (r2^2 + r3^2 - r1^2) / (2 . r2 . r3)
    acos ((r2 ^ 2 + r3 ^ 2 - r1 ^ 2) / (2 * r2 * r3))
        |> inDegrees


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


ringRadius ring =
    ring * config.ringHeight + ((ring - 1) * config.ringSpacing)


moveDiagonal : Float -> Float -> Svg.Attribute Msg
moveDiagonal angle distance =
    let
        x =
            cos angle * distance

        y =
            sin angle * distance
    in
    translate x y


moveDiagonal2 : Float -> Float -> Float -> Svg.Attribute Msg
moveDiagonal2 ring angle distance =
    let
        radius =
            ringRadius ring

        vectorStart =
            polarToCartesian config.x config.y radius angle

        vectorMove =
            polarToCartesian config.x config.y (radius + distance) angle

        delta ( x1, y1 ) ( x2, y2 ) =
            ( x2 - x1, y2 - y1 )

        ( x, y ) =
            delta vectorStart vectorMove
    in
    translate x y


translate x y =
    String.join ""
        [ "translate("
        , String.fromFloat x
        , ","
        , String.fromFloat y
        , ")"
        ]
        |> Attributes.transform
