module Form.Questions.Bracket exposing (Msg, update, view)

import Arc2d exposing (Arc2d)
import Basics.Extra exposing (inDegrees)
import Bets.Bet exposing (setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Bracket(..), CurrentSlot(..), HasQualified(..), Qualifier, SecondRoundCandidate(..), Selection, Slot, Team, Winner(..))
import Bets.Types.Bracket as B
import Bets.Types.Team as T
import Element exposing (alignRight, centerX, height, paddingXY, px, spaceEvenly, spacing, width)
import Form.Questions.Types exposing (Angle, BracketState(..), QState, QuestionType(..))
import Geometry.Svg as Geo
import Html exposing (..)
import List.Extra as Extra
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Events as Events
import Svg.PathD exposing (Point, Segment(..), pathD)
import UI.Button
import UI.Style exposing (ButtonSemantics(..))
import UI.Text


type Msg
    = SetWinner AnswerID Slot Winner
    | SetQualifier AnswerID Slot SecondRoundCandidate Angle Angle
    | SetSlot Slot Team
    | CloseQualifierView


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

        SetQualifier answerId slot candidates startAngle endAngle ->
            let
                bracketState =
                    ShowSecondRoundSelection slot candidates startAngle endAngle

                newQState =
                    { qState | questionType = QBracket bracketState }
            in
            ( bet, newQState, Cmd.none )

        SetSlot slot team ->
            let
                mAnswer =
                    Bets.Bet.getAnswer bet "br"

                newBet =
                    case mAnswer of
                        Just answer ->
                            Bets.Bet.setQualifier bet answer (Debug.log "set" slot) (Just team)

                        Nothing ->
                            bet
            in
            ( newBet, { qState | next = Nothing }, Cmd.none )

        CloseQualifierView ->
            let
                bracketState =
                    ShowMatches

                newQState =
                    { qState | questionType = QBracket bracketState }
            in
            ( bet, newQState, Cmd.none )


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
                , viewRings bet answer bracket qQState.questionType
                ]

        _ ->
            Element.none


viewRings bet answer bracket questionType =
    let
        slotSelected =
            case questionType of
                QBracket (ShowSecondRoundSelection slot _ _ _) ->
                    Just slot

                _ ->
                    Nothing

        positions =
            viewPositionRing bet answer bracket slotSelected

        center =
            case questionType of
                QBracket ShowMatches ->
                    viewMatchRings bet answer bracket

                QBracket (ShowSecondRoundSelection slot secondRoundCandidate startAngle endAngle) ->
                    viewCandidatesCircle bet answer bracket slot secondRoundCandidate startAngle endAngle

                _ ->
                    []

        rings =
            List.concat
                [ positions
                , center
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


viewPositionRing : Bet -> Answer -> Bracket -> Maybe Slot -> List (Svg Msg)
viewPositionRing bet answer bracket mSlot =
    let
        v slot =
            viewLeaf bet answer (B.get bracket slot) (isSelected slot)

        isSelected slot =
            case mSlot of
                Nothing ->
                    UI.Style.Potential

                Just s ->
                    if slot == s then
                        UI.Style.Selected

                    else
                        UI.Style.Potential

        wa =
            v "WA"

        wb =
            v "WB"

        wc =
            v "WC"

        wd =
            v "WD"

        we =
            v "WE"

        wf =
            v "WF"

        ra =
            v "RA"

        rb =
            v "RB"

        rc =
            v "RC"

        rd =
            v "RD"

        re =
            v "RE"

        rf =
            v "RF"

        t1 =
            v "T1"

        t2 =
            v "T2"

        t3 =
            v "T3"

        t4 =
            v "T4"

        mkRingData : List (Float -> Float -> Float -> Svg Msg) -> Svg Msg
        mkRingData positions =
            let
                angle =
                    360.0 / 16.0

                ring =
                    5
            in
            List.map (\_ -> angle) positions
                |> Extra.scanl (+) 0
                |> Extra.zip positions
                |> List.map (\( a, b ) -> ( b - 90, a ))
                |> List.map (\( angleStart, f ) -> f ring angle angleStart)
                |> Svg.g []

        ring5 =
            mkRingData [ wa, wb, wc, wd, we, wf, ra, rb, rc, rd, re, rf, t1, t2, t3, t4 ]
    in
    [ ring5 ]


viewMatchRings : Bet -> Answer -> Bracket -> List (Svg Msg)
viewMatchRings bet answer bracket =
    let
        v mb =
            viewLeaf bet answer mb UI.Style.Potential

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
                |> List.map (\( a, b ) -> ( b - 90, a ))
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
    rings


viewCandidatesCircle : Bet -> Answer -> Bracket -> Slot -> SecondRoundCandidate -> Angle -> Angle -> List (Svg Msg)
viewCandidatesCircle bet ( answerID, answer ) bracket slot candidates startAngle endAngle =
    let
        x =
            config.x

        y =
            config.y

        connectRadius =
            config.ringSpacing / 2

        connectOuterRadius =
            1 + ringRadius 5

        connectInnerRadius =
            ringRadius 5 - config.ringSpacing

        connectCenterRadius =
            ringRadius 5 - (config.ringSpacing / 2)

        connectAngle =
            calculateAngle (config.borderRadius / 2) connectCenterRadius connectCenterRadius

        arcAngle =
            calculateAngle (3 * connectRadius) connectCenterRadius connectCenterRadius

        left =
            startAngle + arcAngle

        right =
            endAngle - arcAngle

        leftXY =
            polarToCartesian x y connectCenterRadius left

        rightXY =
            polarToCartesian x y connectCenterRadius right

        leftInnerXY =
            polarToCartesian x y connectInnerRadius (startAngle + arcAngle)

        leftOuterXY =
            polarToCartesian x y connectOuterRadius (startAngle + arcAngle)

        rightInnerXY =
            polarToCartesian x y connectInnerRadius (endAngle - arcAngle)

        rightOuterXY =
            polarToCartesian x y connectOuterRadius (endAngle - arcAngle)

        connectPath =
            [ M rightOuterXY
            , A ( x, y ) 0 False False leftOuterXY
            , A leftXY 0 False True leftInnerXY
            , A ( x, y ) 0 False True rightInnerXY
            , A rightXY 0 False True rightOuterXY
            ]

        fillColor =
            config.colorSelected

        cds =
            B.candidatesForTeamNode bracket candidates slot

        button : Selection -> ( Int, Int ) -> Svg Msg
        button { currentSlot, group, team } ( row, col ) =
            let
                msg =
                    SetSlot slot team
            in
            mkQualifierButton msg team.teamID currentSlot row col

        rows =
            [ 0, 1, 2 ]

        cols =
            [ 0, 1, 2, 3 ]

        coords : Int -> List ( Int, Int )
        coords r =
            List.map (Tuple.pair r) cols

        uncurry f ( a, b ) =
            f a b

        buttons =
            List.concatMap coords rows
                |> Extra.zip cds
                |> List.map (uncurry button)

        closeView =
            mkButton 200 300 40 25 "sluit" CloseQualifierView UI.Style.Active
    in
    [ Svg.circle
        [ Attributes.cx (String.fromFloat config.x)
        , Attributes.cy (String.fromFloat config.y)
        , Attributes.r (String.fromFloat (ringRadius 5 - config.ringSpacing))
        , Attributes.fill fillColor
        ]
        []
    , Svg.path
        [ Attributes.d <|
            pathD connectPath
        , Attributes.fill fillColor
        , Attributes.stroke fillColor
        ]
        []
    , Svg.g [] (closeView :: buttons)
    ]


type alias Coord =
    { x : Float, y : Float }


mkQualifierButton : Msg -> String -> CurrentSlot -> Int -> Int -> Svg Msg
mkQualifierButton msg teamID currentSlot row col =
    let
        w =
            40

        h =
            25

        x =
            75
                + col
                * (w + 10)
                |> toFloat

        y =
            100
                + row
                * (h + 10)
                |> toFloat

        semantics =
            case Debug.log "current slot" currentSlot of
                ThisSlot ->
                    UI.Style.Selected

                OtherSlot _ ->
                    UI.Style.Perhaps

                NoSlot ->
                    UI.Style.Potential
    in
    mkButton x y w h teamID msg semantics


mkButton : Float -> Float -> Float -> Float -> String -> Msg -> UI.Style.ButtonSemantics -> Svg Msg
mkButton x y w h caption msg semantics =
    let
        textPathStart =
            ( x + 2, y + 18 )

        textPathEnd =
            ( x + w - 2, y + 18 )

        pathId =
            String.join "-" [ "p", String.fromInt (round x), String.fromInt (round y) ]

        ( stroke, fill, txt ) =
            case Debug.log "current slot" semantics of
                UI.Style.Selected ->
                    ( config.colorSelected, config.colorSelected, "black" )

                UI.Style.Potential ->
                    ( "orange", "grey", "black" )

                _ ->
                    ( "white", "black", "white" )

        textPath =
            Svg.path
                [ Attributes.d <|
                    pathD
                        [ M textPathStart
                        , L textPathEnd
                        ]
                , Attributes.stroke "transparent"
                , Attributes.fill "none"
                , Attributes.id pathId
                ]
                []
    in
    Svg.g [ Events.onClick msg ]
        [ Svg.rect
            [ Attributes.x <| String.fromFloat x
            , Attributes.y <| String.fromFloat y
            , Attributes.width <| String.fromFloat w
            , Attributes.height <| String.fromFloat h
            , Attributes.stroke stroke
            , Attributes.fill fill
            ]
            []
        , textPath
        , Svg.text_
            [ Attributes.fill "red"
            , Attributes.fontFamily "Roboto Mono"
            , Attributes.fontSize "18"
            , Attributes.textAnchor "middle"
            ]
            [ Svg.textPath
                [ Attributes.xlinkHref ("#" ++ pathId)
                , Attributes.startOffset "50%"
                ]
                [ Svg.text caption ]
            ]
        ]



-- viewMatchRing : Bet -> Answer -> (Maybe Bracket) -> Float -> Float ->List (Svg Msg)
-- viewMatchRing bet answer  matches segmentAngleSize segmentAngleSize  =
--     let
--     in
--         List.concatMap (uncurry (viewRingMatch bet answer ring segmentAngleSize)) matchesAndAngles


viewLeaf : Bet -> Answer -> Maybe Bracket -> UI.Style.ButtonSemantics -> Float -> Float -> Float -> Svg Msg
viewLeaf bet answer mBracket isSelected ring segmentAngleSize angle =
    case mBracket of
        Just (MatchNode slot winner home away rd _) ->
            let
                awayStartAngle =
                    angle + (segmentAngleSize / 2)

                awayEndAngle =
                    angle + segmentAngleSize

                centerMatchAngle =
                    awayStartAngle

                homeLeaf =
                    Leaf ring angle awayStartAngle "#ececec" HomeLeaf slot

                awayLeaf =
                    Leaf ring awayStartAngle awayEndAngle "#ececec" AwayLeaf slot

                -- dash =
                --     text " - "
                -- moveOut =
                --     if round ring < 2 then
                --         moveDiagonal2 ring (Debug.log "center angle" centerMatchAngle) 0
                --     else
                --         moveDiagonal2 ring (Debug.log "center angle" centerMatchAngle) 2
            in
            List.concat
                [ viewMatchLeaf answer HomeTeam slot (isWinner winner HomeTeam) home homeLeaf
                , viewMatchLeaf answer AwayTeam slot (isWinner winner AwayTeam) away awayLeaf
                ]
                |> Svg.g []

        Just (TeamNode slot candidate qualifier hasQualified) ->
            let
                endAngle =
                    angle + segmentAngleSize

                centerAngle =
                    angle + (segmentAngleSize / 2)

                leaf =
                    Leaf ring angle endAngle "#ececec" QualifierLeaf slot

                msg =
                    SetQualifier (Tuple.first answer) slot candidate angle endAngle
            in
            List.concat
                [ viewCandidateLeaf answer slot isSelected candidate qualifier hasQualified leaf
                ]
                |> Svg.g [ Events.onClick msg ]

        _ ->
            Svg.g [] []


viewCandidateLeaf : ( AnswerID, a2 ) -> Slot -> UI.Style.ButtonSemantics -> SecondRoundCandidate -> Maybe Team -> HasQualified -> Leaf -> List (Svg Msg)
viewCandidateLeaf answer slot isSelected candidate qualifier hasQualified leaf =
    let
        answerId =
            Tuple.first answer

        msg =
            SetQualifier answerId slot candidate
    in
    mkLeaf isSelected qualifier leaf


viewMatchLeaf : ( AnswerID, a2 ) -> Winner -> Slot -> IsWinner -> Bracket -> Leaf -> List (Svg Msg)
viewMatchLeaf answer wnnr slot isSelected bracket leaf =
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

        mTeam =
            B.qualifier bracket
    in
    mkLeaf s mTeam leaf


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

                QualifierLeaf ->
                    "qual"
    in
    String.join " " attrs


mkLeaf : ButtonSemantics -> Maybe Team -> Leaf -> List (Svg Msg)
mkLeaf s team leaf =
    [ describeLeaf s leaf, setText s team leaf ]


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
    | QualifierLeaf


config =
    { x = 170
    , y = 170
    , borderRadius = 10
    , ringHeight = 25
    , ringSpacing = 5
    , colorPotential = "#ececec"
    , colorSelected = "#red"
    }


describeLeaf : ButtonSemantics -> Leaf -> Svg.Svg Msg
describeLeaf s ({ ring, startAngle, endAngle, clr, leafType } as leaf) =
    let
        x =
            config.x

        y =
            config.y

        borderRadius =
            config.borderRadius

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

        gamma =
            calculateAngle borderRadius (outerRadius - borderRadius) (outerRadius - borderRadius)

        beta =
            calculateAngle (outerRadius - borderRadius) borderRadius (outerRadius - borderRadius)

        alpha =
            180 - beta

        segmentPath =
            case leafType of
                HomeLeaf ->
                    let
                        endInner =
                            polarToCartesian x y innerRadius endAngle

                        startOuter =
                            polarToCartesian x y outerRadius endAngle

                        endOuter =
                            polarToCartesian x y outerRadius (startAngle + gamma)

                        ( upperBorderX, upperBorderY ) =
                            polarToCartesian x y (outerRadius - borderRadius) (startAngle + gamma)

                        upperBorderEnd =
                            polarToCartesian upperBorderX upperBorderY borderRadius (startAngle + gamma - alpha)

                        ( lowerBorderX, lowerBorderY ) =
                            polarToCartesian x y (innerRadius + borderRadius) (startAngle + gamma)

                        lowerBorderStart =
                            polarToCartesian lowerBorderX lowerBorderY borderRadius (startAngle + gamma - alpha)

                        lowerBorderEnd =
                            polarToCartesian lowerBorderX lowerBorderY borderRadius (startAngle + 180 + gamma)

                        sweepFlag =
                            if startAngle - endAngle <= 180 then
                                False

                            else
                                True
                    in
                    [ M endInner
                    , L startOuter
                    , A outerRadiusXY xAxisRotation False sweepFlag endOuter
                    , A borderRadiusXY xAxisRotation False sweepFlag upperBorderEnd
                    , L lowerBorderStart
                    , A borderRadiusXY xAxisRotation False sweepFlag lowerBorderEnd
                    , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
                    ]

                AwayLeaf ->
                    let
                        endInner =
                            polarToCartesian x y innerRadius startAngle

                        startOuter =
                            polarToCartesian x y outerRadius startAngle

                        endOuter =
                            polarToCartesian x y outerRadius (endAngle - gamma)

                        ( upperBorderX, upperBorderY ) =
                            polarToCartesian x y (outerRadius - borderRadius) (endAngle - gamma)

                        upperBorderEnd =
                            polarToCartesian upperBorderX upperBorderY borderRadius (endAngle - gamma + alpha)

                        ( lowerBorderX, lowerBorderY ) =
                            polarToCartesian x y (innerRadius + borderRadius) (endAngle - gamma)

                        lowerBorderStart =
                            polarToCartesian lowerBorderX lowerBorderY borderRadius (endAngle - gamma + alpha)

                        lowerBorderEnd =
                            polarToCartesian lowerBorderX lowerBorderY borderRadius (endAngle + 180 - gamma)

                        sweepFlag =
                            if startAngle - endAngle <= 180 then
                                True

                            else
                                False
                    in
                    [ M endInner
                    , L startOuter
                    , A outerRadiusXY xAxisRotation False sweepFlag endOuter
                    , A borderRadiusXY xAxisRotation False sweepFlag upperBorderEnd
                    , L lowerBorderStart
                    , A borderRadiusXY xAxisRotation False sweepFlag lowerBorderEnd
                    , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
                    ]

                QualifierLeaf ->
                    let
                        ( upperBorderLeftX, upperBorderLeftY ) =
                            polarToCartesian x y (outerRadius - borderRadius) (startAngle + gamma)

                        ( upperBorderRightX, upperBorderRightY ) =
                            polarToCartesian x y (outerRadius - borderRadius) (endAngle - gamma)

                        upperBorderLeftStart =
                            polarToCartesian upperBorderLeftX upperBorderLeftY borderRadius (startAngle + gamma)

                        upperBorderRightStart =
                            polarToCartesian upperBorderRightX upperBorderRightY borderRadius (endAngle - gamma)

                        upperBorderLeftEnd =
                            polarToCartesian upperBorderLeftX upperBorderLeftY borderRadius (startAngle + gamma - alpha)

                        upperBorderRightEnd =
                            polarToCartesian upperBorderRightX upperBorderRightY borderRadius (endAngle - gamma + alpha)

                        ( lowerBorderLeftX, lowerBorderLeftY ) =
                            polarToCartesian x y (innerRadius + borderRadius) (startAngle + gamma)

                        ( lowerBorderRightX, lowerBorderRightY ) =
                            polarToCartesian x y (innerRadius + borderRadius) (endAngle - gamma)

                        lowerBorderLeftStart =
                            polarToCartesian lowerBorderLeftX lowerBorderLeftY borderRadius (startAngle + gamma - 180)

                        lowerBorderLeftEnd =
                            polarToCartesian lowerBorderLeftX lowerBorderLeftY borderRadius (startAngle + gamma - alpha)

                        lowerBorderRightStart =
                            polarToCartesian lowerBorderRightX lowerBorderRightY borderRadius (endAngle - gamma - 180)

                        lowerBorderRightEnd =
                            polarToCartesian lowerBorderRightX lowerBorderRightY borderRadius (endAngle - gamma + alpha)

                        sweepFlag =
                            if startAngle - endAngle <= 180 then
                                False

                            else
                                True

                        borderArc coord =
                            A borderRadiusXY xAxisRotation False False coord
                    in
                    [ M lowerBorderRightStart
                    , borderArc lowerBorderRightEnd
                    , L upperBorderRightEnd
                    , borderArc upperBorderRightStart
                    , A outerRadiusXY xAxisRotation False sweepFlag upperBorderLeftStart
                    , borderArc upperBorderLeftEnd
                    , L lowerBorderLeftEnd
                    , borderArc lowerBorderLeftStart
                    , A innerRadiusXY xAxisRotation False (not sweepFlag) lowerBorderRightStart
                    ]

        fillColour =
            case s of
                UI.Style.Selected ->
                    config.colorSelected

                _ ->
                    config.colorPotential

        -- logging =
        --     Debug.log "leaf: " (leafToString leaf)
    in
    Svg.path
        [ Attributes.d <|
            pathD segmentPath
        , Attributes.fill fillColour
        ]
        []


setText : ButtonSemantics -> Qualifier -> Leaf -> Svg.Svg Msg
setText s qualifier { ring, startAngle, endAngle, team } =
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
                [ Attributes.fill "black"
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
{- cosinerule
   r^2 = R^2 + R^2 - 2.R.R cos y

   2 . R^2 . cos y = 2 . R^2 - r^2

   cos y = (2 . R^2 - r^2) / (2 . R^2)

   radians = acos (2 . R^2 - r^2) / (2 . R^2)
-}


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
-- arcs : Float -> Int -> List (Svg Msg)
-- arcs ring segments =
--     let
--         segmentLength =
--             360
--                 / toFloat segments
--         starts : List Float
--         starts =
--             List.range 0 (segments - 1)
--                 |> List.map (toFloat >> (*) segmentLength >> (+) 270)
--         ends =
--             List.map ((+) segmentLength) starts
--         clrs =
--             Extra.cycle segments [ "#d3d2e6", "#d3d2e6", "#f2f2f2", "#f2f2f2" ]
--         apply ( f, a ) =
--             f a
--         homeLeaf ( s, e, c ) =
--             Leaf ring s e c HomeLeaf c
--         awayLeaf ( s, e, c ) =
--             Leaf ring s e c AwayLeaf c
--         consLeafs =
--             Extra.cycle segments [ homeLeaf, awayLeaf ]
--         leafs =
--             Extra.zip3 starts ends clrs
--                 |> Extra.zip consLeafs
--                 |> List.map apply
--         renderLeafs =
--             List.map describeLeaf leafs
--         renderTexts =
--             List.map (setText Nothing) leafs
--     in
--     renderLeafs ++ renderTexts
