module Form.Bracket.View exposing (viewCandidatesPanel, viewRings)

import Basics.Extra exposing (inDegrees)
import Bets.Bet
import Bets.Types exposing (Answer(..), Bet, Bracket(..), Candidate(..), CurrentSlot(..), Group, HasQualified(..), Qualifier, Selection, Slot, Team, TeamID, Winner(..))
import Bets.Types.Answer.Bracket
import Bets.Types.Bracket as B
import Bets.Types.Candidate as Candidate
import Bets.Types.Group as G
import Bets.Types.Team as T
import Dict
import Element exposing (height, px, width)
import Form.Bracket.Types exposing (..)
import List.Extra as Extra
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Events as Events
import Svg.PathD exposing (Segment(..), pathD)
import UI.Button
import UI.Color.RGB as RGB
import UI.Style exposing (ButtonSemantics(..))
import UI.Text


isComplete : Bet -> Bool
isComplete bet =
    Bets.Types.Answer.Bracket.isComplete bet.answers.bracket


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


viewRings : Bet -> Bracket -> State -> Element.Element Msg
viewRings bet bracket state =
    let
        slotSelected =
            case state of
                ShowSecondRoundSelection slot _ ->
                    Just slot

                _ ->
                    Nothing

        rings =
            case state of
                ShowMatches ->
                    viewMatchRings bet bracket

                ShowSecondRoundSelection _ candidate ->
                    List.concat
                        [ viewPositionRing bet bracket slotSelected
                        , viewCandidatesCircle candidate
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


viewPositionRing : Bet -> Bracket -> Maybe Slot -> List (Svg Msg)
viewPositionRing bet bracket mSlot =
    let
        v slot =
            viewLeaf bet (B.get bracket slot) (isSelected slot)

        isSelected slot =
            case mSlot of
                Nothing ->
                    UI.Style.Potential

                Just s ->
                    if slot == s then
                        UI.Style.Focus

                    else
                        UI.Style.Potential

        mkRingData : List ( ( Slot, Qualifier ), ( Angle, Angle ) ) -> Svg Msg
        mkRingData slotdata =
            let
                f ( ( slot, q ), ( a, s ) ) =
                    viewLeaf bet (B.get bracket slot) (isSelected slot) ring s a

                ring =
                    4
            in
            List.map f slotdata
                |> Svg.g []

        ring5 =
            teamNodeSlots bet
                |> mkRingData

        crossHairs =
            List.repeat 8 45.0
                |> Extra.scanl (+) 0
                |> List.map (mkCrossHair 4)
    in
    ring5 :: crossHairs


teamNodeSlots : Bet -> List ( ( Slot, Qualifier ), ( Angle, Angle ) )
teamNodeSlots bet =
    let
        segmentAngle =
            22.5

        slots =
            Bets.Bet.getBracket bet
                |> B.getQualifiers
    in
    slots
        |> List.map (\_ -> segmentAngle)
        |> Extra.scanl (+) 0
        |> List.map (\a -> ( a - 90, segmentAngle ))
        |> Extra.zip slots


viewMatchRings : Bet -> Bracket -> List (Svg Msg)
viewMatchRings bet bracket =
    let
        v mb =
            viewLeaf bet mb UI.Style.Potential

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

        -- List.map (Tuple.pair segmentAngleSize) matches
        mkRingData : Float -> Float -> List (Float -> Float -> Float -> Svg Msg) -> Svg Msg
        mkRingData angle ring ms =
            List.map (\_ -> angle) ms
                |> Extra.scanl (+) 0
                |> Extra.zip ms
                |> List.map (\( a, b ) -> ( b - 90, a ))
                |> List.map (\( angleStart, f ) -> f ring angle angleStart)
                |> Svg.g []

        ringData =
            [ ( 4, [ m41, m42, m37, m39, m38, m40, m43, m44 ] )
            , ( 3, [ m45, m46, m47, m48 ] )
            , ( 2, [ m49, m50 ] )
            , ( 1, [ m51 ] )
            ]

        ringViews d =
            let
                f =
                    mkRingData (360.0 / (List.length >> toFloat) (Tuple.second d))
            in
            uncurry f d

        rings =
            List.append (List.map ringViews ringData) [ viewChampion bracket ]

        mkCrossHairData angle ring segments =
            List.map (\_ -> angle) segments
                |> Extra.scanl (+) 90
                |> List.map (\a -> mkCrossHair ring a)

        crossHairViews d =
            let
                f =
                    mkCrossHairData (360.0 / (List.length >> toFloat) (Tuple.second d))
            in
            uncurry f d

        crossHairs =
            List.concatMap crossHairViews ringData
    in
    rings ++ crossHairs


viewChampion : Bracket -> Svg Msg
viewChampion bracket =
    case
        B.qualifier bracket
    of
        Just team ->
            let
                radius =
                    ringRadius 1 - config.ringSpacing

                x =
                    Attributes.cx <| String.fromFloat config.x

                y =
                    Attributes.cy <| String.fromFloat config.y

                r =
                    Attributes.r <| String.fromFloat radius

                c1 =
                    ( config.x - radius, config.y + 6 )

                c2 =
                    ( config.x + radius, config.y + 6 )

                clr =
                    Attributes.fill config.colorSelected
            in
            Svg.g []
                [ Svg.circle [ x, y, r, clr ] []
                , mkText team.teamID RGB.white c1 c2
                ]

        _ ->
            Svg.g [] []


viewCandidatesPanel : Bet -> Bracket -> Slot -> Candidate -> Element.Element Msg
viewCandidatesPanel _ bracket slot candidates =
    let
        cds =
            B.candidatesForTeamNode bracket candidates slot
                |> List.map (\s -> ( s.group, button s ))
                |> Extra.groupWhile (\t r -> G.equal (Tuple.first t) (Tuple.first r))
                |> List.map (uncurry (::))
                |> List.map (List.map (Tuple.second >> Tuple.second))
                |> List.map (Element.row [ Element.spacing 16 ])

        button : Selection -> ( Group, Element.Element Msg )
        button { currentSlot, group, team } =
            let
                msg =
                    SetSlot slot team
            in
            ( group, mkTeamButton msg team currentSlot )
    in
    Element.column [ Element.spacing 16 ] cds


mkTeamButton : Msg -> Team -> CurrentSlot -> Element.Element Msg
mkTeamButton msg team currentSlot =
    let
        semantics =
            case currentSlot of
                ThisSlot ->
                    UI.Style.Selected

                OtherSlot _ ->
                    UI.Style.Perhaps

                NoSlot ->
                    UI.Style.Potential
    in
    UI.Button.teamButton semantics msg team


viewCandidatesCircle : Candidate -> List (Svg Msg)
viewCandidatesCircle candidates =
    let
        x =
            config.x

        y =
            config.y

        radius =
            ringRadius 4 - config.ringSpacing

        c1 =
            ( x - radius, y - 16 )

        c2 =
            ( x + radius, y - 16 )

        g1 =
            ( x - radius, y + 16 )

        g2 =
            ( x + radius, y + 16 )

        fillColor =
            config.colorSelected

        -- cds =
        --     B.candidatesForTeamNode bracket candidates slot
        -- button : Selection -> ( Int, Int ) -> Svg Msg
        -- button { currentSlot, team } ( row, col ) =
        --     let
        --         msg =
        --             SetSlot slot team
        --     in
        --     mkQualifierButton msg team.teamID currentSlot row col
        -- rows =
        --     [ 0, 1, 2, 3 ]
        -- cols =
        --     [ 0, 1, 2, 3 ]
        -- coords : Int -> List ( Int, Int )
        -- coords r =
        --     List.map (Tuple.pair r) cols
        -- buttons =
        --     List.concatMap coords rows
        --         |> Extra.zip cds
        --         |> List.map (uncurry button)
        closeView =
            mkButton 200 300 40 25 "sluit" CloseQualifierView UI.Style.Active

        ( positionString, groupString ) =
            case candidates of
                FirstPlace grp ->
                    ( "winnaar groep", G.toString grp )

                SecondPlace grp ->
                    ( "tweede groep", G.toString grp )

                BestThirdFrom grps ->
                    List.map G.toString grps
                        |> String.join "-"
                        |> Tuple.pair "beste nr 3 uit"
    in
    [ Svg.circle
        [ Attributes.cx (String.fromFloat config.x)
        , Attributes.cy (String.fromFloat config.y)
        , Attributes.r (String.fromFloat (ringRadius 4 - config.ringSpacing))
        , Attributes.fill fillColor
        ]
        []
    , Svg.g
        []
        [ mkText positionString RGB.white c1 c2
        , mkText groupString RGB.white g1 g2

        -- , mkText groupString RGB.white ( 75, 100 ) ( 265, 100 )
        ]
    , Svg.g [] [ closeView ]
    ]


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
            130
                + row
                * (h + 10)
                |> toFloat

        semantics =
            case currentSlot of
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
            ( x + 2, y + 19 )

        textPathEnd =
            ( x + w - 2, y + 19 )

        pathId =
            String.join "-" [ "p", String.fromInt (round x), String.fromInt (round y) ]

        ( stroke, fill, txt ) =
            case semantics of
                UI.Style.Selected ->
                    ( RGB.background, config.colorSelected, RGB.background )

                UI.Style.Potential ->
                    ( RGB.white, RGB.green, RGB.white )

                _ ->
                    ( RGB.background, RGB.black, RGB.secondaryText )

        textPath =
            Svg.path
                [ Attributes.d <|
                    pathD
                        [ M textPathStart
                        , L textPathEnd
                        ]
                , Attributes.stroke "transparent"
                , Attributes.fill RGB.white
                , Attributes.id pathId
                ]
                []
    in
    Svg.g [ Events.onClick msg, Attributes.cursor "pointer" ]
        [ Svg.rect
            [ Attributes.x <| String.fromFloat x
            , Attributes.y <| String.fromFloat y
            , Attributes.rx <| String.fromFloat 5
            , Attributes.ry <| String.fromFloat 5
            , Attributes.width <| String.fromFloat w
            , Attributes.height <| String.fromFloat h
            , Attributes.stroke stroke
            , Attributes.fill fill
            ]
            []
        , textPath
        , Svg.text_
            [ Attributes.fill txt
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


mkText : String -> String -> ( Float, Float ) -> ( Float, Float ) -> Svg Msg
mkText str clr (( x1, y1 ) as start) (( x2, y2 ) as end) =
    let
        p f =
            round f |> String.fromInt

        pathId =
            String.join "-" [ "p", p x1, p y1, p x2, p y2 ]
    in
    Svg.g
        []
        [ Svg.path
            [ Attributes.d <|
                pathD
                    [ M start
                    , L end
                    ]
            , Attributes.stroke "transparent"
            , Attributes.fill "none"
            , Attributes.id pathId
            ]
            []
        , Svg.text_
            [ Attributes.fill clr
            , Attributes.fontFamily "Roboto Mono"
            , Attributes.fontSize "18"
            , Attributes.textAnchor "middle"
            ]
            [ Svg.textPath
                [ Attributes.xlinkHref ("#" ++ pathId)
                , Attributes.startOffset "50%"
                ]
                [ Svg.text str ]
            ]
        ]



-- viewMatchRing : Bet -> Answer -> (Maybe Bracket) -> Float -> Float ->List (Svg Msg)
-- viewMatchRing bet answer  matches segmentAngleSize segmentAngleSize  =
--     let
--     in
--         List.concatMap (uncurry (viewRingMatch bet answer ring segmentAngleSize)) matchesAndAngles


viewLeaf : Bet -> Maybe Bracket -> UI.Style.ButtonSemantics -> Float -> Float -> Float -> Svg Msg
viewLeaf _ mBracket isSelected ring segmentAngleSize angle =
    case mBracket of
        Just (MatchNode slot winner home away _ _) ->
            let
                awayStartAngle =
                    angle + (segmentAngleSize / 2)

                awayEndAngle =
                    angle + segmentAngleSize

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
            Svg.g []
                [ viewMatchLeaf HomeTeam slot (isWinner winner HomeTeam) home homeLeaf
                , viewMatchLeaf AwayTeam slot (isWinner winner AwayTeam) away awayLeaf
                ]

        Just (TeamNode slot candidate qualifier _) ->
            let
                endAngle =
                    angle + segmentAngleSize

                leaf =
                    Leaf ring angle endAngle "#ececec" QualifierLeaf slot

                msg =
                    SetQualifier slot candidate
            in
            mkLeaf isSelected qualifier leaf msg

        -- List.concat
        --     [ viewCandidateLeaf answer slot isSelected candidate qualifier hasQualified leaf msg
        --     ]
        --     |> Svg.g [ Events.onClick msg ]
        _ ->
            Svg.g [] []


viewMatchLeaf : Winner -> Slot -> IsWinner -> Bracket -> Leaf -> Svg Msg
viewMatchLeaf wnnr slot isSelected bracket leaf =
    let
        s =
            case isSelected of
                Yes ->
                    UI.Style.Selected

                No ->
                    UI.Style.Potential

                Undecided ->
                    UI.Style.Potential

        msg =
            SetWinner slot wnnr

        mTeam =
            B.qualifier bracket
    in
    mkLeaf s mTeam leaf msg


mkLeaf : ButtonSemantics -> Maybe Team -> Leaf -> Msg -> Svg Msg
mkLeaf s team leaf msg =
    Svg.g [ Events.onClick msg, Attributes.cursor "pointer" ]
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
    , ringHeight = 33
    , ringSpacing = 2
    , colorPotential = RGB.panel
    , colorSelected = RGB.panel
    , colorFocus = RGB.green
    }


describeLeaf : ButtonSemantics -> Leaf -> Svg.Svg Msg
describeLeaf s { ring, startAngle, endAngle } =
    let
        x =
            config.x

        y =
            config.y

        innerRadius =
            ringRadius ring

        outerRadius =
            innerRadius + config.ringHeight

        innerRadiusXY =
            ( innerRadius, innerRadius )

        outerRadiusXY =
            ( outerRadius, outerRadius )

        xAxisRotation =
            0

        sweep =
            not <| startAngle - endAngle <= 180

        segmentPath sweepFlag =
            let
                endInner =
                    polarToCartesian x y innerRadius endAngle

                startOuter =
                    polarToCartesian x y outerRadius endAngle

                endOuter =
                    polarToCartesian x y outerRadius startAngle

                startInner =
                    polarToCartesian x y innerRadius startAngle
            in
            [ M endInner
            , L startOuter
            , A outerRadiusXY xAxisRotation False sweepFlag endOuter
            , L startInner
            , A innerRadiusXY xAxisRotation False (not sweepFlag) endInner
            ]

        fillColour =
            case s of
                UI.Style.Selected ->
                    config.colorSelected

                UI.Style.Focus ->
                    config.colorFocus

                _ ->
                    config.colorPotential
    in
    Svg.path
        [ Attributes.d <|
            pathD (segmentPath sweep)
        , Attributes.fill fillColour
        ]
        []


mkCrossHair : Float -> Float -> Svg Msg
mkCrossHair ring angle =
    let
        innerRadius =
            ringRadius (max ring 2)

        outerRadius =
            ringRadius 4 + config.ringHeight

        ( x1, y1 ) =
            polarToCartesian config.x config.y innerRadius angle

        ( x2, y2 ) =
            polarToCartesian config.x config.y outerRadius angle
    in
    Svg.line
        [ Attributes.strokeWidth "2"
        , Attributes.fill "none"
        , Attributes.stroke RGB.white
        , Attributes.x1 <| String.fromFloat x1
        , Attributes.y1 <| String.fromFloat y1
        , Attributes.x2 <| String.fromFloat x2
        , Attributes.y2 <| String.fromFloat y2
        ]
        []


type Half
    = Top
    | Bottom


setText : ButtonSemantics -> Qualifier -> Leaf -> Svg.Svg Msg
setText _ qualifier { ring, startAngle, endAngle } =
    let
        teamId =
            T.mdisplayID qualifier

        x =
            config.x

        y =
            config.y

        half =
            if startAngle < 90 then
                Top

            else
                Bottom

        innerRadius =
            ring * config.ringHeight + ((ring - 1) * config.ringSpacing)

        radius =
            case half of
                Top ->
                    10 + innerRadius

                Bottom ->
                    innerRadius + config.ringHeight - 10

        radiusXY =
            ( radius, radius )

        ( startXY, endXY ) =
            case half of
                Top ->
                    ( polarToCartesian x y radius startAngle, polarToCartesian x y radius endAngle )

                Bottom ->
                    ( polarToCartesian x y radius endAngle, polarToCartesian x y radius startAngle )

        bigArc =
            False

        sweep =
            startAngle - endAngle <= 180

        sweepFlag =
            case half of
                Top ->
                    sweep

                Bottom ->
                    not sweep

        pathId =
            "tp-" ++ String.fromInt (Basics.round startAngle) ++ "-" ++ String.fromInt (Basics.round radius)

        textPath =
            Svg.path
                [ Attributes.d <|
                    pathD
                        [ M startXY
                        , A radiusXY 0 bigArc sweepFlag endXY
                        ]
                , Attributes.stroke "transparent"
                , Attributes.fill "none"
                , Attributes.id pathId
                ]
                []

        textActual =
            Svg.text_
                [ Attributes.fill RGB.white
                , Attributes.fontFamily "Roboto Mono"
                , Attributes.fontSize "18"
                , Attributes.textAnchor "middle"
                ]
                [ Svg.textPath
                    [ Attributes.xlinkHref ("#" ++ pathId)
                    , Attributes.startOffset "50%"
                    ]
                    [ Svg.text <| Debug.log "team " (String.right 3 teamId) ]
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


calculateAngle : Float -> Float -> Float -> Float
calculateAngle r1 r2 r3 =
    -- cosinerule:
    -- r1^2 = r2^2 + r3^2 - 2.r2.r3.cos alpha
    -- alpha = acos (r2^2 + r3^2 - r1^2) / (2 . r2 . r3)
    acos ((r2 ^ 2 + r3 ^ 2 - r1 ^ 2) / (2 * r2 * r3))
        |> inDegrees


polarToCartesian : Float -> Float -> Float -> Float -> ( Float, Float )
polarToCartesian centerX centerY radius angleInDegrees =
    let
        angleInRadians =
            (angleInDegrees - 90) * pi / 180.0
    in
    ( centerX + (radius * cos angleInRadians)
    , centerY + (radius * sin angleInRadians)
    )


ringRadius : Float -> Float
ringRadius ring =
    ring * config.ringHeight + ((ring - 1) * config.ringSpacing)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
