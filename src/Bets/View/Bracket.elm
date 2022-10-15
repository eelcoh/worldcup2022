module Bets.View.Bracket exposing (viewRings)

import Bets.Bet
import Bets.Types exposing (Answer(..), Bet, Bracket(..), Candidate(..), CurrentSlot(..), Group, HasQualified(..), Qualifier, Selection, Slot, Team, Winner(..))
import Bets.Types.Bracket as B
import Bets.Types.Team as T
import Element exposing (height, px, width)
import Form.Bracket.Types exposing (Angle, BracketState(..), IsWinner(..), State)
import List.Extra as Extra
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.PathD exposing (Segment(..), pathD)
import UI.Color
import UI.Color.RGB as RGB
import UI.Screen as Screen
import UI.Style exposing (ButtonSemantics(..))


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


viewRings : Bet -> Bracket -> Screen.Size -> Element.Element msg
viewRings bet bracket state =
    let
        dims =
            maxWidth state

        sz =
            String.fromInt dims

        dimsPx =
            px dims

        rings =
            viewMatchRings bet bracket state
    in
    Element.el [ width dimsPx, height dimsPx, Element.centerX, Screen.className "rings" ]
        (Element.html <|
            Svg.svg
                [ Attributes.width sz
                , Attributes.height sz
                , Attributes.viewBox (String.join " " [ "0", "0", sz, sz ])
                ]
                rings
        )


viewMatchRings : Bet -> Bracket -> Screen.Size -> List (Svg msg)
viewMatchRings bet bracket state =
    let
        v mb =
            viewLeaf bet state mb UI.Style.Potential

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

        -- quarter finals
        m57 =
            v <| B.get bracket "m57"

        m58 =
            v <| B.get bracket "m58"

        m59 =
            v <| B.get bracket "m59"

        m60 =
            v <| B.get bracket "m60"

        -- semi final
        m61 =
            v <| B.get bracket "m61"

        m62 =
            v <|
                B.get bracket "m62"

        -- final
        m64 =
            v <| B.get bracket "m64"

        -- List.map (Tuple.pair segmentAngleSize) matches
        mkRingData : Float -> Float -> List (Float -> Float -> Float -> Svg msg) -> Svg msg
        mkRingData angle ring ms =
            List.map (\_ -> angle) ms
                |> Extra.scanl (+) 0
                |> Extra.zip ms
                |> List.map (\( a, b ) -> ( b - 90, a ))
                |> List.map (\( angleStart, f ) -> f ring angle angleStart)
                |> Svg.g []

        ringData =
            [ ( 4, [ m49, m50, m53, m54, m51, m52, m55, m56 ] )
            , ( 3, [ m57, m58, m59, m60 ] )
            , ( 2, [ m61, m62 ] )
            , ( 1, [ m64 ] )
            ]

        ringViews d =
            let
                f =
                    mkRingData (360.0 / (List.length >> toFloat) (Tuple.second d))
            in
            uncurry f d

        rings =
            List.append (List.map ringViews ringData) [ viewChampion bracket state ]

        mkCrossHairData angle ring segments =
            List.map (\_ -> angle) segments
                |> Extra.scanl (+) 90
                |> List.map (\a -> mkCrossHair state ring a)

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


viewChampion : Bracket -> Screen.Size -> Svg msg
viewChampion bracket state =
    case
        B.qualifier bracket
    of
        Just team ->
            let
                config =
                    sizing state

                radius =
                    ringRadius state 1 - config.ringSpacing

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
                    B.hasQualified bracket
                        |> hasQualifiedToColor
                        |> Attributes.fill
            in
            Svg.g []
                [ Svg.circle [ x, y, r, clr ] []
                , mkText team.teamID RGB.white c1 c2
                ]

        _ ->
            Svg.g [] []


mkText : String -> String -> ( Float, Float ) -> ( Float, Float ) -> Svg msg
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


viewLeaf : Bet -> Screen.Size -> Maybe Bracket -> UI.Style.ButtonSemantics -> Float -> Float -> Float -> Svg msg
viewLeaf _ state mBracket isSelected ring segmentAngleSize angle =
    case mBracket of
        Just (MatchNode slot winner home away _ hasQ) ->
            let
                awayStartAngle =
                    angle + (segmentAngleSize / 2)

                awayEndAngle =
                    angle + segmentAngleSize

                homeColor =
                    B.hasQualified home
                        |> hasQualifiedToColor

                awayColor =
                    B.hasQualified away
                        |> hasQualifiedToColor

                homeLeaf =
                    Leaf ring angle awayStartAngle homeColor HomeLeaf slot

                awayLeaf =
                    Leaf ring awayStartAngle awayEndAngle awayColor AwayLeaf slot
            in
            Svg.g []
                [ viewMatchLeaf state HomeTeam slot (isWinner winner HomeTeam) home homeLeaf
                , viewMatchLeaf state AwayTeam slot (isWinner winner AwayTeam) away awayLeaf
                ]

        Just (TeamNode slot candidate qualifier hasQ) ->
            let
                endAngle =
                    angle + segmentAngleSize

                color =
                    hasQualifiedToColor hasQ

                leaf =
                    Leaf ring angle endAngle color QualifierLeaf slot
            in
            mkLeaf state isSelected qualifier leaf

        _ ->
            Svg.g [] []


viewMatchLeaf : Screen.Size -> Winner -> Slot -> IsWinner -> Bracket -> Leaf -> Svg msg
viewMatchLeaf state wnnr slot isSelected bracket leaf =
    let
        s =
            case hasQ of
                In ->
                    UI.Style.Selected

                Out ->
                    UI.Style.Potential

                TBD ->
                    UI.Style.Potential

        mTeam =
            B.qualifier bracket

        hasQ =
            B.hasQualified bracket
    in
    mkLeaf state s mTeam leaf


mkLeaf : Screen.Size -> ButtonSemantics -> Maybe Team -> Leaf -> Svg msg
mkLeaf state s team leaf =
    Svg.g []
        [ describeLeaf state s leaf, setText state s team leaf ]


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


hasQualifiedToColor : HasQualified -> String
hasQualifiedToColor hasQ =
    case hasQ of
        TBD ->
            RGB.panel

        In ->
            RGB.right

        Out ->
            RGB.wrong


baseConfig : Sizing
baseConfig =
    { x = 170
    , y = 170
    , borderRadius = 10
    , ringHeight = 33
    , ringSpacing = 2
    }


colors : { potential : String, selected : String, focus : String }
colors =
    { potential = RGB.panel
    , selected = RGB.panel
    , focus = RGB.green
    }


type alias Sizing =
    { x : Float
    , y : Float
    , borderRadius : Float
    , ringHeight : Float
    , ringSpacing : Float
    }


sizing : Screen.Size -> Sizing
sizing screen =
    let
        w =
            min 400 screen.width
                |> max 342

        factor =
            w / 342

        x =
            w / 2

        y =
            w / 2

        borderRadius =
            factor * baseConfig.borderRadius

        ringHeight =
            factor * baseConfig.ringHeight

        ringSpacing =
            factor * baseConfig.ringSpacing
    in
    Sizing x y borderRadius ringHeight ringSpacing


maxWidth : Screen.Size -> Int
maxWidth state =
    let
        config =
            sizing state

        -- r =
        --     ringRadius state 4 + config.ringHeight
        maxW =
            config.x * 2
    in
    round maxW


describeLeaf : Screen.Size -> ButtonSemantics -> Leaf -> Svg.Svg msg
describeLeaf state s { ring, startAngle, endAngle, clr } =
    let
        config =
            sizing state

        x =
            config.x

        y =
            config.y

        innerRadius =
            ringRadius state ring

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
                    colors.selected

                UI.Style.Focus ->
                    colors.focus

                _ ->
                    colors.potential
    in
    Svg.path
        [ Attributes.d <|
            pathD (segmentPath sweep)
        , Attributes.fill clr

        -- , Attributes.fill fillColour
        ]
        []


mkCrossHair : Screen.Size -> Float -> Float -> Svg msg
mkCrossHair state ring angle =
    let
        config =
            sizing state

        innerRadius =
            ringRadius state (max ring 2)

        outerRadius =
            ringRadius state 4 + config.ringHeight

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


setText : Screen.Size -> ButtonSemantics -> Qualifier -> Leaf -> Svg.Svg msg
setText state _ qualifier { ring, startAngle, endAngle } =
    let
        teamId =
            T.mdisplayID qualifier

        config =
            sizing state

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


polarToCartesian : Float -> Float -> Float -> Float -> ( Float, Float )
polarToCartesian centerX centerY radius angleInDegrees =
    let
        angleInRadians =
            (angleInDegrees - 90) * pi / 180.0
    in
    ( centerX + (radius * cos angleInRadians)
    , centerY + (radius * sin angleInRadians)
    )


ringRadius : Screen.Size -> Float -> Float
ringRadius state ring =
    let
        config =
            sizing state
    in
    ring * config.ringHeight + ((ring - 1) * config.ringSpacing)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
