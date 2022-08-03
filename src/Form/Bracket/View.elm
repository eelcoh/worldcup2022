module Form.Bracket.View exposing (viewCandidatesPanel, viewRings)

import Bets.Bet
import Bets.Types exposing (Answer(..), Bet, Bracket(..), Candidate(..), CurrentSlot(..), Group, HasQualified(..), Qualifier, Selection, Slot, Team, Winner(..))
import Bets.Types.Bracket as B
import Bets.Types.Group as G
import Bets.Types.Team as T
import Element exposing (height, px, width)
import Form.Bracket.Types exposing (Angle, BracketState(..), IsWinner(..), Msg(..), State)
import List.Extra as Extra
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Events as Events
import Svg.PathD exposing (Segment(..), pathD)
import UI.Button
import UI.Color.RGB as RGB
import UI.Screen as Screen
import UI.Style exposing (ButtonSemantics(..))



-- isComplete : Bet -> Bool
-- isComplete bet =
--     Bets.Types.Answer.Bracket.isComplete bet.answers.bracket


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
            case state.bracketState of
                ShowSecondRoundSelection slot _ ->
                    Just slot

                _ ->
                    Nothing

        dims =
            maxWidth state

        sz =
            String.fromInt dims

        dimsPx =
            px dims

        rings =
            case state.bracketState of
                ShowMatches ->
                    viewMatchRings bet bracket state

                ShowSecondRoundSelection _ candidate ->
                    List.concat
                        [ viewPositionRing bet bracket state slotSelected
                        , viewCandidatesCircle state candidate
                        ]
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


viewPositionRing : Bet -> Bracket -> State -> Maybe Slot -> List (Svg Msg)
viewPositionRing bet bracket state mSlot =
    let
        -- v slot =
        --     viewLeaf bet state (B.get bracket slot) (isSelected slot)
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
                f ( ( slot, _ ), ( a, s ) ) =
                    viewLeaf bet state (B.get bracket slot) (isSelected slot) ring s a

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
                |> List.map (mkCrossHair state 4)
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


viewMatchRings : Bet -> Bracket -> State -> List (Svg Msg)
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

        -- final4
        m64 =
            v <| B.get bracket "m64"

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


viewChampion : Bracket -> State -> Svg Msg
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
                    Attributes.fill colors.focus
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
                    SetSlot slot candidates group team
            in
            ( group, mkTeamButton msg team currentSlot )
    in
    Element.column [ Element.spacing 16, Element.centerX ] cds


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


viewCandidatesCircle : State -> Candidate -> List (Svg Msg)
viewCandidatesCircle state candidates =
    let
        config =
            sizing state

        x =
            config.x

        y =
            config.y

        radius =
            ringRadius state 4 - config.ringSpacing

        c1 =
            ( x - radius, y - 16 )

        c2 =
            ( x + radius, y - 16 )

        g1 =
            ( x - radius, y + 16 )

        g2 =
            ( x + radius, y + 16 )

        fillColor =
            -- colors.potential
            RGB.green

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
        -- --         |> List.map (uncurry button)
        -- closeView =
        --     mkButton 200 300 40 25 "sluit" CloseQualifierView UI.Style.Active
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
        , Attributes.r (String.fromFloat (ringRadius state 4 - config.ringSpacing))
        , Attributes.fill fillColor
        ]
        []
    , Svg.g
        []
        [ mkText positionString RGB.white c1 c2
        , mkText groupString RGB.white g1 g2
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


viewLeaf : Bet -> State -> Maybe Bracket -> UI.Style.ButtonSemantics -> Float -> Float -> Float -> Svg Msg
viewLeaf _ state mBracket isSelected ring segmentAngleSize angle =
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
                [ viewMatchLeaf state HomeTeam slot (isWinner winner HomeTeam) home homeLeaf
                , viewMatchLeaf state AwayTeam slot (isWinner winner AwayTeam) away awayLeaf
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
            mkLeaf state isSelected qualifier leaf msg

        -- List.concat
        --     [ viewCandidateLeaf answer slot isSelected candidate qualifier hasQualified leaf msg
        --     ]
        --     |> Svg.g [ Events.onClick msg ]
        _ ->
            Svg.g [] []


viewMatchLeaf : State -> Winner -> Slot -> IsWinner -> Bracket -> Leaf -> Svg Msg
viewMatchLeaf state wnnr slot isSelected bracket leaf =
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
    mkLeaf state s mTeam leaf msg


mkLeaf : State -> ButtonSemantics -> Maybe Team -> Leaf -> Msg -> Svg Msg
mkLeaf state s team leaf msg =
    Svg.g [ Events.onClick msg, Attributes.cursor "pointer" ]
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


sizing : State -> Sizing
sizing { screen } =
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


maxWidth : State -> Int
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


describeLeaf : State -> ButtonSemantics -> Leaf -> Svg.Svg Msg
describeLeaf state s { ring, startAngle, endAngle } =
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
        , Attributes.fill fillColour
        ]
        []


mkCrossHair : State -> Float -> Float -> Svg Msg
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


setText : State -> ButtonSemantics -> Qualifier -> Leaf -> Svg.Svg Msg
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


ringRadius : State -> Float -> Float
ringRadius state ring =
    let
        config =
            sizing state
    in
    ring * config.ringHeight + ((ring - 1) * config.ringSpacing)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
