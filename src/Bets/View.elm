module Bets.View exposing (view, viewBet)

import Bets.Types exposing (..)
import Bets.Types.Bracket as B
import Bets.Types.Score as S
import Bets.Types.StringField as StringField
import Element exposing (Element, alignRight, centerX, centerY, height, padding, paddingXY, px, spacing, spacingXY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Http exposing (Part)
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web exposing (defaultConfig)
import Types exposing (Model, Msg(..))
import UI.Button
import UI.Color as Color
import UI.Font exposing (scaled)
import UI.Match
import UI.Screen as Screen exposing (Device(..))
import UI.Style exposing (ButtonSemantics(..))
import UI.Team
import UI.Text


view : Model Msg -> Element Msg
view model =
    case model.savedBet of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure err ->
            -- let
            --     t =
            --         Debug.log "error! " err
            -- in
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success bet ->
            viewBet bet model.screen


viewBet : Bet -> Screen.Size -> Element.Element Msg
viewBet bet screenSize =
    let
        w =
            Screen.width screenSize

        device =
            Screen.device screenSize
    in
    Element.column
        [ spacing 40, w ]
        [ displayParticipant bet.participant
        , UI.Text.displayHeader "De wedstrijden"
        , matchesIntro
        , displayMatches bet.answers.matches
        , UI.Text.displayHeader "Het Schema"
        , displayBracket device bet
        , UI.Text.displayHeader "De Topscorer"
        , topscorerIntro
        , displayTopscorer bet.answers.topscorer
        ]



-- type AnswerT
--     = AnswerGroupMatch Group Match (Maybe Score) Points
--     | AnswerGroupPosition Group Pos Draw Points
--     | AnswerGroupBestThirds BestThirds Points
--     | AnswerMatchWinner Round Match NextID (Maybe Team) Points
--     | AnswerBracket Bracket Points
--     | AnswerTopscorer Topscorer Points
--     | AnswerParticipant Participant


intro : Element.Element Msg
intro =
    let
        introtext =
            """Dank voor je inzending!
            """
    in
    Element.paragraph
        []
        [ UI.Text.simpleText introtext
        ]


matchesIntro : Element.Element Msg
matchesIntro =
    let
        introtext =
            """Voor iedere wedstrijd die je helemaal goed hebt voorspeld krijg je 3 punten.
            Heb je enkel de toto goed, krijg je 1 punt. En anders niets.
            """
    in
    Element.paragraph [] [ UI.Text.simpleText introtext ]


topscorerIntro : Element.Element Msg
topscorerIntro =
    let
        introtext =
            """De topscorer levert 9 punten op, mits goed voorspeld natuurlijk.
            """
    in
    Element.paragraph [] [ UI.Text.simpleText introtext ]


displayMatches : List ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatches answers =
    Element.wrappedRow
        [ padding 10, spacingXY 20 40, centerX ]
        (List.map displayMatch answers)


displayMatch : ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatch ( _, Answer groupMatch pts ) =
    let
        semantics =
            case pts of
                Just 3 ->
                    UI.Style.Right

                Just 1 ->
                    UI.Style.Active

                Just 0 ->
                    UI.Style.Wrong

                _ ->
                    UI.Style.Perhaps

        handler =
            onClick NoOp

        disp (GroupMatch _ match mScore) =
            UI.Match.display match mScore handler semantics
    in
    disp groupMatch


scoreString : Int -> Int -> String
scoreString h a =
    List.foldr (++) "" [ " ", String.fromInt h, "-", String.fromInt a, " " ]


displayScore : Maybe Score -> Element.Element msg
displayScore mScore =
    let
        txt =
            case mScore of
                Just score ->
                    S.asString score

                Nothing ->
                    " _-_ "
    in
    Element.el [ centerY, UI.Font.score ] (Element.text txt)



-- --
-- module Form.Questions.Bracket exposing (Msg, update, view)
-- import Bets.Bet exposing (setTeam)
-- import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Bracket(..), Qualifier, Slot, Team, Winner(..))
-- import UI.Text
-- import Bets.Types.Bracket as B
-- import Element
-- import Element.Attributes exposing (alignRight, center, px, spacing, paddingXY, spread, width)
-- import Form.Questions.Types exposing (QState)
-- import Html exposing (..)
-- import UI.Button
-- import UI.Style
-- type Msg
--     = SetWinner AnswerID Slot Winner


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


displayBracket : Device -> Bet -> Element.Element Msg
displayBracket device bet =
    let
        br =
            bet.answers.bracket

        introtext =
            """Dit is het schema voor de tweede ronde en verder. In het midden staat de finale en de kampioen,
         daarboven en onder de ronden die daaraan voorafgaan. Voor ieder team dat je juist hebt in de tweede
         ronde krijg je 1 punt. Voor de juiste kwartfinalisten krijg je 4 punten. Halve finalisten leveren 7
         punten op, finalisten 10 punten en de kampioen 13 punten."""

        introduction =
            Element.paragraph [] [ UI.Text.simpleText introtext ]
    in
    Element.column
        [ spacing 20 ]
        [ introduction
        , viewBracket device bet br
        ]


viewBracket : Device -> Bet -> Answer Bracket -> Element.Element Msg
viewBracket device bet (Answer bracket _) =
    let
        v mb =
            viewMatchWinner device bet mb

        final =
            B.get bracket "m51"

        m50 =
            v <| B.get bracket "m50"

        m49 =
            v <| B.get bracket "m49"

        m48 =
            v <| B.get bracket "m48"

        m47 =
            v <| B.get bracket "m47"

        m46 =
            v <| B.get bracket "m46"

        m45 =
            v <| B.get bracket "m45"

        m44 =
            v <| B.get bracket "m44"

        m43 =
            v <| B.get bracket "m43"

        m42 =
            v <| B.get bracket "m42"

        m41 =
            v <| B.get bracket "m41"

        m40 =
            v <| B.get bracket "m40"

        m39 =
            v <| B.get bracket "m39"

        m38 =
            v <| B.get bracket "m38"

        m37 =
            v <| B.get bracket "m37"

        champion =
            mkButtonChamp device final

        ( spaceH, spaceV ) =
            case device of
                Phone ->
                    ( 6, 6 )

                Computer ->
                    ( 12, 24 )
    in
    Element.column
        [ spacing spaceV ]
        [ Element.row [ spacing (2 * spaceH), centerX ] [ m38, m40, m43, m44 ]
        , Element.row [ spacing (16 * spaceH), centerX ] [ m47, m48 ]
        , Element.row [ centerX ] [ m50 ]
        , Element.row [ centerX, spacing (4 * spaceH) ] [ v final, champion ]
        , Element.row [ centerX ] [ m49 ]
        , Element.row [ spacing (16 * spaceH), centerX ] [ m45, m46 ]
        , Element.row [ spacing (2 * spaceH), centerX ] [ m41, m42, m37, m39 ]
        ]


viewMatchWinner : Device -> Bet -> Maybe Bracket -> Element.Element Msg
viewMatchWinner device bet mBracket =
    case mBracket of
        Just (MatchNode slot winner home away rd hasQ) ->
            let
                homeHasQ =
                    didQualify home

                awayHasQ =
                    didQualify away

                homeButton =
                    mkButton device HomeTeam slot homeHasQ home

                awayButton =
                    mkButton device AwayTeam slot awayHasQ away

                dash =
                    Element.text " - "

                -- in
                -- Element.row [ spacing 7 ] [ homeButton, awayButton ]
                -- homeButton =
                --     mkButton HomeTeam slot homeHasQ home
                -- awayButton =
                --     mkButton AwayTeam slot awayHasQ away
                -- dash =
                --     Element.text " - "
                -- in
                -- Element.row [ spacing 7 ] [ homeButton, awayButton ]
                ( rowStyle, w, h ) =
                    case device of
                        Phone ->
                            ( UI.Style.matchRowVerySmall, px 80, px 50 )

                        Computer ->
                            ( UI.Style.matchRow, px 160, px 100 )

                style =
                    rowStyle UI.Style.Perhaps
                        [ UI.Font.button
                        , Element.centerY
                        , width w
                        , height h
                        , Screen.className "matchrow very small"
                        ]
            in
            Element.row style
                [ homeButton, dash, awayButton ]

        _ ->
            Element.none


didQualify : Bracket -> HasQualified
didQualify b =
    case b of
        MatchNode _ _ _ _ _ hasQ ->
            hasQ

        TeamNode _ _ _ hasQ ->
            hasQ


mkButton : Device -> Winner -> Slot -> HasQualified -> Bracket -> Element.Element Msg
mkButton device wnnr slot hasQualified bracket =
    let
        s =
            case hasQualified of
                In ->
                    Right

                Out ->
                    Wrong

                TBD ->
                    Potential

        attrs =
            []

        team =
            B.qualifier bracket

        btn =
            case device of
                Phone ->
                    UI.Button.maybeTeamBadgeVerySmall

                Computer ->
                    UI.Button.maybeTeamBadgeSmall
    in
    btn s team


mkButtonChamp : Device -> Maybe Bracket -> Element.Element msg
mkButtonChamp device mBracket =
    let
        mTeam =
            mBracket
                |> Maybe.andThen B.winner

        qualified =
            mBracket
                |> Maybe.map didQualify
                |> Maybe.map toQualified
                |> Maybe.withDefault Potential

        toQualified h =
            case h of
                In ->
                    Right

                Out ->
                    Wrong

                TBD ->
                    Potential
    in
    case device of
        Phone ->
            UI.Button.maybeTeamBadgeVerySmall qualified mTeam
                |> badgeVerySmall

        Computer ->
            UI.Button.maybeTeamBadgeSmall qualified mTeam
                |> badge


displayTopscorer : Answer Topscorer -> Element.Element Msg
displayTopscorer (Answer ts points) =
    let
        tsName mTs =
            Maybe.map (UI.Button.pill UI.Style.Right Types.NoOp) mTs
                |> Maybe.withDefault (error "no topscorer")

        teamBadge =
            UI.Button.maybeTeamBadgeSmall Potential (Tuple.second ts)
                |> badge
    in
    Element.row
        [ spacing 20 ]
        [ teamBadge
        , tsName (Tuple.first ts)
        ]


badge el =
    Element.el
        [ height (px 100)
        , width (px 80)
        , Background.color Color.panel
        , Font.color Color.primaryText
        , Font.size (scaled 1)
        , Font.center
        , centerY
        , Element.pointer
        , UI.Font.match
        , Border.rounded 10
        , spacing 0
        ]
        el


badgeVerySmall el =
    Element.el
        [ height (px 50)
        , width (px 40)
        , Background.color Color.panel
        , Font.color Color.primaryText
        , Font.size (scaled 1)
        , Font.center
        , centerY
        , Element.pointer
        , UI.Font.match
        , Border.rounded 3
        , spacing 0
        ]
        el


error : String -> Element.Element msg
error text =
    Element.row [] [ Element.text text ]


displayParticipant : Participant -> Element.Element msg
displayParticipant participant =
    let
        h part =
            StringField.value part.name
                |> UI.Text.displayHeader

        residenceText =
            (++) "uit "

        p part =
            StringField.value part.residence
                |> UI.Text.simpleText
    in
    Element.column
        [ spacing 20, centerY ]
        [ h participant
        , p participant
        ]


errorBox : String -> Element.Element msg
errorBox text =
    Element.row [] [ Element.text text ]
