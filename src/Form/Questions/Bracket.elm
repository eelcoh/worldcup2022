module Form.Questions.Bracket exposing (Msg, update, view)

import Bets.Bet exposing (setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Bracket(..), Qualifier, Slot, Team, Winner(..))
import Bets.Types.Bracket as B
import Element exposing (alignRight, centerX, paddingXY, px, spaceEvenly, spacing, width)
import Form.Questions.Types exposing (QState)
import Html exposing (..)
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


viewMatchWinner :
    a
    -> ( AnswerID, a2 )
    -> Maybe Bracket
    -> Element.Element Msg
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


mkButton :
    ( AnswerID, a2 )
    -> Winner
    -> Slot
    -> IsWinner
    -> Bracket
    -> Element.Element Msg
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
