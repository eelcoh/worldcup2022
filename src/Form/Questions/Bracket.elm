module Form.Questions.Bracket exposing (Msg, update, view)

import Bets.Bet exposing (setTeam)
import Bets.Types exposing (Answer, AnswerT(..), AnswerID, Team, Bet, Answer, Bracket(..), Slot, Winner(..), Qualifier)
import Bets.Types.Bracket as B
import Form.Questions.Types exposing (QState)
import Html exposing (..)
import Html.Events exposing (onClick)
import UI.Grid as UI exposing (container, Color(..), Size(..), cell, Align(..))
import UI.Team exposing (viewTeam)


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


view : Bet -> QState -> Html Msg
view bet qQState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qQState.answerId

        header =
            h1 [] [ text "Klik je een weg door het schema" ]

        introtext =
            """Dit is het schema voor de tweede ronde en verder. In het midden staat de finale,
         daarboven en onder de ronden die daaraan voorafgaan. Voor de juiste kwartfinalisten
         krijg je 4 punten. Halve finalisten leveren 7 punten op, finalisten 10 punten en de
         kampioen 13 punten."""

        introduction =
            p [] [ text introtext ]
    in
        case mAnswer of
            Just (( answerId, AnswerBracket bracket _ ) as answer) ->
                div []
                    [ header
                    , section []
                        [ introduction
                        , section [] [ (viewBracket bet answer bracket) ]
                        ]
                    ]

            _ ->
                div [] [ text "WHOOPS" ]


viewBracket : Bet -> Answer -> Bracket -> Html Msg
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
        v =
            viewMatchWinner bet answer

        final =
            B.get bracket "m51"

        m51 =
            v <| B.get bracket "m51"

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

        champBtn =
            mkButtonChamp final

        champion =
            UI.cell2 M
                Irrelevant
                []
                [ container Center [] [ champBtn ]
                ]
    in
        section []
            [ container Justified [] [ m37, m39, m38, m42 ]
            , container Spaced [] [ m45, m46 ]
            , container Spaced [] [ m49 ]
            , container Rightside [] [ m51, champion ]
            , container Spaced [] [ m50 ]
            , container Spaced [] [ m47, m48 ]
            , container Justified [] [ m41, m43, m40, m44 ]
            ]


viewMatchWinner : Bet -> Answer -> Maybe Bracket -> Html Msg
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
                UI.cell2 M
                    Irrelevant
                    []
                    [ container Center [] [ homeButton, awayButton ]
                    ]

        _ ->
            p [] []


mkButton : Answer -> Winner -> Slot -> IsWinner -> Bracket -> Html Msg
mkButton answer wnnr slot isSelected bracket =
    let
        s =
            case isSelected of
                Yes ->
                    Selected

                No ->
                    Potential

                Undecided ->
                    Potential

        answerId =
            Tuple.first answer

        handler =
            onClick (SetWinner answerId slot wnnr)

        attrs =
            []
    in
        UI.button2 XL s [ handler ] [ viewTeam (B.qualifier bracket) ]


mkButtonChamp : Maybe Bracket -> Html Msg
mkButtonChamp mBracket =
    let
        mTeam =
            mBracket
                |> Maybe.andThen B.winner

        s =
            case mTeam of
                Just t ->
                    Selected

                Nothing ->
                    Potential

        attrs =
            []
    in
        UI.button2 XL s [] [ viewTeam mTeam ]
