module Form.Bracket exposing (isComplete, update, view)

import Bets.Bet
import Bets.Types exposing (Answer(..), Bet, Bracket(..), Candidate(..), CurrentSlot(..), Group(..), HasQualified(..), Winner(..))
import Bets.Types.Answer.Bracket
import Bets.Types.Bracket as B
import Bets.Types.Candidate as Candidate
import Element
import Form.Bracket.Types exposing (..)
import Form.Bracket.View exposing (viewCandidatesPanel, viewRings)
import UI.Button exposing (pill)
import UI.Style exposing (ButtonSemantics(..))
import UI.Text


isComplete : Bet -> Bool
isComplete bet =
    Bets.Types.Answer.Bracket.isComplete bet.answers.bracket


update : Msg -> Bet -> State -> ( Bet, State, Cmd Msg )
update msg bet state =
    case msg of
        SetWinner slot homeOrAway ->
            let
                newBet =
                    Bets.Bet.setWinner bet slot homeOrAway
            in
            ( newBet, state, Cmd.none )

        SetQualifier slot candidates ->
            let
                bracketState =
                    ShowSecondRoundSelection slot candidates
            in
            ( bet, bracketState, Cmd.none )

        SetSlot slot team ->
            let
                newBet =
                    Bets.Bet.setQualifier bet slot (Just team)

                nextSlot =
                    Bets.Bet.getBracket newBet
                        |> B.getFreeSlots
                        |> List.sortBy toSortable
                        |> List.head

                toSortable ( _, c ) =
                    Candidate.toSortable c

                newState =
                    case nextSlot of
                        Just ( s, c ) ->
                            ShowSecondRoundSelection s c

                        Nothing ->
                            state
            in
            ( newBet, newState, Cmd.none )

        CloseQualifierView ->
            ( bet, ShowMatches, Cmd.none )

        OpenQualifierView ->
            ( bet, Maybe.withDefault initialQualifierView (createQualifierView bet), Cmd.none )


createQualifierView : Bet -> Maybe State
createQualifierView bet =
    let
        toSortable ( _, c ) =
            Candidate.toSortable c
    in
    Bets.Bet.getBracket bet
        |> B.getFreeSlots
        |> List.sortBy toSortable
        |> List.head
        |> Maybe.map (\( s, c ) -> ShowSecondRoundSelection s c)


initialQualifierView : State
initialQualifierView =
    ShowSecondRoundSelection "WA" (FirstPlace A)


view : Bet -> State -> Element.Element Msg
view bet state =
    let
        bracket =
            Bets.Bet.getBracket bet

        header =
            UI.Text.displayHeader "Klik je een weg door het schema"

        introtext =
            """Dit is het schema voor de tweede ronde en verder. In het midden staat de finale,
         daarboven en onder de ronden die daaraan voorafgaan. Voor de juiste kwartfinalisten
         krijg je 4 punten. Halve finalisten leveren 7 punten op, finalisten 10 punten en de
         kampioen 13 punten."""

        introduction =
            Element.paragraph [] [ UI.Text.simpleText introtext ]

        candidatePanel =
            case state of
                ShowSecondRoundSelection slot candidate ->
                    viewCandidatesPanel bet bracket slot candidate

                _ ->
                    Element.none

        viewToggle =
            case state of
                ShowSecondRoundSelection _ _ ->
                    pill Active CloseQualifierView "sluit"

                ShowMatches ->
                    pill Active OpenQualifierView "open"
    in
    Element.column
        [ Element.spacingXY 0 10 ]
        [ header
        , introduction
        , viewRings bet bracket state
        , candidatePanel
        , viewToggle
        ]
