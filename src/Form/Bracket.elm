module Form.Bracket exposing (isComplete, isCompleteQualifiers, update, view)

import Bets.Bet
import Bets.Types exposing (Answer(..), Bet, Bracket(..), Candidate(..), CurrentSlot(..), Group(..), HasQualified(..), Winner(..))
import Bets.Types.Answer.Bracket
import Bets.Types.Bracket as B
import Bets.Types.Candidate as Candidate
import Element exposing (fill, spacing, width)
import Form.Bracket.Types exposing (BracketState(..), Msg(..), State, createState, initialQualifierView)
import Form.Bracket.View exposing (viewCandidatesPanel, viewRings)
import UI.Button exposing (pill)
import UI.Style exposing (ButtonSemantics(..))
import UI.Text


isComplete : Bet -> Bool
isComplete bet =
    Bets.Types.Answer.Bracket.isComplete bet.answers.bracket


isCompleteQualifiers : Bet -> Bool
isCompleteQualifiers bet =
    Bets.Types.Answer.Bracket.isCompleteQualifiers bet.answers.bracket


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
            ( bet, { state | bracketState = bracketState }, Cmd.none )

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
                            { state | bracketState = ShowSecondRoundSelection s c }

                        Nothing ->
                            state
            in
            ( newBet, newState, Cmd.none )

        CloseQualifierView ->
            ( bet, { state | bracketState = ShowMatches }, Cmd.none )

        OpenQualifierView ->
            let
                newState =
                    createQualifierView bet
                        |> Maybe.map (createState state.screen)
                        |> Maybe.withDefault (initialQualifierView state)
            in
            ( bet, newState, Cmd.none )


createQualifierView : Bet -> Maybe BracketState
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


view : Bet -> State -> Element.Element Msg
view bet state =
    let
        bracket =
            Bets.Bet.getBracket bet

        header =
            UI.Text.displayHeader "Klik je een weg door het schema"

        introtext =
            case state.bracketState of
                ShowMatches ->
                    [ Element.text "Dit is het schema voor de het knockout schema. In het midden staat de finale, daarboven en onder de ronden die daaraan voorafgaan. "
                    ]

                ShowSecondRoundSelection _ _ ->
                    [ Element.text "Selecteer de landen voor de tweede ronde."
                    ]

        introduction =
            Element.paragraph (UI.Style.introduction []) introtext

        extroduction =
            Element.column (UI.Style.introduction [ spacing 16 ])
                [ UI.Text.bulletText "1 punt voor ieder juist land in de tweede rond. "
                , UI.Text.bulletText "4 punten per kwartfinalist. "
                , UI.Text.bulletText "7 punten per halve finalist. "
                , UI.Text.bulletText "10 punten per finalist. "
                , UI.Text.bulletText "13 punten voor de kampioen. "
                ]

        candidatePanel =
            case state.bracketState of
                ShowSecondRoundSelection slot candidate ->
                    viewCandidatesPanel bet bracket slot candidate

                _ ->
                    Element.none

        -- viewToggle =
        --     case state.bracketState of
        --         ShowSecondRoundSelection _ _ ->
        --             pill Active CloseQualifierView "naar het schema"
        --         ShowMatches ->
        --             pill Active OpenQualifierView "terug naar de tweede ronde"
    in
    Element.column
        [ Element.spacingXY 0 20, width fill, Element.centerX ]
        [ header
        , introduction
        , viewRings bet bracket state
        , candidatePanel
        , extroduction

        -- , viewToggle
        ]
