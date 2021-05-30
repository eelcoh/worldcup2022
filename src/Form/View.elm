module Form.View exposing (view)

import Bets.Bet
import Bets.Types exposing (Group(..))
import Element exposing (padding, paddingXY, px, spacing, width)
import Form.Bracket
import Form.GroupMatches
import Form.Info
import Form.Participant
import Form.Submit
import Form.Topscorer
import Types exposing (Card(..), Model, Msg(..))
import UI.Button
import UI.Screen as Screen
import UI.Style



-- View


view : Model Msg -> Element.Element Msg
view model =
    let
        getCard =
            List.drop model.idx model.cards
                |> List.head

        makeCard mCard =
            case mCard of
                Just card_ ->
                    viewCard model model.idx card_

                Nothing ->
                    Element.none

        card =
            getCard
                |> makeCard
    in
    Element.row [ Element.centerX, Screen.className "card" ]
        [ viewCardChrome model card model.idx ]


viewCard : Model Msg -> Int -> Card -> Element.Element Msg
viewCard model _ card =
    case card of
        IntroCard intro ->
            Element.map InfoMsg (Form.Info.view intro)

        GroupMatchesCard groupMatchesState ->
            Element.map (GroupMatchMsg groupMatchesState.group) (Form.GroupMatches.view model.bet groupMatchesState)

        BracketCard bracketState ->
            Element.map BracketMsg (Form.Bracket.view model.bet bracketState)

        BracketKnockoutsCard bracketState ->
            Element.map BracketMsg (Form.Bracket.view model.bet bracketState)

        TopscorerCard ->
            Element.map TopscorerMsg (Form.Topscorer.view model.bet)

        ParticipantCard ->
            Element.map ParticipantMsg (Form.Participant.view model.bet)

        SubmitCard ->
            let
                submittable =
                    Bets.Bet.isComplete model.bet
            in
            Form.Submit.view model submittable


viewPill : Model Msg -> Int -> ( Int, Card ) -> Element.Element Msg
viewPill model idx ( i, card ) =
    let
        current =
            i == idx

        semantics =
            case card of
                IntroCard _ ->
                    mkpillModel True

                GroupMatchesCard state ->
                    mkpillModel (Form.GroupMatches.isComplete state.group model.bet)

                BracketCard _ ->
                    mkpillModel (Form.Bracket.isCompleteQualifiers model.bet)

                BracketKnockoutsCard _ ->
                    mkpillModel (Form.Bracket.isComplete model.bet)

                TopscorerCard ->
                    mkpillModel (Form.Topscorer.isComplete model.bet)

                ParticipantCard ->
                    mkpillModel (Form.Participant.isComplete model.bet)

                SubmitCard ->
                    mkpillModel False

        -- = Active
        -- | Inactive
        -- | Wrong
        -- | Right
        -- | Perhaps
        -- | Irrelevant
        -- | Potential
        -- | Selected
        -- | Focus
        mkpillModel complete =
            case ( complete, current ) of
                ( True, True ) ->
                    UI.Style.Right

                ( True, False ) ->
                    UI.Style.Right

                ( False, True ) ->
                    UI.Style.Selected

                ( False, False ) ->
                    UI.Style.Potential

        msg =
            NavigateTo i

        contents =
            " "
    in
    UI.Button.pill semantics
        msg
        contents


viewCardChrome : Model Msg -> Element.Element Msg -> Int -> Element.Element Msg
viewCardChrome model card i =
    let
        next =
            Basics.min (i + 1) (List.length model.cards - 1)

        prev =
            Basics.max (i - 1) 0

        pills =
            List.map (viewPill model i) (List.indexedMap (\a b -> ( a, b )) model.cards)

        prevPill =
            UI.Button.pill UI.Style.Focus (NavigateTo prev) "vorige"

        nextPill =
            UI.Button.pill UI.Style.Focus (NavigateTo next) "volgende"

        nav =
            Element.row [ Element.spacing 20, Element.centerX ] [ prevPill, nextPill ]

        pillsPlus =
            Element.row [ spacing 8, padding 0, Element.centerX ] pills
    in
    Element.column
        [ padding 0
        , spacing 30
        , Element.centerX
        , Element.width
            (Element.fill
                |> Element.maximum (Screen.maxWidth model.screen)
            )
        ]
        [ pillsPlus
        , nav
        , card
        ]
