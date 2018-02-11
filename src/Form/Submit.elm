module Form.Submit
    exposing
        ( Msg
        , view
        , update
        )

import API.Bets as API
import Bets.Init
import Bets.Types exposing (Bet)
import Form.QuestionSets.Types as QS
import Form.Questions.Types as Q
import Form.Types exposing (Model, Card(..), Info(..), SubmitMsgType(..), SubmitState(..))
import Html exposing (Html, div, h1, p, section, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import UI.Grid as UI exposing (Align(..), Color(..), Size(..), container)


type alias Msg =
    SubmitMsgType


update : Msg -> Bet -> ( Bet, Cmd Msg, Maybe SubmitState )
update msg bet =
    case msg of
        PlaceBet ->
            let
                fx =
                    API.placeBet bet BetPlaced
            in
                ( bet, fx, Just Sent )

        BetPlaced rBet ->
            case (Debug.log "rBet" rBet) of
                Ok nwBet ->
                    let
                        freshBet =
                            Bets.Init.bet
                    in
                        ( freshBet, Cmd.none, Just Done )

                Err res ->
                    ( bet, Cmd.none, Just Error )

        NoOp ->
            ( bet, Cmd.none, Nothing )

        Again ->
            ( Bets.Init.bet, Cmd.none, Just Reset )


view : Bet -> Bool -> SubmitState -> Html Msg
view bet submittable submitted =
    let
        isComplete card =
            case card of
                IntroCard _ ->
                    True

                QuestionCard qModel ->
                    Q.isComplete bet qModel

                QuestionSetCard qModel ->
                    QS.isComplete bet qModel

                SubmitCard ->
                    True

        ( introText, button ) =
            case ( submittable, submitted ) of
                ( True, Dirty ) ->
                    ( introSubmittable, mkButton Active (onClick PlaceBet) "inzenden" )

                ( False, Dirty ) ->
                    ( introNotReady, mkButton Inactive (onClick NoOp) "inzenden" )

                ( _, Clean ) ->
                    ( introNotReady, mkButton Inactive (onClick NoOp) "inzenden" )

                ( _, Sent ) ->
                    ( introSubmitting, mkButton Inactive (onClick NoOp) "inzenden" )

                ( _, Done ) ->
                    ( introSubmitted, mkButton Right (onClick Again) "opnieuw" )

                ( _, Error ) ->
                    ( introSubmittedErr, mkButton Inactive (onClick NoOp) "inzenden" )

                ( _, Reset ) ->
                    ( introSubmitted, mkButton Inactive (onClick Again) "opnieuw" )

        mkButton buttonState handler txt =
            UI.button M
                buttonState
                [ handler ]
                [ div [ class "inzenden" ] [ text txt ] ]
    in
        section []
            [ container Leftside [] [ introText ]
            , container Leftside [] [ button ]
            ]


header : Html Msg
header =
    h1 [] [ text "Afronden" ]


introSubmittable : Html Msg
introSubmittable =
    p [] [ text "Het formulier is compleet. Klik op inzenden om het in te sturen" ]


introSubmitting : Html Msg
introSubmitting =
    p [] [ text "Het formulier is compleet. Klik op inzenden om het in te sturen. Verzenden...." ]


introNotReady : Html Msg
introNotReady =
    p [] [ text "Het formulier is nog niet helemaal ingevuld. Je kunt het nog niet insturen. Kijk op de 'tabs' bovenin welke er nog niet groengekleurd zijn." ]


introSubmitted : Html Msg
introSubmitted =
    p []
        [ text "Dank voor het meedoen! Neem contact op met Arnaud of Eelco over het overmaken dan wel inleveren van de 5 euro inlegkosten."
        , text "Misschien wil je nog een keer meedoen? Vul dan gewoon het "
        , Html.a [ class "button-like right clickable", href "/voetbalpool/formulier" ] [ text "formulier" ]
        , text " opnieuw in."
        ]


introSubmittedErr : Html Msg
introSubmittedErr =
    p [] [ text "Whoops! Daar ging iets niet goed. " ]
