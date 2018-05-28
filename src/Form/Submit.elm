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
import UI.Button
import Element
import UI.Style
import UI.Text
import Navigation


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

                        cmd =
                            case nwBet.uuid of
                                Nothing ->
                                    Cmd.none

                                Just uuid ->
                                    Navigation.load ("http://www.webstekjes.com/voetbalpool/#inzendingen/" ++ uuid)
                    in
                        ( freshBet, cmd, Just Done )

                Err res ->
                    ( bet, Cmd.none, Just Error )

        NoOp ->
            ( bet, Cmd.none, Nothing )

        Again ->
            ( Bets.Init.bet, Cmd.none, Just Reset )


view : Bet -> Bool -> SubmitState -> Element.Element UI.Style.Style variation SubmitMsgType
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

        ( introText, semantics, msg, buttonText ) =
            case ( submittable, submitted ) of
                ( True, Dirty ) ->
                    ( introSubmittable, UI.Style.Active, PlaceBet, "inzenden" )

                ( False, Dirty ) ->
                    ( introNotReady, UI.Style.Inactive, NoOp, "inzenden" )

                ( _, Clean ) ->
                    ( introNotReady, UI.Style.Inactive, NoOp, "inzenden" )

                ( _, Sent ) ->
                    ( introSubmitting, UI.Style.Inactive, NoOp, "inzenden" )

                ( _, Done ) ->
                    ( introSubmitted, UI.Style.Right, Again, "opnieuw" )

                ( _, Error ) ->
                    ( introSubmittedErr, UI.Style.Inactive, NoOp, "inzenden" )

                ( _, Reset ) ->
                    ( introSubmitted, UI.Style.Inactive, Again, "opnieuw" )
    in
        Element.column UI.Style.None
            []
            [ introText
            , UI.Button.submit semantics msg buttonText
            ]


introSubmittable : Element.Element UI.Style.Style variation msg
introSubmittable =
    Element.paragraph UI.Style.None [] [ UI.Text.simpleText "Het formulier is compleet. Klik op inzenden om het in te sturen" ]


introSubmitting : Element.Element UI.Style.Style variation msg
introSubmitting =
    Element.paragraph UI.Style.None [] [ UI.Text.simpleText "Het formulier is compleet. Klik op inzenden om het in te sturen. Verzenden...." ]


introNotReady : Element.Element UI.Style.Style variation msg
introNotReady =
    Element.paragraph UI.Style.None [] [ UI.Text.simpleText "Het formulier is nog niet helemaal ingevuld. Je kunt het nog niet insturen. Kijk op de 'tabs' bovenin welke er nog rood zijn." ]


introSubmitted : Element.Element UI.Style.Style variation msg
introSubmitted =
    Element.paragraph UI.Style.None
        []
        [ UI.Text.simpleText "Dank voor het meedoen! Neem contact op met Arnaud of Eelco over het overmaken dan wel inleveren van de 5 euro inlegkosten."
        , UI.Text.simpleText "Misschien wil je nog een keer meedoen? Vul dan gewoon het "
        , Element.link "/voetbalpool/formulier" <| Element.el UI.Style.Link [] (Element.text "formulier")
        , UI.Text.simpleText "opnieuw in."
        ]


introSubmittedErr : Element.Element UI.Style.Style variation msg
introSubmittedErr =
    Element.paragraph UI.Style.None [] [ UI.Text.simpleText "Whoops! Daar ging iets niet goed. " ]
