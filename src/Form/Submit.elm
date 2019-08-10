module Form.Submit exposing (view)

import API.Bets as API
import Bets.Init
import Bets.Types exposing (Bet)
import Browser.Navigation as Navigation
import Element
import Form.QuestionSets.Types as QS
import Form.Questions.Types as Q
import Form.Types exposing (Card(..), Info(..), InputState(..), Model, Msg(..))
import RemoteData exposing (RemoteData(..), WebData)
import UI.Button
import UI.Style
import UI.Text



-- update : Msg -> Bet -> ( Bet, Cmd Msg, Maybe SubmitState )
-- update msg bet =
--     case msg of
--         PlaceBet ->
--             let
--                 fx =
--                     API.placeBet bet BetPlaced
--             in
--             ( bet, fx, Just Sent )
--         BetPlaced rBet ->
--             case Debug.log "rBet" rBet of
--                 Ok nwBet ->
--                     let
--                         freshBet =
--                             Bets.Init.bet
--                         cmd =
--                             case nwBet.uuid of
--                                 Nothing ->
--                                     Cmd.none
--                                 Just uuid ->
--                                     Navigation.load ("http://www.webstekjes.com/voetbalpool/#inzendingen/" ++ uuid)
--                     in
--                     ( freshBet, cmd, Just Done )
--                 Err res ->
--                     ( bet, Cmd.none, Just Error )
--         Again ->
--             ( Bets.Init.bet, Cmd.none, Just Reset )


view : Model Msg -> Bool -> Element.Element UI.Style.Style variation Msg
view model submittable =
    let
        isComplete card =
            case card of
                IntroCard _ ->
                    True

                QuestionCard qModel ->
                    Q.isComplete model.bet qModel

                QuestionSetCard qModel ->
                    QS.isComplete model.bet qModel

                SubmitCard ->
                    True

        ( introText, ( semantics, msg, buttonText ) ) =
            case ( submittable, model.savedBet, model.betState ) of
                ( True, _, Dirty ) ->
                    ( introSubmittable, ( UI.Style.Active, SubmitMsg, "inzenden" ) )

                ( False, NotAsked, Dirty ) ->
                    ( introNotReady, ( UI.Style.Inactive, NoOp, "inzenden" ) )

                ( _, Success _, Clean ) ->
                    ( introSubmitted, ( UI.Style.Right, Restart, "opnieuw" ) )

                ( _, Success _, Dirty ) ->
                    ( introSubmittable, ( UI.Style.Right, SubmitMsg, "inzenden" ) )

                ( _, Failure _, _ ) ->
                    ( introSubmittedErr, ( UI.Style.Inactive, NoOp, "inzenden" ) )

                ( _, Loading, _ ) ->
                    ( introSubmitting, ( UI.Style.Inactive, NoOp, "inzenden" ) )

                ( _, _, Clean ) ->
                    ( introNotReady, ( UI.Style.Inactive, NoOp, "inzenden" ) )

        -- ( _, Reset ) ->
        --     ( introSubmitted, UI.Style.Inactive, Restart, "opnieuw" )
    in
    Element.column UI.Style.None
        []
        [ introText
        , UI.Button.submit semantics msg buttonText
        ]



-- view : Bet -> Bool -> SubmitState -> Element.Element UI.Style.Style variation SubmitMsgType
-- view model submittable =
--     let
--         isComplete card =
--             case card of
--                 IntroCard _ ->
--                     True
--                 QuestionCard qModel ->
--                     Q.isComplete bet qModel
--                 QuestionSetCard qModel ->
--                     QS.isComplete bet qModel
--                 SubmitCard ->
--                     True
--         ( introText, semantics, msg, buttonText ) =
--             case ( submittable, savedBet ) of
--                 ( True, Dirty ) ->
--                     ( introSubmittable, UI.Style.Active, PlaceBet, "inzenden" )
--                 ( False, Dirty ) ->
--                     ( introNotReady, UI.Style.Inactive, NoOp, "inzenden" )
--                 ( _, Clean ) ->
--                     ( introNotReady, UI.Style.Inactive, NoOp, "inzenden" )
--                 ( _, Sent ) ->
--                     ( introSubmitting, UI.Style.Inactive, NoOp, "inzenden" )
--                 ( _, Done ) ->
--                     ( introSubmitted, UI.Style.Right, Again, "opnieuw" )
--                 ( _, Error ) ->
--                     ( introSubmittedErr, UI.Style.Inactive, NoOp, "inzenden" )
--                 ( _, Reset ) ->
--                     ( introSubmitted, UI.Style.Inactive, Again, "opnieuw" )
--     in
--     Element.column UI.Style.None
--         []
--         [ introText
--         , UI.Button.submit semantics msg buttonText
--         ]


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
