module Form.Submit exposing (view)


import Element
import Form.QuestionSets.Types as QS
import Form.Questions.Types as Q
import Form.Types exposing (Card(..), Info(..), InputState(..), Model, Msg(..))
import RemoteData exposing (RemoteData(..), WebData)
import UI.Button
import UI.Style
import UI.Text



view : Model Msg -> Bool -> Element.Element Msg
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
    Element.column (UI.Style.none [])
        [ introText
        , UI.Button.submit semantics msg buttonText
        ]




introSubmittable : Element.Element Msg
introSubmittable =
    Element.paragraph (UI.Style.none []) [ UI.Text.simpleText "Het formulier is compleet. Klik op inzenden om het in te sturen" ]


introSubmitting : Element.Element Msg
introSubmitting =
    Element.paragraph (UI.Style.none []) [ UI.Text.simpleText "Het formulier is compleet. Klik op inzenden om het in te sturen. Verzenden...." ]


introNotReady : Element.Element Msg
introNotReady =
    Element.paragraph (UI.Style.none []) [ UI.Text.simpleText "Het formulier is nog niet helemaal ingevuld. Je kunt het nog niet insturen. Kijk op de 'tabs' bovenin welke er nog rood zijn." ]


introSubmitted : Element.Element Msg
introSubmitted =
    Element.paragraph (UI.Style.none [])
        [ UI.Text.simpleText "Dank voor het meedoen! Neem contact op met Arnaud of Eelco over het overmaken dan wel inleveren van de 5 euro inlegkosten."
        , UI.Text.simpleText "Misschien wil je nog een keer meedoen? Vul dan gewoon het "
        , Element.link [] { url = "/voetbalpool/formulier", label = Element.text "formulier" }
        , UI.Text.simpleText "opnieuw in."
        ]


introSubmittedErr : Element.Element Msg
introSubmittedErr =
    Element.paragraph (UI.Style.none []) [ UI.Text.simpleText "Whoops! Daar ging iets niet goed. " ]
