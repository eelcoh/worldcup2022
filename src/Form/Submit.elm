module Form.Submit exposing (view)

import Element exposing (centerX, fill, paddingXY, spacing, width)
import RemoteData exposing (RemoteData(..))
import Types exposing (Card(..), Info(..), InputState(..), Model, Msg(..))
import UI.Button exposing (submit)
import UI.Page exposing (page)
import UI.Screen as Screen
import UI.Style exposing (ButtonSemantics(..))


view : Model Msg -> Bool -> Element.Element Msg
view model submittable =
    let
        ( introText, btn ) =
            case ( submittable, model.savedBet, model.betState ) of
                ( True, _, Dirty ) ->
                    ( introSubmittable, submit Active SubmitMsg "inzenden" )

                ( False, NotAsked, Dirty ) ->
                    ( introNotReady, submit Inactive NoOp "inzenden" )

                ( _, Success _, Clean ) ->
                    ( introSubmitted, submit Right Restart "opnieuw" )

                ( _, Success _, Dirty ) ->
                    ( introSubmittable, submit Right SubmitMsg "inzenden" )

                ( _, Failure _, _ ) ->
                    ( introSubmittedErr, submit Inactive NoOp "inzenden" )

                ( _, Loading, _ ) ->
                    ( introSubmitting, submit Inactive NoOp "inzenden" )

                ( _, _, Clean ) ->
                    ( introNotReady, submit Inactive NoOp "inzenden" )
    in
    page "submit"
        [ introText
        , btn
        ]


introSubmittable : Element.Element Msg
introSubmittable =
    Element.paragraph (UI.Style.introduction []) [ Element.text "Het formulier is compleet. Klik op inzenden om het in te sturen" ]


introSubmitting : Element.Element Msg
introSubmitting =
    Element.paragraph (UI.Style.introduction []) [ Element.text "Het formulier is compleet. Klik op inzenden om het in te sturen. Verzenden...." ]


introNotReady : Element.Element Msg
introNotReady =
    Element.paragraph (UI.Style.introduction []) [ Element.text "Het formulier is nog niet helemaal ingevuld. Je kunt het nog niet insturen. Kijk op de 'tabs' bovenin welke er nog rood zijn." ]


introSubmitted : Element.Element Msg
introSubmitted =
    Element.paragraph (UI.Style.introduction [])
        [ Element.text "Dank voor het meedoen! Neem contact op met Arnaud of Eelco over het overmaken dan wel inleveren van de 5 euro inlegkosten."
        , Element.text "Misschien wil je nog een keer meedoen? Vul dan gewoon het "
        , Element.link (UI.Style.introduction []) { url = "/voetbalpool/formulier", label = Element.text "formulier" }
        , Element.text "opnieuw in."
        ]


introSubmittedErr : Element.Element Msg
introSubmittedErr =
    Element.paragraph (UI.Style.introduction []) [ Element.text "Whoops! Daar ging iets niet goed. " ]
