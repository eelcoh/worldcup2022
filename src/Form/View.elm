module Form.View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Form.Types exposing (..)
import Form.Info
import Form.Submit
import Form.Question
import Form.Questions.Types as QT
import Form.QuestionSet
import Form.QuestionSets.Types as QTS
import UI.Grid as UI exposing (..)
import Html.Attributes exposing (class)
import Element
import Element.Attributes exposing (padding, spacing)
import UI.Style
import UI.Button


-- View


view : Model Msg -> Html Msg
view model =
    let
        getCard =
            List.drop model.idx model.cards
                |> List.head

        makeCard mCard =
            case mCard of
                Just card ->
                    viewCard model model.idx card

                Nothing ->
                    section [] [ text "WHOOPS" ]

        card =
            getCard
                |> makeCard
    in
        viewCardChrome model card model.idx


viewCard : Model Msg -> Int -> Card -> Html Msg
viewCard model i card =
    case card of
        IntroCard intro ->
            map InfoMsg (Form.Info.view intro)

        QuestionCard qModel ->
            map (Answered i) (Form.Question.view model.bet qModel)

        QuestionSetCard qsModel ->
            map (QuestionSetMsg i) (Form.QuestionSet.view qsModel model.bet)

        SubmitCard ->
            let
                isComplete card =
                    case card of
                        IntroCard _ ->
                            True

                        QuestionCard qModel ->
                            QT.isComplete model.bet qModel

                        QuestionSetCard qsModel ->
                            QTS.isComplete model.bet qsModel

                        SubmitCard ->
                            True

                submittable =
                    List.all isComplete model.cards
            in
                map SubmitMsg (Form.Submit.view model.bet submittable model.submitted)


viewPill : Model Msg -> Int -> ( Int, Card ) -> Element.Element UI.Style.Style variation Msg
viewPill model idx ( i, card ) =
    let
        semantics =
            case card of
                IntroCard intro ->
                    UI.Style.Perhaps

                QuestionCard qModel ->
                    mkpillModel (QT.isComplete model.bet qModel) (i == idx)

                QuestionSetCard qsModel ->
                    mkpillModel (QTS.isComplete model.bet qsModel) (i == idx)

                SubmitCard ->
                    UI.Style.Perhaps

        mkpillModel complete current =
            case ( complete, current ) of
                ( True, True ) ->
                    UI.Style.Selected

                ( True, False ) ->
                    UI.Style.Active

                ( False, True ) ->
                    UI.Style.Right

                ( False, False ) ->
                    UI.Style.Potential

        msg =
            NavigateTo i

        contents =
            case card of
                IntroCard intro ->
                    "Start"

                QuestionCard qModel ->
                    QT.display model.bet qModel

                QuestionSetCard model ->
                    QTS.display model

                SubmitCard ->
                    "Insturen"
    in
        UI.Button.pill semantics
            msg
            contents


viewCardChrome : Model Msg -> Html Msg -> Int -> Html Msg
viewCardChrome model card i =
    let
        next =
            Basics.min (i + 1) ((List.length model.cards) - 1)

        prev =
            Basics.max (i - 1) 0

        pills =
            List.map (viewPill model i) (List.indexedMap (,) model.cards)
                |> Element.wrappedRow UI.Style.None [ spacing 7, padding 10 ]
                |> Element.layout UI.Style.stylesheet

        nav =
            [ UI.Button.pill UI.Style.Potential (NavigateTo prev) "terug"
            , UI.Button.pill UI.Style.Potential (NavigateTo next) "volgende"
            ]
                |> Element.wrappedRow UI.Style.None [ spacing 7, padding 10 ]
                |> Element.layout UI.Style.stylesheet
    in
        wrapper []
            [ pills
            , card
            , nav
            ]
