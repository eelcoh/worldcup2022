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


viewPill : Model Msg -> Int -> ( Int, Card ) -> Html Msg
viewPill model idx ( i, card ) =
    let
        pillModel =
            case card of
                IntroCard intro ->
                    Perhaps

                QuestionCard qModel ->
                    mkpillModel (QT.isComplete model.bet qModel) (i == idx)

                QuestionSetCard qsModel ->
                    mkpillModel (QTS.isComplete model.bet qsModel) (i == idx)

                SubmitCard ->
                    Perhaps

        mkpillModel complete current =
            case ( complete, current ) of
                ( True, True ) ->
                    Selected

                ( True, False ) ->
                    Active

                ( False, True ) ->
                    Right

                ( False, False ) ->
                    Potential

        handler =
            onClick (NavigateTo i)

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
        UI.pill pillModel [ handler ] [ text contents ]


viewCardChrome : Model Msg -> Html Msg -> Int -> Html Msg
viewCardChrome model card i =
    let
        next =
            Basics.min (i + 1) ((List.length model.cards) - 1)

        prev =
            Basics.max (i - 1) 0

        backButton =
            UI.button XXXS Potential [ onClick (NavigateTo prev) ] [ text "terug" ]

        nextButton =
            UI.button XXXS Potential [ onClick (NavigateTo next) ] [ text "volgende" ]

        pills =
            List.map (viewPill model i) (List.indexedMap (,) model.cards)
    in
        wrapper []
            [ container Leftside [] pills
            , card
            , div [ class "nav-buttons container left " ] [ backButton, nextButton ]
            ]
