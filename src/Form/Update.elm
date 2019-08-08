module Form.Update exposing (update)

import API.Bets
import Bets.Init
import Bets.Types exposing (AnswerID, Bet)
import Browser
import Browser.Navigation as Navigation
import Form.Info
import Form.Init as Init
import Form.Question
import Form.QuestionSet
import Form.QuestionSets.Types as QS
import Form.Submit
import Form.Types exposing (..)
import List.Extra exposing (find)
import RemoteData exposing (RemoteData(..), WebData)
import Url


findCardByCardId : List Card -> Int -> Maybe Card
findCardByCardId cards idx =
    List.drop idx cards
        |> List.head


findCardIDByAnswerId : Model Msg -> Maybe AnswerID -> Maybe Int
findCardIDByAnswerId model mAnswerId =
    let
        getAnswerIds qsModel =
            QS.findAnswers qsModel model.bet
                |> List.map Tuple.first

        isAnswer answerId ( i, card ) =
            case card of
                IntroCard _ ->
                    False

                QuestionCard qModel ->
                    qModel.answerId == answerId

                QuestionSetCard qsModel ->
                    List.member answerId (getAnswerIds qsModel)

                SubmitCard ->
                    False

        mIndexedCard =
            case mAnswerId of
                Nothing ->
                    Nothing

                Just answerId ->
                    List.indexedMap (\a b -> ( a, b )) model.cards
                        |> find (isAnswer answerId)
    in
    Maybe.map (\( i, _ ) -> i) mIndexedCard


update : Msg -> Model Msg -> ( Model Msg, Cmd Msg )
update msg state =
    case msg of
        NavigateTo page ->
            ( { state | idx = page }, Cmd.none )

        Answered i act ->
            let
                mCard =
                    findCardByCardId state.cards i
            in
            case mCard of
                Just (QuestionCard qModel) ->
                    let
                        ( newBet, newQModel, fx ) =
                            Form.Question.update act state.bet qModel

                        next =
                            .next qModel

                        idx =
                            findCardIDByAnswerId state next
                                |> Maybe.withDefault state.idx
                    in
                    ( { state | bet = newBet, idx = idx, betState = Dirty }, Cmd.map (Answered i) fx )

                _ ->
                    ( state, Cmd.none )

        InfoMsg act ->
            let
                ( newBet, fx ) =
                    Form.Info.update act state.bet
            in
            ( { state | bet = newBet }, Cmd.map InfoMsg fx )

        -- SubmitMsg act ->
        --     let
        --         ( newBet, fx, mSubmitModel ) =
        --             Form.Submit.update act state.bet
        --         submitModel =
        --             case mSubmitModel of
        --                 Just s ->
        --                     s
        --                 Nothing ->
        --                     state.submitted
        --         nwModel =
        --             case submitModel of
        --                 Reset ->
        --                     Init.initModel
        --                 _ ->
        --                     { state | bet = newBet, submitted = submitModel }
        --     in
        --     ( nwModel, Cmd.map SubmitMsg fx )
        SubmitMsg ->
            let
                cmd =
                    API.Bets.placeBet state.bet
            in
            ( state, cmd )

        SubmittedBet savedBet ->
            let
                ( newBet, nwInputState ) =
                    case savedBet of
                        Success b ->
                            ( b, Clean )

                        _ ->
                            ( state.bet, Dirty )
            in
            ( { state | savedBet = savedBet, bet = newBet, betState = nwInputState }, Cmd.none )

        QuestionSetMsg cardId act ->
            let
                ( newBet, cards, fx ) =
                    case findCardByCardId state.cards cardId of
                        Just (QuestionSetCard model) ->
                            let
                                ( newNewBet, newModel, fx_ ) =
                                    Form.QuestionSet.update act model state.bet

                                cards_ =
                                    List.map (updateQuestionSetCard newModel) state.cards
                            in
                            ( newNewBet, cards_, fx_ )

                        _ ->
                            ( state.bet, state.cards, Cmd.none )
            in
            ( { state | bet = newBet, cards = cards, betState = Dirty }, Cmd.map (QuestionSetMsg cardId) fx )

        NoOp ->
            ( state, Cmd.none )

        Restart ->
            let
                nwState =
                    { state
                        | bet = Bets.Init.bet
                        , savedBet = NotAsked
                        , betState = Clean
                    }
            in
            ( nwState, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( state, Cmd.none )

                        Just _ ->
                            ( state
                            , Navigation.pushUrl state.navKey (Url.toString url)
                            )

                Browser.External href ->
                    ( state
                    , Navigation.load href
                    )

        UrlChange url ->
            ( state, Cmd.none )


updateQuestionSetCard : QS.Model -> Card -> Card
updateQuestionSetCard qSmodel card =
    case card of
        QuestionSetCard qSet ->
            case ( qSet.questionType, qSmodel.questionType ) of
                ( QS.MatchScore g1, QS.MatchScore g2 ) ->
                    if g1 == g2 then
                        QuestionSetCard qSmodel

                    else
                        card

                ( QS.GroupPosition g1, QS.GroupPosition g2 ) ->
                    if g1 == g2 then
                        QuestionSetCard qSmodel

                    else
                        card

                _ ->
                    card

        _ ->
            card
