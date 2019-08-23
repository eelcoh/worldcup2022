module Form.Card exposing (findByCardId, findIDByAnswerId, update)

import Bets.Types exposing (AnswerID, Bet)
import Form.QuestionSets.Types as QS
import Form.Types exposing (..)
import List.Extra exposing (find)


findByCardId : List Card -> Int -> Maybe Card
findByCardId cards idx =
    List.drop idx cards
        |> List.head


findIDByAnswerId : Model Msg -> Maybe AnswerID -> Maybe Int
findIDByAnswerId model mAnswerId =
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


update : List Card -> Int -> Card -> List Card
update cards idx newCard =
    let
        firstSet =
            List.take idx cards

        tail =
            List.drop (idx + 1) cards
    in
    firstSet ++ (newCard :: tail)
