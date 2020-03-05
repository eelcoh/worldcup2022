module Form.Card exposing (findByCardId, getBracketCard, getGroupMatchesCard, init, update, updateBracketCard, updateGroupMatchesCard, updateScreenCard, updateScreenCards)

import Bets.Types exposing (Group(..), Round(..))
import Form.Bracket.Types as Bracket
import Form.GroupMatches.Types as GroupMatches
import Form.Screen as Screen
import Form.Types exposing (Card(..), Info(..))


findByCardId : List Card -> Int -> Maybe Card
findByCardId cards idx =
    List.drop idx cards
        |> List.head


getBracketCard : List Card -> Maybe Card
getBracketCard cards =
    let
        getCard_ card =
            case card of
                BracketCard _ ->
                    Just card

                _ ->
                    Nothing
    in
    List.filterMap getCard_ cards
        |> List.head


updateBracketCard : List Card -> Bracket.State -> List Card
updateBracketCard cards newBracketState =
    let
        updateCard_ card =
            case card of
                BracketCard _ ->
                    BracketCard newBracketState

                _ ->
                    card
    in
    List.map updateCard_ cards


getGroupMatchesCard : List Card -> Group -> Maybe Card
getGroupMatchesCard cards grp =
    let
        updateCard_ card =
            case card of
                GroupMatchesCard groupMatchesState ->
                    if groupMatchesState.group == grp then
                        Just card

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.filterMap updateCard_ cards
        |> List.head


updateGroupMatchesCard : List Card -> GroupMatches.State -> List Card
updateGroupMatchesCard cards newGroupMatchesState =
    let
        updateCard_ card =
            case card of
                GroupMatchesCard groupMatchesState ->
                    if groupMatchesState.group == newGroupMatchesState.group then
                        GroupMatchesCard newGroupMatchesState

                    else
                        card

                _ ->
                    card
    in
    List.map updateCard_ cards



-- findIDByAnswerId : Model Msg -> Maybe AnswerID -> Maybe Int
-- findIDByAnswerId model mAnswerId =
--     let
--         getAnswerIds qsModel =
--             QS.findAnswers qsModel model.bet
--                 |> List.map Tuple.first
--         isAnswer answerId ( _, card ) =
--             case card of
--                 IntroCard _ ->
--                     False
--                 QuestionCard qModel ->
--                     qModel.answerId == answerId
--                 QuestionSetCard qsModel ->
--                     List.member answerId (getAnswerIds qsModel)
--                 ParticipantCard ->
--                     False
--                 BracketCard BracketState ->
--                     False
--                 TopscorerCard ->
--                     False
--                 SubmitCard ->
--                     False
--         indexedCard answerId =
--             List.indexedMap (\a b -> ( a, b )) model.cards
--                 |> find (isAnswer answerId)
--     in
--     mAnswerId
--         |> Maybe.andThen indexedCard
--         |> Maybe.map (\( i, _ ) -> i)


update : List Card -> Int -> Card -> List Card
update cards idx newCard =
    let
        firstSet =
            List.take idx cards

        tail =
            List.drop (idx + 1) cards
    in
    firstSet ++ (newCard :: tail)


updateScreenCards : Screen.Size -> List Card -> List Card
updateScreenCards sz crds =
    List.map (updateScreenCard sz) crds


updateScreenCard : Screen.Size -> Card -> Card
updateScreenCard sz card =
    case card of
        BracketCard { bracketState } ->
            BracketCard <| Bracket.State sz bracketState

        _ ->
            card


init : Screen.Size -> List Card
init sz =
    [ IntroCard Intro
    , GroupMatchesCard <| GroupMatches.init A "m01"
    , GroupMatchesCard <| GroupMatches.init B "m03"
    , GroupMatchesCard <| GroupMatches.init C "m05"
    , GroupMatchesCard <| GroupMatches.init D "m07"
    , GroupMatchesCard <| GroupMatches.init E "m10"
    , GroupMatchesCard <| GroupMatches.init F "m11"
    , BracketCard <| Bracket.init sz
    , TopscorerCard
    , ParticipantCard
    , SubmitCard
    ]
