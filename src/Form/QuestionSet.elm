module Form.QuestionSet exposing (Msg, update, view)

import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Answers, Bet)
import Element
import Form.QuestionSets.MatchScoreQuestions
import Form.QuestionSets.Types exposing (ChangeCursor(..), Model, QuestionType(..), findAnswers)


type Msg
    = UpdateMatchScoreQuestions Form.QuestionSets.MatchScoreQuestions.Msg


update : Msg -> Model -> Bet -> ( Bet, Model, Cmd Msg )
update (UpdateMatchScoreQuestions act) model bet =
    let
        ( newBet, newModel, fx ) =
            Form.QuestionSets.MatchScoreQuestions.update act model bet
    in
    ( newBet, { newModel | cursor = newModel.cursor }, Cmd.map UpdateMatchScoreQuestions fx )


getAnswer : AnswerID -> Answers -> Maybe Answer
getAnswer cursor answers =
    let
        isAnswer answerId ( aId, _ ) =
            answerId == aId
    in
    List.filter (isAnswer cursor) answers
        |> List.head


view : Model -> Bet -> Element.Element Msg
view model bet =
    let
        answers =
            findAnswers model bet

        mCurrentAnswer =
            getAnswer model.cursor answers
    in
    case model.questionType of
        MatchScore _ ->
            Form.QuestionSets.MatchScoreQuestions.view model bet mCurrentAnswer answers
                |> Element.map UpdateMatchScoreQuestions
