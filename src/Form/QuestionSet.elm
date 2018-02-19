module Form.QuestionSet exposing (Msg, update, view)

import Html exposing (Html, p)
import Bets.Types exposing (Bet, Answer, AnswerT(..), AnswerID, Answers, Group, Round)
import UI.Style
import Element
import Form.QuestionSets.Types exposing (Model, QuestionType(..), findAnswers, ChangeCursor(..))
import Form.QuestionSets.MatchScoreQuestions
import Form.QuestionSets.GroupPositionQuestions


type Msg
    = UpdateMatchScoreQuestions Form.QuestionSets.MatchScoreQuestions.Msg
    | UpdateGroupPositionQuestions Form.QuestionSets.GroupPositionQuestions.Msg


update : Msg -> Model -> Bet -> ( Bet, Model, Cmd Msg )
update msg model bet =
    case msg of
        UpdateMatchScoreQuestions act ->
            let
                ( newBet, newModel, fx ) =
                    Form.QuestionSets.MatchScoreQuestions.update act model bet
            in
                ( newBet, { newModel | cursor = newModel.cursor }, Cmd.map UpdateMatchScoreQuestions fx )

        UpdateGroupPositionQuestions act ->
            let
                ( newBet, newModel, fx ) =
                    Form.QuestionSets.GroupPositionQuestions.update act model bet
            in
                ( newBet, newModel, Cmd.map UpdateGroupPositionQuestions fx )


getAnswer : AnswerID -> Answers -> Maybe Answer
getAnswer cursor answers =
    let
        isAnswer answerId ( aId, _ ) =
            answerId == aId
    in
        List.filter (isAnswer cursor) answers
            |> List.head


view : Model -> Bet -> Html Msg
view model bet =
    let
        answers =
            findAnswers model bet

        mCurrentAnswer =
            getAnswer model.cursor answers
    in
        case model.questionType of
            MatchScore grp ->
                Form.QuestionSets.MatchScoreQuestions.view model bet mCurrentAnswer answers
                    |> Element.layout UI.Style.stylesheet
                    |> Html.map UpdateMatchScoreQuestions

            GroupPosition grp ->
                Form.QuestionSets.GroupPositionQuestions.view model bet mCurrentAnswer answers
                    |> Element.layout UI.Style.stylesheet
                    |> Html.map UpdateGroupPositionQuestions
