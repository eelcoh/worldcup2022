module Form.QuestionSets.Types exposing
    ( ChangeCursor(..)
    , Model
    , QuestionType(..)
    , display
    , findAnswers
    , isComplete
    , matchScoreQuestions
    , updateCursor
    )

import Bets.Bet exposing (findGroupMatchAnswers)
import Bets.Types exposing (AnswerID, Answers, Bet, Group(..))
import Bets.Types.Answer
import Bets.Types.Group as Group
import List.Extra exposing (dropWhile)


type QuestionType
    = MatchScore Group



-- type alias QuestionSetAnswers = Array AnswerID


type alias Model =
    { questionType : QuestionType
    , prev : Maybe AnswerID
    , cursor : AnswerID
    , next : Maybe AnswerID
    }


init : QuestionType -> AnswerID -> Model
init questionType cursor =
    { questionType = questionType
    , prev = Nothing
    , cursor = cursor
    , next = Nothing
    }


type ChangeCursor
    = Explicit AnswerID
    | Implicit
    | Dont


matchScoreQuestions : Group -> AnswerID -> Model
matchScoreQuestions grp cursor =
    init (MatchScore grp) cursor


findAnswers : Model -> Bet -> Answers
findAnswers model bet =
    case model.questionType of
        MatchScore grp ->
            findGroupMatchAnswers grp bet


nextAnswer : AnswerID -> List AnswerID -> AnswerID
nextAnswer answerId answers =
    let
        isNotAnswer aId =
            aId /= answerId

        findNext =
            dropWhile isNotAnswer answers
                |> List.tail
    in
    Maybe.withDefault answerId (findNext |> Maybe.andThen List.head)


updateCursor : Model -> Bet -> ChangeCursor -> Model
updateCursor model bet changeCursor =
    let
        newAnswer answrs =
            case changeCursor of
                Implicit ->
                    nextAnswer model.cursor answrs

                Explicit newCur ->
                    newCur

                Dont ->
                    model.cursor

        answers =
            findAnswers model bet
                |> List.map Tuple.first

        newCursor =
            newAnswer answers

        prev =
            Just model.cursor
    in
    { model | cursor = newCursor, prev = prev }


isComplete : Bet -> Model -> Bool
isComplete bet model =
    let
        answers =
            findAnswers model bet
    in
    List.all Bets.Types.Answer.isComplete answers


display : Model -> String
display model =
    case model.questionType of
        MatchScore group ->
            case group of
                A ->
                    "Scores " ++ Group.toString group

                _ ->
                    Group.toString group
