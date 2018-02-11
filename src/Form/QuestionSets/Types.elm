module Form.QuestionSets.Types
    exposing
        ( QuestionType(..)
        , Model
        , ChangeCursor(..)
        , matchScoreQuestions
        , groupPositionQuestions
        , findAnswers
        , updateCursor
        , isComplete
        , display
        )

import Bets.Types exposing (AnswerID, Group(..), Bet, Answers)
import Bets.Bet exposing (findGroupMatchAnswers, findGroupPositionAnswers)
import List.Extra exposing (dropWhile)
import Bets.Types.Answer
import Bets.Types.Group as Group


type QuestionType
    = MatchScore Group
    | GroupPosition Group



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


groupPositionQuestions : Group -> AnswerID -> Model
groupPositionQuestions grp cursor =
    init (GroupPosition grp) cursor


findAnswers : Model -> Bet -> Answers
findAnswers model bet =
    case model.questionType of
        MatchScore grp ->
            findGroupMatchAnswers grp bet

        GroupPosition grp ->
            findGroupPositionAnswers grp bet


nextAnswer : AnswerID -> List AnswerID -> AnswerID
nextAnswer answerId answers =
    let
        isNotAnswer aId =
            aId /= answerId

        findNext =
            dropWhile isNotAnswer answers
                |> List.tail

        -- `Maybe.andThen` List.head
    in
        Maybe.withDefault answerId (findNext |> Maybe.andThen List.head)


updateCursor : Model -> Bet -> ChangeCursor -> Model
updateCursor model bet changeCursor =
    let
        newAnswer answers =
            case changeCursor of
                Implicit ->
                    nextAnswer model.cursor answers

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
                    "Scores " ++ (Group.toString group)

                _ ->
                    Group.toString group

        GroupPosition group ->
            case group of
                A ->
                    "Stand " ++ (Group.toString group)

                _ ->
                    Group.toString group
