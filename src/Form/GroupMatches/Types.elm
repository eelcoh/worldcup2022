module Form.GroupMatches.Types exposing
    ( ChangeCursor(..)
    , Msg(..)
    , State
    , display
    , findMatches
    , init
    , isComplete
    , matchScoreQuestions
    , updateCursor
    )

import Bets.Bet exposing (findGroupMatchAnswers)
import Bets.Types exposing (AnswerGroupMatches, Bet, Group(..), MatchID)
import Bets.Types.Answer.GroupMatches as GroupMatches
import Bets.Types.Group as Group
import List.Extra exposing (dropWhile)



-- type alias QuestionSetAnswers = Array MatchID


type Msg
    = UpdateHome MatchID Int
    | UpdateAway MatchID Int
    | Update MatchID Int Int
    | SelectMatch MatchID
    | NoOp


type alias State =
    { group : Group
    , prev : Maybe MatchID
    , cursor : MatchID
    , next : Maybe MatchID
    }


init : Group -> MatchID -> State
init group cursor =
    { group = group
    , prev = Nothing
    , cursor = cursor
    , next = Nothing
    }


type ChangeCursor
    = Explicit MatchID
    | Implicit
    | Dont


matchScoreQuestions : Group -> MatchID -> State
matchScoreQuestions grp cursor =
    init grp cursor


findMatches : Group -> Bet -> AnswerGroupMatches
findMatches grp bet =
    findGroupMatchAnswers grp bet


nextMatch : MatchID -> List MatchID -> MatchID
nextMatch matchID matches =
    let
        isNotCurrentMatch mId =
            mId /= matchID

        findNext =
            dropWhile isNotCurrentMatch matches
                |> List.tail
    in
    Maybe.withDefault matchID (findNext |> Maybe.andThen List.head)


updateCursor : State -> Bet -> ChangeCursor -> State
updateCursor model bet changeCursor =
    let
        nxtMatch matches_ =
            case changeCursor of
                Implicit ->
                    nextMatch model.cursor matches_

                Explicit newCur ->
                    newCur

                Dont ->
                    model.cursor

        matches =
            findMatches model.group bet
                |> List.map Tuple.first

        newCursor =
            nxtMatch matches

        prev =
            Just model.cursor
    in
    { model | cursor = newCursor, prev = prev }


isComplete : Bet -> State -> Bool
isComplete bet model =
    findMatches model.group bet
        |> GroupMatches.isComplete


display : State -> String
display model =
    let
        groupString =
            Group.toString model.group
    in
    case model.group of
        A ->
            "Scores " ++ groupString

        _ ->
            groupString
