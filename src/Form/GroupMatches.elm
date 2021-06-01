module Form.GroupMatches exposing (isComplete, update, view)

import Bets.Bet exposing (setMatchScore)
import Bets.Types exposing (Answer(..), AnswerGroupMatch, Bet, Group, GroupMatch(..), MatchID, Score, Team)
import Bets.Types.Answer.GroupMatches as GroupMatches
import Bets.Types.Group as G
import Bets.Types.Match as M
import Bets.Types.Score as S
import Element exposing (centerX, centerY, fill, height, padding, paddingXY, px, spacing, width)
import Element.Events
import Element.Input as Input
import Form.GroupMatches.Types exposing (ChangeCursor(..), Msg(..), State, updateCursor)
import List.Extra exposing (groupsOf)
import UI.Button.Score exposing (displayScore)
import UI.Match
import UI.Page exposing (page)
import UI.Style
import UI.Team
import UI.Text


isComplete : Group -> Bet -> Bool
isComplete grp bet =
    GroupMatches.isCompleteGroup grp bet.answers.matches


update : Msg -> State -> Bet -> ( Bet, State, Cmd Msg )
update action state bet =
    case action of
        UpdateHome matchID h ->
            ( setMatchScore bet matchID ( Just h, Nothing ), updateCursor state bet Dont, Cmd.none )

        UpdateAway matchID a ->
            ( setMatchScore bet matchID ( Nothing, Just a ), updateCursor state bet Implicit, Cmd.none )

        Update matchID h a ->
            ( setMatchScore bet matchID ( Just h, Just a ), updateCursor state bet Implicit, Cmd.none )

        SelectMatch matchID ->
            ( bet, updateCursor state bet (Explicit matchID), Cmd.none )

        NoOp ->
            ( bet, state, Cmd.none )


view : Bet -> State -> Element.Element Msg
view bet state =
    let
        groupMatches =
            GroupMatches.findGroupMatchAnswers state.group bet.answers.matches

        mCurrentMatch =
            let
                isCurrentMatch matchID ( mId, _ ) =
                    matchID == mId
            in
            List.filter (isCurrentMatch state.cursor) groupMatches
                |> List.head
    in
    view_ state mCurrentMatch groupMatches


view_ : State -> Maybe ( MatchID, AnswerGroupMatch ) -> List ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
view_ state mMatch matches =
    case mMatch of
        Just ( matchID, Answer (GroupMatch g match mScore) _ ) ->
            page "groupmatch"
                [ displayHeader g
                , introduction
                , displayMatches state.cursor matches
                , viewInput state matchID (M.homeTeam match) (M.awayTeam match) mScore
                , viewKeyboard matchID
                ]

        _ ->
            Element.none


displayHeader : Group -> Element.Element Msg
displayHeader grp =
    UI.Text.displayHeader ("Voorspel de uitslagen in group " ++ G.toString grp)


introduction : Element.Element Msg
introduction =
    Element.paragraph [] [ UI.Text.simpleText "Voorspel de uitslagen door op de knop met de gewenste score te klikken. Voor een juiste uitslag krijg je 3 punten. Heb je enkel de toto goed levert je dat 1 punt op." ]


viewInput :
    a
    -> MatchID
    -> Maybe Team
    -> Maybe Team
    -> Maybe Score
    -> Element.Element Msg
viewInput _ matchID homeTeam awayTeam mScore =
    let
        makeAction act val =
            case String.toInt val of
                Just v ->
                    act v

                Nothing ->
                    NoOp

        inputField v act =
            let
                inp =
                    { onChange = makeAction act
                    , text = v
                    , label = Input.labelHidden ".."
                    , placeholder = Just (Input.placeholder [] (Element.text v))
                    }
            in
            Input.text (UI.Style.scoreInput [ width (px 45) ]) inp

        wrap fld =
            Element.el (UI.Style.wrapper [ width (px 34), centerX, centerY ]) fld

        extractScore extractor =
            mScore
                |> Maybe.andThen extractor
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        homeInput =
            inputField (extractScore S.homeScore) (UpdateHome matchID)
                |> wrap

        awayInput =
            inputField (extractScore S.awayScore) (UpdateAway matchID)
                |> wrap

        homeBadge =
            UI.Team.viewTeamFull homeTeam

        awayBadge =
            UI.Team.viewTeamFull awayTeam
    in
    Element.row (UI.Style.activeMatch [ centerX, padding 20, spacing 20 ])
        [ homeBadge
        , homeInput
        , awayInput
        , awayBadge
        ]


displayMatches : MatchID -> List ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatches cursor matches =
    let
        display =
            displayMatch cursor

        rows =
            groupsOf 2 matches

        row matches_ =
            Element.row (UI.Style.matches [ spacing 20 ]) (List.filterMap display matches_)
    in
    --
    Element.column [ centerX, spacing 20 ] (List.map row rows)


displayMatch : MatchID -> ( MatchID, AnswerGroupMatch ) -> Maybe (Element.Element Msg)
displayMatch cursor ( answerId, Answer (GroupMatch _ match mScore) _ ) =
    let
        semantics =
            if cursor == answerId then
                UI.Style.Active

            else
                UI.Style.Potential

        disp =
            let
                handler =
                    Element.Events.onClick (SelectMatch answerId)
            in
            UI.Match.display match mScore handler semantics
    in
    Just <| disp



-- scoreButton : UI.Style.ButtonSemantics -> MatchID -> ScoreButtonX -> Element.Element Msg
-- scoreButton c matchID sb =
--     let
--         ( msg, txt ) =
--             case sb of
--                 ZB _ ->
--                     ( NoOp, "" )
--                 SB _ ( home, away ) t ->
--                     ( Update matchID home away, t )
--     in
--     UI.Button.scoreButton c msg txt


viewKeyboard : MatchID -> Element.Element Msg
viewKeyboard matchId =
    UI.Button.Score.viewKeyboard NoOp (Update matchId)



-- viewKeyboard : MatchID -> Element.Element Msg
-- viewKeyboard matchID =
--     let
--         toButton s =
--             scoreButton UI.Style.Potential matchID s
--         toRow scoreList =
--             Element.row (UI.Style.scoreRow [ centerX, spacing 2, centerY ])
--                 (List.map toButton scoreList)
--     in
--     Element.column (UI.Style.scoreColumn [ centerX, spacing 2 ])
--         (List.map toRow UI.Button.Score.scores)
