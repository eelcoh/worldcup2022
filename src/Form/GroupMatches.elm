module Form.GroupMatches exposing (isComplete, update, view)

import Bets.Bet exposing (setMatchScore)
import Bets.Types exposing (Answer(..), AnswerGroupMatch, AnswerGroupMatches, Bet, Group, GroupMatch(..), MatchID, Score, Team)
import Bets.Types.Answer.GroupMatches as GroupMatches
import Bets.Types.Group as G
import Bets.Types.Match as M
import Bets.Types.Score as S
import Element exposing (centerX, centerY, height, padding, paddingXY, px, spaceEvenly, spacing, width)
import Element.Events
import Element.Input as Input
import Form.GroupMatches.Types exposing (ChangeCursor(..), Msg(..), State, updateCursor)
import List.Extra exposing (groupsOf)
import UI.Button exposing (ScoreButton(..), ScoreButtonX(..))
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
            Element.column (UI.Style.page [ centerX, spacing 20 ])
                [ displayHeader g
                , introduction
                , displayMatches state.cursor matches
                , viewInput state matchID (M.homeTeam match) (M.awayTeam match) mScore
                , viewKeyboard state matchID
                ]

        _ ->
            Element.none


displayHeader : Group -> Element.Element Msg
displayHeader grp =
    UI.Text.displayHeader ("Voorspel de uitslagen in group " ++ G.toString grp)


introduction : Element.Element Msg
introduction =
    Element.paragraph [] [ UI.Text.simpleText "Voorspel de uitslagen door op de knop met de gewenste score te klikken. Voor een juiste uitslag krijg je 3 punten. Heb je enkel de toto goed levert je dat 1 punt op." ]


row00 : List ScoreButton
row00 =
    [ SR ( 3, 0 )
    , SR ( 2, 0 )
    , SR ( 1, 0 )
    , SR ( 0, 0 )
    , SR ( 0, 1 )
    , SR ( 0, 2 )
    , SR ( 0, 3 )
    ]


row01 : List ScoreButton
row01 =
    [ SR ( 6, 0 )
    , SR ( 5, 0 )
    , SR ( 4, 0 )
    , ZR
    , SR ( 0, 4 )
    , SR ( 0, 5 )
    , SR ( 0, 6 )
    ]


row10 : List ScoreButton
row10 =
    [ SR ( 4, 1 )
    , SR ( 3, 1 )
    , SR ( 2, 1 )
    , SR ( 1, 1 )
    , SR ( 1, 2 )
    , SR ( 1, 3 )
    , SR ( 1, 4 )
    ]


row11 : List ScoreButton
row11 =
    [ SR ( 7, 1 )
    , SR ( 6, 1 )
    , SR ( 5, 1 )
    , ZR
    , SR ( 1, 5 )
    , SR ( 1, 6 )
    , SR ( 1, 7 )
    ]


row20 : List ScoreButton
row20 =
    [ SR ( 5, 2 )
    , SR ( 4, 2 )
    , SR ( 3, 2 )
    , SR ( 2, 2 )
    , SR ( 2, 3 )
    , SR ( 2, 4 )
    , SR ( 2, 5 )
    ]


row3 : List ScoreButton
row3 =
    [ SR ( 6, 3 )
    , SR ( 5, 3 )
    , SR ( 4, 3 )
    , SR ( 3, 3 )
    , SR ( 3, 4 )
    , SR ( 3, 5 )
    , SR ( 3, 6 )
    ]


row4 : List ScoreButton
row4 =
    [ SR ( 5, 4 )
    , SR ( 4, 4 )
    , SR ( 4, 5 )
    ]


row5 : List ScoreButton
row5 =
    [ SR ( 5, 5 )
    ]


scores : List (List ScoreButtonX)
scores =
    [ row00
    , row01
    , row10
    , row11
    , row20
    , row3
    , row4
    , row5
    ]
        |> List.map indexedScores


indexedScores : List ScoreButton -> List ScoreButtonX
indexedScores scoreList =
    let
        mkScoreButtonX s =
            case s of
                ( i, SR ( h, a ) ) ->
                    SB i ( h, a ) (scoreString h a)

                ( i, ZR ) ->
                    ZB i
    in
    scoreList
        |> List.indexedMap Tuple.pair
        |> List.map mkScoreButtonX


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


viewKeyboard : a -> MatchID -> Element.Element Msg
viewKeyboard _ matchID =
    let
        toButton s =
            scoreButton UI.Style.Potential matchID s

        toRow scoreList =
            Element.row (UI.Style.scoreRow [ centerX, spacing 2, centerY ])
                (List.map toButton scoreList)
    in
    Element.column (UI.Style.scoreColumn [ centerX, spacing 2 ])
        (List.map toRow scores)


scoreButton : UI.Style.ButtonSemantics -> MatchID -> ScoreButtonX -> Element.Element Msg
scoreButton c matchID sb =
    let
        ( msg, txt ) =
            case sb of
                ZB _ ->
                    ( NoOp, "" )

                SB _ ( home, away ) t ->
                    ( Update matchID home away, t )
    in
    UI.Button.scoreButton c msg txt


displayMatches : MatchID -> List ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatches cursor matches =
    let
        display =
            displayMatch cursor

        rows =
            groupsOf 2 matches

        row matches_ =
            Element.row (UI.Style.matches [ spacing 20, width (px 400) ]) (List.filterMap display matches_)
    in
    --
    Element.column [ centerX, spacing 20 ] (List.map row rows)


displayMatch : MatchID -> ( MatchID, AnswerGroupMatch ) -> Maybe (Element.Element Msg)
displayMatch cursor ( answerId, Answer (GroupMatch g match mScore) _ ) =
    let
        semantics =
            if cursor == answerId then
                UI.Style.Active

            else
                UI.Style.Potential

        disp match_ mScore_ =
            let
                handler =
                    Element.Events.onClick (SelectMatch answerId)

                home =
                    UI.Team.viewTeam (M.homeTeam match_)

                away =
                    UI.Team.viewTeam (M.awayTeam match_)

                sc =
                    displayScore mScore_
            in
            Element.row (UI.Style.matchRow semantics [ handler, spacing 5, centerY, centerX, padding 0, width (px 160), height (px 100) ])
                [ home, sc, away ]
    in
    Just <| disp match mScore


scoreString : Int -> Int -> String
scoreString h a =
    List.foldr (++) "" [ " ", String.fromInt h, "-", String.fromInt a, " " ]


displayScore : Maybe Score -> Element.Element Msg
displayScore mScore =
    let
        txt =
            case mScore of
                Just score ->
                    S.asString score

                Nothing ->
                    " _-_ "
    in
    Element.el (UI.Style.score [ width (px 50), centerY ]) (Element.text txt)
