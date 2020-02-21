module Form.GroupMatches exposing (isComplete, update, view)

import Bets.Bet exposing (setMatchScore)
import Bets.Types exposing (Answer(..), AnswerGroupMatch, AnswerGroupMatches, Bet, Group, GroupMatch(..), MatchID, Score, Team)
import Bets.Types.Answer.GroupMatches as GroupMatches
import Bets.Types.Group as G
import Bets.Types.Match as M
import Bets.Types.Score as S
import Element exposing (centerX, centerY, height, padding, px, spaceEvenly, spacing, width)
import Element.Events
import Element.Input as Input
import Form.GroupMatches.Types exposing (ChangeCursor(..), Msg(..), State, updateCursor)
import UI.Button
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
            Element.column (UI.Style.page [ width (px 650), spacing 20 ])
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
    Element.paragraph (UI.Style.introduction [ spacing 7 ])
        [ Element.text "Voorspel de uitslagen door op de knop met de gewenste score te klikken. Voor een juiste uitslag krijg je 3 punten. Heb je enkel de toto goed levert je dat 1 punt op." ]


row0 : List ( Int, Int )
row0 =
    [ ( 5, 0 )
    , ( 4, 0 )
    , ( 3, 0 )
    , ( 2, 0 )
    , ( 1, 0 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 0, 2 )
    , ( 0, 3 )
    , ( 0, 4 )
    , ( 0, 5 )
    ]


row1 : List ( Int, Int )
row1 =
    [ ( 5, 1 )
    , ( 4, 1 )
    , ( 3, 1 )
    , ( 2, 1 )
    , ( 1, 1 )
    , ( 1, 2 )
    , ( 1, 3 )
    , ( 1, 4 )
    , ( 1, 5 )
    ]


row2 : List ( Int, Int )
row2 =
    [ ( 5, 2 )
    , ( 4, 2 )
    , ( 3, 2 )
    , ( 2, 2 )
    , ( 2, 3 )
    , ( 2, 4 )
    , ( 2, 5 )
    ]


row3 : List ( Int, Int )
row3 =
    [ ( 5, 3 )
    , ( 4, 3 )
    , ( 3, 3 )
    , ( 3, 4 )
    , ( 3, 5 )
    ]


row4 : List ( Int, Int )
row4 =
    [ ( 5, 4 )
    , ( 4, 4 )
    , ( 4, 5 )
    ]


row5 : List ( Int, Int )
row5 =
    [ ( 5, 5 )
    ]


scores : List (List ( Int, ( Int, Int, String ) ))
scores =
    [ row0
    , row1
    , row2
    , row3
    , row4
    , row5
    ]
        |> List.map indexedScores


indexedScores : List ( Int, Int ) -> List ( Int, ( Int, Int, String ) )
indexedScores scoreList =
    scoreList
        |> List.map (\( h, a ) -> ( h, a, scoreString h a ))
        |> List.indexedMap (\a b -> ( a, b ))


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
            Input.text (UI.Style.scoreInput [ width (px 30) ]) inp

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
    Element.row (UI.Style.activeMatch [ centerX, padding 20, spacing 7 ])
        [ homeBadge
        , homeInput
        , awayInput
        , awayBadge
        ]


viewKeyboard : a -> MatchID -> Element.Element Msg
viewKeyboard _ matchID =
    let
        toButton ( _, ( h, a, t ) ) =
            scoreButton UI.Style.Potential matchID h a t

        toRow scoreList =
            Element.row (UI.Style.scoreRow [ centerX, spacing 2, centerY ])
                (List.map toButton scoreList)
    in
    Element.column (UI.Style.scoreColumn [ spacing 2 ])
        (List.map toRow scores)


scoreButton : UI.Style.ButtonSemantics -> MatchID -> Int -> Int -> String -> Element.Element Msg
scoreButton c matchID home away t =
    let
        msg =
            Update matchID home away
    in
    UI.Button.scoreButton c msg t


displayMatches : MatchID -> List ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatches cursor answers =
    let
        display =
            displayMatch cursor
    in
    --
    Element.wrappedRow (UI.Style.matches [ padding 10, spacing 7, centerX, width (px 600) ])
        (List.filterMap display answers)


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
            Element.row (UI.Style.matchRow semantics [ handler, spaceEvenly, centerY, padding 10, spacing 7, width (px 150), height (px 70) ])
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
    Element.el (UI.Style.score [ centerY ]) (Element.text txt)
