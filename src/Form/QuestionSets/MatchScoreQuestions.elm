module Form.QuestionSets.MatchScoreQuestions exposing (Msg, update, view)

import Bets.Bet exposing (setMatchScore)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Answers, Bet, Group, Match, Score, Team)
import Bets.Types.Group as G
import Bets.Types.Match as M
import Bets.Types.Score as S
import Element
import Element.Attributes as Attributes exposing (center, height, padding, px, spacing, spread, verticalCenter, width)
import Element.Events
import Element.Input as Input
import Form.QuestionSets.Types exposing (ChangeCursor(..), Model, updateCursor)
import UI.Button
import UI.Style
import UI.Team exposing (viewTeam, viewTeamLarge)
import UI.Text


type Msg
    = UpdateHome Answer Int
    | UpdateAway Answer Int
    | Update Answer Int Int
    | SelectMatch AnswerID
    | NoOp


update : Msg -> Model -> Bet -> ( Bet, Model, Cmd Msg )
update action model bet =
    case action of
        UpdateHome answer h ->
            ( setMatchScore bet answer ( Just h, Nothing ), updateCursor model bet Dont, Cmd.none )

        UpdateAway answer a ->
            ( setMatchScore bet answer ( Nothing, Just a ), updateCursor model bet Implicit, Cmd.none )

        Update answer h a ->
            ( setMatchScore bet answer ( Just h, Just a ), updateCursor model bet Implicit, Cmd.none )

        SelectMatch answerId ->
            ( bet, updateCursor model bet (Explicit answerId), Cmd.none )

        NoOp ->
            ( bet, model, Cmd.none )


view :
    { a | cursor : AnswerID }
    -> b
    -> Maybe ( AnswerID, AnswerT )
    -> List ( AnswerID, AnswerT )
    -> Element.Element UI.Style.Style variation Msg
view model bet mAnswer answers =
    case mAnswer of
        Just (( answerId, AnswerGroupMatch g match mScore points ) as answer) ->
            Element.column UI.Style.Page
                [ width (px 650), spacing 20 ]
                [ displayHeader g
                , introduction
                , displayMatches model.cursor answers
                , viewInput model answer (M.homeTeam match) (M.awayTeam match) mScore
                , viewKeyboard model answer
                ]

        _ ->
            Element.empty


displayHeader : Group -> Element.Element UI.Style.Style variation msg
displayHeader grp =
    UI.Text.displayHeader ("Voorspel de uitslagen in group " ++ G.toString grp)


introduction : Element.Element UI.Style.Style variation msg
introduction =
    Element.paragraph UI.Style.Introduction
        [ spacing 7 ]
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
    -> Answer
    -> Maybe Team
    -> Maybe Team
    -> Maybe Score
    -> Element.Element UI.Style.Style variation Msg
viewInput model answer homeTeam awayTeam mScore =
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
                    , value = v
                    , label = Input.hiddenLabel ".."
                    , options = []
                    }
            in
            Input.text UI.Style.ScoreInput [ width (px 30) ] inp

        wrap fld =
            Element.el UI.Style.Wrapper [ width (px 34), center, verticalCenter ] fld

        extractScore extractor =
            mScore
                |> Maybe.andThen extractor
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        homeInput =
            inputField (extractScore S.homeScore) (UpdateHome answer)
                |> wrap

        awayInput =
            inputField (extractScore S.awayScore) (UpdateAway answer)
                |> wrap

        homeBadge =
            UI.Team.viewTeamFull homeTeam

        awayBadge =
            UI.Team.viewTeamFull awayTeam
    in
    Element.row UI.Style.ActiveMatch
        [ center, padding 20, spacing 7 ]
        [ homeBadge
        , homeInput
        , awayInput
        , awayBadge
        ]


viewKeyboard : a -> Answer -> Element.Element UI.Style.Style variation Msg
viewKeyboard model answer =
    let
        toButton ( _, ( h, a, t ) ) =
            scoreButton UI.Style.SBPotential answer h a t

        toRow scoreList =
            Element.row UI.Style.ScoreRow
                [ center, spacing 2, verticalCenter ]
                (List.map toButton scoreList)
    in
    Element.column UI.Style.ScoreColumn
        [ spacing 2 ]
        (List.map toRow scores)


scoreButton : UI.Style.ScoreButtonSemantics -> Answer -> Int -> Int -> String -> Element.Element UI.Style.Style variation Msg
scoreButton c answer home away t =
    let
        msg =
            Update answer home away
    in
    UI.Button.scoreButton c msg t


displayMatches :
    AnswerID
    -> List ( AnswerID, AnswerT )
    -> Element.Element UI.Style.Style variation Msg
displayMatches cursor answers =
    let
        display =
            displayMatch cursor
    in
    --
    Element.wrappedRow UI.Style.Matches
        [ padding 10, spacing 7, center, width (px 600) ]
        (List.filterMap display answers)


displayMatch : AnswerID -> ( AnswerID, AnswerT ) -> Maybe (Element.Element UI.Style.Style variation Msg)
displayMatch cursor ( answerId, answer ) =
    let
        semantics =
            if cursor == answerId then
                UI.Style.Active

            else
                UI.Style.Potential

        disp match mScore =
            let
                c =
                    case ( cursor == answerId, S.isComplete mScore ) of
                        ( True, _ ) ->
                            3

                        ( _, True ) ->
                            12

                        ( _, False ) ->
                            13

                handler =
                    Element.Events.onClick (SelectMatch answerId)

                home =
                    UI.Team.viewTeamEl (M.homeTeam match)

                away =
                    UI.Team.viewTeamEl (M.awayTeam match)

                sc =
                    displayScore mScore
            in
            Element.row (UI.Style.MatchRow semantics)
                [ handler, spread, verticalCenter, padding 10, spacing 7, width (px 150), height (px 70) ]
                [ home, sc, away ]
    in
    case answer of
        AnswerGroupMatch g match mScore _ ->
            Just <| disp match mScore

        _ ->
            Nothing


scoreString : Int -> Int -> String
scoreString h a =
    List.foldr (++) "" [ " ", String.fromInt h, "-", String.fromInt a, " " ]


displayScore : Maybe Score -> Element.Element UI.Style.Style variation msg
displayScore mScore =
    let
        txt =
            case mScore of
                Just score ->
                    S.asString score

                Nothing ->
                    " _-_ "
    in
    Element.el UI.Style.Score [ verticalCenter ] (Element.text txt)
