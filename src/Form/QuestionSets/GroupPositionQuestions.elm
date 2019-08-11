module Form.QuestionSets.GroupPositionQuestions exposing (Msg, update, view)

import Bets.Bet exposing (candidates, setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Answers, Bet, Group, Pos(..), Team)
import Bets.Types.Group as G
import Element exposing (centerX, centerY, height, padding, px, spacing, width)
import Form.QuestionSets.Types exposing (ChangeCursor(..), Model, QuestionType(..), updateCursor)
import UI.Button
import UI.Style
import UI.Text


type Msg
    = SetTeam Answer Group Team
    | SelectPos AnswerID


update : Msg -> Model -> Bet -> ( Bet, Model, Cmd Msg )
update msg model bet =
    case msg of
        SetTeam answer team grp ->
            ( setTeam bet answer team grp, updateCursor model bet Implicit, Cmd.none )

        SelectPos answerId ->
            ( bet, updateCursor model bet (Explicit answerId), Cmd.none )


view : { a | questionType : QuestionType } -> Bet -> b -> List ( AnswerID, AnswerT ) -> Element.Element Msg
view model bet mAnswer answers =
    case model.questionType of
        GroupPosition grp ->
            Element.column (UI.Style.page [ spacing 20 ])
                [ displayHeader grp
                , introduction
                , Element.column (UI.Style.groupPositions []) (List.filterMap (viewAnswer model bet) answers)
                ]

        _ ->
            Element.none


displayHeader : Group -> Element.Element Msg
displayHeader grp =
    UI.Text.displayHeader ("Voorspel de eindstand van poule " ++ G.toString grp)


introduction : Element.Element Msg
introduction =
    Element.paragraph (UI.Style.introduction [ spacing 7 ])
        [ Element.text """Voorspel de nummers 1, 2 en 3 van de eindstand in de poule.
      De nummers 1 en 2 gaan door, en daarvoor krijg je 1 punt per land dat de tweede ronde haalt. """
        , Element.el (UI.Style.emphasis []) (Element.text "Klik")
        , Element.text """op een team om het te selecteren voor de positie in de eindstand.
        Voor de nummers 3 volgt nog een verdere vraag."""
        ]


viewAnswer : a -> Bet -> ( AnswerID, AnswerT ) -> Maybe (Element.Element Msg)
viewAnswer model bet (( aid, answerT ) as answer) =
    let
        cs =
            candidates bet answer

        posText p =
            case p of
                First ->
                    Element.text "1e"

                Second ->
                    Element.text "2e"

                Third ->
                    Element.text "3e"

                _ ->
                    Element.text ""

        header g p =
            Element.el (UI.Style.groupBadge [ width (px 64), height (px 76), centerX, centerY ])
                (posText p)
    in
    case answerT of
        AnswerGroupPosition g pos ( drawID, mTeam ) _ ->
            let
                buttons =
                    List.map (mkButton answer g pos) cs

                hdr =
                    header g pos

                rowItems =
                    hdr :: buttons
            in
            Element.row (UI.Style.groupPosition [ padding 10, spacing 7 ])
                rowItems
                |> Just

        _ ->
            Nothing


mkButton : Answer -> Group -> a -> ( b, Team, a ) -> Element.Element Msg
mkButton answer forGrp forPos ( grp, team, pos ) =
    let
        c =
            if pos == forPos then
                UI.Style.Selected

            else
                UI.Style.Potential

        msg =
            SetTeam answer forGrp team
    in
    UI.Button.teamButton c msg team
