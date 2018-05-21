module Form.QuestionSets.GroupPositionQuestions exposing (Msg, update, view)

import Bets.Bet exposing (candidates, setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Answers, Bet, Group, Pos(..), Team)
import UI.Text
import Bets.Types.Group as G
import Element
import Element.Attributes exposing (px, spacing, padding, width, height, center, verticalCenter)
import Form.QuestionSets.Types exposing (ChangeCursor(..), Model, QuestionType(..), updateCursor)
import UI.Button
import UI.Style


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


view :
    { a | questionType : QuestionType }
    -> Bet
    -> b
    -> List ( AnswerID, AnswerT )
    -> Element.Element UI.Style.Style variation Msg
view model bet mAnswer answers =
    case model.questionType of
        GroupPosition grp ->
            Element.column UI.Style.Page
                [ spacing 20 ]
                [ displayHeader grp
                , introduction
                , Element.column UI.Style.GroupPositions [] (List.filterMap (viewAnswer model bet) answers)
                ]

        _ ->
            Element.empty


displayHeader : Group -> Element.Element UI.Style.Style variation msg
displayHeader grp =
    UI.Text.displayHeader ("Voorspel de eindstand van poule " ++ (G.toString grp))


introduction : Element.Element UI.Style.Style variation msg
introduction =
    Element.paragraph UI.Style.Introduction
        [ spacing 7 ]
        [ Element.text """Voorspel de nummers 1, 2 en 3 van de eindstand in de poule.
      De nummers 1 en 2 gaan door, en daarvoor krijg je 1 punt per land dat de tweede ronde haalt. """
        , Element.el UI.Style.Emphasis [] (Element.text "Klik")
        , Element.text """op een team om het te selecteren voor de positie in de eindstand.
        Voor de nummers 3 volgt nog een verdere vraag."""
        ]


viewAnswer :
    a
    -> Bet
    -> ( AnswerID, AnswerT )
    -> Maybe (Element.Element UI.Style.Style variation Msg)
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
            Element.el UI.Style.GroupBadge
                [ width (px 64), height (px 76), center, verticalCenter ]
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
                    Element.row UI.Style.GroupPosition
                        [ padding 10, spacing 7 ]
                        rowItems
                        |> Just

            _ ->
                Nothing


mkButton :
    Answer
    -> Group
    -> a
    -> ( b, Team, a )
    -> Element.Element UI.Style.Style variation Msg
mkButton answer forGrp forPos ( grp, team, pos ) =
    let
        c =
            if pos == forPos then
                UI.Style.TBSelected
            else
                UI.Style.TBPotential

        msg =
            SetTeam answer forGrp team
    in
        UI.Button.teamButton c msg team
