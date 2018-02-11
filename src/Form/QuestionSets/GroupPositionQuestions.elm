module Form.QuestionSets.GroupPositionQuestions exposing (Msg, update, view)

import Bets.Bet exposing (setTeam, candidates)
import Bets.Types exposing (Answer, AnswerT(..), AnswerID, Answers, Team, Group, Pos(..), Bet)
import Bets.Types.Group as G
import UI.Team exposing (viewTeam)
import Form.QuestionSets.Types exposing (QuestionType(..), Model, ChangeCursor(..), updateCursor)
import Html exposing (Html, h1, h2, text, p, section, div)
import Html.Events exposing (onClick)
import UI.Grid as UI exposing (Color(..), Size(..), Align(..))


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


view : Model -> Bet -> Maybe Answer -> Answers -> Html Msg
view model bet mAnswer answers =
    Html.div []
        [ displayHeader model.questionType
        , introduction
        , section [] (List.map (viewAnswer model bet) answers)
        ]


displayHeader : QuestionType -> Html Msg
displayHeader qT =
    case qT of
        GroupPosition grp ->
            h1 [] [ text ("Voorspel de eindstand van poule " ++ (G.toString grp)) ]

        _ ->
            p [] []


introduction : Html Msg
introduction =
    p []
        [ text """Voorspel de nummers 1, 2 en 3 van de eindstand in de poule.
      De nummers 1 en 2 gaan door, en daarvoor krijg je 1 punt per
      land dat de tweede ronde haalt. """
        , Html.b [] [ text "Klik" ]
        , text """ op een team om het te selecteren voor de positie in de eindstand.
        Voor de nummers 3 volgt nog
        een verdere vraag."""
        ]


viewAnswer : Model -> Bet -> Answer -> Html Msg
viewAnswer model bet (( aid, answerT ) as answer) =
    let
        cs =
            candidates bet answer

        posText p =
            case p of
                First ->
                    text "1e"

                Second ->
                    text "2e"

                Third ->
                    text "3e"

                _ ->
                    text ""

        header g p =
            UI.cell XS Irrelevant [] [ (h2 [] [ posText p ]) ]
    in
        case answerT of
            AnswerGroupPosition g pos ( drawID, mTeam ) _ ->
                UI.container Leftside
                    []
                    ((header g pos) :: (List.map (mkButton answer g pos) cs))

            _ ->
                div [] [ text "WHOOPS" ]


mkButton : Answer -> Group -> Pos -> ( Group, Team, Pos ) -> Html Msg
mkButton answer forGrp forPos ( grp, team, pos ) =
    let
        c =
            if pos == forPos then
                Selected
            else
                Potential

        handler =
            onClick (SetTeam answer forGrp team)
    in
        UI.button XS c [ handler ] [ viewTeam (Just team) ]
