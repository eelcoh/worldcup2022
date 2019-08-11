module Form.Questions.GroupBestThirds exposing (Msg, update, view)

import Bets.Bet exposing (candidates, getAnswer, setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Candidates, Group, Pos(..), Team)
import Element exposing (layout, padding, px, spacing, width)
import Form.Questions.Types exposing (QState)
import UI.Button
import UI.Style
import UI.Team exposing (viewTeam)
import UI.Text


type Msg
    = SetTeam AnswerID Group Team


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        SetTeam answerId grp team ->
            let
                mAnswer =
                    getAnswer bet answerId

                newBet =
                    case mAnswer of
                        Just answer ->
                            setTeam bet answer grp team

                        Nothing ->
                            bet
            in
            ( newBet, { qState | next = Nothing }, Cmd.none )


view : Bet -> QState -> Element.Element Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId

        act answerId =
            SetTeam answerId

        cs answr =
            candidates bet answr

        header =
            UI.Text.displayHeader "Wat worden de beste nummers 3?"

        body =
            case mAnswer of
                Just (( answerId, AnswerGroupBestThirds bestThirds _ ) as answer) ->
                    let
                        rowItems =
                            List.map (displayTeam (act answerId)) (cs answer)
                    in
                    Element.row (UI.Style.groupPosition [ padding 10, spacing 7 ])
                        rowItems

                _ ->
                    Element.none
    in
    Element.column (UI.Style.none [ width (px 600) ])
        [ header
        , introduction
        , body
        ]


introduction : Element.Element Msg
introduction =
    Element.paragraph (UI.Style.introduction [ width (px 600), spacing 7 ])
        [ Element.text "Van de zes beste nummers 3 gaan er vier door. Welke zijn dat? Voor elk land dat je juist hebt voorspeld voor de tweede ronde krijg je 1 punt." ]


displayTeam : (a -> Team -> Msg) -> ( a, Team, Pos ) -> Element.Element Msg
displayTeam act ( grp, team, pos ) =
    let
        c =
            case pos of
                TopThird ->
                    UI.Style.Selected

                Third ->
                    UI.Style.Potential

                _ ->
                    UI.Style.Inactive

        msg =
            act grp team

        contents =
            [ viewTeam (Just team) ]
    in
    UI.Button.teamButton c msg team
