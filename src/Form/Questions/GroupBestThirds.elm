module Form.Questions.GroupBestThirds exposing (Msg, update, view)

import Bets.Bet exposing (candidates, getAnswer, setTeam)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Candidates, Group, Pos(..), Team)
import Element exposing (layout)
import Element.Attributes exposing (padding, spacing, width, px)
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


view : Bet -> QState -> Element.Element UI.Style.Style variation Msg
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
                        Element.row UI.Style.GroupPosition
                            [ padding 10, spacing 7 ]
                            rowItems

                _ ->
                    Element.empty
    in
        Element.column UI.Style.None
            [ width (px 600) ]
            [ header
            , introduction
            , body
            ]


introduction : Element.Element UI.Style.Style variation msg
introduction =
    Element.paragraph UI.Style.Introduction
        [ width (px 600), spacing 7 ]
        [ Element.text "Van de zes beste nummers 3 gaan er vier door. Welke zijn dat? Voor elk land dat je juist hebt voorspeld voor de tweede ronde krijg je 1 punt." ]


displayTeam :
    (a -> Team -> b)
    -> ( a, Team, Pos )
    -> Element.Element UI.Style.Style variation b
displayTeam act ( grp, team, pos ) =
    let
        c =
            case pos of
                TopThird ->
                    UI.Style.TBSelected

                Third ->
                    UI.Style.TBPotential

                _ ->
                    UI.Style.TBInactive

        msg =
            (act grp team)

        contents =
            [ viewTeam (Just team) ]
    in
        UI.Button.teamButton c msg team
