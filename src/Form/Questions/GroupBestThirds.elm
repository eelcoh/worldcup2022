module Form.Questions.GroupBestThirds exposing (Msg, update, view)

import Bets.Bet exposing (setTeam, getAnswer, candidates)
import Bets.Types exposing (Answer, AnswerT(..), AnswerID, Team, Pos(..), Candidates, Bet, Group)
import Html exposing (..)
import Html.Events exposing (onClick)
import Form.Questions.Types exposing (QState)
import UI.Grid as UI exposing (..)
import UI.Team exposing (viewTeam)


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


view : Bet -> QState -> Html Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId

        act answerId =
            SetTeam answerId

        cs answr =
            candidates bet answr

        header =
            h1 [] [ text "Wat worden de beste nummers 3? " ]

        body =
            case mAnswer of
                Just (( answerId, AnswerGroupBestThirds bestThirds _ ) as answer) ->
                    List.map (displayTeam (act answerId)) (cs answer)

                _ ->
                    [ cell XS Perhaps [] [ text "WHOOPS" ] ]

        contents =
            [ header
            , introduction
            , container Leftside [] body
            ]
    in
        div [] contents


introduction : Html Msg
introduction =
    p [] [ text "Van de zes beste nummers 3 gaan er vier door. Welke zijn dat? Voor elk land dat je juist hebt voorspeld voor de tweede ronde krijg je 1 punt." ]


displayTeam : (Group -> Team -> Msg) -> ( Group, Team, Pos ) -> Html Msg
displayTeam act ( grp, team, pos ) =
    let
        c =
            case pos of
                TopThird ->
                    Selected

                Third ->
                    Potential

                _ ->
                    Inactive

        handler =
            onClick (act grp team)

        contents =
            [ viewTeam (Just team) ]
    in
        UI.button XS c [ handler ] contents
