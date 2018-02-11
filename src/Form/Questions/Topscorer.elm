module Form.Questions.Topscorer exposing (Msg, update, view)

import Html exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Bets.Bet exposing (setTopscorer)
import Bets.Types exposing (Bet, Answer, AnswerT(..), AnswerID, Team, Topscorer, Player, TeamData, TeamDatum)
import Bets.Types.Topscorer as TS
import Bets.Types.Team exposing (initTeamData)
import Form.Questions.Types exposing (QState)
import UI.Grid as UI
import UI.Team exposing (viewTeam)


type Msg
    = SelectTeam AnswerID Team
    | SelectPlayer AnswerID String


type IsSelected
    = Selected
    | NotSelected


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        SelectTeam answerId team ->
            let
                mAnswer =
                    Bets.Bet.getAnswer bet answerId

                newBet =
                    case mAnswer of
                        Just (( _, AnswerTopscorer topscorer _ ) as answer) ->
                            let
                                newTopscorer =
                                    TS.setTeam topscorer team
                            in
                                setTopscorer bet answer newTopscorer

                        _ ->
                            bet
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )

        SelectPlayer answerId player ->
            let
                mAnswer =
                    Bets.Bet.getAnswer bet answerId

                newBet =
                    case mAnswer of
                        Just (( _, AnswerTopscorer topscorer _ ) as answer) ->
                            let
                                newTopscorer =
                                    TS.setPlayer topscorer player
                            in
                                setTopscorer bet answer newTopscorer

                        _ ->
                            bet
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )


view : Bet -> QState -> Html Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId
    in
        case mAnswer of
            Just (( answerId, AnswerTopscorer topscorer _ ) as answer) ->
                viewTopscorer bet answerId topscorer

            _ ->
                section [] [ h1 [] [ text "WHOOPS" ] ]


viewTopscorer : Bet -> AnswerID -> Topscorer -> Html Msg
viewTopscorer bet answerId topscorer =
    let
        teamData =
            initTeamData

        groups : List (List ( TeamDatum, IsSelected ))
        groups =
            initTeamData
                |> List.map isSelected
                |> List.Extra.groupWhile (\x y -> (.group (Tuple.first x)) == (.group (Tuple.first y)))

        isSelected : TeamDatum -> ( TeamDatum, IsSelected )
        isSelected t =
            case (TS.getTeam topscorer) of
                Nothing ->
                    ( t, NotSelected )

                Just team ->
                    if (.team t) == team then
                        ( t, Selected )
                    else
                        ( t, NotSelected )

        forGroup teams =
            UI.container UI.Leftside [] (List.map (mkTeamButton (SelectTeam answerId)) teams)

        headertext =
            "Wie wordt de topscorer"

        introtext =
            """
        Voorspel de topscorer. Kies eerst het land, dan de speler. 9 punten als je het goed hebt.
        Let op: dit zijn de voorlopige selecties.
      """
    in
        div []
            [ h1 [] [ text headertext ]
            , p [] [ text introtext, Html.b [] [ text "Spelers kunnen nog afvallen, of al afgevallen zijn!" ] ]
            , viewPlayers bet answerId topscorer teamData
            , section [] (List.map forGroup groups)
            ]


viewPlayers : Bet -> AnswerID -> Topscorer -> TeamData -> Html Msg
viewPlayers bet answerId topscorer teamData =
    let
        isSelectedTeam teamDatum =
            case (TS.getTeam topscorer) of
                Just t ->
                    t == (.team teamDatum)

                Nothing ->
                    False

        selectedTeam =
            List.filter isSelectedTeam initTeamData
                |> List.head

        isSelected player =
            case (TS.getPlayer topscorer) of
                Nothing ->
                    ( player, NotSelected )

                Just p ->
                    if player == p then
                        ( player, Selected )
                    else
                        ( player, NotSelected )

        players teamWP =
            .players teamWP
                |> List.map isSelected
    in
        case selectedTeam of
            Nothing ->
                div [] []

            Just teamWP ->
                UI.container UI.Leftside [] (List.map (mkPlayerButton (SelectPlayer answerId)) (players teamWP))


mkTeamButton : (Team -> Msg) -> ( TeamDatum, IsSelected ) -> Html Msg
mkTeamButton act ( teamDatum, isSelected ) =
    let
        c =
            case isSelected of
                Selected ->
                    UI.Selected

                _ ->
                    UI.Potential

        team =
            .team teamDatum

        handler =
            onClick (act team)

        contents =
            [ viewTeam (Just team) ]
    in
        UI.button UI.XS c [ handler ] contents


mkPlayerButton : (Player -> Msg) -> ( Player, IsSelected ) -> Html Msg
mkPlayerButton act ( player, isSelected ) =
    let
        c =
            case isSelected of
                Selected ->
                    UI.Selected

                _ ->
                    UI.Potential

        handler =
            onClick (act player)

        contents =
            [ text player ]
    in
        UI.pill c [ handler ] contents
