module Form.Questions.Topscorer exposing (Msg, update, view)

import Html exposing (..)
import List.Extra
import Bets.Bet exposing (setTopscorer)
import Bets.Types exposing (Bet, Answer, AnswerT(..), AnswerID, Team, Topscorer, Player, TeamData, TeamDatum)
import UI.Text
import UI.Style
import Bets.Types.Topscorer as TS
import Bets.Types.Team exposing (initTeamData)
import Form.Questions.Types exposing (QState)
import UI.Team exposing (viewTeam)
import UI.Button
import Element
import Element.Attributes exposing (width, px, spacing, padding)


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
            Element.row UI.Style.None [ spacing 7, padding 10, width (px 600) ] (List.map (mkTeamButton (SelectTeam answerId)) teams)

        headertext =
            UI.Text.displayHeader "Wie wordt de topscorer?"
    in
        Element.column UI.Style.None
            []
            ([ headertext
             , introduction
             , viewPlayers bet answerId topscorer teamData
             ]
                ++ (List.map forGroup groups)
            )
            |> Element.layout UI.Style.stylesheet


introduction : Element.Element UI.Style.Style variation msg
introduction =
    Element.paragraph UI.Style.Introduction
        [ width (px 600), spacing 7 ]
        [ UI.Text.simpleText """
    Voorspel de topscorer. Kies eerst het land, dan de speler. 9 punten als je het goed hebt.
    Let op: dit zijn de voorlopige selecties.
  """
        , UI.Text.boldText "Spelers kunnen nog afvallen, of al afgevallen zijn!"
        ]


viewPlayers :
    a
    -> AnswerID
    -> Topscorer
    -> b
    -> Element.Element UI.Style.Style variation Msg
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
                Element.empty

            Just teamWP ->
                Element.wrappedRow UI.Style.None
                    [ width (px 600), padding 10, spacing 7 ]
                    (List.map (mkPlayerButton (SelectPlayer answerId)) (players teamWP))


mkTeamButton :
    (Team -> a)
    -> ( { b | team : Team }, IsSelected )
    -> Element.Element UI.Style.Style variation a
mkTeamButton act ( teamDatum, isSelected ) =
    let
        c =
            case isSelected of
                Selected ->
                    UI.Style.TBSelected

                _ ->
                    UI.Style.TBPotential

        team =
            .team teamDatum

        msg =
            act team

        contents =
            [ viewTeam (Just team) ]
    in
        UI.Button.teamButton c msg team


mkPlayerButton :
    (String -> a)
    -> ( String, IsSelected )
    -> Element.Element UI.Style.Style variation a
mkPlayerButton act ( player, isSelected ) =
    let
        c =
            case isSelected of
                Selected ->
                    UI.Style.Selected

                _ ->
                    UI.Style.Potential

        msg =
            act player

        contents =
            player
    in
        UI.Button.pill c msg player
