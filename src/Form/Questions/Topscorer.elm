module Form.Questions.Topscorer exposing (Msg, update, view)

import Bets.Bet exposing (setTopscorer)
import Bets.Init.Euro2020.Tournament exposing (initTeamData)
import Bets.Types exposing (AnswerID, AnswerT(..), Bet, Team, TeamDatum, Topscorer)
import Bets.Types.Topscorer as TS
import Element exposing (padding, px, spacing, width)
import Form.Questions.Types exposing (QState)
import List.Extra
import UI.Button
import UI.Style
import UI.Text


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


view : Bet -> QState -> Element.Element Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId
    in
    case mAnswer of
        Just ( answerId, AnswerTopscorer topscorer _ ) ->
            viewTopscorer bet answerId topscorer

        _ ->
            Element.none


viewTopscorer : Bet -> AnswerID -> Topscorer -> Element.Element Msg
viewTopscorer bet answerId topscorer =
    let
        teamData =
            initTeamData

        groupWhile : (a -> a -> Bool) -> List a -> List (List a)
        groupWhile eq xs_ =
            case xs_ of
                [] ->
                    []

                x :: xs ->
                    let
                        ( ys, zs ) =
                            List.Extra.span (eq x) xs
                    in
                    (x :: ys) :: groupWhile eq zs

        groups : List (List ( TeamDatum, IsSelected ))
        groups =
            initTeamData
                |> List.map isSelected
                |> groupWhile (\x y -> .group (Tuple.first x) == .group (Tuple.first y))

        isSelected : TeamDatum -> ( TeamDatum, IsSelected )
        isSelected t =
            case TS.getTeam topscorer of
                Nothing ->
                    ( t, NotSelected )

                Just team ->
                    if .team t == team then
                        ( t, Selected )

                    else
                        ( t, NotSelected )

        forGroup teams =
            Element.row (UI.Style.none [ spacing 7, padding 10, width (px 600) ]) (List.map (mkTeamButton (SelectTeam answerId)) teams)

        headertext =
            UI.Text.displayHeader "Wie wordt de topscorer?"
    in
    Element.column (UI.Style.none [])
        ([ headertext
         , introduction
         , viewPlayers bet answerId topscorer teamData
         ]
            ++ List.map forGroup groups
        )


introduction : Element.Element Msg
introduction =
    Element.paragraph (UI.Style.introduction [ width (px 600) ])
        [ UI.Text.simpleText """
    Voorspel de topscorer. Kies eerst het land, dan de speler. 9 punten als je het goed hebt.
    Let op: dit zijn de voorlopige selecties.
  """
        , UI.Text.boldText "Spelers kunnen nog afvallen, of al afgevallen zijn!"
        ]


viewPlayers : a -> AnswerID -> Topscorer -> b -> Element.Element Msg
viewPlayers _ answerId topscorer _ =
    let
        isSelectedTeam teamDatum =
            case TS.getTeam topscorer of
                Just t ->
                    t == .team teamDatum

                Nothing ->
                    False

        selectedTeam =
            List.filter isSelectedTeam initTeamData
                |> List.head

        isSelected player =
            case TS.getPlayer topscorer of
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
            Element.none

        Just teamWP ->
            Element.wrappedRow (UI.Style.none [ width (px 600), padding 10, spacing 7 ])
                (List.map (mkPlayerButton (SelectPlayer answerId)) (players teamWP))


mkTeamButton : (Team -> Msg) -> ( { b | team : Team }, IsSelected ) -> Element.Element Msg
mkTeamButton act ( teamDatum, isSelected ) =
    let
        c =
            case isSelected of
                Selected ->
                    UI.Style.Selected

                _ ->
                    UI.Style.Potential

        team =
            .team teamDatum

        msg =
            act team
    in
    UI.Button.teamButton c msg team


mkPlayerButton : (String -> Msg) -> ( String, IsSelected ) -> Element.Element Msg
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
    in
    UI.Button.pill c msg player
