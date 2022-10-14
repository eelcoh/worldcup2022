module Form.Topscorer exposing (isComplete, update, view)

import Bets.Bet exposing (getTopscorer, setTopscorer)
import Bets.Init.WorldCup2022.Tournament exposing (initTeamData)
import Bets.Types exposing (Answer(..), Bet, Team, TeamDatum, Topscorer)
import Bets.Types.Answer.Topscorer
import Bets.Types.Topscorer as TS
import Element exposing (centerX, fill, padding, paddingXY, spacing, width)
import Form.Topscorer.Types exposing (IsSelected(..), Msg(..))
import List.Extra
import UI.Button
import UI.Page exposing (page)
import UI.Style
import UI.Text


update : Msg -> Bet -> ( Bet, Cmd Msg )
update msg bet =
    case msg of
        SelectTeam team ->
            let
                newBet (Answer topscorer _) =
                    let
                        newTopscorer =
                            TS.setTeam topscorer team
                    in
                    setTopscorer bet newTopscorer
            in
            ( newBet bet.answers.topscorer, Cmd.none )

        SelectPlayer player ->
            let
                newBet (Answer topscorer _) =
                    let
                        newTopscorer =
                            TS.setPlayer topscorer player
                    in
                    setTopscorer bet newTopscorer
            in
            ( newBet bet.answers.topscorer, Cmd.none )


view : Bet -> Element.Element Msg
view bet =
    viewTopscorer (getTopscorer bet)


viewTopscorer : Topscorer -> Element.Element Msg
viewTopscorer topscorer =
    let
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
            Element.row (UI.Style.none [ spacing 20, padding 10, Element.centerX ]) (List.map (mkTeamButton SelectTeam) teams)

        headertext =
            UI.Text.displayHeader "Wie wordt de topscorer?"
    in
    page "topscorer"
        ([ headertext
         , introduction
         , warning
         , viewPlayers topscorer
         ]
            ++ List.map forGroup groups
        )


introduction : Element.Element Msg
introduction =
    Element.paragraph (UI.Style.introduction [])
        [ UI.Text.simpleText """
    Voorspel de topscorer. Kies eerst het land, dan de speler. 9 punten als je het goed hebt.
    Let op: dit zijn de voorlopige selecties.
  """ ]


warning : Element.Element Msg
warning =
    Element.paragraph (UI.Style.introduction [])
        [ UI.Text.boldText "Spelers kunnen nog afvallen, of al afgevallen zijn!"
        ]


viewPlayers : Topscorer -> Element.Element Msg
viewPlayers topscorer =
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
            Element.wrappedRow (UI.Style.none [ Element.alignLeft, padding 10, spacing 16 ])
                (List.map (mkPlayerButton SelectPlayer) (players teamWP))


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
                    UI.Style.Focus

                _ ->
                    UI.Style.Potential

        msg =
            act player
    in
    UI.Button.pill c msg player


isComplete : Bet -> Bool
isComplete bet =
    Bets.Types.Answer.Topscorer.isComplete bet.answers.topscorer
