module Form.Questions.GroupPosition exposing (Msg, update, view)

import Bets.Bet exposing (setTeam, candidates)
import Bets.Types exposing (Answer, AnswerT(..), AnswerID, Team, Group, Pos(..), Bet)
import Bets.Types.Team as T
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Form.Questions.Types exposing (QState)


type Msg
    = SetTeam Answer Group Team


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        SetTeam answer team grp ->
            let
                newBet =
                    setTeam bet answer team grp
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )


view : Bet -> QState -> Html Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId

        act answer =
            SetTeam answer

        cs answer =
            candidates bet answer

        txt grp pos =
            case pos of
                First ->
                    text ("Which team will finish first group " ++ (toString grp))

                Second ->
                    text ("Which team will finish second in group " ++ (toString grp))

                Third ->
                    text ("Which team will finish third in group " ++ (toString grp))

                _ ->
                    text ("WHOOPS")
    in
        case mAnswer of
            Just (( answerId, AnswerGroupPosition g pos ( drawID, mTeam ) _ ) as answer) ->
                div []
                    [ h2 [] [ (txt g pos) ]
                    , div [] (List.map (mkButton (act answer) g pos) (cs answer))
                    ]

            _ ->
                div [] [ text "WHOOPS" ]


mkButton : (Group -> Team -> Msg) -> Group -> Pos -> ( Group, Team, Pos ) -> Html Msg
mkButton act grp forPos ( _, team, pos ) =
    let
        event =
            onClick (act grp team)

        stle =
            bStyle pos

        attrs =
            [ event, stle ]

        -- @todo:
        -- mark selected and unselectable if selected (forPos == candidates pos)
        -- mark selected elsewhere if candidates pos != Free
        -- mark free to select otherwise
    in
        button attrs [ text (T.display team) ]


bStyle : Pos -> Attribute msg
bStyle pos =
    case pos of
        First ->
            style [ ( "backgroundColor", "red" ) ]

        Second ->
            style [ ( "backgroundColor", "blue" ) ]

        Third ->
            style [ ( "backgroundColor", "yellow" ) ]

        _ ->
            style [ ( "backgroundColor", "green" ) ]
