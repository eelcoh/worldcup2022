module Form.Questions.MatchWinner exposing (Msg, update, view)

import Bets.Types exposing (Bet, Answer, AnswerT(..), AnswerID, Team)
import Bets.Bet exposing (setMatchWinner)
import Bets.Types.Match as M
import UI.Team exposing (viewTeam)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Form.Questions.Types exposing (QState)


type Msg
    = SetWinner Answer Team


type IsSelected
    = Selected
    | NotSelected


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    case msg of
        SetWinner answer team ->
            let
                newBet =
                    setMatchWinner bet answer team
            in
                ( newBet, { qState | next = Nothing }, Cmd.none )


view : Bet -> QState -> Html Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId

        isSelected mTeam mWinner =
            case ( mTeam, mWinner ) of
                ( Just t1, Just winner ) ->
                    if t1 == winner then
                        ( mTeam, Selected )
                    else
                        ( mTeam, NotSelected )

                _ ->
                    ( mTeam, NotSelected )

        v answer match mWinner =
            div []
                [ mkButton (SetWinner answer) (isSelected (M.homeTeam match) mWinner)
                , span [] [ text " - " ]
                , mkButton (SetWinner answer) (isSelected (M.awayTeam match) mWinner)
                ]
    in
        case mAnswer of
            Just (( _, AnswerMatchWinner r match mNextId mWinner points ) as answer) ->
                v answer match mWinner

            _ ->
                p [] []


mkButton : (Team -> Msg) -> ( Maybe Team, IsSelected ) -> Html Msg
mkButton act ( mTeam, isSelected ) =
    case ( mTeam, isSelected ) of
        ( Nothing, _ ) ->
            bttn NotSelected [] [ text ("---") ]

        ( Just team, Selected ) ->
            bttn Selected [ onClick (act team) ] [ viewTeam (Just team) ]

        ( Just team, NotSelected ) ->
            bttn NotSelected [ onClick (act team) ] [ viewTeam (Just team) ]


bttn : IsSelected -> List (Attribute msg) -> List (Html msg) -> Html msg
bttn isSelected attrs el =
    let
        s =
            case isSelected of
                Selected ->
                    style [ ( "backgroundColor", "white" ) ]

                NotSelected ->
                    style [ ( "backgroundColor", "darkgrey" ) ]
    in
        button (s :: attrs) el
