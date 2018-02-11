module Form.Questions.Participant
    exposing
        ( Msg
        , update
        , view
        )

import Bets.Bet exposing (setParticipant)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Participant)
import Form.Questions.Types exposing (QState)
import Html exposing (..)
import Html.Attributes exposing (id, placeholder, value)
import Html.Events exposing (on, onClick, onInput, targetValue)


type Attr
    = Name String
    | Postal String
    | Residence String
    | Email String
    | Phone String
    | Knows String


type Msg
    = Set Attr AnswerID
    | NoOp


update : Msg -> Bet -> QState -> ( Bet, QState, Cmd Msg )
update msg bet qState =
    let
        newParticipant attr participant =
            case attr of
                Name n ->
                    { participant | name = Just n }

                Postal a ->
                    { participant | address = Just a }

                Residence e ->
                    { participant | residence = Just e }

                Email e ->
                    { participant | email = Just e }

                Phone p ->
                    { participant | phone = Just p }

                Knows h ->
                    { participant | howyouknowus = Just h }

        newBet answer attr participant =
            newParticipant attr participant
                |> setParticipant bet answer
    in
        case msg of
            Set attr aid ->
                let
                    mAnswer =
                        Bets.Bet.getAnswer bet aid

                    newNewBet =
                        case mAnswer of
                            Just (( answerId, AnswerParticipant participant ) as answer) ->
                                newBet answer attr participant

                            _ ->
                                bet
                in
                    ( newNewBet, { qState | next = Nothing }, Cmd.none )

            _ ->
                ( bet, { qState | next = Nothing }, Cmd.none )


view : Bet -> QState -> Html Msg
view bet qState =
    let
        mAnswer =
            Bets.Bet.getAnswer bet qState.answerId

        keys =
            [ Name, Postal, Residence, Email, Phone, Knows ]

        values =
            case mAnswer of
                Just ( _, AnswerParticipant p ) ->
                    List.map (Maybe.withDefault "") [ (p.name), p.address, p.residence, p.email, p.phone, p.howyouknowus ]

                _ ->
                    [ "", "", "", "", "", "" ]

        inputLine ( k, v ) =
            div []
                [ input
                    [ value (Tuple.first v)
                    , placeholder (Tuple.second v)
                    , onInput (\val -> (Set (k val) qState.answerId))
                    ]
                    []
                ]

        lines =
            List.map2 (,) values [ "Naam", "Adres", "Woonplaats", "Email", "Telefoonnummer", "Waar ken je ons van?" ]
                |> List.map2 (,) keys
                |> List.map inputLine

        header =
            h1 [] [ text ("Wie ben jij") ]

        introtext =
            p [] [ text "Graag volledig invullen, zodat wij je goed kunnen bereiken als je gewonnen hebt." ]
    in
        div [] (header :: introtext :: lines)
