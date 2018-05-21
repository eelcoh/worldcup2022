module Form.Questions.Participant
    exposing
        ( Msg
        , update
        , view
        )

import Bets.Bet exposing (setParticipant)
import Bets.Types exposing (Answer, AnswerID, AnswerT(..), Bet, Participant)
import UI.Text
import Element
import UI.Style
import Element.Attributes exposing (width, height, px, spacing, padding)
import Element.Input
import Form.Questions.Types exposing (QState)


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


view : Bet -> QState -> Element.Element UI.Style.Style variation Msg
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

        placeholder p =
            Element.Input.placeholder { text = p, label = Element.Input.hiddenLabel p }

        inputField ( k, v ) =
            let
                inp =
                    { onChange = (\val -> (Set (k val) qState.answerId))
                    , value = (Tuple.second v)
                    , label = placeholder (Tuple.first v)
                    , options = []
                    }
            in
                Element.Input.text UI.Style.TextInput [ width (px 260), height (px 36) ] inp

        lines =
            List.map2 (,) [ "Naam", "Adres", "Woonplaats", "Email", "Telefoonnummer", "Waar ken je ons van?" ] values
                |> List.map2 (,) keys
                |> List.map inputField

        header =
            UI.Text.displayHeader "Wie ben jij"

        introduction =
            Element.paragraph UI.Style.Introduction
                [ width (px 600), spacing 7 ]
                [ UI.Text.simpleText """Graag volledig invullen, zodat wij je goed kunnen bereiken als je gewonnen hebt."""
                ]
    in
        Element.column UI.Style.None [ spacing 7 ] (header :: introduction :: lines)
