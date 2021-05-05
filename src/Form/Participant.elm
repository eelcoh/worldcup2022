module Form.Participant exposing
    ( isComplete
    , update
    , view
    )

import Bets.Bet exposing (setParticipant)
import Bets.Types exposing (Bet)
import Bets.Types.Participant
import Element exposing (height, px, spacing, width)
import Element.Input
import Form.Participant.Types exposing (Attr(..), Msg(..))
import UI.Style
import UI.Text


update : Msg -> Bet -> ( Bet, Cmd Msg )
update msg bet =
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

        newBet attr participant =
            newParticipant attr participant
                |> setParticipant bet
    in
    case msg of
        Set attr ->
            let
                newNewBet =
                    newBet attr bet.participant
            in
            ( newNewBet, Cmd.none )


view : Bet -> Element.Element Msg
view bet =
    let
        keys =
            [ Name, Postal, Residence, Email, Phone, Knows ]

        values p =
            List.map (Maybe.withDefault "") [ p.name, p.address, p.residence, p.email, p.phone, p.howyouknowus ]

        placeholder p =
            Element.Input.placeholder [] (Element.text p)

        inputField ( k, v ) =
            let
                inp =
                    { onChange = \val -> Set (k val)
                    , text = Tuple.second v
                    , label = UI.Text.labelText (Tuple.first v)
                    , placeholder = Just (placeholder (Tuple.first v))
                    }
            in
            Element.Input.text (UI.Style.textInput [ width (px 260), height (px 36) ]) inp

        lines =
            values bet.participant
                |> List.map2 (\a b -> ( a, b )) [ "Naam", "Adres", "Woonplaats", "Email", "Telefoonnummer", "Waar ken je ons van?" ]
                |> List.map2 (\a b -> ( a, b )) keys
                |> List.map inputField

        header =
            UI.Text.displayHeader "Wie ben jij"

        introduction =
            Element.paragraph (UI.Style.introduction [ width (px 600), spacing 7 ])
                [ UI.Text.simpleText """Graag volledig invullen, zodat wij je goed kunnen bereiken als je gewonnen hebt."""
                ]
    in
    Element.column (UI.Style.none [ spacing 12 ]) (header :: introduction :: lines)


isComplete : Bet -> Bool
isComplete bet =
    Bets.Types.Participant.isComplete bet.participant
