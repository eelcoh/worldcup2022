module Form.Participant exposing
    ( isComplete
    , update
    , view
    )

import Bets.Bet exposing (setParticipant)
import Bets.Types exposing (Bet, StringField(..))
import Bets.Types.Participant
import Bets.Types.StringField as StringField
import Element exposing (fill, height, paddingXY, px, spacing, width)
import Element.Input
import Email
import Form.Participant.Types exposing (Attr(..), Msg(..))
import UI.Page exposing (page)
import UI.Screen as Screen
import UI.Style
import UI.Text


update : Msg -> Bet -> ( Bet, Cmd Msg )
update msg bet =
    let
        toStringField s =
            if s == "" then
                Error s

            else
                Changed s

        newParticipant attr participant =
            case attr of
                Name n ->
                    { participant | name = toStringField n }

                Postal a ->
                    { participant | address = toStringField a }

                Residence e ->
                    { participant | residence = toStringField e }

                Email e ->
                    { participant | email = toStringField e }

                Phone p ->
                    { participant | phone = toStringField p }

                Knows h ->
                    { participant | howyouknowus = toStringField h }

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

        stringFieldValue sf =
            case sf of
                Initial s ->
                    s

                Changed s ->
                    s

                Error s ->
                    s

        values p =
            List.map stringFieldValue [ p.name, p.address, p.residence, p.email, p.phone, p.howyouknowus ]

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
            Element.paragraph (UI.Style.introduction [ width fill, spacing 7 ])
                [ UI.Text.simpleText """Graag volledig invullen, zodat wij je goed kunnen bereiken als je gewonnen hebt."""
                ]
    in
    page "participant"
        (header :: introduction :: lines)


isComplete : Bet -> Bool
isComplete bet =
    Bets.Types.Participant.isComplete bet.participant
