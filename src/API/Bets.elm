module API.Bets exposing (placeBet)

import Bets.Bet
import Bets.Types exposing (Bet)
import Http exposing (expectJson)


placeBet : Bet -> (Result Http.Error Bet -> msg) -> Cmd msg
placeBet bet msg =
    let
        ( vrb, url ) =
            case bet.uuid of
                Just uuid ->
                    ( "PUT", "/app/bets/" ++ uuid )

                Nothing ->
                    ( "POST", "/app/bets/" )

        body =
            Bets.Bet.encode bet
                |> Http.jsonBody

        req =
            Http.request
                { method = vrb
                , headers = []
                , url = url
                , body = body
                , expect = expectJson Bets.Bet.decode
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send msg req
