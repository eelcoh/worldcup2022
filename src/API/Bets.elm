module API.Bets exposing (createBet, fetchBet, placeBet, updateBet)

-- import Http exposing (expectJson)

import Bets.Bet
import Bets.Types exposing (Bet)
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Http
import Types exposing (Msg(..), UUID)


placeBet : Bet -> Cmd Msg
placeBet bet =
    -- let
    --     ( vrb, url ) =
    case bet.uuid of
        Just uuid ->
            updateBet bet uuid

        Nothing ->
            createBet bet



--     body =
--         Bets.Bet.encode bet
--             |> Http.jsonBody
--     req =
--         Http.request
--             { method = vrb
--             , headers = []
--             , url = url
--             , body = body
--             , expect = expectJson Bets.Bet.decode
--             , timeout = Nothing
--             , withCredentials = False
--             }
-- in
-- Http.post msg req


fetchBet : UUID -> Cmd Msg
fetchBet uuid =
    Http.get ("/bets/" ++ uuid) FetchedBet Bets.Bet.decode


createBet : Bet -> Cmd Msg
createBet bet =
    Http.post "/bets/" SubmittedBet Bets.Bet.decode (Bets.Bet.encode bet)


updateBet : Bet -> String -> Cmd Msg
updateBet bet uuid =
    Http.put ("/bets/" ++ uuid) SubmittedBet Bets.Bet.decode (Bets.Bet.encode bet)



-- retrieveBet : String -> (WebData Bet -> msg) -> Cmd msg
-- retrieveBet uuid handlerMsg =
--     let
--         url =
--             "/bets/" ++ uuid
--     in
--         Http.get url RetrievedBet Bets.Bet.decode
