module Main exposing (main)

-- import Html exposing (Html)

import Bets.Init
import Browser
import Browser.Events as Events
import Browser.Navigation as Navigation
import Form.Init exposing (cards)
import Form.Types exposing (Flags, InputState(..), Model, Msg(..))
import Form.Update exposing (update)
import Form.View exposing (view)
import Html exposing (div)
import RemoteData exposing (RemoteData(..))
import Url



-- app stuff


init : Flags -> Url.Url -> Navigation.Key -> ( Model Msg, Cmd Msg )
init flags _ navKey =
    ( { cards = cards
      , bet = Bets.Init.bet
      , savedBet = NotAsked
      , idx = 0
      , card = div [] []
      , formId = flags.formId
      , betState = Clean
      , navKey = navKey
      }
    , Cmd.none
    )


subscriptions : Model Msg -> Sub Msg
subscriptions _ =
    Sub.batch [ Events.onResize ScreenResize ]



-- main : Program Flags Model Msg


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
