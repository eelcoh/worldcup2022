module Main exposing (main)

-- import Html exposing (Html)

import Browser
import Browser.Events as Events
import Browser.Navigation as Navigation
import Form.Screen as Screen
import Form.View exposing (view)
import RemoteData exposing (RemoteData(..))
import Task
import Time
import Types exposing (Flags, InputState(..), Model, Msg(..))
import Update exposing (update)
import Url



-- app stuff


init : Flags -> Url.Url -> Navigation.Key -> ( Model Msg, Cmd Msg )
init flags _ navKey =
    let
        model =
            Types.init flags.formId (Screen.size flags.width flags.height) navKey
    in
    ( model
    , Task.perform FoundTimeZone Time.here
    )


subscriptions : Model Msg -> Sub Msg
subscriptions _ =
    Sub.batch [ Events.onResize ScreenResize ]



-- main : Program Flags Model Msg


main : Program Flags (Model Msg) Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
