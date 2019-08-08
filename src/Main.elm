module Main exposing (main)

-- import Html exposing (Html)

import Browser
import Browser.Navigation as Navigation
import Form.Init exposing (init)
import Form.Types exposing (Flags, Model, Msg(..))
import Form.Update exposing (update)
import Form.View exposing (view)
import Url



-- app stuff


init : Flags -> Url.Url -> Navigation.Key -> ( Model Msg, Cmd Msg )
init flags url navKey =
    let
        ( page, msg ) =
            getPage loc.fragment

        screenSize =
            classifyDevice { width = flags.width, height = 0 }

        model =
            { newModel | navKey = key }

        -- cmd =
        --     fetchActivities model
    in
    update msg model


subscriptions : Model Msg -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes SetScreenSize ]


type alias Flags =
    { width : Int
    }



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



-- http
{-
      port loader : Task Http.Error (List Score)
      port loader =
        getForms


   port formId : Maybe String


   port tasks : Signal (Task.Task Never ())
   port tasks =
       app.tasks
-}
--port toElm : (Value -> msg) -> Sub msg
