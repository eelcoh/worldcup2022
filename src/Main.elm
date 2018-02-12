module Main exposing (..)

import Html exposing (Html)
import Form.View exposing (view)
import Form.Update exposing (update)
import Form.Init exposing (init)
import Form.Types exposing (Model, Msg, Flags)


main : Program Flags (Model Msg) Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
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
