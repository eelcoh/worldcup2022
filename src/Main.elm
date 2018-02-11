module Main exposing (..)

import Html exposing (Html)
import Form.View exposing (view)
import Form.Update exposing (update)
import Form.Init exposing (init)
import Form.Types exposing (Model, Msg)


main : Program Never (Model Msg) Msg
main =
    Html.program
        { init = init Nothing
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
