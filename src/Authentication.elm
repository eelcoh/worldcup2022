module Authentication exposing (..)

import Element exposing (fill, height, padding, px, spacing, width)
import Element.Events as Events
import Element.Input as Input
import Json.Decode exposing (Decoder, field)
import Json.Encode
import RemoteData exposing (RemoteData(..))
import RemoteData.Http as Web
import Types exposing (Access(..), Credentials(..), Model, Msg(..), Token(..))
import UI.Button
import UI.Style
import UI.Text


authenticate : String -> String -> Cmd Msg
authenticate uid pw =
    let
        credentials =
            encodeCredentials uid pw
    in
    Web.post "/authentication/authentications" FetchedToken decode credentials


view : Model Msg -> Element.Element Msg
view model =
    let
        username v =
            let
                area =
                    { onChange = SetUsername
                    , text = v
                    , placeholder = Nothing
                    , label = UI.Text.labelText "username"
                    , spellcheck = True
                    }
            in
            Input.multiline [ Events.onFocus ShowCommentInput, height (px 36) ] area

        password v =
            let
                area =
                    { onChange = SetPassword
                    , text = v
                    , placeholder = Nothing
                    , label = UI.Text.labelText "password"
                    }
            in
            Input.text [ height (px 36) ] area

        loginButton isSubmittable_ =
            if isSubmittable_ then
                UI.Button.pill UI.Style.Active Authenticate "login!"

            else
                UI.Button.pill UI.Style.Inactive NoOp "je moet beide velden invullen"

        ( inpUsername, inpPassword, isSubmittable ) =
            case model.credentials of
                Types.Empty ->
                    ( username "", password "", False )

                WithPassword pw ->
                    ( username "", password pw, False )

                WithUsername uid ->
                    ( username uid, password "", False )

                Submittable uid pw ->
                    ( username uid, password pw, True )
    in
    Element.column
        (UI.Style.darkBox [ padding 10, spacing 20, width fill ])
        [ inpUsername
        , inpPassword
        , loginButton isSubmittable
        ]


isAuthorised : Model Msg -> Access
isAuthorised model =
    case model.token of
        Success _ ->
            Authorised

        _ ->
            Unauthorised



-- Json


encodeCredentials : String -> String -> Json.Encode.Value
encodeCredentials uid pw =
    Json.Encode.object
        [ ( "username", Json.Encode.string uid )
        , ( "password", Json.Encode.string pw )
        ]


decode : Decoder Token
decode =
    field "token" Json.Decode.string
        |> Json.Decode.map
            Token
