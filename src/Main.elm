module Main exposing (main)

-- import Html exposing (Html)

import Activities
import Browser
import Browser.Events as Events
import Browser.Navigation as Navigation
import Element exposing (px)
import Form.Screen as Screen
import Form.View
import RemoteData exposing (RemoteData(..))
import Task
import Time
import Types exposing (App(..), Flags, InputState(..), Model, Msg(..))
import UI.Button
import UI.Style
import Update exposing (update)
import Url



-- app stuff


init : Flags -> Url.Url -> Navigation.Key -> ( Model Msg, Cmd Msg )
init flags url navKey =
    let
        ( app, msg ) =
            Maybe.map getApp url.fragment
                |> Maybe.withDefault ( Home, RefreshActivities )

        cmd =
            Task.succeed msg
                |> Task.perform identity

        model =
            Types.init flags.formId (Screen.size flags.width flags.height) navKey
    in
    ( { model | app = app }
    , Cmd.batch [ cmd, Task.perform FoundTimeZone Time.here ]
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


view : Model Msg -> Browser.Document Msg
view model =
    let
        contents =
            let
                contents_ =
                    case model.app of
                        Home ->
                            Activities.view model

                        Form ->
                            Form.View.view model

                        Blog ->
                            Activities.view model
            in
            Element.el [ Element.padding 24 ] contents_

        title =
            "Voetbalpool - Corona Editie"

        link app =
            let
                semantics =
                    if app == model.app then
                        UI.Style.Active

                    else
                        UI.Style.Potential

                ( linkUrl, linkText ) =
                    case app of
                        Home ->
                            ( "#home", "/home" )

                        Form ->
                            ( "#formulier", "/formulier" )

                        Blog ->
                            ( "#blog", "/blog" )
            in
            UI.Button.navlink semantics linkUrl linkText

        links =
            Element.row [ Element.padding 12, Element.spacing 12 ]
                [ link Home
                , link Form
                , link Blog
                ]

        page =
            Element.column
                [ Element.padding 24, Element.spacing 24 ]
                [ links
                , contents
                ]

        body =
            Element.layout (UI.Style.body []) page
    in
    { title = title, body = [ body ] }


getApp : String -> ( App, Msg )
getApp hash =
    let
        locs =
            String.split "/" hash

        -- emptyFunc =
        --     \_ -> Cmd.none
    in
    case locs of
        "#home" :: _ ->
            ( Home, RefreshActivities )

        "#blog" :: _ ->
            ( Blog, RefreshActivities )

        "#formulier" :: _ ->
            ( Form, NoOp )

        -- "#inzendingen" :: uuid :: _ ->
        --     if (Uuid.isValidUuid uuid) then
        --         ( Bets uuid, BetSelected )
        --     else
        --         ( Ranking, None )
        -- "#inzendingen" :: _ ->
        --     ( Ranking, None )
        -- "#stand" :: uuid :: _ ->
        --     if (Uuid.isValidUuid uuid) then
        --         ( RankingDetailsView, RetrieveRankingDetails uuid )
        --     else
        --         ( Ranking, None )
        -- "#stand" :: _ ->
        --     ( Ranking, RefreshRanking )
        -- "#wedstrijden" :: "wedstrijd" :: _ ->
        --     ( EditMatchResult, None )
        -- "#wedstrijden" :: [] ->
        --     ( Results, RefreshResults )
        -- "#knockouts" :: [] ->
        --     ( KOResults, RefreshKnockoutsResults )
        -- "#topscorer" :: [] ->
        --     ( TSResults, RefreshTopscorerResults )
        -- "#login" :: _ ->
        --     ( Login, None )
        _ ->
            let
                page =
                    Debug.log "page" locs
            in
            ( Home, RefreshActivities )
