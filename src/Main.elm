module Main exposing (main)

-- import Html exposing (Html)

import API.Bets
import Activities
import Authentication
import Bets.Init
import Browser
import Browser.Events as Events
import Browser.Navigation as Navigation
import Element
import Form.Bracket as Bracket
import Form.Card as Cards
import Form.GroupMatches as GroupMatches
import Form.Info
import Form.Participant as Participant
import Form.Topscorer as Topscorer
import Form.View
import RemoteData exposing (RemoteData(..))
import Results.Knockouts as Knockouts
import Results.Matches as Matches
import Results.Ranking as Ranking
import Results.Topscorers
import Task
import Time
import Types exposing (App(..), Card(..), Credentials(..), DataStatus(..), Flags, InputState(..), Model, Msg(..), Token(..))
import Types.DataStatus as DataStatus
import UI.Button
import UI.Screen as Screen
import UI.Style
import Url
import Uuid.Barebones as Uuid



-- app stuff


init : Flags -> Url.Url -> Navigation.Key -> ( Model Msg, Cmd Msg )
init flags url navKey =
    let
        ( app, cmd ) =
            getApp url

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

                        Login ->
                            Authentication.view model

                        Ranking ->
                            Ranking.view model

                        RankingDetailsView ->
                            Ranking.viewDetails model

                        Results ->
                            Matches.view model

                        EditMatchResult ->
                            Matches.edit model
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

                        Ranking ->
                            ( "#stand", "/stand" )

                        Results ->
                            ( "#wedstrijden", "/wedstrijden" )

                        _ ->
                            ( "#blog", "/blog" )
            in
            UI.Button.navlink semantics linkUrl linkText

        links =
            Element.row [ Element.padding 12, Element.spacing 12 ]
                [ link Home
                , link Form
                , link Blog
                , link Results
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


getApp : Url.Url -> ( App, Cmd Msg )
getApp url =
    let
        -- emptyFunc =
        --     \_ -> Cmd.none
        ( app, msg ) =
            Maybe.map inspect url.fragment
                |> Maybe.withDefault ( Home, RefreshActivities )

        inspect hash =
            case String.split "/" hash of
                "home" :: _ ->
                    ( Home, RefreshActivities )

                "blog" :: _ ->
                    ( Blog, RefreshActivities )

                "formulier" :: _ ->
                    ( Form, NoOp )

                -- "#inzendingen" :: uuid :: _ ->
                --     if (Uuid.isValidUuid uuid) then
                --         ( Bets uuid, BetSelected )
                --     else
                --         ( Ranking, None )
                -- "#inzendingen" :: _ ->
                --     ( Ranking, None )
                "stand" :: uuid :: _ ->
                    if Uuid.isValidUuid uuid then
                        ( RankingDetailsView, RetrieveRankingDetails uuid )

                    else
                        ( Ranking, NoOp )

                "stand" :: _ ->
                    ( Ranking, RefreshRanking )

                "wedstrijden" :: "wedstrijd" :: _ ->
                    ( EditMatchResult, NoOp )

                "wedstrijden" :: [] ->
                    ( Results, RefreshResults )

                -- "#knockouts" :: [] ->
                --     ( KOResults, RefreshKnockoutsResults )
                -- "#topscorer" :: [] ->
                --     ( TSResults, RefreshTopscorerResults )
                "login" :: _ ->
                    ( Login, NoOp )

                _ ->
                    let
                        page =
                            Debug.log "page ::" hash
                    in
                    ( Home, RefreshActivities )

        cmd =
            Task.succeed msg
                |> Task.perform identity
    in
    ( app, cmd )



--


update : Msg -> Model Msg -> ( Model Msg, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            ( { model | idx = page }, Cmd.none )

        SetApp app ->
            ( { model | app = app }, Cmd.none )

        FoundTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        BracketMsg act ->
            let
                mCard =
                    Cards.getBracketCard model.cards
            in
            case mCard of
                Just (BracketCard bracketState) ->
                    Bracket.update act model.bet bracketState
                        |> (\( newBet, newState, fx ) ->
                                ( { model
                                    | bet = newBet
                                    , betState = Dirty
                                    , cards = Cards.updateBracketCard model.cards newState
                                  }
                                , Cmd.map BracketMsg fx
                                )
                           )

                _ ->
                    ( model, Cmd.none )

        GroupMatchMsg grp act ->
            let
                mCard =
                    Cards.getGroupMatchesCard model.cards grp
            in
            case mCard of
                Just (GroupMatchesCard groupMatchesState) ->
                    GroupMatches.update act groupMatchesState model.bet
                        |> (\( newBet, newState, fx ) ->
                                ( { model
                                    | bet = newBet
                                    , betState = Dirty
                                    , cards = Cards.updateGroupMatchesCard model.cards newState
                                  }
                                , Cmd.map (GroupMatchMsg groupMatchesState.group) fx
                                )
                           )

                _ ->
                    ( model, Cmd.none )

        TopscorerMsg act ->
            let
                ( newBet, fx ) =
                    Topscorer.update act model.bet
            in
            ( { model | bet = newBet, betState = Dirty }, Cmd.map TopscorerMsg fx )

        ParticipantMsg act ->
            let
                ( newBet, fx ) =
                    Participant.update act model.bet
            in
            ( { model | bet = newBet, betState = Dirty }, Cmd.map ParticipantMsg fx )

        InfoMsg act ->
            let
                ( newBet, fx ) =
                    Form.Info.update act model.bet
            in
            ( { model | bet = newBet }, Cmd.map InfoMsg fx )

        SubmitMsg ->
            let
                cmd =
                    API.Bets.placeBet model.bet
            in
            ( model, cmd )

        SubmittedBet savedBet ->
            let
                ( newBet, nwInputState ) =
                    case savedBet of
                        Success b ->
                            ( b, Clean )

                        _ ->
                            ( model.bet, Dirty )
            in
            ( { model | savedBet = savedBet, bet = newBet, betState = nwInputState }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Restart ->
            let
                nwState =
                    { model
                        | bet = Bets.Init.bet
                        , savedBet = NotAsked
                        , betState = Clean
                    }
            in
            ( nwState, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just f ->
                            let
                                app =
                                    case f of
                                        "home" ->
                                            Home

                                        "formulier" ->
                                            Form

                                        _ ->
                                            Home
                            in
                            ( { model | app = app }
                            , Navigation.pushUrl model.navKey (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChange url ->
            let
                ( app, cmd ) =
                    getApp url
            in
            ( { model | app = app }, cmd )

        ScreenResize w h ->
            let
                sz =
                    Screen.size w h

                newCards =
                    Cards.updateScreenCards sz model.cards
            in
            ( { model | screen = sz, cards = newCards }, Cmd.none )

        -- activities
        FetchedBet bet ->
            ( model, Cmd.none )

        SetCommentMsg newMessage ->
            let
                oldComment =
                    model.activities.comment

                newComment =
                    { oldComment | msg = newMessage }

                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | comment = newComment }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SetCommentAuthor newAuthor ->
            let
                oldComment =
                    model.activities.comment

                newComment =
                    { oldComment | author = newAuthor }

                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | comment = newComment }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SaveComment ->
            let
                cmd =
                    Activities.saveComment model.activities
            in
            ( model, cmd )

        SavedComment res ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | activities = res, comment = Types.initComment, showComment = False }
            in
            ( { model | activities = newActivities }, Cmd.none )

        ShowCommentInput ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | showComment = True }
            in
            ( { model | activities = newActivities }, Cmd.none )

        HideCommentInput ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | showComment = False }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SetPostTitle newTitle ->
            let
                oldPost =
                    model.activities.post

                newPost =
                    { oldPost | title = newTitle }

                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | post = newPost }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SetPostPassphrase newPassphrase ->
            let
                oldPost =
                    model.activities.post

                newPost =
                    { oldPost | passphrase = newPassphrase }

                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | post = newPost }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SetPostMsg newMsg ->
            let
                oldPost =
                    model.activities.post

                newPost =
                    { oldPost | msg = newMsg }

                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | post = newPost }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SetPostAuthor newAuthor ->
            let
                oldPost =
                    model.activities.post

                newPost =
                    { oldPost | author = newAuthor }

                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | post = newPost }
            in
            ( { model | activities = newActivities }, Cmd.none )

        SavePost ->
            let
                cmd =
                    case model.token of
                        RemoteData.Success (Token token) ->
                            Activities.savePost model.activities token

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        SavedPost res ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | activities = res, post = Types.initPost, showPost = False }
            in
            ( { model | activities = newActivities }, Cmd.none )

        HidePostInput ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | showPost = False }
            in
            ( { model | activities = newActivities }, Cmd.none )

        ShowPostInput ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | showPost = False }
            in
            ( { model | activities = newActivities }, Cmd.none )

        FetchedActivities res ->
            let
                oldActivities =
                    model.activities

                newActivities =
                    { oldActivities | activities = res }
            in
            ( { model | activities = newActivities }, Cmd.none )

        RefreshActivities ->
            ( model, Activities.fetchActivities model.activities )

        SetUsername uid ->
            let
                newCredentials =
                    case model.credentials of
                        Empty ->
                            WithUsername uid

                        WithPassword pw ->
                            Submittable uid pw

                        WithUsername _ ->
                            WithUsername uid

                        Submittable _ pw ->
                            Submittable uid pw
            in
            ( { model | credentials = newCredentials }, Cmd.none )

        SetPassword pw ->
            let
                newCredentials =
                    case model.credentials of
                        Empty ->
                            WithPassword pw

                        WithPassword _ ->
                            WithPassword pw

                        WithUsername uid ->
                            Submittable uid pw

                        Submittable uid _ ->
                            Submittable uid pw
            in
            ( { model | credentials = newCredentials }, Cmd.none )

        FetchedToken token ->
            let
                newCredentials =
                    case token of
                        Success _ ->
                            Empty

                        _ ->
                            model.credentials
            in
            ( { model | token = token, credentials = newCredentials }, Cmd.none )

        Authenticate ->
            case model.credentials of
                Submittable uid pw ->
                    ( model, Authentication.authenticate uid pw )

                _ ->
                    ( model, Cmd.none )

        RecreateRanking ->
            let
                cmd =
                    case model.token of
                        RemoteData.Success token ->
                            Ranking.recreate token

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        FetchedRanking ranking ->
            ( { model | ranking = ranking }, Cmd.none )

        --
        ViewRankingDetails uuid ->
            let
                url =
                    "#stand/" ++ uuid

                cmd =
                    Navigation.load url
            in
            ( model, cmd )

        RetrieveRankingDetails uuid ->
            let
                cmd =
                    Ranking.fetchRankingDetails uuid
            in
            ( model, cmd )

        FetchedRankingDetails results ->
            ( { model | rankingDetails = results }, Cmd.none )

        RefreshRanking ->
            case model.ranking of
                Success _ ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Ranking.fetchRanking )

        FetchedMatchResults results ->
            let
                nwModel =
                    case results of
                        Failure e ->
                            let
                                d =
                                    Debug.log "oeps" e
                            in
                            { model | matchResults = results }

                        _ ->
                            { model | matchResults = results }
            in
            ( nwModel, Cmd.none )

        StoredMatchResult result ->
            ( { model | matchResult = result }, Matches.fetchMatchResults )

        RefreshResults ->
            case model.matchResults of
                Success _ ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Matches.fetchMatchResults )

        EditMatch match ->
            let
                url =
                    "#wedstrijden/wedstrijd/" ++ match.match

                cmd =
                    Navigation.load url
            in
            ( { model | matchResult = Success match, app = EditMatchResult }, cmd )

        UpdateMatchResult match ->
            case model.token of
                Success token ->
                    let
                        cmd =
                            Matches.updateMatchResults token match
                    in
                    ( model, cmd )

                _ ->
                    ( model, Cmd.none )

        CancelMatchResult match ->
            case model.token of
                Success token ->
                    let
                        canceledMatch =
                            { match | score = Nothing }

                        cmd =
                            Matches.updateMatchResults token canceledMatch
                    in
                    ( model, cmd )

                _ ->
                    ( model, Cmd.none )

        -- Knockouts
        FetchedKnockoutsResults results ->
            ( { model | knockoutsResults = Fresh results }, Cmd.none )

        StoredKnockoutsResults results ->
            ( { model | knockoutsResults = Fresh results }, Cmd.none )

        Qualify rnd q team ->
            let
                kos =
                    DataStatus.map (Knockouts.update rnd q team) model.knockoutsResults
            in
            ( { model | knockoutsResults = kos }, Cmd.none )

        UpdateKnockoutsResults ->
            let
                cmd =
                    case ( model.knockoutsResults, model.token ) of
                        ( Filthy (Success res), Success token ) ->
                            Knockouts.storeKnockoutsResults token res

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        InitialiseKnockoutsResults ->
            let
                cmd =
                    case model.token of
                        Success token ->
                            Knockouts.inititaliseKnockoutsResults token

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        RefreshKnockoutsResults ->
            let
                cmd =
                    Knockouts.fetchKnockoutsResults
            in
            ( model, cmd )

        ChangeQualify round qualified team ->
            let
                newKnockoutsResults =
                    case model.knockoutsResults of
                        Fresh knockouts ->
                            Knockouts.update round qualified team knockouts
                                |> Filthy

                        Filthy knockouts ->
                            Knockouts.update round qualified team knockouts
                                |> Filthy

                        Stale _ ->
                            model.knockoutsResults
            in
            ( { model | knockoutsResults = newKnockoutsResults }, Cmd.none )

        -- Topscorer
        RefreshTopscorerResults ->
            let
                cmd =
                    Results.Topscorers.fetchTopscorerResults
            in
            ( model, cmd )

        ChangeTopscorerResults hasQualified topscorer ->
            let
                newTopscorersResults =
                    case model.topscorerResults of
                        Fresh topscorerResults ->
                            Results.Topscorers.update hasQualified topscorer topscorerResults
                                |> Filthy

                        Filthy topscorerResults ->
                            Results.Topscorers.update hasQualified topscorer topscorerResults
                                |> Filthy

                        Stale _ ->
                            model.topscorerResults
            in
            ( { model | topscorerResults = newTopscorersResults }, Cmd.none )

        UpdateTopscorerResults ->
            let
                cmd =
                    case ( model.topscorerResults, model.token ) of
                        ( Filthy (Success res), Success token ) ->
                            Results.Topscorers.storeTopscorerResults token res

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        InitialiseTopscorerResults ->
            let
                cmd =
                    case model.token of
                        Success token ->
                            Results.Topscorers.inititaliseTopscorerResults token

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        FetchedTopscorerResults results ->
            ( { model | topscorerResults = Fresh results }, Cmd.none )

        StoredTopscorerResults results ->
            ( { model | topscorerResults = Fresh results }, Cmd.none )
