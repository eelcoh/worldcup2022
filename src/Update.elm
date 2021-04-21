module Update exposing (update)

import API.Bets
import Activities
import Bets.Init
import Browser
import Browser.Navigation as Navigation
import Form.Bracket as Bracket
import Form.Card as Cards
import Form.GroupMatches as GroupMatches
import Form.Info
import Form.Participant as Participant
import Form.Screen as Screen
import Form.Topscorer as Topscorer
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (Card(..), InputState(..), Model, Msg(..), Token(..))
import Url


update : Msg -> Model Msg -> ( Model Msg, Cmd Msg )
update msg model =
    case msg of
        NavigateTo page ->
            ( { model | idx = page }, Cmd.none )

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

                        Just _ ->
                            ( model
                            , Navigation.pushUrl model.navKey (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChange _ ->
            ( model, Cmd.none )

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
                    { oldPost | msg = "" }

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
