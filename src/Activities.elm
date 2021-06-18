module Activities exposing (..)

import Element exposing (Length, alignLeft, alignRight, column, fill, height, paddingXY, px, row, spacingXY, width)
import Element.Events as Events
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, andThen, field)
import Json.Encode
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Web exposing (defaultConfig)
import Time
import Types exposing (ActivitiesModel, Activity(..), ActivityMeta, Comment, Msg(..), Post)
import UI.Button
import UI.Screen as Screen
import UI.Style
import UI.Text


fetchActivities : ActivitiesModel Msg -> Cmd Msg
fetchActivities _ =
    Web.get "/activities" FetchedActivities decode


saveComment : ActivitiesModel Msg -> Cmd Msg
saveComment model =
    let
        comment =
            encodeComment model.comment
    in
    Web.post "/activities/comments" SavedComment decode comment


savePost : ActivitiesModel Msg -> String -> Cmd Msg
savePost model token =
    let
        post =
            encodePost model.post

        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }
    in
    Web.postWithConfig config "/activities/blogs" SavedPost decode post


view : Types.Model Msg -> Element.Element Msg
view model =
    let
        screenWidth =
            px <| Screen.maxWidth model.screen
    in
    viewActivities screenWidth model.timeZone model.activities.activities


viewActivities : Length -> Time.Zone -> WebData (List Activity) -> Element.Element Msg
viewActivities wdth tz wActivities =
    case wActivities of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure _ ->
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success activities ->
            List.map (viewActivity tz) activities
                |> Element.column [ Screen.className "activities", width wdth, spacingXY 0 20, paddingXY 0 20 ]


viewActivity : Time.Zone -> Activity -> Element.Element Msg
viewActivity tz activity =
    case activity of
        ANewBet activityMeta name uuid ->
            Element.el [ paddingXY 0 20 ] (Element.text (name ++ "doet mee"))

        AComment activityMeta author comment ->
            commentBox author comment tz activityMeta.date

        APost activityMeta author blogTitle blog ->
            blogBox author blogTitle blog tz activityMeta.date

        ANewRanking activityMeta ->
            Element.el [ paddingXY 0 20 ] (Element.text "De stand is bijgewerkt")


blogBox : String -> String -> String -> Time.Zone -> Time.Posix -> Element.Element Msg
blogBox author title blog tz dt =
    column
        (UI.Style.normalBox [ paddingXY 0 20, Screen.className "blogBox" ])
        [ Element.paragraph (UI.Style.header2 []) [ Element.text title ]
        , blogView blog
        , Element.el (UI.Style.attribution [ alignRight ]) (Element.text (author ++ ", " ++ UI.Text.dateText tz dt))
        ]


commentBox : String -> String -> Time.Zone -> Time.Posix -> Element.Element Msg
commentBox author comment tz dt =
    column
        (UI.Style.darkBox [ paddingXY 0 20, Screen.className "commentBox" ])
        [ row
            [ alignLeft ]
            [ Element.el (UI.Style.attribution []) (Element.text (author ++ " zegt:")) ]
        , row
            [ alignLeft ]
            [ commentView comment ]
        , row
            [ alignRight ]
            [ timeView tz dt ]
        ]


blogView : String -> Element.Element Msg
blogView c =
    let
        comment =
            Markdown.toHtml [] c
                |> Element.html
    in
    Element.paragraph (UI.Style.introduction []) [ comment ]


commentView : String -> Element.Element Msg
commentView c =
    let
        comment =
            Markdown.toHtml [] c
                |> Element.html
    in
    Element.paragraph (UI.Style.introduction []) [ comment ]


timeView : Time.Zone -> Time.Posix -> Element.Element Msg
timeView tz dt =
    Element.el (UI.Style.attribution []) (Element.text (UI.Text.dateText tz dt))


viewCommentInput : ActivitiesModel Msg -> Element.Element Msg
viewCommentInput model =
    let
        commentInput v =
            let
                area =
                    { onChange = SetCommentMsg
                    , text = v
                    , placeholder = Nothing
                    , label = UI.Text.labelText "zeg wat"
                    , spellcheck = True
                    }
            in
            Input.multiline [ height (px 120) ] area

        commentInputTrap =
            let
                area =
                    { onChange = \_ -> NoOp
                    , label = UI.Text.labelText "zeg wat"
                    , text = ""
                    , placeholder = Nothing
                    }
            in
            Input.text
                [ Events.onFocus ShowCommentInput, height (px 36) ]
                area

        authorInput v =
            let
                area =
                    { onChange = SetCommentAuthor
                    , text = v
                    , label = UI.Text.labelText "naam"
                    , placeholder = Nothing
                    }
            in
            Input.text [ height (px 36) ] area

        saveButton =
            if (model.comment.msg == "") || (model.comment.author == "") then
                UI.Button.pill UI.Style.Inactive NoOp "prik!"

            else
                UI.Button.pill UI.Style.Active SaveComment "prik!"

        input =
            if model.showComment then
                Element.column
                    [ paddingXY 0 20, spacingXY 0 20, width fill ]
                    [ commentInput model.comment.msg
                    , authorInput model.comment.author
                    , saveButton
                    ]

            else
                commentInputTrap

        -- Element.column UI.Style.CommentInputBox
        --     [ padding 10, spacing 20 ]
        --     [ commentInputTrap
        --     ]
    in
    Element.el (UI.Style.normalBox [ Screen.className "commentInputBox" ]) input


viewPostInput : ActivitiesModel Msg -> Element.Element Msg
viewPostInput model =
    let
        titleInput v =
            let
                area =
                    { onChange = SetPostTitle
                    , text = v
                    , placeholder = Nothing
                    , label = UI.Text.labelText "titel"
                    }
            in
            Input.text [ height (px 36) ] area

        postInput v =
            let
                area =
                    { onChange = SetPostMsg
                    , text = v
                    , placeholder = Nothing
                    , label = UI.Text.labelText "tekst"
                    , spellcheck = True
                    }
            in
            Input.multiline [ height (px 200) ] area

        postInputTrap =
            let
                area =
                    { onChange = \_ -> NoOp
                    , label = UI.Text.labelText "nieuwe blogpost"
                    , text = ""
                    , placeholder = Nothing
                    }
            in
            Input.text [ Events.onFocus ShowPostInput, height (px 36) ] area

        passphraseInput v =
            let
                area =
                    { onChange = SetPostPassphrase
                    , label = UI.Text.labelText "wachtwoord"
                    , text = v
                    , placeholder = Nothing
                    }
            in
            Input.text [ height (px 36) ] area

        authorInput v =
            let
                area =
                    { onChange = SetPostAuthor
                    , label = UI.Text.labelText "naam"
                    , text = v
                    , placeholder = Nothing
                    }
            in
            Input.text [ height (px 36) ] area

        saveButton =
            if (model.post.msg == "") || (model.post.author == "") || (model.post.passphrase == "") then
                UI.Button.pill UI.Style.Inactive NoOp "post!"

            else
                UI.Button.pill UI.Style.Active SavePost "post!"

        input =
            if model.showPost then
                Element.column
                    [ paddingXY 0 20, spacingXY 0 20, width fill ]
                    [ titleInput model.post.title
                    , postInput model.post.msg
                    , passphraseInput model.post.passphrase
                    , authorInput model.post.author
                    , saveButton
                    ]

            else
                Element.column
                    [ paddingXY 0 20, spacingXY 0 20, width fill ]
                    [ postInputTrap
                    ]
    in
    Element.el (UI.Style.darkBox []) input



-- Json


encodeComment : Comment -> Json.Encode.Value
encodeComment comment =
    let
        encodedComment =
            Json.Encode.object
                [ ( "author", Json.Encode.string comment.author )
                , ( "msg", encodeMessage comment.msg )
                ]
    in
    Json.Encode.object
        [ ( "comment", encodedComment ) ]


encodePost : Post -> Json.Encode.Value
encodePost post =
    let
        encodedPost =
            Json.Encode.object
                [ ( "author", Json.Encode.string post.author )
                , ( "title", Json.Encode.string post.title )
                , ( "msg", encodeMessage post.msg )
                , ( "passphrase", Json.Encode.string post.passphrase )
                ]
    in
    Json.Encode.object
        [ ( "blog", encodedPost ) ]


encodeMessage : String -> Json.Encode.Value
encodeMessage msg =
    let
        multlineMsg =
            String.split "\n" msg
    in
    Json.Encode.list Json.Encode.string multlineMsg


encodeActivity : Activity -> Json.Encode.Value
encodeActivity activity =
    case activity of
        AComment am author msg ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "comment" )
                , ( "author", Json.Encode.string author )
                , ( "msg", encodeMessage msg )
                , ( "meta", encodeActivityMeta am )
                ]

        APost am author title msg ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "blog" )
                , ( "author", Json.Encode.string author )
                , ( "title", Json.Encode.string title )
                , ( "msg", encodeMessage msg )
                , ( "meta", encodeActivityMeta am )
                ]

        ANewRanking am ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "new-ranking" )
                , ( "meta", encodeActivityMeta am )
                ]

        ANewBet am name betUuid ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "comment" )
                , ( "name", Json.Encode.string name )
                , ( "bet-uuid", Json.Encode.string betUuid )
                , ( "meta", encodeActivityMeta am )
                ]


encodeActivityMeta : ActivityMeta -> Json.Encode.Value
encodeActivityMeta am =
    Json.Encode.object
        [ ( "date", Json.Encode.int (Time.posixToMillis am.date) )
        , ( "active", Json.Encode.bool am.active )
        , ( "uuid", Json.Encode.string am.uuid )
        ]


decode : Decoder (List Activity)
decode =
    field "activities" (Json.Decode.list decodeActivity)


decodeActivity : Decoder Activity
decodeActivity =
    field "type" Json.Decode.string |> andThen decodeActivityDetails


decodeActivityDetails : String -> Decoder Activity
decodeActivityDetails tp =
    case tp of
        "comment" ->
            Json.Decode.map3 AComment
                (field "meta" decodeMeta)
                (field "author" Json.Decode.string)
                (field "msg" decodeMessage)

        "blog" ->
            Json.Decode.map4 APost
                (field "meta" decodeMeta)
                (field "author" Json.Decode.string)
                (field "title" Json.Decode.string)
                (field "msg" decodeMessage)

        "new-bet" ->
            Json.Decode.map3 ANewBet
                (field "meta" decodeMeta)
                (field "name" Json.Decode.string)
                (field "bet-uuid" Json.Decode.string)

        "new-ranking" ->
            Json.Decode.map ANewRanking
                (field "meta" decodeMeta)

        _ ->
            Json.Decode.fail "WHOOPS"


decodeMeta : Decoder ActivityMeta
decodeMeta =
    Json.Decode.map3 ActivityMeta
        (field "date" decodeDate)
        (field "active" Json.Decode.bool)
        (field "uuid" Json.Decode.string)


decodeDate : Decoder Time.Posix
decodeDate =
    Json.Decode.int
        |> Json.Decode.map Time.millisToPosix


decodeMessage : Decoder String
decodeMessage =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.map (String.join "\n")
