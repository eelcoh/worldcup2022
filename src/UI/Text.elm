module UI.Text exposing
    ( boldText
    , bulletText
    , dateText
    , displayHeader
    , error
    , labelText
    , simpleText
    , style
    , textSecondary
    )

import Element exposing (alignLeft, alignTop, spacing, width)
import Element.Font as Font
import Element.Input as Input
import Time exposing (Month(..), Weekday(..))
import UI.Color
import UI.Style as Style


style : List (Element.Attribute msg)
style =
    Style.text []


textSecondary : String -> Element.Element msg
textSecondary txt =
    Element.el (Style.secondaryText []) (Element.text txt)


displayHeader : String -> Element.Element msg
displayHeader txt =
    Element.el (Style.header2 [ width Element.fill ]) (Element.text txt)


simpleText : String -> Element.Element msg
simpleText txt =
    Element.el (Style.text []) (Element.text txt)


error : String -> Element.Element msg
error txt =
    Element.paragraph [ Font.color UI.Color.red ] [ Element.text txt ]


bulletText : String -> Element.Element msg
bulletText txt =
    let
        bullet =
            Element.el
                [ Element.paddingEach { top = 7, right = 0, bottom = 0, left = 0 }, alignTop ]
                (Element.el (Style.bullet [ alignLeft ]) (Element.text "•"))

        contents =
            Element.paragraph (Style.introduction [ width Element.fill ]) [ Element.text txt ]
    in
    Element.row [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 5 }, spacing 16 ] [ bullet, contents ]


boldText : String -> Element.Element msg
boldText txt =
    Element.el (Style.emphasis [ Element.spacing 10 ]) (Element.text txt)


labelText : String -> Input.Label msg
labelText txt =
    Input.labelAbove (Style.emphasis [ Element.spacing 10 ]) (Element.text txt)


dateText : Time.Zone -> Time.Posix -> String
dateText tz dt =
    let
        m =
            Time.toMonth tz dt
                |> toMonth

        d =
            Time.toDay tz dt

        dd =
            Time.toWeekday tz dt
                |> toDay

        h =
            Time.toHour tz dt

        mn =
            Time.toMinute tz dt

        toMonth mon =
            case mon of
                Jan ->
                    "januari"

                Feb ->
                    "februari"

                Mar ->
                    "maart"

                Apr ->
                    "april"

                May ->
                    "mei"

                Jun ->
                    "juni"

                Jul ->
                    "juli"

                Aug ->
                    "augustus"

                Sep ->
                    "september"

                Oct ->
                    "oktober"

                Nov ->
                    "november"

                Dec ->
                    "december"

        toDay day =
            case day of
                Mon ->
                    "maandag"

                Tue ->
                    "dinsdag"

                Wed ->
                    "woensdag"

                Thu ->
                    "donderdag"

                Fri ->
                    "vrijdag"

                Sat ->
                    "zaterdag"

                Sun ->
                    "zondag"

        twoDigitString n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n

        dateString =
            dd ++ " " ++ String.fromInt d ++ " " ++ m ++ ", " ++ String.fromInt h ++ ":" ++ twoDigitString mn
    in
    dateString
