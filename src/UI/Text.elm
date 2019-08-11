module UI.Text exposing (boldText, bulletText, displayHeader, simpleText)

import Element exposing (alignLeft, padding, paddingXY, px, spacing, width)
import UI.Style as Style


displayHeader : String -> Element.Element msg
displayHeader txt =
    Element.el (Style.header2 [ width Element.fill ]) (Element.text txt)


simpleText : String -> Element.Element msg
simpleText txt =
    Element.el (Style.text []) (Element.text txt)


bulletText : String -> Element.Element msg
bulletText txt =
    let
        bullet =
            Element.column
                [ Element.paddingEach { top = 5, right = 0, bottom = 0, left = 0 } ]
                [ Element.el (Style.bullet [ alignLeft ]) (Element.text "•") ]

        contents =
            Element.paragraph (Style.introduction [ width Element.fill ]) [ Element.text txt ]
    in
    Element.row [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 5 }, spacing 7 ] [ bullet, contents ]


boldText : String -> Element.Element msg
boldText txt =
    Element.el (Style.emphasis []) (Element.text txt)
