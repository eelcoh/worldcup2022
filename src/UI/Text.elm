module UI.Text exposing (..)

import Element
import Element.Attributes exposing (alignLeft, px, spacing, padding, paddingXY, paddingLeft, paddingBottom, paddingTop, verticalCenter, width)
import UI.Style


displayHeader : String -> Element.Element UI.Style.Style variation msg
displayHeader txt =
    Element.header UI.Style.Header2
        [ width Element.Attributes.fill ]
        (Element.text txt)


simpleText : String -> Element.Element UI.Style.Style variation msg
simpleText txt =
    Element.el UI.Style.Text [] (Element.text txt)


bulletText : String -> Element.Element UI.Style.Style variation msg
bulletText txt =
    let
        bullet =
            Element.column UI.Style.None
                [ paddingTop 5 ]
                [ Element.circle 3 UI.Style.Bullet [ alignLeft ] Element.empty ]

        contents =
            Element.paragraph UI.Style.Introduction [ width Element.Attributes.fill ] [ (Element.text txt) ]
    in
        Element.row UI.Style.None [ paddingLeft 5, spacing 7 ] [ bullet, contents ]


boldText : String -> Element.Element UI.Style.Style variation msg
boldText txt =
    Element.el UI.Style.Emphasis [] (Element.text txt)
