module Component.Icons exposing (spinnerDone, spinnerError, spinnerLoading)

import Html exposing (Html)
import Svg exposing (circle, polyline, svg, text, text_)
import Svg.Attributes exposing (class, cx, cy, dominantBaseline, fill, height, points, r, textAnchor, viewBox, width, x, y)


spinner : String -> Html msg
spinner cls =
    svg [ class ("spinner spinner-" ++ cls), fill "transparent", width "24", height "24", viewBox "0 0 100 100" ]
        [ circle [ class "circle", cx "50", cy "50", r "46" ] []
        , polyline [ class "checkmark", points "25,55 45,70 75,33" ] []
        , text_ [ class "exclamation", dominantBaseline "middle", textAnchor "middle", x "50%", y "50%" ] [ text "!" ]
        ]


spinnerDone : Html msg
spinnerDone =
    spinner "done"


spinnerLoading : Html msg
spinnerLoading =
    spinner "loading"


spinnerError : Html msg
spinnerError =
    spinner "error"
