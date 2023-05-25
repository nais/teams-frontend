module Component.Buttons exposing (smallButton)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


smallButton : msg -> String -> String -> Html msg
smallButton msg iconClass title =
    button [ class "small button", onClick msg ]
        [ div [ class "icon", class iconClass ] []
        , text title
        ]