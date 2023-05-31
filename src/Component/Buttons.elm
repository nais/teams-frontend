module Component.Buttons exposing (smallButton, smallButtonWithAttrs)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


smallButton : msg -> String -> String -> Html msg
smallButton msg iconClass title =
    button [ class "small button", onClick msg ]
        [ div [ class "icon", class iconClass ] []
        , text title
        ]


smallButtonWithAttrs : msg -> String -> String -> List (Html.Attribute msg) -> Html msg
smallButtonWithAttrs msg iconClass title attrs =
    button ([ class "small button", onClick msg ] ++ attrs)
        [ div [ class "icon", class iconClass ] []
        , text title
        ]
