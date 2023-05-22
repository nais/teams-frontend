module Component.Card exposing (default, error)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)


default : String -> List (Html msg) -> List (Html msg) -> Html msg
default title buttons content =
    div [ class "card" ]
        [ div [ class "title" ] (h2 [] [ text title ] :: buttons)
        , div [ class "column" ] content
        ]


error : String -> List (Html msg) -> List (Html msg) -> Html msg
error title buttons content =
    div [ class "card error" ]
        [ div [ class "title" ] (h2 [] [ text title ] :: buttons)
        , div [ class "column" ] content
        ]
