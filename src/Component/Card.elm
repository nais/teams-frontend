module Component.Card exposing (new, render, withAttributes, withButtons, withContents, Card)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)


type alias Card msg =
    { title : String
    , buttons : List (Html msg)
    , content : List (Html msg)
    , attributes : List (Html.Attribute msg)
    }


new : String -> Card msg
new t =
    { title = t
    , buttons = []
    , attributes = []
    , content = []
    }


withAttributes : List (Html.Attribute msg) -> Card msg -> Card msg
withAttributes attributes card =
    { card | attributes = card.attributes ++ attributes }


withButtons : List (Html msg) -> Card msg -> Card msg
withButtons buttons card =
    { card | buttons = card.buttons ++ buttons }


withContents : List (Html msg) -> Card msg -> Card msg
withContents content card =
    { card | content = card.content ++ content }


render : Card msg -> Html msg
render card =
    div (class "card" :: card.attributes)
        [ div [ class "title" ] (h2 [] [ text card.title ] :: card.buttons)
        , div [ class "column" ] card.content
        ]
