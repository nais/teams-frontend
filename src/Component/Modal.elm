port module Component.Modal exposing (close, open, view)

import Html exposing (Html)
import Html.Attributes exposing (id)


port openModal : String -> Cmd msg


port closeModal : String -> Cmd msg


view : String -> List (Html msg) -> Html msg
view modalId =
    Html.node "dialog" [ id modalId ]


open : String -> Cmd msg
open =
    openModal


close : String -> Cmd msg
close =
    closeModal
