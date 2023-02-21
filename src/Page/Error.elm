module Page.Error exposing (Model, init, view)

import Html exposing (Html, div, text)
import Session exposing (Session)


type alias Model =
    { error : String
    , session : Session
    }


init : Session -> String -> ( Model, Cmd msg )
init session error =
    ( { error = error
      , session = session
      }
    , Cmd.none
    )


view : Model -> Html msg
view model =
    div [] [ text model.error ]
