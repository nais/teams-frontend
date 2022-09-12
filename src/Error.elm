module Error exposing (..)

import Browser.Navigation
import Html exposing (Html, text, div)


type alias Model =
    { error : String
    , navKey : Browser.Navigation.Key
    }



init : Browser.Navigation.Key -> String -> ( Model, Cmd msg )
init navigationKey error =
    ( { error = error
      , navKey = navigationKey
      }
    , Cmd.none
    )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )

view : Model -> Html msg
view model =
    div [] [ text model.error ]


navKey : Model -> Browser.Navigation.Key
navKey model =
    model.navKey
