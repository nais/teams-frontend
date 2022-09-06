module Home exposing (..)

import Backend.Query exposing (me)
import Browser.Navigation
import Graphql.Http
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Queries.Do exposing (do)
import Queries.UserQueries exposing (UserData, getMeQuery)


type Actor
    = LoggedIn UserData
    | Unauthorized
    | Unknown


type alias Model =
    { user : Actor
    }


type Msg
    = GotMeResponse (Result (Graphql.Http.Error UserData) UserData)
    | LoginClicked


init : ( Model, Cmd Msg )
init =
    ( { user = Unknown
      }
    , do getMeQuery GotMeResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginClicked ->
            ( model, Browser.Navigation.load "http://localhost:3000/oauth2/login" )

        GotMeResponse r ->
            case r of
                Ok u ->
                    ( { model | user = LoggedIn u }, Cmd.none )

                Err e ->
                    Debug.log "no user"
                        ( model, Cmd.none ) -- should we have a "log error Cmd ?"


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "nais console" ]
        , case model.user of
            LoggedIn user ->
                div []
                    [ text user.email
                    , button [ onClick LoginClicked ] [ text "Log out" ] -- fake news logout
                    ]

            _ ->
                button [ onClick LoginClicked ] [ text "Single sign-on" ]
        ]
