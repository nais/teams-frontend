module Home exposing (..)

import Browser.Navigation
import Graphql.Http
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Queries.Do exposing (query)
import Queries.UserQueries exposing (UserData, getMeQuery)
import Session exposing (Session)


type Actor
    = LoggedIn UserData
    | Unauthorized
    | Unknown


type alias Model =
    { user : Actor
    , session : Session
    }


type Msg
    = GotMeResponse (Result (Graphql.Http.Error UserData) UserData)
    | LoginClicked
    | LogoutClicked


init : Session -> ( Model, Cmd Msg )
init session =
    ( { user = Unknown
      , session = session
      }
    , query getMeQuery GotMeResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginClicked ->
            ( model, Browser.Navigation.load "http://localhost:3000/oauth2/login" )

        LogoutClicked ->
            ( model, Browser.Navigation.load "http://localhost:3000/oauth2/logout" )

        GotMeResponse r ->
            case r of
                Ok u ->
                    ( { model | user = LoggedIn u }, Cmd.none )

                Err e ->
                    ( { model | user = Unauthorized }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.user of
        LoggedIn user ->
            div []
                [ p [] [ text ("Logged in as " ++ user.email) ]
                , button [ onClick LogoutClicked ] [ text "Log out" ] -- fake news logout
                ]

        Unknown ->
            div [] [ text "Loading..." ]

        Unauthorized ->
            div []
                [ p []
                    [ text "Welcome to NAIS console."
                    , text "This is your one stop shop for team management. Please log in to continue."
                    ]
                , button [ onClick LoginClicked ] [ text "Single sign-on" ]
                ]
