module Home exposing (..)

import Browser.Navigation
import Graphql.Http
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Queries.Do exposing (query)
import Queries.UserQueries exposing (UserData, getMeQuery)
import Session exposing (Session, User(..))


type alias Model =
    { session : Session
    }


type Msg
    = GotMeResponse (Result (Graphql.Http.Error UserData) UserData)
    | GotUser Session.User
    | LoginClicked
    | LogoutClicked


init : Session -> Model
init session =
    { session = session
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginClicked ->
            ( model, Browser.Navigation.load "/oauth2/login" )

        LogoutClicked ->
            ( model, Browser.Navigation.load "/oauth2/logout" )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case Session.user model.session of
        LoggedIn user ->
            div []
                [ p [] [ text "Welcome to NAIS console." ]
                , p [] [ text ("You are logged in as " ++ user.email ++ ".") ]
                , p [] [ text "Please continue as you wish." ]
                , button [ onClick LogoutClicked ] [ text "Log out" ] -- fake news logout
                ]

        Unknown ->
            div [] [ text "Loading..." ]

        Anonymous ->
            div []
                [ p [] [ text "Welcome to NAIS console." ]
                , p [] [ text "This is your one stop shop for team management. Please log in to continue." ]
                , button [ onClick LoginClicked ] [ text "Single sign-on" ]
                ]


mapUser : User -> Model -> Model
mapUser user model =
    { model | session = Session.mapUser user model.session }
