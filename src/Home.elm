module Home exposing (..)

import Browser.Navigation
import Graphql.Http
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Queries.UserQueries exposing (UserData)
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
    div []
        (p [] [ text "Welcome to NAIS console." ]
            :: (case Session.user model.session of
                    LoggedIn _ ->
                        [ p [] [ text "Please continue as you wish." ]
                        , button [ onClick LogoutClicked ] [ text "Log out" ]
                        ]

                    Unknown ->
                        [ text "Loading..." ]

                    Anonymous ->
                        [ p [] [ text "Please log in to continue." ]
                        , button [ onClick LoginClicked ] [ text "Log in" ]
                        ]
               )
        )


mapUser : User -> Model -> Model
mapUser user model =
    { model | session = Session.mapUser user model.session }
