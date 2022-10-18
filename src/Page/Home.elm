module Page.Home exposing (..)

import Browser.Navigation
import Graphql.Http
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Queries.UserQueries exposing (UserData)
import Route exposing (Route)
import Session exposing (Session, User(..))
import Url.Builder


type alias Model =
    { session : Session
    , maybeRoute : Maybe Route
    }


type Msg
    = GotMeResponse (Result (Graphql.Http.Error UserData) UserData)
    | GotUser Session.User
    | LoginClicked
    | LogoutClicked


init : Session -> Maybe Route -> Model
init session maybeRoute =
    { session = session
    , maybeRoute = maybeRoute
    }


maybeRouteToString : Maybe Route -> String
maybeRouteToString maybeRoute =
    case maybeRoute of
        Just r ->
            Route.routeToString r

        Nothing ->
            Route.routeToString Route.Home


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        login =
            Url.Builder.absolute [ "oauth2", "login" ] [ Url.Builder.string "redirect_uri" (maybeRouteToString model.maybeRoute) ]

        logout =
            Url.Builder.absolute [ "oauth2", "logout" ] [ Url.Builder.string "redirect_uri" (Route.routeToString Route.Home) ]
    in
    case msg of
        LoginClicked ->
            ( model, Browser.Navigation.load login )

        LogoutClicked ->
            ( model, Browser.Navigation.load logout )

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
