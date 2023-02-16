module Page.Home exposing (..)

import Browser.Navigation
import DataModel exposing (..)
import Graphql.Http
import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Session exposing (Session, Viewer(..))
import Url.Builder


type alias Model =
    { session : Session
    , maybeRoute : Maybe Route
    }


type Msg
    = GotMeResponse (Result (Graphql.Http.Error Viewer) Viewer)
    | GotUser Session.Viewer
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
view _ =
    div []
        [ div [ class "card" ]
            [ h2 [] [ text "Welcome to NAIS console" ]
            , p [] [ text "This site enables self-service management of teams. Any user in the organization may create a team and assign team members to teams they own." ]
            , p [] [ text "Each team will get a Google group, a GCP project for each environment, a GitHub team, and an Azure AD security group." ]
            , p [] [ text "Console will automatically keep these resources up to date with current team members." ]
            , p [] [ text "Please log in to continue." ]
            , div [ class "button-row" ]
                [ button [ onClick LoginClicked ] [ text "Login" ]
                ]
            ]
        ]


mapUser : Viewer -> Model -> Model
mapUser user model =
    { model | session = Session.mapViewer user model.session }
