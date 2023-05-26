module Page.Home exposing (Model, Msg(..), init, update, view)

import Browser.Navigation
import Component.Card as Card
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Session exposing (Session)
import Url.Builder


type alias Model =
    { session : Session
    , maybeRoute : Maybe Route
    }


type Msg
    = LoginClicked


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
update _ model =
    let
        login : String
        login =
            Url.Builder.absolute [ "oauth2", "login" ] [ Url.Builder.string "redirect_uri" (maybeRouteToString model.maybeRoute) ]
    in
    ( model, Browser.Navigation.load login )


view : Model -> Html Msg
view _ =
    Card.new "Welcome to NAIS Teams"
        |> Card.withContents
            [ p [] [ text "This site enables self-service management of teams. Any user in the organization may create a team and assign team members to teams they own." ]
            , p [] [ text "Each team will get a Google group, a GCP project for each environment, a GitHub team, and an Azure AD security group." ]
            , p [] [ text "NAIS Teams will automatically keep these resources up to date with current team members." ]
            , p [] [ text "Please log in to continue." ]
            , div [ class "button-row" ]
                [ button [ onClick LoginClicked ] [ text "Login" ]
                ]
            ]
        |> Card.render
