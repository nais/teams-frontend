module Main exposing (Model(..), Msg(..), main)

import Api.Do exposing (query)
import Api.User
import Browser exposing (Document)
import Browser.Navigation as Nav
import DataModel exposing (User)
import Graphql.Http
import Html
import Page.CreateTeam exposing (Model)
import Pages exposing (Page, changeRouteTo, toSession)
import Route exposing (Route)
import Session
import Url


type Model
    = Model Pages.Page


type Msg
    = GotPageMsg Pages.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotMeResponse (Maybe Route) (Result (Graphql.Http.Error (Maybe User)) (Maybe User))


init : a -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nk =
    let
        r =
            Route.fromUrl url

        ( page, cmd ) =
            changeRouteTo r (Session.init nk url)
    in
    ( Model page
    , Cmd.batch
        [ Cmd.map GotPageMsg cmd
        , query Api.User.getMe (GotMeResponse r)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model page) =
    case msg of
        GotPageMsg pageMsg ->
            Pages.update pageMsg page |> fromPage

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Route.fromUrl url of
                        Just _ ->
                            ( Model page, Nav.pushUrl (toSession page |> Session.navKey) (Url.toString url) )

                        Nothing ->
                            ( Model page, Nav.load <| Url.toString url )

                Browser.External href ->
                    ( Model page, Nav.load href )

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) (toSession page) |> fromPage

        GotMeResponse route resp ->
            let
                v : Session.Viewer
                v =
                    case resp of
                        Ok maybeU ->
                            case maybeU of
                                Just u ->
                                    Session.LoggedIn u

                                Nothing ->
                                    Session.Anonymous

                        Err _ ->
                            Session.Anonymous
            in
            changeRouteTo route (Session.mapViewer v (toSession page)) |> fromPage


view : Model -> Document Msg
view model =
    let
        (Model page) =
            model
    in
    { title = "NAIS console"
    , body = Pages.view page |> List.map (Html.map GotPageMsg)
    }


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


fromPage : ( Page, Cmd Pages.Msg ) -> ( Model, Cmd Msg )
fromPage ( page, cmd ) =
    ( Model page
    , Cmd.map GotPageMsg cmd
    )
