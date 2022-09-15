module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import CreateTeam
import Error
import Home
import Html exposing (div, h1, header, li, main_, nav, p, text, ul)
import Html.Attributes exposing (class)
import Route exposing (Route(..), link)
import Session exposing (Session)
import Team
import Teams
import Url



---- MODEL ----


type Model
    = Redirect Session
    | Home Home.Model
    | Team Team.Model
    | Teams Teams.Model
    | CreateTeam CreateTeam.Model
    | Error Error.Model



---- UPDATE ----


type Msg
    = NoOp
    | GotHomeMsg Home.Msg
    | GotTeamMsg Team.Msg
    | GotTeamsMsg Teams.Msg
    | GotCreateTeamMsg CreateTeam.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


init : a -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nk =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.init nk))



-- just use the home model's init in main (should be route based later on)


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Just Route.Home ->
            Home.init session |> updateWith Home GotHomeMsg

        Just Route.CreateTeam ->
            CreateTeam.init session |> updateWith CreateTeam GotCreateTeamMsg

        Just Route.Teams ->
            Teams.init session |> updateWith Teams GotTeamsMsg

        Just (Route.Team id) ->
            Team.init session id |> updateWith Team GotTeamMsg

        Nothing ->
            Error.init session "no route" |> updateWith Error (\_ -> NoOp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- Add handler here when we add new modules
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (toSession model |> Session.navKey) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel |> updateWith Home GotHomeMsg

        ( GotTeamMsg subMsg, Team subModel ) ->
            Team.update subMsg subModel |> updateWith Team GotTeamMsg

        ( GotTeamsMsg subMsg, Teams subModel ) ->
            Teams.update subMsg subModel |> updateWith Teams GotTeamsMsg

        ( GotCreateTeamMsg (CreateTeam.GotTeamCreatedResponse (Ok team)), CreateTeam subModel ) ->
            ( model, Nav.pushUrl (Session.navKey subModel.session) (Route.routeToString (Route.Team team.id)) )

        ( GotCreateTeamMsg subMsg, CreateTeam subModel ) ->
            CreateTeam.update subMsg subModel |> updateWith CreateTeam GotCreateTeamMsg

        ( _, _ ) ->
            Debug.todo "this is not a valid case"



-- (_, _) ->
--     Debug.log "got update from invalid location"
---- VIEW ----


view : Model -> Document Msg
view model =
    let
        html =
            case model of
                -- Add new view here when we add new modules
                Home subModel ->
                    Home.view subModel |> Html.map GotHomeMsg

                Team subModel ->
                    Team.view subModel |> Html.map GotTeamMsg

                Teams subModel ->
                    Teams.view subModel |> Html.map GotTeamsMsg

                CreateTeam subModel ->
                    CreateTeam.view subModel |> Html.map GotCreateTeamMsg

                Error subModel ->
                    Error.view subModel |> Html.map (\_ -> NoOp)

                Redirect _ ->
                    div [] [ text "redirect" ]
    in
    { title = "NAIS console"
    , body =
        [ header []
            [ h1 []
                [ link Route.Home [] [ text "Console" ]
                ]
            , p [] [ text (Session.username (Session.user (toSession model))) ]
            ]
        , nav []
            [ ul []
                [ li [ class "active" ] [ link Route.Home [] [ text "Home" ] ]
                , li [] [ link Route.Teams [] [ text "Teams" ] ]
                ]
            ]
        , main_ []
            [ html ]
        ]
    }



---- PROGRAM ----


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


toSession : Model -> Session
toSession model =
    case model of
        Home m ->
            m.session

        Error m ->
            m.session

        Team m ->
            m.session

        Teams m ->
            m.session

        CreateTeam m ->
            m.session

        Redirect session ->
            session
