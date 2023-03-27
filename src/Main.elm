module Main exposing (Model(..), Msg(..), main)

import Api.Do exposing (query)
import Api.User
import Browser exposing (Document)
import Browser.Navigation as Nav
import Component.Layout
import DataModel exposing (User)
import Graphql.Http
import Html exposing (Html)
import Page.CreateTeam as CreateTeam
import Page.DeleteTeam as DeleteTeam
import Page.Error as Error
import Page.Home as Home
import Page.ReconcilerAdmin as ReconcilerAdmin
import Page.Team as Team
import Page.Teams as Teams
import Page.Users as Users
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Session exposing (Session, Viewer(..))
import Url



---- MODEL ----


type Model
    = Home Home.Model
    | Admin ReconcilerAdmin.Model
    | Team Team.Model
    | Teams Teams.Model
    | CreateTeam CreateTeam.Model
    | Users Users.Model
    | DeleteTeam DeleteTeam.Model
    | Error Error.Model



---- UPDATE ----


type Msg
    = NoOp
    | GotMeResponse (Maybe Route) (Result (Graphql.Http.Error (Maybe User)) (Maybe User))
    | GotHomeMsg Home.Msg
    | GotAdminMsg ReconcilerAdmin.Msg
    | GotTeamMsg Team.Msg
    | GotTeamsMsg Teams.Msg
    | GotCreateTeamMsg CreateTeam.Msg
    | GotUsersMsg Users.Msg
    | GotDeleteTeamMsg DeleteTeam.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


init : a -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nk =
    changeRouteTo (Route.fromUrl url) (Session.init nk url)



--


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> Session -> ( Model, Cmd Msg )
changeRouteTo r session =
    case Session.viewer session of
        Anonymous ->
            ( Home (Home.init session r), query Api.User.getMe (GotMeResponse r) )

        LoggedIn _ ->
            case r of
                Just Route.Home ->
                    ( Home (Home.init session r)
                    , Nav.pushUrl (Session.navKey session) (Route.routeToString Route.MyTeams)
                    )

                Just Route.ReconcilerAdmin ->
                    ReconcilerAdmin.init session |> updateWith Admin GotAdminMsg

                Just Route.CreateTeam ->
                    CreateTeam.init session |> updateWith CreateTeam GotCreateTeamMsg

                Just Route.AllTeams ->
                    Teams.init session Teams.AllTeams |> updateWith Teams GotTeamsMsg

                Just Route.MyTeams ->
                    Teams.init session Teams.MyTeams |> updateWith Teams GotTeamsMsg

                Just (Route.Team id) ->
                    Team.init session id |> updateWith Team GotTeamMsg

                Just Route.Users ->
                    Users.init session |> updateWith Users GotUsersMsg

                Just (Route.DeleteTeam slug) ->
                    DeleteTeam.requestTeamDeletion session slug |> updateWith DeleteTeam GotDeleteTeamMsg

                Just (Route.DeleteTeamConfirm key) ->
                    DeleteTeam.confirmTeamDeletion session key |> updateWith DeleteTeam GotDeleteTeamMsg

                Nothing ->
                    Error.init session "changeRouteTo: no route found" |> updateWith Error (\_ -> NoOp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- Add handler here when we add new modules
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case Route.fromUrl url of
                        Just _ ->
                            ( model, Nav.pushUrl (toSession model |> Session.navKey) (Url.toString url) )

                        Nothing ->
                            ( model, Nav.load <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) (toSession model)

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel |> updateWith Home GotHomeMsg

        ( GotAdminMsg subMsg, Admin subModel ) ->
            ReconcilerAdmin.update subMsg subModel |> updateWith Admin GotAdminMsg

        ( GotTeamMsg subMsg, Team subModel ) ->
            Team.update subMsg subModel |> updateWith Team GotTeamMsg

        ( GotTeamsMsg subMsg, Teams subModel ) ->
            Teams.update subMsg subModel |> updateWith Teams GotTeamsMsg

        ( GotCreateTeamMsg (CreateTeam.GotTeamCreatedResponse (Ok team)), CreateTeam subModel ) ->
            ( model, Nav.pushUrl (Session.navKey subModel.session) (Route.routeToString (Route.Team team.slug)) )

        ( GotCreateTeamMsg subMsg, CreateTeam subModel ) ->
            CreateTeam.update subMsg subModel |> updateWith CreateTeam GotCreateTeamMsg

        ( GotMeResponse route resp, _ ) ->
            handleMeResponse (toSession model) route resp

        ( GotUsersMsg subMsg, Users subModel ) ->
            Users.update subMsg subModel |> updateWith Users GotUsersMsg

        ( GotDeleteTeamMsg subMsg, DeleteTeam subModel ) ->
            DeleteTeam.update subMsg subModel |> updateWith DeleteTeam GotDeleteTeamMsg

        _ ->
            ( model, Cmd.none )



-- (_, _) ->
--     Debug.log "got update from invalid location"
---- VIEW ----


view : Model -> Document Msg
view model =
    let
        html : Html Msg
        html =
            case model of
                -- Add new view here when we add new modules
                Home subModel ->
                    Home.view subModel |> Html.map GotHomeMsg

                Admin subModel ->
                    ReconcilerAdmin.view subModel |> Html.map GotAdminMsg

                Team subModel ->
                    Team.view subModel |> Html.map GotTeamMsg

                Teams subModel ->
                    Teams.view subModel |> Html.map GotTeamsMsg

                CreateTeam subModel ->
                    CreateTeam.view subModel |> Html.map GotCreateTeamMsg

                Users subModel ->
                    Users.view subModel |> Html.map GotUsersMsg

                DeleteTeam subModel ->
                    DeleteTeam.view subModel |> Html.map GotDeleteTeamMsg

                Error subModel ->
                    Error.view subModel |> Html.map (\_ -> NoOp)
    in
    { title = "NAIS console"
    , body = Component.Layout.view (toRoute model) (toSession model) html
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

        Admin m ->
            m.session

        Error m ->
            m.session

        Team m ->
            m.session

        Teams m ->
            m.session

        Users m ->
            m.session

        DeleteTeam m ->
            m.session

        CreateTeam m ->
            m.session


toRoute : Model -> Route
toRoute model =
    case model of
        Home _ ->
            Route.Home

        Teams _ ->
            Route.MyTeams

        CreateTeam _ ->
            Route.CreateTeam

        Team m ->
            Route.Team m.slug

        Admin _ ->
            Route.ReconcilerAdmin

        Users _ ->
            Route.Users

        DeleteTeam m ->
            Route.DeleteTeam m.slug

        Error _ ->
            Route.Home


handleMeResponse : Session -> Maybe Route -> Result (Graphql.Http.Error (Maybe User)) (Maybe User) -> ( Model, Cmd Msg )
handleMeResponse session route result =
    case result of
        Ok maybeU ->
            case maybeU of
                Just u ->
                    changeRouteTo route (Session.mapViewer (Session.LoggedIn u) session)

                Nothing ->
                    changeRouteTo route (Session.mapViewer Session.Anonymous session)

        Err _ ->
            changeRouteTo route (Session.mapViewer Session.Anonymous session)
