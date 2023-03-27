module Pages exposing (Msg(..), Page(..), changeRouteTo, toSession, update, view)

import Api.Do exposing (query)
import Api.User
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
import Route exposing (Route)
import Session exposing (Session, Viewer(..))


type Page
    = Home Home.Model
    | Admin ReconcilerAdmin.Model
    | Team Team.Model
    | Teams Teams.Model
    | CreateTeam CreateTeam.Model
    | Users Users.Model
    | DeleteTeam DeleteTeam.Model
    | Error Error.Model


type Msg
    = GotHomeMsg Home.Msg
    | GotAdminMsg ReconcilerAdmin.Msg
    | GotTeamMsg Team.Msg
    | GotTeamsMsg Teams.Msg
    | GotCreateTeamMsg CreateTeam.Msg
    | GotUsersMsg Users.Msg
    | GotDeleteTeamMsg DeleteTeam.Msg
    | GotMeResponse (Maybe Route) (Result (Graphql.Http.Error (Maybe User)) (Maybe User))


toSession : Page -> Session
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


toRoute : Page -> Route
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


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case ( msg, page ) of
        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel |> updateWith Home GotHomeMsg

        ( GotAdminMsg subMsg, Admin subModel ) ->
            ReconcilerAdmin.update subMsg subModel |> updateWith Admin GotAdminMsg

        ( GotTeamMsg subMsg, Team subModel ) ->
            Team.update subMsg subModel |> updateWith Team GotTeamMsg

        ( GotTeamsMsg subMsg, Teams subModel ) ->
            Teams.update subMsg subModel |> updateWith Teams GotTeamsMsg

        ( GotCreateTeamMsg (CreateTeam.GotTeamCreatedResponse (Ok team)), CreateTeam subModel ) ->
            ( page, Nav.pushUrl (Session.navKey subModel.session) (Route.routeToString (Route.Team team.slug)) )

        ( GotCreateTeamMsg subMsg, CreateTeam subModel ) ->
            CreateTeam.update subMsg subModel |> updateWith CreateTeam GotCreateTeamMsg

        ( GotUsersMsg subMsg, Users subModel ) ->
            Users.update subMsg subModel |> updateWith Users GotUsersMsg

        ( GotDeleteTeamMsg subMsg, DeleteTeam subModel ) ->
            DeleteTeam.update subMsg subModel |> updateWith DeleteTeam GotDeleteTeamMsg

        ( GotMeResponse route resp, _ ) ->
            case resp of
                Ok maybeU ->
                    case maybeU of
                        Just u ->
                            changeRouteTo route (Session.mapViewer (Session.LoggedIn u) (toSession page))

                        Nothing ->
                            changeRouteTo route (Session.mapViewer Session.Anonymous (toSession page))

                Err _ ->
                    changeRouteTo route (Session.mapViewer Session.Anonymous (toSession page))

        _ ->
            -- Debug.log "got update from invalid location" -- use this when developing new module
            ( page, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Page, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> Session -> ( Page, Cmd Msg )
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
                    ( Error (Error.init session "changeRouteTo: no route found"), Cmd.none )


view : Page -> List (Html Msg)
view page =
    Component.Layout.view
        (toRoute page)
        (toSession page)
        (case page of
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
                Error.view subModel
        )
