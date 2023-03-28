module Pages exposing (Msg(..), Page(..), changeRouteTo, toSession, update, view)

import Browser.Navigation as Nav
import Component.Layout
import Html exposing (Html)
import Page.Admin as Admin
import Page.CreateTeam as CreateTeam
import Page.DeleteTeam as DeleteTeam
import Page.Error as Error
import Page.Home as Home
import Page.Team as Team
import Page.Teams as Teams
import Page.Users as Users
import Route exposing (Route, ViewMode(..))
import Session exposing (Session, Viewer(..))


type Page
    = AdminModel Admin.Model
    | CreateTeamModel CreateTeam.Model
    | DeleteTeamModel DeleteTeam.Model
    | ErrorModel Error.Model
    | HomeModel Home.Model
    | TeamModel Team.Model
    | TeamsModel Teams.Model
    | UsersModel Users.Model


type Msg
    = AdminMsg Admin.Msg
    | CreateTeamMsg CreateTeam.Msg
    | DeleteTeamMsg DeleteTeam.Msg
    | HomeMsg Home.Msg
    | TeamMsg Team.Msg
    | TeamsMsg Teams.Msg
    | UsersMsg Users.Msg


toSession : Page -> Session
toSession model =
    case model of
        AdminModel m ->
            m.session

        CreateTeamModel m ->
            m.session

        DeleteTeamModel m ->
            m.session

        ErrorModel m ->
            m.session

        HomeModel m ->
            m.session

        TeamModel m ->
            m.session

        TeamsModel m ->
            m.session

        UsersModel m ->
            m.session


toRoute : Page -> Route
toRoute model =
    case model of
        AdminModel _ ->
            Route.Admin

        CreateTeamModel _ ->
            Route.CreateTeam

        DeleteTeamModel m ->
            Route.DeleteTeam m.slug

        ErrorModel _ ->
            Route.Home

        HomeModel _ ->
            Route.Home

        TeamModel m ->
            Route.Team m.slug

        TeamsModel m ->
            Route.Teams m.viewMode

        UsersModel _ ->
            Route.Users


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case ( msg, page ) of
        ( AdminMsg subMsg, AdminModel subModel ) ->
            Admin.update subMsg subModel |> updateWith AdminModel AdminMsg

        ( CreateTeamMsg (CreateTeam.GotTeamCreatedResponse (Ok team)), CreateTeamModel subModel ) ->
            ( page, Nav.pushUrl (Session.navKey subModel.session) (Route.routeToString (Route.Team team.slug)) )

        ( CreateTeamMsg subMsg, CreateTeamModel subModel ) ->
            CreateTeam.update subMsg subModel |> updateWith CreateTeamModel CreateTeamMsg

        ( DeleteTeamMsg subMsg, DeleteTeamModel subModel ) ->
            DeleteTeam.update subMsg subModel |> updateWith DeleteTeamModel DeleteTeamMsg

        ( HomeMsg subMsg, HomeModel subModel ) ->
            Home.update subMsg subModel |> updateWith HomeModel HomeMsg

        ( TeamMsg subMsg, TeamModel subModel ) ->
            Team.update subMsg subModel |> updateWith TeamModel TeamMsg

        ( TeamsMsg subMsg, TeamsModel subModel ) ->
            Teams.update subMsg subModel |> updateWith TeamsModel TeamsMsg

        ( UsersMsg subMsg, UsersModel subModel ) ->
            Users.update subMsg subModel |> updateWith UsersModel UsersMsg

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
            ( HomeModel (Home.init session r), Cmd.none )

        LoggedIn _ ->
            case r of
                Just Route.Admin ->
                    Admin.init session |> updateWith AdminModel AdminMsg

                Just Route.Home ->
                    ( HomeModel (Home.init session r)
                    , Nav.pushUrl (Session.navKey session) (Route.routeToString (Route.Teams MyTeams))
                    )

                Just Route.CreateTeam ->
                    CreateTeam.init session |> updateWith CreateTeamModel CreateTeamMsg

                Just (Route.Teams mode) ->
                    Teams.init session mode |> updateWith TeamsModel TeamsMsg

                Just (Route.Team slug) ->
                    Team.init session slug |> updateWith TeamModel TeamMsg

                Just Route.Users ->
                    Users.init session |> updateWith UsersModel UsersMsg

                Just (Route.DeleteTeam slug) ->
                    DeleteTeam.requestTeamDeletion session slug |> updateWith DeleteTeamModel DeleteTeamMsg

                Just (Route.DeleteTeamConfirm key) ->
                    DeleteTeam.confirmTeamDeletion session key |> updateWith DeleteTeamModel DeleteTeamMsg

                Nothing ->
                    ( ErrorModel (Error.init session "changeRouteTo: no route found"), Cmd.none )


view : Page -> List (Html Msg)
view page =
    Component.Layout.view
        (toRoute page)
        (toSession page)
        (case page of
            HomeModel subModel ->
                Home.view subModel |> Html.map HomeMsg

            AdminModel subModel ->
                Admin.view subModel |> Html.map AdminMsg

            TeamModel subModel ->
                Team.view subModel |> Html.map TeamMsg

            TeamsModel subModel ->
                Teams.view subModel |> Html.map TeamsMsg

            CreateTeamModel subModel ->
                CreateTeam.view subModel |> Html.map CreateTeamMsg

            UsersModel subModel ->
                Users.view subModel |> Html.map UsersMsg

            DeleteTeamModel subModel ->
                DeleteTeam.view subModel |> Html.map DeleteTeamMsg

            ErrorModel subModel ->
                Error.view subModel
        )
