module Main exposing (Model(..), Msg(..), main)

import Api.Do exposing (query)
import Api.Str exposing (slugStr)
import Api.User
import Browser exposing (Document)
import Browser.Navigation as Nav
import DataModel exposing (User)
import Graphql.Http
import Html exposing (Html, a, div, h1, header, li, main_, nav, p, text, ul)
import Html.Attributes exposing (class, classList, href, id)
import Page.CreateTeam as CreateTeam
import Page.DeleteTeam as DeleteTeam
import Page.Error as Error
import Page.Home as Home
import Page.ReconcilerAdmin as ReconcilerAdmin
import Page.Team as Team
import Page.Teams as Teams
import Page.Users as Users
import RemoteData exposing (RemoteData(..))
import Route exposing (Route, link)
import Session exposing (Session, Viewer(..))
import Url
import Url.Builder



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
            ( Home (Home.init session r), getMe r )

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

        user : Viewer
        user =
            Session.viewer (toSession model)

        auth : Html msg
        auth =
            case user of
                LoggedIn loggedInUser ->
                    let
                        logoutURL : String
                        logoutURL =
                            Url.Builder.absolute [ "oauth2", "logout" ] []
                    in
                    div [ class "user-info" ]
                        [ p [] [ text loggedInUser.name ]
                        , a [ href logoutURL, class "button small" ] [ text "Logout" ]
                        ]

                _ ->
                    let
                        loginURL : String
                        loginURL =
                            Url.Builder.absolute [ "oauth2", "login" ] []
                    in
                    a [ href loginURL, class "button small" ] [ text "Login" ]
    in
    { title = "NAIS console"
    , body =
        [ header []
            [ div [ class "content" ]
                [ div []
                    [ div [ id "logo" ] []
                    , h1 []
                        [ link Route.Home [] [ text "Console" ]
                        ]
                    ]
                , auth
                ]
            ]
        , div [ id "layout" ]
            [ viewNav model
            , main_ []
                [ html ]
            , div [] []
            ]
        ]
    }


viewNav : Model -> Html msg
viewNav model =
    case Session.viewer (toSession model) of
        LoggedIn _ ->
            let
                teamsButton : List (Html msg)
                teamsButton =
                    [ menuItem model Route.MyTeams False "Teams" ]

                adminButtons : List (Html msg)
                adminButtons =
                    if Session.isGlobalAdmin (Session.viewer (toSession model)) then
                        [ menuItem model Route.ReconcilerAdmin False "Synchronizers"
                        , menuItem model Route.Users False "Users"
                        ]

                    else
                        []

                ephemeralButtons : List (Html msg)
                ephemeralButtons =
                    case model of
                        Team teamPage ->
                            case teamPage.team of
                                Success team ->
                                    [ menuItem model (Route.Team team.slug) True (slugStr team.slug) ]

                                _ ->
                                    []

                        DeleteTeam deletePage ->
                            [ menuItem model (Route.DeleteTeam deletePage.slug) True "Delete team"
                            ]

                        CreateTeam _ ->
                            [ menuItem model Route.CreateTeam True "Create team"
                            ]

                        _ ->
                            []
            in
            nav [] [ ul [] (teamsButton ++ ephemeralButtons ++ adminButtons) ]

        _ ->
            nav [] []


isActiveRoute : Model -> Route -> Bool
isActiveRoute model target =
    case ( model, target ) of
        ( Home _, Route.Home ) ->
            True

        ( Teams _, Route.MyTeams ) ->
            True

        ( CreateTeam _, Route.CreateTeam ) ->
            True

        ( Team _, Route.Team _ ) ->
            True

        ( Admin _, Route.ReconcilerAdmin ) ->
            True

        ( Users _, Route.Users ) ->
            True

        ( DeleteTeam _, Route.DeleteTeam _ ) ->
            True

        ( DeleteTeam _, Route.DeleteTeamConfirm _ ) ->
            True

        _ ->
            False


menuItem : Model -> Route -> Bool -> String -> Html.Html msg
menuItem model target indent title =
    let
        classes : List ( String, Bool )
        classes =
            [ ( "active", isActiveRoute model target ) -- Remember to update isActiveRoute with model/route combo
            , ( "indent", indent ) -- Remember to update isActiveRoute with model/route combo
            ]
    in
    li [ classList classes ] [ link target [ Html.Attributes.title title ] [ text title ] ]



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


getMe : Maybe Route -> Cmd Msg
getMe r =
    query Api.User.getMe (GotMeResponse r)


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
