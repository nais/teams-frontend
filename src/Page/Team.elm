port module Page.Team exposing (EditMode(..), ExpandableList(..), Model, Msg(..), SubModel, copy, init, update, view)

import Api.Do exposing (query, queryRD)
import Api.Error exposing (errorToString)
import Api.Str exposing (auditActionStr, deployKeyStr, slugStr)
import Api.Team exposing (getTeam, teamSyncSelection, updateTeam)
import Api.User
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Mutation as Mutation
import Backend.Scalar exposing (Slug)
import Component.Buttons exposing (smallButton)
import Component.Card as Card
import Component.ResourceTable as ResourceTable
import DataModel exposing (AuditLog, DeployKey, Expandable(..), GitHubRepository, SlackAlertsChannel, SyncError, Team, TeamMember, TeamSync, User, expandableAll)
import Graphql.Http
import Graphql.OptionalArgument
import Html exposing (Html, a, button, dd, div, dl, dt, em, form, h3, input, label, li, p, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, disabled, for, href, id, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import ISO8601
import List
import Page.Team.Members as Members
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session, Viewer)
import Util exposing (flattenMaybe)


port copy : String -> Cmd msg


type EditMode
    = View
    | EditMain (Maybe (Graphql.Http.Error Team))


type ExpandableList
    = Repositories
    | AuditLogs


type SubModel
    = NotInitialized
    | MembersModel Members.Model


type alias Model =
    { team : RemoteData (Graphql.Http.Error Team) Team
    , slug : Slug
    , edit : EditMode
    , userList : RemoteData (Graphql.Http.Error (List User)) (List User)
    , deployKey : RemoteData (Graphql.Http.Error DeployKey) DeployKey
    , session : Session
    , membersModel : SubModel
    , error : String -- TODO RENDER
    }


type Msg
    = GotTeamResponse (RemoteData (Graphql.Http.Error Team) Team)
    | GotUserListResponse (RemoteData (Graphql.Http.Error (List User)) (List User))
    | GotDeployKey (RemoteData (Graphql.Http.Error DeployKey) DeployKey)
    | GotSaveOverviewResponse (RemoteData (Graphql.Http.Error Team) Team)
    | GotSynchronizeResponse (RemoteData (Graphql.Http.Error TeamSync) TeamSync)
    | GotMembersMsg Members.Msg
    | ClickedEditMain
    | ClickedSaveOverview Team
    | ClickedCancelEditOverview
    | ClickedSynchronize
    | ClickedGetDeployKeys Slug
    | PurposeChanged String
    | SlackChannelChanged String
    | SlackAlertsChannelChanged String String
    | ToggleExpandableList ExpandableList
    | Copy String


init : Session -> Backend.Scalar.Slug -> ( Model, Cmd Msg )
init session slug =
    ( { team = NotAsked
      , slug = slug
      , session = session
      , edit = View
      , userList = NotAsked
      , deployKey = NotAsked
      , membersModel = NotInitialized
      , error = ""
      }
    , Cmd.batch [ fetchTeam slug, getUserList ]
    )


initSubModels : Model -> Model
initSubModels model =
    let
        t : RemoteData String Team
        t =
            RemoteData.mapError errorToString model.team

        u : RemoteData String (List User)
        u =
            RemoteData.mapError errorToString model.userList
    in
    case RemoteData.append t u of
        Success ( team, allUsers ) ->
            { model
                | membersModel = MembersModel (Members.init team allUsers (editor team (Session.viewer model.session)))
            }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamResponse r ->
            ( { model | team = r } |> initSubModels, Cmd.none )

        GotUserListResponse r ->
            ( { model | userList = r } |> initSubModels, Cmd.none )

        GotSaveOverviewResponse r ->
            case r of
                Success _ ->
                    ( { model | team = r, edit = View }, Cmd.none )

                Failure error ->
                    ( { model | edit = EditMain (Just error) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedEditMain ->
            ( { model | edit = EditMain Nothing }, Cmd.none )

        PurposeChanged s ->
            ( mapTeam (\team -> { team | purpose = s }) model, Cmd.none )

        SlackChannelChanged s ->
            ( mapTeam (\team -> { team | slackChannel = s }) model, Cmd.none )

        SlackAlertsChannelChanged e s ->
            ( mapTeam (\team -> { team | slackAlertsChannels = mapSlackAlertsChannels e s team.slackAlertsChannels }) model, Cmd.none )

        ClickedSaveOverview team ->
            ( model, saveOverview team )

        ClickedCancelEditOverview ->
            case model.team of
                Success team ->
                    ( { model | edit = View }, fetchTeam team.slug )

                _ ->
                    ( { model | edit = View }, Cmd.none )

        ClickedSynchronize ->
            case model.team of
                Success t ->
                    ( model, synchronize t.slug )

                _ ->
                    ( model, Cmd.none )

        GotSynchronizeResponse r ->
            case r of
                Failure e ->
                    ( { model | error = Api.Error.errorToString e }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleExpandableList l ->
            case model.team of
                Success team ->
                    case l of
                        Repositories ->
                            ( mapTeam (\t -> { t | repositories = flipExpanded team.repositories }) model, Cmd.none )

                        AuditLogs ->
                            ( mapTeam (\t -> { t | auditLogs = flipExpanded team.auditLogs }) model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedGetDeployKeys slug ->
            ( { model | deployKey = Loading }, queryRD (Api.Team.getDeployKey slug) GotDeployKey )

        GotDeployKey r ->
            ( { model | deployKey = r }, Cmd.none )

        Copy s ->
            ( model, copy s )

        GotMembersMsg subMsg ->
            case model.membersModel of
                MembersModel subModel ->
                    Members.update subMsg subModel
                        |> subUpdate MembersModel GotMembersMsg
                        |> into model

                _ ->
                    -- TODO: render error somewhere, as this should not happen
                    ( model, Cmd.none )


flipExpanded : Expandable a -> Expandable a
flipExpanded e =
    case e of
        Preview i ->
            Expanded i

        Expanded i ->
            Preview i


mapSlackAlertsChannel : String -> String -> SlackAlertsChannel -> SlackAlertsChannel
mapSlackAlertsChannel environment channelName channel =
    if environment == channel.environment then
        { channel
            | channelName = channelName
        }

    else
        channel


mapSlackAlertsChannels : String -> String -> List SlackAlertsChannel -> List SlackAlertsChannel
mapSlackAlertsChannels environment channelName channels =
    List.map (mapSlackAlertsChannel environment channelName) channels


synchronize : Slug -> Cmd Msg
synchronize slug =
    Api.Do.mutateRD
        (Mutation.synchronizeTeam { slug = slug } teamSyncSelection)
        GotSynchronizeResponse


saveOverview : Team -> Cmd Msg
saveOverview team =
    Api.Do.mutate
        (updateTeam
            team.slug
            { purpose = Graphql.OptionalArgument.Present team.purpose
            , slackChannel = Graphql.OptionalArgument.Present team.slackChannel
            , slackAlertsChannels =
                Graphql.OptionalArgument.Present
                    (List.map
                        (\s ->
                            { environment = s.environment
                            , channelName =
                                if s.channelName == team.slackChannel then
                                    Graphql.OptionalArgument.Absent

                                else
                                    Graphql.OptionalArgument.Present s.channelName
                            }
                        )
                        team.slackAlertsChannels
                    )
            }
        )
        (GotSaveOverviewResponse << RemoteData.fromResult)


mapTeam : (Team -> Team) -> Model -> Model
mapTeam fn model =
    case model.team of
        Success team ->
            { model | team = Success <| fn team }

        _ ->
            model


fetchTeam : Slug -> Cmd Msg
fetchTeam slug =
    query (getTeam slug) (RemoteData.fromResult >> GotTeamResponse)


logLine : ISO8601.Time -> String -> String -> Html msg
logLine ts actor message =
    li []
        [ p [] [ text message ]
        , div [ class "meta" ]
            [ p [] [ text (ISO8601.toString ts) ]
            , p [] [ text actor ]
            ]
        ]


errorLine : SyncError -> Html Msg
errorLine log =
    logLine log.timestamp log.reconcilerName log.message


auditLogLine : AuditLog -> Html Msg
auditLogLine log =
    let
        actor : String
        actor =
            case log.actor of
                Nothing ->
                    auditActionStr log.action

                Just s ->
                    s
    in
    logLine log.createdAt actor log.message


editorButton : Msg -> Viewer -> Team -> Maybe (Html Msg)
editorButton msg viewer team =
    if editor team viewer then
        Just (smallButton msg "edit" "Edit")

    else
        Nothing


deleteTeamButton : Viewer -> Team -> Maybe (Html Msg)
deleteTeamButton user team =
    if editor team user then
        Just
            (Route.link (Route.DeleteTeam team.slug)
                [ class "nostyle" ]
                [ div [ class "small button danger" ]
                    [ div [ class "icon", class "delete-red" ] []
                    , text "Delete"
                    ]
                ]
            )

    else
        Nothing


syncButton : Msg -> Viewer -> Team -> Maybe (Html Msg)
syncButton msg viewer team =
    if editor team viewer then
        Just (smallButton msg "synchronize" "Synchronize")

    else
        Nothing


viewSyncSuccess : Team -> Html msg
viewSyncSuccess team =
    case team.lastSuccessfulSync of
        Nothing ->
            Html.text ""

        Just ts ->
            p []
                [ em [] [ text <| "Status: last fully synchronized on " ++ ISO8601.toString ts ++ "." ]
                ]


viewSyncErrors : Team -> Html Msg
viewSyncErrors team =
    case team.syncErrors of
        [] ->
            text ""

        _ ->
            Card.new "Synchronization error"
                |> Card.withAttributes [ class "error" ]
                |> Card.withContents
                    [ p [] [ text "Console failed to synchronize team ", strong [] [ text (slugStr team.slug) ], text " with external systems. The operations will be automatically retried. The messages below indicate what went wrong." ]
                    , p [] [ text "If errors are caused by network outage, they will resolve automatically. If they persist for more than a few hours, please contact NAIS support." ]
                    , viewSyncSuccess team
                    , h3 [] [ text "Error messages" ]
                    , ul [ class "logs" ] (List.map errorLine team.syncErrors)
                    ]
                |> Card.render


viewTeamState : Team -> Html Msg
viewTeamState team =
    Card.new "Managed resources"
        |> Card.withContents [ ResourceTable.view team.syncState ]
        |> Card.render


viewSlackChannel : SlackAlertsChannel -> List (Html msg)
viewSlackChannel channel =
    [ dt [] [ text channel.environment ]
    , dd []
        [ text channel.channelName
        ]
    ]


viewSlackChannels : Team -> Html msg
viewSlackChannels team =
    dl []
        (List.concatMap viewSlackChannel team.slackAlertsChannels)


viewTeamOverview : Viewer -> Team -> Html Msg
viewTeamOverview viewer team =
    Card.new ("Team " ++ slugStr team.slug)
        |> Card.withButtons
            ([ deleteTeamButton viewer team
             , syncButton ClickedSynchronize viewer team
             , editorButton ClickedEditMain viewer team
             ]
                |> flattenMaybe
            )
        |> Card.withContents
            [ p [] [ text team.purpose ]
            , h3 [] [ text "Slack channel" ]
            , p [] [ text team.slackChannel ]
            , h3 [] [ text "Slack alert channels" ]
            , p []
                [ text "Per-environment slack channels to be used for alerts sent by the platform." ]
            , viewSlackChannels team
            , viewSyncSuccess team
            ]
        |> Card.render


viewEditSlackAlertsChannel : String -> SlackAlertsChannel -> Html Msg
viewEditSlackAlertsChannel placeholder entry =
    let
        inputID : String
        inputID =
            "slack-alerts-channel" ++ entry.environment
    in
    li []
        [ label [ for inputID ] [ text entry.environment ]
        , input
            [ id inputID
            , type_ "text"
            , value entry.channelName
            , Html.Attributes.placeholder placeholder
            , onInput (SlackAlertsChannelChanged entry.environment)
            ]
            []
        ]


viewEditTeamOverview : Team -> Maybe (Graphql.Http.Error Team) -> Html Msg
viewEditTeamOverview team error =
    let
        errorMessage : Html msg
        errorMessage =
            case error of
                Nothing ->
                    text ""

                Just err ->
                    div [ class "error" ] [ text <| Api.Error.errorToString err ]
    in
    Card.new ("Team " ++ slugStr team.slug)
        |> Card.withContents
            [ form [ onSubmit (ClickedSaveOverview team) ]
                [ ul []
                    ([ li []
                        [ label [] [ text "Purpose" ]
                        , input [ type_ "text", Html.Attributes.placeholder "Describe team's purpose", onInput PurposeChanged, value team.purpose ] []
                        ]
                     , li []
                        [ label [] [ text "Slack channel" ]
                        , input [ type_ "text", Html.Attributes.placeholder "#team-slack-channel", onInput SlackChannelChanged, value team.slackChannel ] []
                        ]
                     , li []
                        [ h3 [] [ text "Slack alerts channels" ]
                        , p []
                            [ text "Per-environment slack channels to be used for alerts sent by the platform." ]
                        ]
                     ]
                        ++ List.map (viewEditSlackAlertsChannel team.slackChannel) team.slackAlertsChannels
                        ++ [ errorMessage
                           , div [ class "button-row" ]
                                [ input [ type_ "submit", class "button", value "Save changes" ] []
                                , button [ class "transparent", onClick ClickedCancelEditOverview ] [ text "Cancel changes" ]
                                ]
                           ]
                    )
                ]
            ]
        |> Card.render


unexpand : Expandable (List a) -> List a
unexpand l =
    case l of
        Preview p ->
            List.take numberOfPreviewElements p

        Expanded e ->
            e


viewLogs : Team -> Html Msg
viewLogs team =
    Card.new "Logs"
        |> Card.withContents
            ([ ul [ class "logs" ] (team.auditLogs |> unexpand |> List.map auditLogLine) ]
                |> concatMaybe (showMoreButton team.auditLogs numberOfPreviewElements (ToggleExpandableList AuditLogs))
            )
        |> Card.render


viewCards : Model -> Team -> Html Msg
viewCards model team =
    let
        user : Viewer
        user =
            Session.viewer model.session
    in
    div [ class "cards" ]
        (case model.edit of
            View ->
                [ viewTeamOverview user team
                , viewSyncErrors team
                , viewSubModel model.membersModel
                , viewTeamState team
                , viewGitHubRepositories team
                , viewLogs team
                , viewDeployKey user team model.deployKey
                ]

            EditMain err ->
                [ viewEditTeamOverview team err
                , viewSyncErrors team
                , viewSubModel model.membersModel
                , viewTeamState team
                , viewLogs team
                , viewDeployKey user team model.deployKey
                ]
        )


viewDeployKey : Viewer -> Team -> RemoteData (Graphql.Http.Error DeployKey) DeployKey -> Html Msg
viewDeployKey viewer team deployKey =
    if not (isMemberOrOwner team viewer) then
        div [] []

    else
        Card.new "Deploy key"
            |> Card.withContents
                [ div [ class "row" ]
                    [ div [ class "column wide" ]
                        [ p [] [ text "This is the api key used to communicate with nais deploy." ]
                        , case deployKey of
                            NotAsked ->
                                smallButton (ClickedGetDeployKeys team.slug) "download" "get deploy key"

                            Loading ->
                                text "loading"

                            Success d ->
                                div [ class "row" ]
                                    [ input [ type_ "text", class "deploykey", disabled True, value (deployKeyStr d.key) ] []
                                    , smallButton (Copy (deployKeyStr d.key)) "copy" "copy"
                                    ]

                            Failure e ->
                                text (Api.Error.errorToString e)
                        ]
                    ]
                ]
            |> Card.render


numberOfPreviewElements : Int
numberOfPreviewElements =
    5


viewGitHubRepositories : Team -> Html Msg
viewGitHubRepositories team =
    Card.new "Repositories"
        |> Card.withContents
            ([ p []
                [ text "These are repositories that "
                , strong [] [ text (slugStr team.slug) ]
                , text " has access to. If it has the "
                , strong [] [ text "push" ]
                , text " permission it will be able to push images to this teams artifact registry."
                ]
             , table [ class "repolist" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Repository" ]
                        , th [] [ text "Permissions" ]
                        ]
                    ]
                , tbody []
                    ((case team.repositories of
                        Preview r ->
                            List.take numberOfPreviewElements r

                        Expanded r ->
                            r
                     )
                        |> List.map viewGitHubRepository
                    )
                ]
             ]
                |> concatMaybe
                    (showMoreButton team.repositories numberOfPreviewElements (ToggleExpandableList Repositories))
            )
        |> Card.render


showMoreButton : Expandable (List a) -> Int -> Msg -> Maybe (Html Msg)
showMoreButton expandable previewSize msg =
    let
        ( belowPreview, t ) =
            case expandable of
                Preview list ->
                    if List.length list > previewSize then
                        ( False, "show more" )

                    else
                        ( True, "" )

                Expanded _ ->
                    ( False, "show less" )
    in
    if belowPreview then
        Nothing

    else
        div []
            [ button
                [ class "text", onClick msg ]
                [ text t ]
            ]
            |> Just


viewGitHubRepository : GitHubRepository -> Html msg
viewGitHubRepository repository =
    tr []
        [ td [] [ a [ href ("https://github.com/" ++ repository.name) ] [ text repository.name ] ]
        , td []
            [ repository.permissions
                |> List.filter (\p -> p.granted)
                |> List.map (\p -> p.name)
                |> String.join ", "
                |> text
            ]
        ]


view : Model -> Html Msg
view model =
    case model.team of
        Success team ->
            viewCards model team

        Failure err ->
            Card.new "Loading"
                |> Card.withContents [ errorToString err |> text ]
                |> Card.withAttributes [ class "error" ]
                |> Card.render

        Loading ->
            Card.new "Loading"
                |> Card.withContents [ text "Loading data..." ]
                |> Card.render

        NotAsked ->
            Card.new "Loading"
                |> Card.withContents [ text "No data loaded" ]
                |> Card.render


teamRoleForViewer : List TeamMember -> Viewer -> Maybe TeamRole
teamRoleForViewer members viewer =
    Session.user viewer
        |> Maybe.andThen
            (\u ->
                List.filter (\m -> m.user.id == u.id) members |> List.head |> Maybe.map (\m -> m.role)
            )


isMemberOrOwner : Team -> Viewer -> Bool
isMemberOrOwner team viewer =
    List.any (\b -> b)
        [ teamRoleForViewer (expandableAll team.members) viewer == Just Member
        , teamRoleForViewer (expandableAll team.members) viewer == Just Owner
        ]


editor : Team -> Viewer -> Bool
editor team viewer =
    List.any (\b -> b)
        [ Session.isGlobalAdmin viewer
        , teamRoleForViewer (expandableAll team.members) viewer == Just Owner
        ]


getUserList : Cmd Msg
getUserList =
    Api.Do.query
        Api.User.getAllUsers
        (RemoteData.fromResult >> GotUserListResponse)


concatMaybe : Maybe a -> List a -> List a
concatMaybe maybe list =
    case maybe of
        Just toConcat ->
            list ++ List.singleton toConcat

        Nothing ->
            list



-- SubModel stuff


viewSubModel : SubModel -> Html Msg
viewSubModel subModel =
    case subModel of
        MembersModel m ->
            Members.view m |> Html.map GotMembersMsg

        NotInitialized ->
            Card.new "not initalized" |> Card.render


into : Model -> ( SubModel, Cmd Msg ) -> ( Model, Cmd Msg )
into model ( subModel, cmd ) =
    case subModel of
        MembersModel m ->
            ( { model | membersModel = MembersModel m }, cmd )

        NotInitialized ->
            ( model, cmd )


subUpdate : (subModel -> SubModel) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( SubModel, Cmd Msg )
subUpdate toSubModel toMsg ( subModel, subMsg ) =
    ( toSubModel subModel
    , Cmd.map toMsg subMsg
    )
