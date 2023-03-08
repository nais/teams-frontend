module Page.Team exposing (EditError(..), EditMode(..), ExpandableList(..), MemberChange(..), Model, Msg(..), init, update, view)

import Api.Do exposing (query)
import Api.Error exposing (errorToString)
import Api.Str exposing (auditActionStr, roleStr, slugStr)
import Api.Team exposing (addMemberToTeam, addOwnerToTeam, getTeam, removeMemberFromTeam, setTeamMemberRole, teamSyncSelection, updateTeam)
import Api.User
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Mutation as Mutation
import Backend.Scalar exposing (Slug)
import Component.ResourceTable as ResourceTable
import DataModel exposing (AuditLog, Expandable(..), GitHubRepository, SlackAlertsChannel, SyncError, Team, TeamMember, TeamSync, User, expandableAll)
import Graphql.Http
import Graphql.OptionalArgument
import Html exposing (Html, a, button, datalist, dd, div, dl, dt, em, form, h2, h3, input, label, li, option, p, select, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, disabled, for, href, id, list, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import ISO8601
import List
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session, Viewer)


type EditError
    = ErrString String
    | ErrGraphql (Graphql.Http.Error Team)


type EditMode
    = View
    | EditMain (Maybe (Graphql.Http.Error Team))
    | EditMembers (Maybe EditError)


type MemberChange
    = Unchanged TeamMember
    | Remove TeamMember
    | Add TeamRole TeamMember
    | ChangeRole TeamRole TeamMember


type ExpandableList
    = Members
    | Repositories
    | AuditLogs


type alias Model =
    { team : RemoteData (Graphql.Http.Error Team) Team
    , edit : EditMode
    , userList : RemoteData (Graphql.Http.Error (List User)) (List User)
    , memberChanges : List MemberChange
    , session : Session
    , addMemberQuery : String
    , addMemberRole : TeamRole
    }


type Msg
    = GotTeamResponse (RemoteData (Graphql.Http.Error Team) Team)
    | GotSaveOverviewResponse (RemoteData (Graphql.Http.Error Team) Team)
    | GotSaveTeamMembersResponse (RemoteData (Graphql.Http.Error Team) Team)
    | GotSynchronizeResponse (RemoteData (Graphql.Http.Error TeamSync) TeamSync)
    | ClickedEditMain
    | ClickedEditMembers
    | ClickedSaveOverview Team
    | ClickedSaveTeamMembers Team (List MemberChange)
    | ClickedCancelEditMembers
    | ClickedCancelEditOverview
    | ClickedSynchronize
    | PurposeChanged String
    | SlackChannelChanged String
    | SlackAlertsChannelChanged String String
    | AddMemberQueryChanged String
    | RemoveMember MemberChange
    | Undo MemberChange
    | RoleDropDownClicked MemberChange String
    | AddMemberRoleDropDownClicked String
    | GotUserListResponse (RemoteData (Graphql.Http.Error (List User)) (List User))
    | OnSubmitAddMember
    | ToggleExpandableList ExpandableList


init : Session -> Backend.Scalar.Slug -> ( Model, Cmd Msg )
init session slug =
    ( { team = NotAsked
      , session = session
      , edit = View
      , userList = NotAsked
      , memberChanges = []
      , addMemberQuery = ""
      , addMemberRole = Backend.Enum.TeamRole.Member
      }
    , Cmd.batch [ fetchTeam slug, getUserList ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamResponse r ->
            ( { model | team = r }, Cmd.none )

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

        ClickedEditMembers ->
            ( { model | edit = EditMembers Nothing, memberChanges = initMembers model.team }, Cmd.none )

        PurposeChanged s ->
            ( mapTeam (\team -> { team | purpose = s }) model, Cmd.none )

        SlackChannelChanged s ->
            ( mapTeam (\team -> { team | slackChannel = s }) model, Cmd.none )

        SlackAlertsChannelChanged e s ->
            ( mapTeam (\team -> { team | slackAlertsChannels = mapSlackAlertsChannels e s team.slackAlertsChannels }) model, Cmd.none )

        ClickedSaveOverview team ->
            ( model, saveOverview team )

        AddMemberQueryChanged s ->
            ( { model | addMemberQuery = s }, Cmd.none )

        RemoveMember m ->
            ( { model | memberChanges = List.map (mapMember Remove (memberData m)) model.memberChanges }, Cmd.none )

        Undo m ->
            ( { model
                | memberChanges =
                    case m of
                        Add _ _ ->
                            removeMember (memberData m) model.memberChanges

                        _ ->
                            List.map (mapMember Unchanged (memberData m)) model.memberChanges
              }
            , Cmd.none
            )

        RoleDropDownClicked member roleStr ->
            let
                role : TeamRole
                role =
                    teamRoleFromString roleStr

                op : TeamMember -> MemberChange
                op =
                    case member of
                        Add _ _ ->
                            Add role

                        Remove _ ->
                            Remove

                        Unchanged _ ->
                            ChangeRole role

                        ChangeRole _ _ ->
                            ChangeRole role
            in
            ( { model | memberChanges = List.map (mapMember op (memberData member)) model.memberChanges }, Cmd.none )

        GotUserListResponse r ->
            ( { model | userList = r }, Cmd.none )

        OnSubmitAddMember ->
            case model.userList of
                Success userList ->
                    case queryUserList model.addMemberQuery userList of
                        Just u ->
                            ( { model | addMemberQuery = "", memberChanges = addUserToTeam u model.addMemberRole model.memberChanges, addMemberRole = Member }, Cmd.none )

                        Nothing ->
                            ( { model | edit = EditMembers (Just (ErrString "no user found in userlist")) }, Cmd.none )

                _ ->
                    ( { model | edit = EditMembers (Just (ErrString "failed to fetch userlist")) }, Cmd.none )

        ClickedCancelEditMembers ->
            ( { model | edit = View }, Cmd.none )

        ClickedCancelEditOverview ->
            case model.team of
                Success team ->
                    ( { model | edit = View }, fetchTeam team.slug )

                _ ->
                    ( { model | edit = View }, Cmd.none )

        ClickedSaveTeamMembers team changes ->
            ( model, Cmd.batch (List.concatMap (mapMemberChangeToCmds team) changes) )

        GotSaveTeamMembersResponse r ->
            case r of
                Success _ ->
                    ( { model | team = r, edit = View }, Cmd.none )

                Failure error ->
                    ( { model | edit = EditMembers (Just (ErrGraphql error)) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddMemberRoleDropDownClicked r ->
            ( { model | addMemberRole = teamRoleFromString r }, Cmd.none )

        ClickedSynchronize ->
            case model.team of
                Success t ->
                    ( model, synchronize t.slug )

                _ ->
                    ( model, Cmd.none )

        GotSynchronizeResponse _ ->
            ( model, Cmd.none )

        ToggleExpandableList l ->
            case model.team of
                Success team ->
                    case l of
                        Repositories ->
                            ( mapTeam (\t -> { t | repositories = flipExpanded team.repositories }) model, Cmd.none )

                        Members ->
                            ( mapTeam (\t -> { t | members = flipExpanded team.members }) model, Cmd.none )

                        AuditLogs ->
                            ( mapTeam (\t -> { t | auditLogs = flipExpanded team.auditLogs }) model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


flipExpanded : Expandable a -> Expandable a
flipExpanded e =
    case e of
        Preview i ->
            Expanded i

        Expanded i ->
            Preview i


teamRoleFromString : String -> TeamRole
teamRoleFromString s =
    Maybe.withDefault Backend.Enum.TeamRole.Member <| Backend.Enum.TeamRole.fromString (String.toUpper s)


mapMemberChangeToCmds : Team -> MemberChange -> List (Cmd Msg)
mapMemberChangeToCmds team change =
    case change of
        Add r m ->
            case r of
                Backend.Enum.TeamRole.Member ->
                    [ Api.Do.mutate (addMemberToTeam team m.user) (RemoteData.fromResult >> GotSaveTeamMembersResponse) ]

                Backend.Enum.TeamRole.Owner ->
                    [ Api.Do.mutate (addOwnerToTeam team m.user) (RemoteData.fromResult >> GotSaveTeamMembersResponse) ]

        Remove m ->
            [ Api.Do.mutate (removeMemberFromTeam team m.user) (RemoteData.fromResult >> GotSaveTeamMembersResponse) ]

        ChangeRole r m ->
            [ Api.Do.mutate (setTeamMemberRole team m r) (RemoteData.fromResult >> GotSaveTeamMembersResponse) ]

        Unchanged _ ->
            []


initMembers : RemoteData (Graphql.Http.Error Team) Team -> List MemberChange
initMembers response =
    case response of
        Success t ->
            List.map Unchanged (expandableAll t.members)

        _ ->
            []


queryUserList : String -> List User -> Maybe User
queryUserList query userList =
    List.filter (\u -> u.email == query) userList
        |> List.head


memberData : MemberChange -> TeamMember
memberData member =
    case member of
        Unchanged m ->
            m

        ChangeRole _ m ->
            m

        Add _ m ->
            m

        Remove m ->
            m


removeMember : TeamMember -> List MemberChange -> List MemberChange
removeMember member members =
    List.filter (\m -> not (member.user.email == (memberData m).user.email)) members


isMember : List MemberChange -> TeamMember -> Bool
isMember members member =
    List.filter (\m -> member.user.email == (memberData m).user.email) members
        |> List.isEmpty
        |> not


addUserToTeam : User -> TeamRole -> List MemberChange -> List MemberChange
addUserToTeam user role members =
    let
        member : { user : User, role : TeamRole }
        member =
            { user = user, role = role }
    in
    if not (isMember members member) then
        Add role member :: members

    else
        members


mapMember : (TeamMember -> MemberChange) -> TeamMember -> MemberChange -> MemberChange
mapMember typ memberToChange m =
    if (memberData m).user.email == memberToChange.user.email then
        typ (memberData m)

    else
        m


noMemberChanges : List MemberChange -> Bool
noMemberChanges changes =
    List.filter
        (\c ->
            case c of
                Unchanged _ ->
                    False

                _ ->
                    True
        )
        changes
        |> List.isEmpty


mapSlackAlertsChannel : String -> String -> SlackAlertsChannel -> SlackAlertsChannel
mapSlackAlertsChannel environment channelName channel =
    if environment == channel.environment then
        { channel
            | channelName =
                case channelName of
                    "" ->
                        Nothing

                    _ ->
                        Just channelName
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
                            , channelName = Graphql.OptionalArgument.fromMaybe s.channelName
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


memberRow : TeamMember -> Html Msg
memberRow member =
    tr []
        [ td [] [ text member.user.email ]
        , td [ classList [ ( "team-owner", member.role == Owner ) ] ] [ text <| roleStr member.role ]
        ]


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


smallButton : Msg -> String -> String -> Html Msg
smallButton msg iconClass title =
    div [ class "small button", onClick msg ]
        [ div [ class "icon", class iconClass ] []
        , text title
        ]


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
            div [ class "card error" ]
                [ h2 [] [ text "Synchronization error" ]
                , p [] [ text "Console failed to synchronize team ", strong [] [ text (slugStr team.slug) ], text " with external systems. The operations will be automatically retried. The messages below indicate what went wrong." ]
                , p [] [ text "If errors are caused by network outage, they will resolve automatically. If they persist for more than a few hours, please contact NAIS support." ]
                , viewSyncSuccess team
                , h3 [] [ text "Error messages" ]
                , ul [ class "logs" ] (List.map errorLine team.syncErrors)
                ]


viewTeamState : Team -> Html Msg
viewTeamState team =
    div [ class "card" ]
        [ h2 [] [ text "Managed resources" ]
        , ResourceTable.view team.syncState team.metadata
        ]


viewSlackChannel : String -> SlackAlertsChannel -> List (Html msg)
viewSlackChannel defaultSlackChannel channel =
    [ dt [] [ text channel.environment ]
    , dd []
        [ text (Maybe.withDefault defaultSlackChannel channel.channelName)
        ]
    ]


viewSlackChannels : Team -> Html msg
viewSlackChannels team =
    dl []
        (List.concatMap (viewSlackChannel team.slackChannel) team.slackAlertsChannels)


viewTeamOverview : Viewer -> Team -> Html Msg
viewTeamOverview viewer team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text <| "Team " ++ slugStr team.slug ] ]
                |> concatMaybe (deleteTeamButton viewer team)
                |> concatMaybe (syncButton ClickedSynchronize viewer team)
                |> concatMaybe (editorButton ClickedEditMain viewer team)
            )
        , p [] [ text team.purpose ]
        , h3 [] [ text "Slack channel" ]
        , p [] [ text team.slackChannel ]
        , h3 [] [ text "Slack alert channels" ]
        , p []
            [ text "Per-environment slack channels to be used for alerts sent by the platform." ]
        , viewSlackChannels team
        , viewSyncSuccess team
        ]


viewSlackAlertsChannel : String -> SlackAlertsChannel -> List (Html Msg)
viewSlackAlertsChannel placeholder entry =
    let
        inputID : String
        inputID =
            "slack-alerts-channel" ++ entry.environment

        val : String
        val =
            Maybe.withDefault "" entry.channelName
    in
    [ label [ for inputID ] [ entry.environment |> text ]
    , input [ id inputID, type_ "text", value val, Html.Attributes.placeholder placeholder, onInput (SlackAlertsChannelChanged entry.environment) ] []
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
    div [ class "card" ]
        ([ h2 [] [ text ("Team " ++ slugStr team.slug) ]
         , label [] [ text "Purpose" ]
         , input [ type_ "text", Html.Attributes.placeholder "Describe team's purpose", onInput PurposeChanged, value team.purpose ] []
         , label [] [ text "Slack channel" ]
         , input [ type_ "text", Html.Attributes.placeholder "#team-slack-channel", onInput SlackChannelChanged, value team.slackChannel ] []
         , h3 [] [ text "Slack alerts channels" ]
         , p []
            [ text "Per-environment slack channels to be used for alerts sent by the platform." ]
         ]
            ++ List.concatMap (viewSlackAlertsChannel team.slackChannel) team.slackAlertsChannels
            ++ [ errorMessage
               , div [ class "button-row" ]
                    [ button [ onClick (ClickedSaveOverview team) ] [ text "Save changes" ]
                    , button [ class "transparent", onClick ClickedCancelEditOverview ] [ text "Cancel changes" ]
                    ]
               ]
        )


viewMembers : Viewer -> Team -> Html Msg
viewMembers viewer team =
    div [ class "card" ]
        ([ div [ class "title" ]
            ([ h2 [] [ text "Members" ] ]
                |> concatMaybe (editorButton ClickedEditMembers viewer team)
            )
         , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    ]
                ]
            , tbody []
                (if List.length (unexpand team.members) == 0 then
                    [ tr [] [ td [ colspan 2 ] [ text "This team has no members" ] ] ]

                 else
                    List.map memberRow (unexpand team.members)
                )
            ]
         ]
            |> concatMaybe (showMoreButton team.members numberOfPreviewElements (ToggleExpandableList Members))
        )


addUserCandidateOption : User -> Html msg
addUserCandidateOption user =
    option [] [ text user.email ]


editMemberRow : MemberChange -> Html Msg
editMemberRow member =
    let
        role : TeamRole
        role =
            case member of
                Unchanged m ->
                    m.role

                Add r _ ->
                    r

                ChangeRole r _ ->
                    r

                Remove m ->
                    m.role

        roleSelector : Bool -> Html Msg
        roleSelector =
            viewRoleSelector role (RoleDropDownClicked member)

        viewButton : String -> String -> String -> msg -> Html msg
        viewButton cls svg txt msg =
            button [ class <| "button small " ++ cls, onClick msg ]
                [ div [ class <| "icon " ++ svg ] []
                , text txt
                ]
    in
    case member of
        Unchanged m ->
            tr []
                [ td [] [ text m.user.email ]
                , td [] [ roleSelector False ]
                , td [] [ viewButton "" "delete" "Remove" (RemoveMember member) ]
                ]

        Remove m ->
            tr []
                [ td [ class "strikethrough" ] [ text m.user.email ]
                , td [] [ roleSelector True ]
                , td [] [ viewButton "transparent" "cancel" "Undo" (Undo member) ]
                ]

        Add _ m ->
            tr []
                [ td [] [ text m.user.email ]
                , td [] [ roleSelector False ]
                , td [] [ viewButton "transparent" "cancel" "Undo" (Undo member) ]
                ]

        ChangeRole _ m ->
            tr []
                [ td [] [ text m.user.email ]
                , td [] [ roleSelector False, text " *" ]
                , td [] [ viewButton "transparent" "cancel" "Undo" (Undo member) ]
                ]


viewRoleSelector : TeamRole -> (String -> Msg) -> Bool -> Html Msg
viewRoleSelector currentRole action disable =
    select
        [ value (roleStr currentRole)
        , onInput action
        , disabled disable
        ]
        (Backend.Enum.TeamRole.list |> List.map (roleOption currentRole))


roleOption : TeamRole -> TeamRole -> Html Msg
roleOption currentRole role =
    option
        [ selected (role == currentRole)
        , value (roleStr role)
        ]
        [ text (roleStr role) ]


viewEditMembers : Model -> Team -> Maybe EditError -> Html Msg
viewEditMembers model team _ =
    div [ class "card" ]
        (case model.userList of
            NotAsked ->
                [ text "userlist not asked for" ]

            Failure _ ->
                [ text "failed" ]

            Loading ->
                [ text "loading" ]

            Success userList ->
                [ h2 [] [ text "Members" ]
                , form [ id "addMemberForm", onSubmit OnSubmitAddMember ] []
                , table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Email" ]
                            , th [] [ text "Role" ]
                            , th [] [ text "" ]
                            ]
                        ]
                    , tbody []
                        (tr []
                            [ td []
                                [ input [ list "userCandidates", Html.Attributes.form "addMemberForm", type_ "text", value model.addMemberQuery, onInput AddMemberQueryChanged ] []
                                , datalist [ id "userCandidates" ] (List.map addUserCandidateOption userList)
                                ]

                            --
                            , td [] [ viewRoleSelector model.addMemberRole AddMemberRoleDropDownClicked False ]
                            , td [ colspan 2 ]
                                [ button [ type_ "submit", class "small button", Html.Attributes.form "addMemberForm", disabled (queryUserList model.addMemberQuery userList == Nothing) ]
                                    [ div [ class "icon add" ] []
                                    , text "Add"
                                    ]
                                ]
                            ]
                            :: List.map editMemberRow model.memberChanges
                        )
                    ]
                , div [ class "button-row" ]
                    [ button [ disabled (noMemberChanges model.memberChanges || not (model.addMemberQuery == "")), onClick (ClickedSaveTeamMembers team model.memberChanges) ] [ text "Save members" ]
                    , button [ class "transparent", onClick ClickedCancelEditMembers ] [ text "Cancel changes" ]
                    ]
                ]
        )


unexpand : Expandable (List a) -> List a
unexpand l =
    case l of
        Preview p ->
            List.take numberOfPreviewElements p

        Expanded e ->
            e


viewLogs : Team -> Html Msg
viewLogs team =
    div [ class "card" ]
        ([ h2 [] [ text "Logs" ]
         , ul [ class "logs" ] (team.auditLogs |> unexpand |> List.map auditLogLine)
         ]
            |> concatMaybe (showMoreButton team.auditLogs numberOfPreviewElements (ToggleExpandableList AuditLogs))
        )


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
                , viewMembers user team
                , viewTeamState team
                , viewGitHubRepositories team
                , viewLogs team
                ]

            EditMain err ->
                [ viewEditTeamOverview team err
                , viewSyncErrors team
                , viewMembers user team
                , viewTeamState team
                , viewLogs team
                ]

            EditMembers err ->
                [ viewTeamOverview user team
                , viewSyncErrors team
                , viewEditMembers model team err
                , viewTeamState team
                , viewLogs team
                ]
        )


numberOfPreviewElements : Int
numberOfPreviewElements =
    5


viewGitHubRepositories : Team -> Html Msg
viewGitHubRepositories team =
    div [ class "card" ]
        ([ h2 [] [ text "Repositories" ]
         , p []
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
            div [ class "card error" ] [ text <| errorToString err ]

        Loading ->
            div [ class "card" ] [ text "Loading data..." ]

        NotAsked ->
            div [ class "card" ] [ text "No data loaded" ]


teamRoleForViewer : List TeamMember -> Viewer -> Maybe TeamRole
teamRoleForViewer members viewer =
    Session.user viewer
        |> Maybe.andThen
            (\u ->
                List.filter (\m -> m.user.id == u.id) members |> List.head |> Maybe.map (\m -> m.role)
            )


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
            list ++ [ toConcat ]

        Nothing ->
            list
