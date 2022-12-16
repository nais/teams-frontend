module Page.Team exposing (..)

import Api.Do exposing (query)
import Api.Error exposing (errorToString)
import Api.Team exposing (AuditLogData, KeyValueData, SyncErrorData, TeamData, TeamMemberData, TeamSync, TeamSyncState, addMemberToTeam, addOwnerToTeam, getTeam, removeMemberFromTeam, roleString, setTeamMemberRole, teamSyncSelection, updateTeam)
import Api.User exposing (UserData)
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Mutation as Mutation
import Backend.Scalar exposing (RoleName(..), Slug, Uuid(..))
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, button, datalist, div, em, form, h2, h3, input, label, li, option, p, select, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, disabled, id, list, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import ISO8601
import List exposing (member)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session, User(..))


type EditError
    = ErrString String
    | ErrGraphql (Graphql.Http.Error TeamData)


type EditMode
    = View
    | EditMain (Maybe (Graphql.Http.Error TeamData))
    | EditMembers (Maybe EditError)


type MemberChange
    = Unchanged TeamMemberData
    | Remove TeamMemberData
    | Add TeamRole TeamMemberData
    | ChangeRole TeamRole TeamMemberData


type alias Model =
    { team : RemoteData (Graphql.Http.Error TeamData) TeamData
    , edit : EditMode
    , userList : RemoteData (Graphql.Http.Error (List UserData)) (List UserData)
    , memberChanges : List MemberChange
    , session : Session
    , addMemberQuery : String
    , addMemberRole : TeamRole
    }


type Msg
    = GotTeamResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | GotSaveOverviewResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | GotSaveTeamMembersResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | GotSynchronizeResponse (RemoteData (Graphql.Http.Error TeamSync) TeamSync)
    | ClickedEditMain
    | ClickedEditMembers
    | ClickedSaveOverview TeamData
    | ClickedSaveTeamMembers TeamData (List MemberChange)
    | ClickedCancelEditMembers
    | ClickedCancelEditOverview
    | ClickedSynchronize
    | PurposeChanged String
    | SlackAlertChannelChanged String
    | AddMemberQueryChanged String
    | RemoveMember MemberChange
    | Undo MemberChange
    | RoleDropDownClicked MemberChange String
    | AddMemberRoleDropDownClicked String
    | GotUserListResponse (RemoteData (Graphql.Http.Error (List UserData)) (List UserData))
    | OnSubmitAddMember


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

        SlackAlertChannelChanged s ->
            ( mapTeam (\team -> { team | slackAlertChannel = s }) model, Cmd.none )

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

        RoleDropDownClicked member roleString ->
            let
                role =
                    teamRoleFromString roleString

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
                    case List.head (List.filter (\u -> nameAndEmail u == model.addMemberQuery) userList) of
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


teamRoleFromString : String -> TeamRole
teamRoleFromString s =
    Maybe.withDefault Backend.Enum.TeamRole.Member <| Backend.Enum.TeamRole.fromString (String.toUpper s)


mapMemberChangeToCmds : TeamData -> MemberChange -> List (Cmd Msg)
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


initMembers : RemoteData (Graphql.Http.Error TeamData) TeamData -> List MemberChange
initMembers response =
    case response of
        Success t ->
            List.map Unchanged t.members

        _ ->
            []


memberData : MemberChange -> TeamMemberData
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


removeMember : TeamMemberData -> List MemberChange -> List MemberChange
removeMember member members =
    List.filter (\m -> not (member.user.email == (memberData m).user.email)) members


isMember : List MemberChange -> TeamMemberData -> Bool
isMember members member =
    List.filter (\m -> member.user.email == (memberData m).user.email) members
        |> List.isEmpty
        |> not


addUserToTeam : UserData -> TeamRole -> List MemberChange -> List MemberChange
addUserToTeam user role members =
    let
        member =
            { user = user, role = role }
    in
    if not (isMember members member) then
        Add role member :: members

    else
        members


mapMember : (TeamMemberData -> MemberChange) -> TeamMemberData -> MemberChange -> MemberChange
mapMember typ memberToChange m =
    if (memberData m).user.email == memberToChange.user.email then
        typ (memberData m)

    else
        m


synchronize : Slug -> Cmd Msg
synchronize slug =
    Api.Do.mutate
        (Mutation.synchronizeTeam { slug = slug } teamSyncSelection)
        (GotSynchronizeResponse << RemoteData.fromResult)


saveOverview : TeamData -> Cmd Msg
saveOverview team =
    Api.Do.mutate
        (updateTeam
            team.slug
            { purpose = Graphql.OptionalArgument.Present team.purpose
            , slackAlertsChannel = Graphql.OptionalArgument.Present team.slackAlertChannel
            }
        )
        (GotSaveOverviewResponse << RemoteData.fromResult)


mapTeam : (TeamData -> TeamData) -> Model -> Model
mapTeam fn model =
    case model.team of
        Success team ->
            { model | team = Success <| fn team }

        _ ->
            model


fetchTeam : Slug -> Cmd Msg
fetchTeam slug =
    query (getTeam slug) (RemoteData.fromResult >> GotTeamResponse)


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


timestr : ISO8601.Time -> String
timestr u =
    let
        pad len int =
            String.fromInt int |> String.padLeft len '0'
    in
    pad 4 u.year
        ++ "-"
        ++ pad 2 u.month
        ++ "-"
        ++ pad 2 u.day
        ++ " "
        ++ pad 2 u.hour
        ++ ":"
        ++ pad 2 u.minute
        ++ ":"
        ++ pad 2 u.second


actionstr : Backend.Scalar.AuditAction -> String
actionstr (Backend.Scalar.AuditAction u) =
    u


memberRow : TeamMemberData -> Html Msg
memberRow member =
    tr []
        [ td [] [ text member.user.email ]
        , td [ classList [ ( "team-owner", member.role == Owner ) ] ] [ text <| roleString member.role ]
        ]


logLine : ISO8601.Time -> String -> String -> Html msg
logLine ts actor message =
    li []
        [ p [] [ text message ]
        , div [ class "meta" ]
            [ p [] [ text (timestr ts) ]
            , p [] [ text actor ]
            ]
        ]


errorLine : SyncErrorData -> Html Msg
errorLine log =
    logLine log.timestamp log.reconcilerName log.message


auditLogLine : AuditLogData -> Html Msg
auditLogLine log =
    let
        actor =
            case log.actor of
                Nothing ->
                    actionstr log.action

                Just s ->
                    s
    in
    logLine log.createdAt actor log.message


simpleRow : String -> String -> Html msg
simpleRow header content =
    let
        headerText =
            case header of
                "slack-channel-generic" ->
                    "Generic Slack channel"

                "slack-channel-platform-alerts" ->
                    "Alerting Slack channel"

                _ ->
                    header
    in
    tr []
        [ td [] [ text headerText ]
        , td [] [ text content ]
        ]


metadataRow : KeyValueData -> Html msg
metadataRow kv =
    case kv.value of
        Just v ->
            simpleRow kv.key v

        Nothing ->
            simpleRow kv.key ""


smallButton : Msg -> String -> String -> Html Msg
smallButton msg iconClass title =
    div [ class "small button", onClick msg ]
        [ div [ classList [ ( "icon", True ), ( iconClass, True ) ] ] []
        , text title
        ]


editorButton : Msg -> User -> TeamData -> Maybe (Html Msg)
editorButton msg user team =
    if editor team user then
        Just (smallButton msg "edit" "Edit")

    else
        Nothing


syncButton : Msg -> User -> TeamData -> Maybe (Html Msg)
syncButton msg user team =
    if editor team user then
        if team.enabled then
            Just (smallButton msg "refresh" "Synchronize")

        else
            Nothing

    else
        Nothing


viewSyncErrors : TeamData -> Html Msg
viewSyncErrors team =
    let
        syncSuccess =
            case team.lastSuccessfulSync of
                Nothing ->
                    Html.text ""

                Just ts ->
                    p []
                        [ em [] [ text <| "The last successful synchronization was on " ++ timestr ts ++ "." ]
                        ]
    in
    case team.syncErrors of
        [] ->
            text ""

        _ ->
            div [ class "card error" ]
                [ h2 [] [ text "Synchronization error" ]
                , p [] [ text "Console failed to synchronize team ", strong [] [ text (slugstr team.slug) ], text " with external systems. The operations will be automatically retried. The messages below indicate what went wrong." ]
                , p [] [ text "If errors are caused by network outage, they will resolve automatically. If they persist for more than a few hours, please contact NAIS support." ]
                , syncSuccess
                , h3 [] [ text "Error messages" ]
                , ul [ class "logs" ] (List.map errorLine team.syncErrors)
                ]


syncStateRows : TeamSyncState -> List (Html msg)
syncStateRows state =
    let
        gitHub =
            case state.githubTeamSlug of
                Nothing ->
                    []

                Just slug ->
                    [ tr []
                        [ td [] [ text "GitHub team slug" ]
                        , td [] [ text <| slugstr slug ]
                        ]
                    ]

        googleWorkspaceGroupEmail =
            case state.googleWorkspaceGroupEmail of
                Nothing ->
                    []

                Just email ->
                    [ tr []
                        [ td [] [ text "Google Workspace group email" ]
                        , td [] [ text email ]
                        ]
                    ]

        azureADGroupID =
            case state.azureADGroupID of
                Nothing ->
                    []

                Just (Uuid id) ->
                    [ tr []
                        [ td [] [ text "Azure AD group ID" ]
                        , td [] [ text id ]
                        ]
                    ]

        gcpProjects =
            List.map
                (\project ->
                    tr []
                        [ td [] [ text <| "GCP project for '" ++ project.environment ++ "'" ]
                        , td [] [ text project.projectID ]
                        ]
                )
                state.gcpProjects

        naisNamespaces =
            List.map
                (\namespace ->
                    tr []
                        [ td [] [ text <| "NAIS namespace in '" ++ namespace.environment ++ "' cluster" ]
                        , td [] [ text <| slugstr namespace.namespace ]
                        ]
                )
                state.naisNamespaces
    in
    gitHub ++ googleWorkspaceGroupEmail ++ gcpProjects ++ naisNamespaces ++ azureADGroupID


viewStateTable : TeamData -> Html msg
viewStateTable team =
    let
        metaRows =
            List.map metadataRow team.metadata

        stateRows =
            case team.syncState of
                Nothing ->
                    []

                Just syncState ->
                    syncStateRows syncState

        rows =
            metaRows ++ stateRows
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Description" ]
                , th [] [ text "Value" ]
                ]
            ]
        , tbody []
            (if List.length rows == 0 then
                [ tr [] [ td [ colspan 2 ] [ text "Console has not created any resources yet" ] ] ]

             else
                rows
            )
        ]


viewTeamState : TeamData -> Html Msg
viewTeamState team =
    div [ class "card" ]
        [ h2 [] [ text "Managed resources" ]
        , viewStateTable team
        ]


viewTeamOverview : User -> TeamData -> Html Msg
viewTeamOverview user team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text <| "Team " ++ slugstr team.slug ] ]
                |> concatMaybe (syncButton ClickedSynchronize user team)
                |> concatMaybe (editorButton ClickedEditMain user team)
            )
        , p [] [ text team.purpose ]
        ]


viewEditTeamOverview : TeamData -> Maybe (Graphql.Http.Error TeamData) -> Html Msg
viewEditTeamOverview team error =
    let
        errorMessage =
            case error of
                Nothing ->
                    text ""

                Just err ->
                    div [ class "error" ] [ text <| Api.Error.errorToString err ]
    in
    div [ class "card" ]
        [ h2 [] [ text ("Team " ++ slugstr team.slug) ]
        , label [] [ text "Purpose" ]
        , input [ type_ "text", Html.Attributes.placeholder "Describe team's purpose", onInput PurposeChanged, value team.purpose ] []
        , label [] [ text "Slack alert channel" ]
        , input [ type_ "text", Html.Attributes.placeholder "#slack-alert-channel", onInput SlackAlertChannelChanged, value team.slackAlertChannel ] []
        , errorMessage
        , div [ class "button-row" ]
            [ button [ onClick (ClickedSaveOverview team) ] [ text "Save changes" ]
            , button [ class "transparent", onClick ClickedCancelEditOverview ] [ text "Cancel changes" ]
            ]
        ]


viewMembers : User -> TeamData -> Html Msg
viewMembers user team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text "Members" ] ]
                |> concatMaybe (editorButton ClickedEditMembers user team)
            )
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    ]
                ]
            , tbody []
                (if List.length team.members == 0 then
                    [ tr [] [ td [ colspan 2 ] [ text "This team has no members" ] ] ]

                 else
                    List.map memberRow team.members
                )
            ]
        ]


nameAndEmail : UserData -> String
nameAndEmail user =
    user.name ++ " <" ++ user.email ++ ">"


addUserCandidateOption : UserData -> Html msg
addUserCandidateOption user =
    option [] [ text (nameAndEmail user) ]


editMemberRow : MemberChange -> Html Msg
editMemberRow member =
    let
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

        roleSelector =
            viewRoleSelector role (RoleDropDownClicked member)

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
        [ value (roleString currentRole)
        , onInput action
        , disabled disable
        ]
        (Backend.Enum.TeamRole.list |> List.map (roleOption currentRole))


roleOption : TeamRole -> TeamRole -> Html Msg
roleOption currentRole role =
    option
        [ selected (role == currentRole)
        , value (roleString role)
        ]
        [ text (roleString role) ]


viewEditMembers : Model -> TeamData -> Maybe EditError -> Html Msg
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
                                [ button [ type_ "submit", class "small button", Html.Attributes.form "addMemberForm" ]
                                    [ div [ class "icon add" ] []
                                    , text "Add"
                                    ]
                                ]
                            ]
                            :: List.map editMemberRow model.memberChanges
                        )
                    ]
                , div [ class "button-row" ]
                    [ button [ onClick (ClickedSaveTeamMembers team model.memberChanges) ] [ text "Save members" ]
                    , button [ class "transparent", onClick ClickedCancelEditMembers ] [ text "Cancel changes" ]
                    ]
                ]
        )


viewLogs : TeamData -> Html Msg
viewLogs team =
    div [ class "card" ]
        [ h2 [] [ text "Logs" ]
        , ul [ class "logs" ] (List.map auditLogLine team.auditLogs)
        ]


viewCards : Model -> TeamData -> Html Msg
viewCards model team =
    let
        user =
            Session.user model.session
    in
    div [ class "cards" ]
        (case model.edit of
            View ->
                [ viewTeamOverview user team
                , viewSyncErrors team
                , viewMembers user team
                , viewTeamState team
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


teamRoleForUser : TeamData -> User -> Maybe TeamRole
teamRoleForUser team user =
    case user of
        LoggedIn u ->
            List.head (List.filter (\m -> m.user.id == u.id) team.members)
                |> Maybe.map (\m -> m.role)

        Anonymous ->
            Nothing

        Unknown ->
            Nothing


editor : TeamData -> User -> Bool
editor team user =
    List.any (\b -> b)
        [ Session.isGlobalAdmin user
        , teamRoleForUser team user == Just Owner
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
