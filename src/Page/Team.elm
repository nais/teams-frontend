module Page.Team exposing (..)

import Api.Do exposing (query)
import Api.Error exposing (errorToString)
import Api.Team exposing (AuditLogData, KeyValueData, SyncErrorData, TeamData, TeamMemberData, getTeam, roleString, updateTeam)
import Api.User exposing (RoleData, UserData)
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (RoleName(..), Slug)
import Dict exposing (remove)
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, button, datalist, div, em, form, h2, h3, input, li, option, p, select, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, disabled, id, list, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import ISO8601
import List exposing (member)
import Page.EditTeam exposing (Msg(..), nameAndEmail)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session, User(..))


type EditMode
    = View
    | EditMain (Maybe (Graphql.Http.Error TeamData))
    | EditMembers (Maybe (Graphql.Http.Error TeamData))


type Member
    = Unchanged TeamMemberData
    | Remove TeamMemberData
    | Add TeamRole TeamMemberData
    | ChangeRole TeamRole TeamMemberData


type alias Model =
    { team : RemoteData (Graphql.Http.Error TeamData) TeamData
    , edit : EditMode
    , members : List Member
    , session : Session
    , addMemberQuery : String
    , userList : RemoteData (Graphql.Http.Error (List UserData)) (List UserData)
    }


type Msg
    = GotTeamResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | GotSaveOverviewResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | ClickedEditMain
    | ClickedEditMembers TeamData
    | ClickedSaveOverview TeamData
    | PurposeChanged String
    | AddMemberQueryChanged String
    | RemoveMember Member
    | Undo Member
    | AddMember TeamRole TeamMemberData
    | RoleDropDownClicked TeamRole Member
    | GotUserListResponse (RemoteData (Graphql.Http.Error (List UserData)) (List UserData))
    | OnSubmitAddMember


init : Session -> Backend.Scalar.Slug -> ( Model, Cmd Msg )
init session slug =
    ( { team = NotAsked
      , session = session
      , edit = View
      , userList = NotAsked
      , members = []
      , addMemberQuery = ""
      }
    , Cmd.batch [ fetchTeam slug, getUserList ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamResponse r ->
            ( { model | team = r, members = initMembers r }, Cmd.none )

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

        ClickedEditMembers team ->
            ( { model | edit = EditMembers Nothing }, Cmd.none )

        PurposeChanged s ->
            ( mapTeam (\team -> { team | purpose = s }) model, Cmd.none )

        ClickedSaveOverview team ->
            ( model, saveOverview team )

        AddMemberQueryChanged s ->
            ( { model | addMemberQuery = s }, Cmd.none )

        RemoveMember m ->
            ( { model | members = List.map (mapMember Remove (memberData m)) model.members }, Cmd.none )

        AddMember r m ->
            ( { model | members = model.members ++ [ Add r m ] }, Cmd.none )

        Undo m ->
            ( { model
                | members =
                    case m of
                        Add _ _ ->
                            removeMember (memberData m) model.members

                        _ ->
                            List.map (mapMember Unchanged (memberData m)) model.members
              }
            , Cmd.none
            )

        RoleDropDownClicked role member ->
            let
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
            ( { model | members = List.map (mapMember op (memberData member)) model.members }, Cmd.none )

        GotUserListResponse r ->
            ( { model | userList = r }, Cmd.none )

        OnSubmitAddMember ->
            case model.userList of
                Success userList ->
                    case List.head (List.filter (\u -> nameAndEmail u == model.addMemberQuery) userList) of
                        Just u ->
                            ( { model
                                | addMemberQuery = ""
                                , members =
                                    List.map
                                        (mapMember (Add Backend.Enum.TeamRole.Member)
                                            { user = u, role = Backend.Enum.TeamRole.Member }
                                        )
                                        model.members
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


initMembers : RemoteData (Graphql.Http.Error TeamData) TeamData -> List Member
initMembers response =
    case response of
        Success t ->
            List.map Unchanged t.members

        _ ->
            []


memberData : Member -> TeamMemberData
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


removeMember : TeamMemberData -> List Member -> List Member
removeMember member members =
    List.filter (\m -> not (member.user.email == (memberData m).user.email)) members


mapMember : (TeamMemberData -> Member) -> TeamMemberData -> Member -> Member
mapMember typ memberToChange m =
    if (memberData m).user.email == memberToChange.user.email then
        typ (memberData m)

    else
        m


saveOverview : TeamData -> Cmd Msg
saveOverview team =
    Api.Do.mutate
        (updateTeam
            team.slug
            { purpose = Graphql.OptionalArgument.Present team.purpose
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
    tr []
        [ td [] [ text header ]
        , td [] [ text content ]
        ]


metadataRow : KeyValueData -> Html msg
metadataRow kv =
    case kv.value of
        Just v ->
            simpleRow kv.key v

        Nothing ->
            simpleRow kv.key ""


editorButton : Msg -> Model -> TeamData -> List (Html Msg)
editorButton msg model team =
    if editor team (Session.user model.session) then
        [ div [ class "small button", onClick msg ] [ text "Edit" ] ]

    else
        []


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
                , p [] [ text "If errors are caused by network outage, they will resolve automatically. If they persist for more than a few hours, please NAIS support." ]
                , syncSuccess
                , h3 [] [ text "Error messages" ]
                , ul [ class "logs" ] (List.map errorLine team.syncErrors)
                ]


viewTeamMetaTable : List KeyValueData -> Html msg
viewTeamMetaTable metadata =
    case metadata of
        [] ->
            text ""

        _ ->
            table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Key" ]
                        , th [] [ text "Value" ]
                        ]
                    ]
                , tbody []
                    (List.map metadataRow metadata)
                ]


viewTeamOverview : Model -> TeamData -> Html Msg
viewTeamOverview model team =
    div [ class "card" ]
        [ div [ class "title" ]
            (h2 [] [ text ("Team " ++ slugstr team.slug) ]
                :: editorButton ClickedEditMain model team
            )
        , p [] [ text team.purpose ]
        , viewTeamMetaTable team.metadata
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
        , input [ type_ "text", Html.Attributes.placeholder "Describe team's purpose", onInput PurposeChanged, value team.purpose ] []
        , errorMessage
        , button [ onClick (ClickedSaveOverview team) ] [ text "Save changes" ]
        , viewTeamMetaTable team.metadata
        ]


viewMembers : Model -> TeamData -> Html Msg
viewMembers model team =
    div [ class "card" ]
        [ div [ class "title" ]
            (h2 [] [ text "Members" ]
                :: editorButton (ClickedEditMembers team) model team
            )
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    ]
                ]
            , tbody [] (List.map memberRow team.members)
            ]
        ]


nameAndEmail : UserData -> String
nameAndEmail user =
    user.name ++ " <" ++ user.email ++ ">"


addUserCandidateOption : UserData -> Html msg
addUserCandidateOption user =
    option [] [ text (nameAndEmail user) ]


editMemberRow : User -> Member -> Html Msg
editMemberRow currentUser member =
    case member of
        Unchanged m ->
            tr []
                [ td [] [ text (m.user.email ++ " unchanged") ]
                , td [] [ roleSelector currentUser member False ]
                , td [] [ button [ class "red", onClick (RemoveMember member) ] [ text "Remove" ] ]
                ]

        Remove m ->
            tr []
                [ td [ class "strikethrough" ] [ text (m.user.email ++ " remove") ]
                , td [] [ roleSelector currentUser member True ]
                , td [] [ button [ class "red", onClick (Undo member) ] [ text "Undo" ] ]
                ]

        Add r m ->
            tr []
                [ td [] [ text (m.user.email ++ " add") ]
                , td [] [ roleSelector currentUser member False ]
                , td [] [ button [ class "red", onClick (Undo member) ] [ text "Undo" ] ]
                ]

        ChangeRole r m ->
            tr []
                [ td [] [ text (m.user.email ++ " rolechange") ]
                , td [] [ roleSelector currentUser member False, text "*" ]
                , td [] [ button [ class "red", onClick (Undo member) ] [ text "Undo" ] ]
                ]


userIsMember : User -> TeamMemberData -> Bool
userIsMember currentUser member =
    case currentUser of
        LoggedIn u ->
            u.id == member.user.id

        _ ->
            False


roleSelector : User -> Member -> Bool -> Html Msg
roleSelector currentUser member disable =
    let
        isMember =
            userIsMember currentUser (memberData member)

        isAdmin =
            (memberData member).role == Owner

        isGlobalAdmin =
            Session.isGlobalAdmin currentUser
    in
    select [ disabled (isMember && isAdmin && not isGlobalAdmin || disable) ] (Backend.Enum.TeamRole.list |> List.map (roleOption member))


roleOption : Member -> TeamRole -> Html Msg
roleOption member role =
    let
        roleID =
            Backend.Enum.TeamRole.toString role

        roleStr =
            roleString role

        newRole =
            case member of
                ChangeRole r _ ->
                    r

                _ ->
                    (memberData member).role
    in
    option
        [ onClick (RoleDropDownClicked role member)
        , selected (role == newRole)
        , value roleID
        ]
        [ text roleStr ]


viewEditMembers : Model -> TeamData -> Maybe (Graphql.Http.Error TeamData) -> Html Msg
viewEditMembers model team err =
    div [ class "card" ]
        (case model.userList of
            NotAsked ->
                [ text "userlist not asked for" ]

            Failure f ->
                [ text "failed" ]

            Loading ->
                [ text "loading" ]

            Success userList ->
                [ div [ class "title" ]
                    (h2 [] [ text "Members" ]
                        :: editorButton (ClickedEditMembers team) model team
                    )
                , form [ id "newMember", onSubmit OnSubmitAddMember ] []
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
                                [ input [ list "userCandidates", Html.Attributes.form "newMember", type_ "text", value model.addMemberQuery, onInput AddMemberQueryChanged ] []
                                , datalist [ id "userCandidates" ] (List.map addUserCandidateOption userList)
                                ]
                            , td [ colspan 2 ] [ button [ type_ "submit", Html.Attributes.form "newMember" ] [ text "add" ] ]
                            ]
                            :: List.map (editMemberRow (Session.user model.session)) model.members
                        )
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
    div [ class "cards" ]
        (case model.edit of
            View ->
                [ viewTeamOverview model team
                , viewSyncErrors team
                , viewMembers model team
                , viewLogs team
                ]

            EditMain err ->
                [ viewEditTeamOverview team err
                , viewSyncErrors team
                , viewMembers model team
                , viewLogs team
                ]

            EditMembers err ->
                [ viewTeamOverview model team
                , viewSyncErrors team
                , viewEditMembers model team err
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
