module Page.Users exposing (Model, Msg(..), init, update, view)

import Api.Do exposing (mutateRD)
import Api.Error
import Api.Str exposing (uuidStr)
import Api.User
import Backend.Enum.UserSyncRunStatus exposing (UserSyncRunStatus)
import Backend.Mutation as Mutation
import Backend.Scalar exposing (RoleName(..), Slug(..), Uuid)
import Component.Buttons exposing (smallButton)
import Component.Card as Card exposing (Card)
import DataModel exposing (AuditLog, Role, User, UserSyncRun)
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, b, dd, div, dl, dt, i, input, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, rowspan, type_, value)
import ISO8601
import Page.Team exposing (copy)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)


type alias Model =
    { session : Session
    , error : Maybe String
    , users : RemoteData (Graphql.Http.Error (List User)) (List User)
    , userSyncRuns : RemoteData (Graphql.Http.Error (List UserSyncRun)) (List UserSyncRun)
    , synchronizeUsersCorrelationID : RemoteData (Graphql.Http.Error Uuid) Uuid
    }


type Msg
    = GotUsers (RemoteData (Graphql.Http.Error (List User)) (List User))
    | GotUserSyncRuns (RemoteData (Graphql.Http.Error (List UserSyncRun)) (List UserSyncRun))
    | Copy String
    | GotSynchronizeUsersResponse (RemoteData (Graphql.Http.Error Uuid) Uuid)
    | SynchronizeUsersClicked


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , error = Nothing
      , users = Loading
      , userSyncRuns = Loading
      , synchronizeUsersCorrelationID = NotAsked
      }
    , Cmd.batch [ getUsers, getUserSyncRuns ]
    )


getUsers : Cmd Msg
getUsers =
    Api.Do.queryRD Api.User.getAllUsersWithRoles GotUsers


getUserSyncRuns : Cmd Msg
getUserSyncRuns =
    Api.Do.queryRD Api.User.getAllUserSyncRuns GotUserSyncRuns


view : Model -> Html Msg
view model =
    case model.users of
        Loading ->
            p [] [ text "loading" ]

        Success users ->
            div [ class "cards" ]
                ([ viewAdminActions model.synchronizeUsersCorrelationID
                 , viewUserSyncRuns model.userSyncRuns
                 , Card.new "Users"
                    |> Card.withContents
                        [ table [ class "disable-alternate-background-color" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Full name" ]
                                    , th [] [ text "Email" ]
                                    , th [] [ text "External ID" ]
                                    , th [] [ text "Role name" ]
                                    , th [] [ text "Role target" ]
                                    ]
                                ]
                            , tbody [] (List.concatMap viewUser users)
                            ]
                        ]
                 ]
                    |> List.map Card.render
                )

        NotAsked ->
            p [] [ text "NotAsked" ]

        Failure f ->
            p [] [ text (f |> Api.Error.errorToString) ]


viewAdminActions : RemoteData (Graphql.Http.Error Uuid) Uuid -> Card Msg
viewAdminActions synchronizeUsersCorrelationID =
    Card.new "Admin actions"
        |> Card.withButtons [ smallButton SynchronizeUsersClicked "synchronize" "Synchronize users" ]
        |> Card.withContents
            (case synchronizeUsersCorrelationID of
                Success uuid ->
                    [ text "User sync triggered. Correlation ID: "
                    , input [ type_ "text", class "synchronizeUsersCorrelationID", disabled True, value (uuidStr uuid) ] []
                    , smallButton (Copy (uuidStr uuid)) "copy" "copy"
                    ]

                _ ->
                    [ text "" ]
            )


viewUserSyncRuns : RemoteData (Graphql.Http.Error (List UserSyncRun)) (List UserSyncRun) -> Card Msg
viewUserSyncRuns userSyncRuns =
    Card.new "User sync logs"
        |> Card.withContents
            (case userSyncRuns of
                Success runs ->
                    if List.isEmpty runs then
                        [ p [] [ text "There have been no user sync runs since the backend started. Trigger a user sync by clicking on the Synchronize users button above." ] ]

                    else
                        List.map viewUserSyncRun runs

                Loading ->
                    [ text "Loading" ]

                NotAsked ->
                    [ text "NotAsked" ]

                Failure f ->
                    [ text (f |> Api.Error.errorToString) ]
            )


viewUser : User -> List (Html Msg)
viewUser user =
    let
        roleRows =
            List.map viewRoleData user.roles

        rs =
            1 + List.length roleRows
    in
    tr []
        [ td [ rowspan rs ] [ text user.name ]
        , td [ rowspan rs ] [ text user.email ]
        , td [ rowspan rs ] [ text user.externalId ]
        ]
        :: roleRows


viewUserSyncRun : UserSyncRun -> Html Msg
viewUserSyncRun run =
    div [ class "user-sync-run" ]
        [ dl []
            ([ dt [] [ text "Correlation ID:" ]
             , dd [] [ b [] [ text (uuidStr run.correlationID) ] ]
             , dt [] [ text "Started at:" ]
             , dd [] [ text (ISO8601.toString run.startedAt) ]
             , dt [] [ text "Finished at:" ]
             , dd [] [ text (finishedAtToString run.finishedAt) ]
             , dt [] [ text "Status:" ]
             , dd [] [ text (syncStatusToString run.status) ]
             ]
                ++ viewSyncRunError run.error
            )
        , viewUserSyncRunLogEntries run.logEntries
        ]


finishedAtToString : Maybe ISO8601.Time -> String
finishedAtToString finishedAt =
    case finishedAt of
        Just t ->
            ISO8601.toString t

        Nothing ->
            "Not yet finished"


syncStatusToString : UserSyncRunStatus -> String
syncStatusToString status =
    case status of
        Backend.Enum.UserSyncRunStatus.InProgress ->
            "In progress"

        Backend.Enum.UserSyncRunStatus.Success ->
            "Finished"

        Backend.Enum.UserSyncRunStatus.Failure ->
            "Error"


viewSyncRunError : Maybe String -> List (Html Msg)
viewSyncRunError err =
    case err of
        Just msg ->
            [ dt [] [ text "Error message:" ]
            , dd [ class "server-error-message" ] [ text msg ]
            ]

        Nothing ->
            [ text "" ]


viewUserSyncRunLogEntries : List AuditLog -> Html Msg
viewUserSyncRunLogEntries logEntries =
    if List.isEmpty logEntries then
        p [] [ i [] [ text "No log entries exists for this run. This means that there was no changes to be made to the user database, or that the run failed (see potential error message above)." ] ]

    else
        table []
            [ thead []
                [ tr []
                    [ th [] [ text "Created at" ]
                    , th [] [ text "Message" ]
                    ]
                ]
            , tbody [] (List.map viewAuditLogEntry logEntries)
            ]


viewAuditLogEntry : AuditLog -> Html Msg
viewAuditLogEntry entry =
    tr []
        [ td [] [ text (ISO8601.toString entry.createdAt) ]
        , td [] [ text entry.message ]
        ]


roleNameToString : RoleName -> String
roleNameToString (RoleName s) =
    s


slugToString : Slug -> String
slugToString (Slug s) =
    s


viewRoleData : Role -> Html Msg
viewRoleData r =
    tr []
        [ td [] [ text (roleNameToString r.name) ]
        , td []
            [ text
                (if r.isGlobal then
                    "global"

                 else
                    case r.targetTeamSlug of
                        Just slug ->
                            slugToString slug

                        Nothing ->
                            "no target slug - bug?"
                )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUserSyncRuns r ->
            ( { model | userSyncRuns = r }, Cmd.none )

        GotUsers r ->
            ( { model | users = r }, Cmd.none )

        GotSynchronizeUsersResponse r ->
            ( { model | synchronizeUsersCorrelationID = r }, Cmd.none )

        SynchronizeUsersClicked ->
            ( model, mutateRD synchronizeUsers GotSynchronizeUsersResponse )

        Copy s ->
            ( model, copy s )


synchronizeUsers : SelectionSet Uuid RootMutation
synchronizeUsers =
    Mutation.synchronizeUsers
