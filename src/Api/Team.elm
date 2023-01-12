module Api.Team exposing (..)

import Api.User exposing (UserData, userDataSelection)
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.InputObject exposing (CreateTeamInput, UpdateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.AuditLog as AuditLog
import Backend.Object.GcpProject as GcpProject
import Backend.Object.NaisNamespace as NaisNamespace
import Backend.Object.ReconcilerState as ReconcilerState
import Backend.Object.SyncError as SyncError
import Backend.Object.Team as Team
import Backend.Object.TeamMember as TeamMember
import Backend.Object.TeamMetadata as TeamMetadata
import Backend.Object.TeamSync
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (ReconcilerName(..), Slug, Uuid)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)
import ISO8601



---- MODEL ----


type alias TeamMemberData =
    { user : UserData
    , role : TeamRole
    }


type alias TeamSync =
    { correlationID : Uuid
    }


type alias AuditLogData =
    { action : Scalar.AuditAction
    , actor : Maybe String
    , message : String
    , createdAt : ISO8601.Time
    }


type alias KeyValueData =
    { key : String
    , value : Maybe String
    }


type alias SyncErrorData =
    { timestamp : ISO8601.Time
    , reconcilerName : String
    , message : String
    }


type alias GCPProject =
    { environment : String
    , projectID : String
    }


type alias NaisNamespace =
    { environment : String
    , namespace : Slug
    }


type alias TeamSyncState =
    { githubTeamSlug : Maybe Slug
    , googleWorkspaceGroupEmail : Maybe String
    , gcpProjects : List GCPProject
    , naisNamespaces : List NaisNamespace
    , azureADGroupID : Maybe Uuid
    }


type alias TeamData =
    { slug : Slug
    , purpose : String
    , slackAlertChannel : String
    , members : List TeamMemberData
    , auditLogs : List AuditLogData
    , metadata : List KeyValueData
    , syncErrors : List SyncErrorData
    , lastSuccessfulSync : Maybe ISO8601.Time
    , syncState : Maybe TeamSyncState
    , enabled : Bool
    }


getTeams : SelectionSet (List TeamData) RootQuery
getTeams =
    Query.teams teamDataSelection


getTeam : Scalar.Slug -> SelectionSet TeamData RootQuery
getTeam slug =
    Query.team { slug = slug } teamDataFullSelection


createTeam : CreateTeamInput -> SelectionSet TeamData RootMutation
createTeam team =
    Mutation.createTeam { input = team } teamDataFullSelection


updateTeam : Slug -> UpdateTeamInput -> SelectionSet TeamData RootMutation
updateTeam slug team =
    Mutation.updateTeam { slug = slug, input = team } teamDataFullSelection


addMemberToTeam : TeamData -> UserData -> SelectionSet TeamData RootMutation
addMemberToTeam team user =
    Mutation.addTeamMembers
        { slug = team.slug
        , userIds = [ user.id ]
        }
        teamDataFullSelection


addOwnerToTeam : TeamData -> UserData -> SelectionSet TeamData RootMutation
addOwnerToTeam team user =
    Mutation.addTeamOwners
        { slug = team.slug
        , userIds = [ user.id ]
        }
        teamDataFullSelection


removeMemberFromTeam : TeamData -> UserData -> SelectionSet TeamData RootMutation
removeMemberFromTeam team user =
    Mutation.removeUsersFromTeam
        { userIds = [ user.id ]
        , slug = team.slug
        }
        teamDataFullSelection


setTeamMemberRole : TeamData -> TeamMemberData -> Backend.Enum.TeamRole.TeamRole -> SelectionSet TeamData RootMutation
setTeamMemberRole team member role =
    Mutation.setTeamMemberRole
        { slug = team.slug
        , userId = member.user.id
        , role = role
        }
        teamDataFullSelection


enableTeam : TeamData -> SelectionSet TeamData RootMutation
enableTeam team =
    Mutation.enableTeam { slug = team.slug } teamDataFullSelection


disableTeam : TeamData -> SelectionSet TeamData RootMutation
disableTeam team =
    Mutation.disableTeam { slug = team.slug } teamDataFullSelection


teamDataSelection : SelectionSet TeamData Backend.Object.Team
teamDataSelection =
    Graphql.SelectionSet.succeed TeamData
        |> with Team.slug
        |> with Team.purpose
        |> with (Team.slackChannel |> mapMaybeToString)
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded Nothing
        |> Graphql.SelectionSet.hardcoded Nothing
        |> with Team.enabled


teamDataFullSelection : SelectionSet TeamData Backend.Object.Team
teamDataFullSelection =
    Graphql.SelectionSet.succeed TeamData
        |> with Team.slug
        |> with Team.purpose
        |> with (Team.slackChannel |> mapMaybeToString)
        |> with (Team.members teamMemberSelection)
        |> with (Team.auditLogs auditLogSelection)
        |> with (Team.metadata keyValueSelection)
        |> with (Team.syncErrors syncErrorSelection)
        |> with (Team.lastSuccessfulSync |> mapToMaybeDateTime)
        |> with (Graphql.SelectionSet.map Just (Team.reconcilerState syncStateSelection))
        |> with Team.enabled


teamMemberSelection : SelectionSet TeamMemberData Backend.Object.TeamMember
teamMemberSelection =
    Graphql.SelectionSet.map2
        TeamMemberData
        (TeamMember.user userDataSelection)
        TeamMember.role


auditLogSelection : SelectionSet AuditLogData Backend.Object.AuditLog
auditLogSelection =
    Graphql.SelectionSet.succeed AuditLogData
        |> with AuditLog.action
        |> with AuditLog.actor
        |> with AuditLog.message
        |> with (AuditLog.createdAt |> mapToDateTime)


teamSyncSelection : SelectionSet TeamSync Backend.Object.TeamSync
teamSyncSelection =
    Graphql.SelectionSet.succeed TeamSync
        |> with Backend.Object.TeamSync.correlationID



-- teamToggleSelection :


keyValueSelection : SelectionSet KeyValueData Backend.Object.TeamMetadata
keyValueSelection =
    Graphql.SelectionSet.map2 KeyValueData
        TeamMetadata.key
        TeamMetadata.value


syncStateSelection : SelectionSet TeamSyncState Backend.Object.ReconcilerState
syncStateSelection =
    Graphql.SelectionSet.succeed TeamSyncState
        |> with ReconcilerState.gitHubTeamSlug
        |> with ReconcilerState.googleWorkspaceGroupEmail
        |> with (ReconcilerState.gcpProjects gcpProjectSelection)
        |> with (ReconcilerState.naisNamespaces naisNamespaceSelection)
        |> with ReconcilerState.azureADGroupId


gcpProjectSelection : SelectionSet GCPProject Backend.Object.GcpProject
gcpProjectSelection =
    Graphql.SelectionSet.map2 GCPProject
        GcpProject.environment
        GcpProject.projectId


naisNamespaceSelection : SelectionSet NaisNamespace Backend.Object.NaisNamespace
naisNamespaceSelection =
    Graphql.SelectionSet.map2 NaisNamespace
        NaisNamespace.environment
        NaisNamespace.namespace


syncErrorSelection : SelectionSet SyncErrorData Backend.Object.SyncError
syncErrorSelection =
    Graphql.SelectionSet.succeed SyncErrorData
        |> with (SyncError.createdAt |> mapToDateTime)
        |> with (Graphql.SelectionSet.map (\(ReconcilerName x) -> x) SyncError.reconciler)
        |> with SyncError.error


mapMaybeToString : SelectionSet (Maybe String) scope -> SelectionSet String scope
mapMaybeToString =
    Graphql.SelectionSet.map
        (\x ->
            case x of
                Just s ->
                    s

                Nothing ->
                    ""
        )


mapToMaybeDateTime : SelectionSet (Maybe Scalar.Time) scope -> SelectionSet (Maybe ISO8601.Time) scope
mapToMaybeDateTime =
    Graphql.SelectionSet.map
        (Maybe.map (\(Scalar.Time value) -> ISO8601.fromString value) >> Maybe.andThen Result.toMaybe)


mapToDateTime : SelectionSet Scalar.Time scope -> SelectionSet ISO8601.Time scope
mapToDateTime =
    Graphql.SelectionSet.mapOrFail
        (\(Scalar.Time value) ->
            ISO8601.fromString value
                |> Result.mapError (\_ -> "Failed to parse " ++ value ++ " as ISO8601 timestamp.")
        )


roleString : TeamRole -> String
roleString teamRole =
    case teamRole of
        Member ->
            "Member"

        Owner ->
            "Owner"
