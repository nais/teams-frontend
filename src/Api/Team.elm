module Api.Team exposing (addMemberToTeam, addOwnerToTeam, createTeam, getTeam, getTeams, removeMemberFromTeam, setTeamMemberRole, teamFullSelection, teamSyncSelection, updateTeam)

import Api.User
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.InputObject exposing (CreateTeamInput, UpdateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.AuditLog as BOAuditLog
import Backend.Object.GcpProject as BOGcpProject
import Backend.Object.GitHubRepository as BOGitHubRepository
import Backend.Object.GitHubRepositoryPermission as BOGitHubRepositoryPermission
import Backend.Object.NaisNamespace as BONaisNamespace
import Backend.Object.ReconcilerState as BOReconcilerState
import Backend.Object.SlackAlertsChannel as BOSlackAlertsChannel
import Backend.Object.SyncError as BOSyncError
import Backend.Object.Team as BOTeam
import Backend.Object.TeamMember as BOTeamMember
import Backend.Object.TeamMetadata as BOTeamMetadata
import Backend.Object.TeamSync as BOTeamSync
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (ReconcilerName(..), Slug)
import DataModel exposing (..)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)
import ISO8601


getTeams : SelectionSet (List Team) RootQuery
getTeams =
    Query.teams teamSelection


getTeam : Scalar.Slug -> SelectionSet Team RootQuery
getTeam slug =
    Query.team { slug = slug } teamFullSelection


createTeam : CreateTeamInput -> SelectionSet Team RootMutation
createTeam team =
    Mutation.createTeam { input = team } teamFullSelection


updateTeam : Slug -> UpdateTeamInput -> SelectionSet Team RootMutation
updateTeam slug team =
    Mutation.updateTeam { slug = slug, input = team } teamFullSelection


addMemberToTeam : Team -> User -> SelectionSet Team RootMutation
addMemberToTeam team user =
    Mutation.addTeamMembers
        { slug = team.slug
        , userIds = [ user.id ]
        }
        teamFullSelection


addOwnerToTeam : Team -> User -> SelectionSet Team RootMutation
addOwnerToTeam team user =
    Mutation.addTeamOwners
        { slug = team.slug
        , userIds = [ user.id ]
        }
        teamFullSelection


removeMemberFromTeam : Team -> User -> SelectionSet Team RootMutation
removeMemberFromTeam team user =
    Mutation.removeUsersFromTeam
        { userIds = [ user.id ]
        , slug = team.slug
        }
        teamFullSelection


setTeamMemberRole : Team -> TeamMember -> Backend.Enum.TeamRole.TeamRole -> SelectionSet Team RootMutation
setTeamMemberRole team member role =
    Mutation.setTeamMemberRole
        { slug = team.slug
        , userId = member.user.id
        , role = role
        }
        teamFullSelection


teamSelection : SelectionSet Team Backend.Object.Team
teamSelection =
    Graphql.SelectionSet.succeed Team
        |> with BOTeam.slug
        |> with BOTeam.purpose
        |> with BOTeam.slackChannel
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded (Preview [])
        |> Graphql.SelectionSet.hardcoded (Preview [])
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded Nothing
        |> Graphql.SelectionSet.hardcoded Nothing
        |> Graphql.SelectionSet.hardcoded (Preview [])
        |> with BOTeam.enabled


teamFullSelection : SelectionSet Team Backend.Object.Team
teamFullSelection =
    Graphql.SelectionSet.succeed Team
        |> with BOTeam.slug
        |> with BOTeam.purpose
        |> with BOTeam.slackChannel
        |> with (BOTeam.slackAlertsChannels slackAlertsChannelsSelection)
        |> with (Graphql.SelectionSet.map Preview (BOTeam.members teamMemberSelection))
        |> with (Graphql.SelectionSet.map Preview (BOTeam.auditLogs auditLogSelection))
        |> with (BOTeam.metadata keyValueSelection)
        |> with (BOTeam.syncErrors syncErrorSelection)
        |> with (BOTeam.lastSuccessfulSync |> mapToMaybeDateTime)
        |> with (Graphql.SelectionSet.map Just (BOTeam.reconcilerState syncStateSelection))
        |> with (Graphql.SelectionSet.map Preview (BOTeam.gitHubRepositories gitHubRepositorySelection))
        |> with BOTeam.enabled


gitHubRepositorySelection : SelectionSet GitHubRepository Backend.Object.GitHubRepository
gitHubRepositorySelection =
    Graphql.SelectionSet.map2
        GitHubRepository
        BOGitHubRepository.name
        (BOGitHubRepository.permissions gitHubRepositoryPermissionSelection)


gitHubRepositoryPermissionSelection : SelectionSet GitHubRepositoryPermission Backend.Object.GitHubRepositoryPermission
gitHubRepositoryPermissionSelection =
    Graphql.SelectionSet.map2
        GitHubRepositoryPermission
        BOGitHubRepositoryPermission.name
        BOGitHubRepositoryPermission.granted


teamMemberSelection : SelectionSet TeamMember Backend.Object.TeamMember
teamMemberSelection =
    Graphql.SelectionSet.map2
        TeamMember
        (BOTeamMember.user Api.User.userSelection)
        BOTeamMember.role


slackAlertsChannelsSelection : SelectionSet SlackAlertsChannel Backend.Object.SlackAlertsChannel
slackAlertsChannelsSelection =
    Graphql.SelectionSet.map2
        SlackAlertsChannel
        BOSlackAlertsChannel.environment
        BOSlackAlertsChannel.channelName


auditLogSelection : SelectionSet AuditLog Backend.Object.AuditLog
auditLogSelection =
    Graphql.SelectionSet.succeed AuditLog
        |> with BOAuditLog.action
        |> with BOAuditLog.actor
        |> with BOAuditLog.message
        |> with (BOAuditLog.createdAt |> mapToDateTime)


teamSyncSelection : SelectionSet TeamSync Backend.Object.TeamSync
teamSyncSelection =
    Graphql.SelectionSet.succeed TeamSync
        |> with BOTeamSync.correlationID



-- teamToggleSelection :


keyValueSelection : SelectionSet KeyValue Backend.Object.TeamMetadata
keyValueSelection =
    Graphql.SelectionSet.map2 KeyValue
        BOTeamMetadata.key
        BOTeamMetadata.value


syncStateSelection : SelectionSet TeamSyncState Backend.Object.ReconcilerState
syncStateSelection =
    Graphql.SelectionSet.map6
        TeamSyncState
        BOReconcilerState.gitHubTeamSlug
        BOReconcilerState.googleWorkspaceGroupEmail
        (BOReconcilerState.gcpProjects gcpProjectSelection)
        (BOReconcilerState.naisNamespaces naisNamespaceSelection)
        BOReconcilerState.azureADGroupId
        BOReconcilerState.garRepositoryName


gcpProjectSelection : SelectionSet GCPProject Backend.Object.GcpProject
gcpProjectSelection =
    Graphql.SelectionSet.map2 GCPProject
        BOGcpProject.environment
        BOGcpProject.projectId


naisNamespaceSelection : SelectionSet NaisNamespace Backend.Object.NaisNamespace
naisNamespaceSelection =
    Graphql.SelectionSet.map2 NaisNamespace
        BONaisNamespace.environment
        BONaisNamespace.namespace


syncErrorSelection : SelectionSet SyncError Backend.Object.SyncError
syncErrorSelection =
    Graphql.SelectionSet.succeed SyncError
        |> with (BOSyncError.createdAt |> mapToDateTime)
        |> with (Graphql.SelectionSet.map (\(ReconcilerName x) -> x) BOSyncError.reconciler)
        |> with BOSyncError.error


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
