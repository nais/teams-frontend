module Api.Team exposing (addReconcilerOptOut, addTeamMember, createTeam, getDeployKey, getTeam, getTeams, removeMemberFromTeam, removeReconcilerOptOut, setTeamMemberRole, teamFullSelection, teamSyncSelection, updateTeam)

import Api.User
import Backend.Enum.TeamRole exposing (TeamRole)
import Backend.InputObject exposing (CreateTeamInput, UpdateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.AuditLog as BOAuditLog
import Backend.Object.GcpProject as BOGcpProject
import Backend.Object.GitHubRepository as BOGitHubRepository
import Backend.Object.GitHubRepositoryPermission as BOGitHubRepositoryPermission
import Backend.Object.NaisNamespace as BONaisNamespace
import Backend.Object.Reconciler as BOReconciler
import Backend.Object.ReconcilerState as BOReconcilerState
import Backend.Object.SlackAlertsChannel as BOSlackAlertsChannel
import Backend.Object.SyncError as BOSyncError
import Backend.Object.Team as BOTeam
import Backend.Object.TeamMember as BOTeamMember
import Backend.Object.TeamMemberReconciler as BOTeamMemberReconciler
import Backend.Object.TeamSync as BOTeamSync
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (ReconcilerName(..), Slug)
import DataModel exposing (AuditLog, DeployKey, Expandable(..), GCPProject, GitHubRepository, GitHubRepositoryPermission, NaisNamespace, Reconciler, SlackAlertsChannel, SyncError, Team, TeamMember(..), TeamMemberReconciler(..), TeamSync, TeamSyncState, User, tmTeam, tmUser)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import ISO8601


getTeams : SelectionSet (List Team) RootQuery
getTeams =
    Query.teams teamSelection


getTeam : Scalar.Slug -> SelectionSet Team RootQuery
getTeam slug =
    Query.team { slug = slug } teamFullSelection


getDeployKey : Scalar.Slug -> SelectionSet DeployKey RootQuery
getDeployKey slug =
    Query.deployKey { slug = slug }
        |> SelectionSet.map DeployKey


createTeam : CreateTeamInput -> SelectionSet Team RootMutation
createTeam team =
    Mutation.createTeam { input = team } teamFullSelection


updateTeam : Slug -> UpdateTeamInput -> SelectionSet Team RootMutation
updateTeam slug team =
    Mutation.updateTeam { slug = slug, input = team } teamFullSelection


addTeamMember : Team -> User -> TeamRole -> Maybe (List ReconcilerName) -> SelectionSet Team RootMutation
addTeamMember team user role optOuts =
    Mutation.addTeamMember
        { member =
            { userId = user.id
            , role = role
            , reconcilerOptOuts = Graphql.OptionalArgument.fromMaybe optOuts
            }
        , slug = team.slug
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
        , userId = (tmUser member).id
        , role = role
        }
        teamFullSelection


addReconcilerOptOut : TeamMember -> Reconciler -> SelectionSet TeamMember RootMutation
addReconcilerOptOut member reconciler =
    Mutation.addReconcilerOptOut
        { teamSlug = (tmTeam member).slug
        , userId = (tmUser member).id
        , reconciler = reconciler.name
        }
        teamMemberSelection


removeReconcilerOptOut : TeamMember -> Reconciler -> SelectionSet TeamMember RootMutation
removeReconcilerOptOut member reconciler =
    Mutation.removeReconcilerOptOut
        { teamSlug = (tmTeam member).slug
        , userId = (tmUser member).id
        , reconciler = reconciler.name
        }
        teamMemberSelection


teamSelection : SelectionSet Team Backend.Object.Team
teamSelection =
    SelectionSet.succeed Team
        |> SelectionSet.with BOTeam.slug
        |> SelectionSet.with BOTeam.purpose
        |> SelectionSet.with BOTeam.slackChannel
        |> SelectionSet.hardcoded []
        |> SelectionSet.hardcoded (Preview [])
        |> SelectionSet.hardcoded (Preview [])
        |> SelectionSet.hardcoded []
        |> SelectionSet.hardcoded Nothing
        |> SelectionSet.hardcoded Nothing
        |> SelectionSet.hardcoded (Preview [])
        |> SelectionSet.hardcoded False


teamFullSelection : SelectionSet Team Backend.Object.Team
teamFullSelection =
    SelectionSet.succeed Team
        |> SelectionSet.with BOTeam.slug
        |> SelectionSet.with BOTeam.purpose
        |> SelectionSet.with BOTeam.slackChannel
        |> SelectionSet.with (BOTeam.slackAlertsChannels slackAlertsChannelsSelection)
        |> SelectionSet.with (SelectionSet.map Preview (BOTeam.members teamMemberSelection))
        |> SelectionSet.with (SelectionSet.map Preview (BOTeam.auditLogs auditLogSelection))
        |> SelectionSet.with (BOTeam.syncErrors syncErrorSelection)
        |> SelectionSet.with (BOTeam.lastSuccessfulSync |> mapToMaybeDateTime)
        |> SelectionSet.with (SelectionSet.map Just (BOTeam.reconcilerState syncStateSelection))
        |> SelectionSet.with (SelectionSet.map Preview (BOTeam.gitHubRepositories gitHubRepositorySelection))
        |> SelectionSet.with BOTeam.deletionInProgress


gitHubRepositorySelection : SelectionSet GitHubRepository Backend.Object.GitHubRepository
gitHubRepositorySelection =
    SelectionSet.map2
        GitHubRepository
        BOGitHubRepository.name
        (BOGitHubRepository.permissions gitHubRepositoryPermissionSelection)


gitHubRepositoryPermissionSelection : SelectionSet GitHubRepositoryPermission Backend.Object.GitHubRepositoryPermission
gitHubRepositoryPermissionSelection =
    SelectionSet.map2
        GitHubRepositoryPermission
        BOGitHubRepositoryPermission.name
        BOGitHubRepositoryPermission.granted


teamMemberSelection : SelectionSet TeamMember Backend.Object.TeamMember
teamMemberSelection =
    SelectionSet.map4
        TeamMember
        (BOTeamMember.user Api.User.userSelection)
        (BOTeamMember.team teamSelection)
        BOTeamMember.role
        (BOTeamMember.reconcilers teamMemberReconcilerSelection)


teamMemberReconcilerSelection : SelectionSet TeamMemberReconciler Backend.Object.TeamMemberReconciler
teamMemberReconcilerSelection =
    SelectionSet.succeed TeamMemberReconciler
        |> SelectionSet.with BOTeamMemberReconciler.enabled
        |> SelectionSet.with (BOTeamMemberReconciler.reconciler reconcilerSelection)


reconcilerSelection : SelectionSet Reconciler Backend.Object.Reconciler
reconcilerSelection =
    SelectionSet.succeed Reconciler
        |> SelectionSet.hardcoded True
        |> SelectionSet.with BOReconciler.description
        |> SelectionSet.with BOReconciler.displayName
        |> SelectionSet.with BOReconciler.enabled
        |> SelectionSet.with BOReconciler.name
        |> SelectionSet.hardcoded 0
        |> SelectionSet.hardcoded []
        |> SelectionSet.with BOReconciler.usesTeamMemberships


slackAlertsChannelsSelection : SelectionSet SlackAlertsChannel Backend.Object.SlackAlertsChannel
slackAlertsChannelsSelection =
    SelectionSet.map2
        SlackAlertsChannel
        BOSlackAlertsChannel.environment
        BOSlackAlertsChannel.channelName


auditLogSelection : SelectionSet AuditLog Backend.Object.AuditLog
auditLogSelection =
    SelectionSet.succeed AuditLog
        |> SelectionSet.with BOAuditLog.action
        |> SelectionSet.with BOAuditLog.actor
        |> SelectionSet.with BOAuditLog.message
        |> SelectionSet.with (BOAuditLog.createdAt |> mapToDateTime)


teamSyncSelection : SelectionSet TeamSync Backend.Object.TeamSync
teamSyncSelection =
    SelectionSet.succeed TeamSync
        |> SelectionSet.with BOTeamSync.correlationID


syncStateSelection : SelectionSet TeamSyncState Backend.Object.ReconcilerState
syncStateSelection =
    SelectionSet.map6
        TeamSyncState
        BOReconcilerState.gitHubTeamSlug
        BOReconcilerState.googleWorkspaceGroupEmail
        (BOReconcilerState.gcpProjects gcpProjectSelection)
        (BOReconcilerState.naisNamespaces naisNamespaceSelection)
        BOReconcilerState.azureADGroupId
        BOReconcilerState.garRepositoryName


gcpProjectSelection : SelectionSet GCPProject Backend.Object.GcpProject
gcpProjectSelection =
    SelectionSet.map2 GCPProject
        BOGcpProject.environment
        BOGcpProject.projectId


naisNamespaceSelection : SelectionSet NaisNamespace Backend.Object.NaisNamespace
naisNamespaceSelection =
    SelectionSet.map2 NaisNamespace
        BONaisNamespace.environment
        BONaisNamespace.namespace


syncErrorSelection : SelectionSet SyncError Backend.Object.SyncError
syncErrorSelection =
    SelectionSet.succeed SyncError
        |> SelectionSet.with (BOSyncError.createdAt |> mapToDateTime)
        |> SelectionSet.with (SelectionSet.map (\(ReconcilerName x) -> x) BOSyncError.reconciler)
        |> SelectionSet.with BOSyncError.error


mapToMaybeDateTime : SelectionSet (Maybe Scalar.Time) scope -> SelectionSet (Maybe ISO8601.Time) scope
mapToMaybeDateTime =
    SelectionSet.map
        (Maybe.map (\(Scalar.Time value) -> ISO8601.fromString value) >> Maybe.andThen Result.toMaybe)


mapToDateTime : SelectionSet Scalar.Time scope -> SelectionSet ISO8601.Time scope
mapToDateTime =
    SelectionSet.mapOrFail
        (\(Scalar.Time value) ->
            ISO8601.fromString value
                |> Result.mapError (\_ -> "Failed to parse " ++ value ++ " as ISO8601 timestamp.")
        )
