module Api.User exposing (getAllUserSyncRuns, getAllUsers, getAllUsersWithRoles, getMe, userSelection)

import Backend.Object
import Backend.Object.AuditLog as BOAuditLog
import Backend.Object.Reconciler as BOReconciler
import Backend.Object.Role as BORole
import Backend.Object.Team as BOTeam
import Backend.Object.TeamMember as BOTeamMember
import Backend.Object.TeamMemberReconciler as TeamMemberReconciler
import Backend.Object.User as BOUser
import Backend.Object.UserSyncRun as BOUserSyncRun
import Backend.Query as Query
import Backend.Scalar as Scalar
import Backend.Union
import Backend.Union.AuthenticatedUser
import DataModel exposing (AuditLog, Expandable(..), Reconciler, Role, Team, TeamMember(..), TeamMemberReconciler(..), User, UserSyncRun)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import ISO8601



---- MODEL ----


getMe : SelectionSet (Maybe User) RootQuery
getMe =
    Query.me meSelection


getAllUsers : SelectionSet (List User) RootQuery
getAllUsers =
    Query.users userSelection


getAllUsersWithRoles : SelectionSet (List User) RootQuery
getAllUsersWithRoles =
    Query.users userWithRoleSelection


getAllUserSyncRuns : SelectionSet (List UserSyncRun) RootQuery
getAllUserSyncRuns =
    Query.userSync userSyncSelection


userSyncSelection : SelectionSet UserSyncRun Backend.Object.UserSyncRun
userSyncSelection =
    SelectionSet.map6 UserSyncRun
        BOUserSyncRun.correlationID
        (BOUserSyncRun.startedAt |> mapToDateTime)
        (BOUserSyncRun.finishedAt |> mapToMaybeDateTime)
        BOUserSyncRun.status
        (BOUserSyncRun.logEntries auditLogSelection)
        BOUserSyncRun.error


auditLogSelection : SelectionSet AuditLog Backend.Object.AuditLog
auditLogSelection =
    SelectionSet.map4 AuditLog
        BOAuditLog.action
        BOAuditLog.actor
        BOAuditLog.message
        (BOAuditLog.createdAt |> mapToDateTime)


userWithRoleSelection : SelectionSet User Backend.Object.User
userWithRoleSelection =
    SelectionSet.succeed User
        |> SelectionSet.with BOUser.id
        |> SelectionSet.with BOUser.email
        |> SelectionSet.with BOUser.name
        |> SelectionSet.with BOUser.externalId
        |> SelectionSet.with (BOUser.teams teamMemberSelection)
        |> SelectionSet.with (BOUser.roles roleSelection)


userSelection : SelectionSet User Backend.Object.User
userSelection =
    SelectionSet.succeed User
        |> SelectionSet.with BOUser.id
        |> SelectionSet.with BOUser.email
        |> SelectionSet.with BOUser.name
        |> SelectionSet.with BOUser.externalId
        |> SelectionSet.hardcoded []
        |> SelectionSet.hardcoded []


roleSelection : SelectionSet Role Backend.Object.Role
roleSelection =
    SelectionSet.map3 Role
        BORole.name
        BORole.isGlobal
        BORole.targetTeamSlug


teamMemberSelection : SelectionSet TeamMember Backend.Object.TeamMember
teamMemberSelection =
    SelectionSet.map4 TeamMember
        (BOTeamMember.user userSelection)
        (BOTeamMember.team teamSelection)
        BOTeamMember.role
        (BOTeamMember.reconcilers teamMemberReconcilerSelection)


teamMemberReconcilerSelection : SelectionSet TeamMemberReconciler Backend.Object.TeamMemberReconciler
teamMemberReconcilerSelection =
    SelectionSet.succeed TeamMemberReconciler
        |> SelectionSet.with TeamMemberReconciler.enabled
        |> SelectionSet.with (TeamMemberReconciler.reconciler reconcilerSelection)


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


meSelection : SelectionSet (Maybe User) Backend.Union.AuthenticatedUser
meSelection =
    Backend.Union.AuthenticatedUser.fragments
        { onUser = SelectionSet.map Just userWithRoleSelection
        , onServiceAccount = SelectionSet.map (\_ -> Nothing) SelectionSet.empty
        }


mapToDateTime : SelectionSet Scalar.Time scope -> SelectionSet ISO8601.Time scope
mapToDateTime =
    SelectionSet.mapOrFail
        (\(Scalar.Time value) ->
            ISO8601.fromString value
                |> Result.mapError (\_ -> "Failed to parse " ++ value ++ " as ISO8601 timestamp.")
        )


mapToMaybeDateTime : SelectionSet (Maybe Scalar.Time) scope -> SelectionSet (Maybe ISO8601.Time) scope
mapToMaybeDateTime =
    SelectionSet.map
        (Maybe.map (\(Scalar.Time value) -> ISO8601.fromString value) >> Maybe.andThen Result.toMaybe)


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
        |> SelectionSet.with (BOTeam.lastSuccessfulSync |> mapToMaybeDateTime)
        |> SelectionSet.hardcoded Nothing
        |> SelectionSet.hardcoded (Preview [])
        |> SelectionSet.hardcoded False
