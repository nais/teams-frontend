module Queries.TeamQueries exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.InputObject exposing (CreateTeamInput, UpdateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.AuditLog as AuditLog
import Backend.Object.SyncError as SyncError
import Backend.Object.Team as Team
import Backend.Object.TeamMember as TeamMember
import Backend.Object.TeamMetadata as TeamMetadata
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (ReconcilerName(..), Slug, Uuid)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)
import ISO8601
import Queries.UserQueries exposing (UserData, userDataSelection)



---- MODEL ----


type alias TeamMemberData =
    { user : UserData
    , role : TeamRole
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


type alias TeamData =
    { slug : Slug
    , purpose : String
    , members : List TeamMemberData
    , auditLogs : List AuditLogData
    , metadata : List KeyValueData
    , syncErrors : List SyncErrorData
    , lastSuccessfulSync : Maybe ISO8601.Time
    }


getTeamsQuery : SelectionSet (List TeamData) RootQuery
getTeamsQuery =
    Query.teams teamDataSelection


getTeamQuery : Scalar.Slug -> SelectionSet TeamData RootQuery
getTeamQuery slug =
    Query.team { slug = slug } teamDataFullSelection


createTeamMutation : CreateTeamInput -> SelectionSet TeamData RootMutation
createTeamMutation team =
    Mutation.createTeam { input = team } teamDataFullSelection


updateTeamMutation : Slug -> UpdateTeamInput -> SelectionSet TeamData RootMutation
updateTeamMutation slug team =
    Mutation.updateTeam { slug = slug, input = team } teamDataFullSelection


addMemberToTeamMutation : TeamData -> UserData -> SelectionSet TeamData RootMutation
addMemberToTeamMutation team user =
    Mutation.addTeamMembers
        { slug = team.slug
        , userIds = [ user.id ]
        }
        teamDataFullSelection


removeMemberFromTeamMutation : TeamData -> UserData -> SelectionSet TeamData RootMutation
removeMemberFromTeamMutation team user =
    Mutation.removeUsersFromTeam
        { userIds = [ user.id ]
        , slug = team.slug
        }
        teamDataFullSelection


setTeamMemberRoleMutation : TeamData -> TeamMemberData -> Backend.Enum.TeamRole.TeamRole -> SelectionSet TeamData RootMutation
setTeamMemberRoleMutation team member role =
    Mutation.setTeamMemberRole
        { slug = team.slug
        , userId = member.user.id
        , role = role
        }
        teamDataFullSelection


teamDataSelection : SelectionSet TeamData Backend.Object.Team
teamDataSelection =
    Graphql.SelectionSet.succeed TeamData
        |> with Team.slug
        |> with Team.purpose
        |> with (Team.members teamMemberSelection)
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded []
        |> Graphql.SelectionSet.hardcoded Nothing


teamDataFullSelection : SelectionSet TeamData Backend.Object.Team
teamDataFullSelection =
    Graphql.SelectionSet.succeed TeamData
        |> with Team.slug
        |> with Team.purpose
        |> with (Team.members teamMemberSelection)
        |> with (Team.auditLogs auditLogSelection)
        |> with (Team.metadata keyValueSelection)
        |> with (Team.syncErrors syncErrorSelection)
        |> with (Team.lastSuccessfulSync |> mapToMaybeDateTime)


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


keyValueSelection : SelectionSet KeyValueData Backend.Object.TeamMetadata
keyValueSelection =
    Graphql.SelectionSet.map2 KeyValueData
        TeamMetadata.key
        TeamMetadata.value


syncErrorSelection : SelectionSet SyncErrorData Backend.Object.SyncError
syncErrorSelection =
    Graphql.SelectionSet.succeed SyncErrorData
        |> with (SyncError.createdAt |> mapToDateTime)
        |> with (Graphql.SelectionSet.map (\(ReconcilerName x) -> x) SyncError.reconciler)
        |> with SyncError.error


mapToMaybeDateTime : SelectionSet (Maybe Scalar.Time) scope -> SelectionSet (Maybe ISO8601.Time) scope
mapToMaybeDateTime =
    Graphql.SelectionSet.map
        (\x ->
            case x of
                Just (Scalar.Time value) ->
                    ISO8601.fromString value
                        |> Result.toMaybe

                Nothing ->
                    Nothing
        )


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
