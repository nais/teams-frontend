module Queries.TeamQueries exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.InputObject exposing (CreateTeamInput, UpdateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.AuditLog as AuditLog
import Backend.Object.Team as Team
import Backend.Object.TeamMember as TeamMember
import Backend.Object.TeamMetadata as TeamMetadata
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (Slug, Uuid)
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


type alias TeamData =
    { id : Uuid
    , slug : Slug
    , purpose : String
    , members : List TeamMemberData
    , auditLogs : List AuditLogData
    , metadata : List KeyValueData
    }


getTeamsQuery : SelectionSet (List TeamData) RootQuery
getTeamsQuery =
    Query.teams teamDataSelection


getTeamQuery : Scalar.Uuid -> SelectionSet TeamData RootQuery
getTeamQuery id =
    Query.team { id = id } teamDataSelection


createTeamMutation : CreateTeamInput -> SelectionSet TeamData RootMutation
createTeamMutation team =
    Mutation.createTeam { input = team } teamDataSelection


updateTeamMutation : Uuid -> UpdateTeamInput -> SelectionSet TeamData RootMutation
updateTeamMutation id team =
    Mutation.updateTeam { teamId = id, input = team } teamDataSelection


addMemberToTeamMutation : TeamData -> UserData -> SelectionSet TeamData RootMutation
addMemberToTeamMutation team user =
    Mutation.addTeamMembers
        { input =
            { teamId = team.id
            , userIds = [ user.id ]
            }
        }
        teamDataSelection


removeMemberFromTeamMutation : TeamData -> UserData -> SelectionSet TeamData RootMutation
removeMemberFromTeamMutation team user =
    Mutation.removeUsersFromTeam
        { input =
            { userIds = [ user.id ]
            , teamId = team.id
            }
        }
        teamDataSelection


setTeamMemberRoleMutation : TeamData -> TeamMemberData -> Backend.Enum.TeamRole.TeamRole -> SelectionSet TeamData RootMutation
setTeamMemberRoleMutation team member role =
    Mutation.setTeamMemberRole
        { input =
            { teamId = team.id
            , userId = member.user.id
            , role = role
            }
        }
        teamDataSelection


teamDataSelection : SelectionSet TeamData Backend.Object.Team
teamDataSelection =
    Graphql.SelectionSet.map6 TeamData
        Team.id
        Team.slug
        Team.purpose
        (Team.members teamMemberSelection)
        (Team.auditLogs auditLogSelection)
        (Team.metadata keyValueSelection)


teamMemberSelection : SelectionSet TeamMemberData Backend.Object.TeamMember
teamMemberSelection =
    Graphql.SelectionSet.map2 TeamMemberData
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
