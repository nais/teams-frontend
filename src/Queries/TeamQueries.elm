module Queries.TeamQueries exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole)
import Backend.InputObject exposing (CreateTeamInput, UpdateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object exposing (AuditLog)
import Backend.Object.AuditLog as AuditLog
import Backend.Object.Team as Team
import Backend.Object.TeamMember as TeamMember
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (Slug, Uuid)
import Backend.ScalarCodecs
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
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
    , createdAt : Scalar.Time
    }


type alias TeamData =
    { id : Uuid
    , name : String
    , slug : Slug
    , purpose : Maybe String
    , members : List TeamMemberData
    , auditLogs : List AuditLogData
    }


getTeamsQuery : SelectionSet (List TeamData) RootQuery
getTeamsQuery =
    Query.teams teamDataSelection


getTeamQuery : Backend.ScalarCodecs.Uuid -> SelectionSet TeamData RootQuery
getTeamQuery id =
    Query.team { id = id } teamDataSelection


createTeamMutation : CreateTeamInput -> SelectionSet TeamData RootMutation
createTeamMutation team =
    Mutation.createTeam { input = team } teamDataSelection


updateTeamMutation : Uuid -> UpdateTeamInput -> SelectionSet TeamData RootMutation
updateTeamMutation id team =
    Mutation.updateTeam { teamId = id, input = team } teamDataSelection


addMemberToTeamMutation : Backend.ScalarCodecs.Uuid -> Backend.ScalarCodecs.Uuid -> SelectionSet TeamData RootMutation
addMemberToTeamMutation userID teamID =
    Mutation.addTeamMembers
        { input =
            { teamId = teamID
            , userIds = [ userID ]
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
        Team.name
        Team.slug
        Team.purpose
        (Team.members teamMemberSelection)
        (Team.auditLogs auditLogSelection)


teamMemberSelection : SelectionSet TeamMemberData Backend.Object.TeamMember
teamMemberSelection =
    Graphql.SelectionSet.map2 TeamMemberData
        (TeamMember.user userDataSelection)
        TeamMember.role


auditLogSelection : SelectionSet AuditLogData Backend.Object.AuditLog
auditLogSelection =
    Graphql.SelectionSet.map4 AuditLogData
        AuditLog.action
        AuditLog.actorEmail
        AuditLog.message
        AuditLog.createdAt
