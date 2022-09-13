module Queries.TeamQueries exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole)
import Backend.InputObject exposing (CreateTeamInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.Team as Team
import Backend.Object.TeamMember as TeamMember
import Backend.Query as Query
import Backend.Scalar exposing (Slug, Uuid)
import Backend.ScalarCodecs
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import Queries.UserQueries exposing (UserData, userDataSelection)



---- MODEL ----


type alias TeamMemberData =
    { user : UserData
    , role : TeamRole
    }


type alias TeamData =
    { id : Uuid
    , name : String
    , slug : Slug
    , purpose : Maybe String
    , members : List TeamMemberData
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


addMemberToTeamMutation : Backend.ScalarCodecs.Uuid -> Backend.ScalarCodecs.Uuid -> SelectionSet TeamData RootMutation
addMemberToTeamMutation userID teamID =
    Mutation.addTeamMembers
        { input =
            { teamId = teamID
            , userIds = [ userID ]
            }
        }
        teamDataSelection


setTeamMemberRoleMutation : Backend.Scalar.Uuid -> Backend.Scalar.Uuid -> Backend.Enum.TeamRole.TeamRole -> SelectionSet TeamData RootMutation
setTeamMemberRoleMutation userID teamID role =
    Mutation.setTeamMemberRole
        { input =
            { teamId = teamID
            , userId = userID
            , role = role
            }
        }
        teamDataSelection


teamDataSelection : SelectionSet TeamData Backend.Object.Team
teamDataSelection =
    Graphql.SelectionSet.map5 TeamData
        Team.id
        Team.name
        Team.slug
        Team.purpose
        (Team.members teamMemberSelection)


teamMemberSelection : SelectionSet TeamMemberData Backend.Object.TeamMember
teamMemberSelection =
    Graphql.SelectionSet.map2 TeamMemberData
        (TeamMember.user userDataSelection)
        TeamMember.role
