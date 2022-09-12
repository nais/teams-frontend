module Queries.TeamQueries exposing (..)

import Backend.InputObject exposing (CreateTeamInput)
import Backend.Mutation as Mutation exposing (CreateTeamRequiredArguments)
import Backend.Object
import Backend.Object.Team as Team
import Backend.Query as Query
import Backend.Scalar exposing (Slug, Uuid)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)



---- MODEL ----


type alias TeamData =
    { id : Uuid
    , name : String
    , slug : Slug
    , purpose : Maybe String
    }


getTeamsQuery : SelectionSet (List TeamData) RootQuery
getTeamsQuery =
    Query.teams teamDataSelection


createTeamQuery : CreateTeamInput -> SelectionSet TeamData RootMutation
createTeamQuery team =
    Mutation.createTeam { input = team } teamDataSelection


teamDataSelection : SelectionSet TeamData Backend.Object.Team
teamDataSelection =
    Graphql.SelectionSet.map4 TeamData
        Team.id
        Team.name
        Team.slug
        Team.purpose
