module Queries.TeamQueries exposing (..)

import Backend.Object
import Backend.Object.Team as Team
import Backend.Query as Query
import Backend.Scalar exposing (Slug, Uuid)
import Graphql.Operation exposing (RootQuery)
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


teamDataSelection : SelectionSet TeamData Backend.Object.Team
teamDataSelection =
    Graphql.SelectionSet.map4 TeamData
        Team.id
        Team.name
        Team.slug
        Team.purpose
