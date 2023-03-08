module Api.DeleteTeam exposing (confirmTeamDeletion, getTeamDeleteKey, requestTeamDeletion)

import Api.Team
import Api.User
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.TeamDeleteKey as BOTeamDeleteKey
import Backend.Query as Query
import Backend.Scalar as Scalar exposing (Uuid)
import DataModel exposing (Team, TeamDeleteConfirmed, TeamDeleteKey)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import ISO8601


requestTeamDeletion : Team -> SelectionSet TeamDeleteKey RootMutation
requestTeamDeletion team =
    Mutation.requestTeamDeletion { slug = team.slug } teamDeleteKeySelection


confirmTeamDeletion : TeamDeleteKey -> SelectionSet TeamDeleteConfirmed RootMutation
confirmTeamDeletion key =
    Mutation.confirmTeamDeletion { key = key.key }
        |> Graphql.SelectionSet.map TeamDeleteConfirmed


getTeamDeleteKey : Uuid -> SelectionSet TeamDeleteKey RootQuery
getTeamDeleteKey uuid =
    Query.teamDeleteKey { key = uuid } teamDeleteKeySelection


mapToDateTime : SelectionSet Scalar.Time scope -> SelectionSet ISO8601.Time scope
mapToDateTime =
    Graphql.SelectionSet.mapOrFail
        (\(Scalar.Time value) ->
            ISO8601.fromString value
                |> Result.mapError (\_ -> "Failed to parse " ++ value ++ " as ISO8601 timestamp.")
        )


teamDeleteKeySelection : SelectionSet TeamDeleteKey Backend.Object.TeamDeleteKey
teamDeleteKeySelection =
    Graphql.SelectionSet.map4
        TeamDeleteKey
        BOTeamDeleteKey.key
        (BOTeamDeleteKey.expires |> mapToDateTime)
        (BOTeamDeleteKey.team Api.Team.teamFullSelection)
        (BOTeamDeleteKey.createdBy Api.User.userSelection)
