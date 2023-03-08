module Api.Do exposing (mutate, mutateRD, query, queryRD)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import RemoteData exposing (RemoteData)



---- Queries ----


graphQLBackend : String
graphQLBackend =
    "/query"


query : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
query q toMsg =
    q
        |> Graphql.Http.queryRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg


queryRD : SelectionSet decodesTo RootQuery -> (RemoteData (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
queryRD q toMsg =
    q
        |> Graphql.Http.queryRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


mutate : SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
mutate m toMsg =
    m
        |> Graphql.Http.mutationRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg


mutateRD : SelectionSet decodesTo RootMutation -> (RemoteData (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
mutateRD m toMsg =
    m
        |> Graphql.Http.mutationRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)
