module Queries.Do exposing (mutate, query)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)



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


mutate : SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
mutate m toMsg =
    m
        |> Graphql.Http.mutationRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg
