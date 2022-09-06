module Queries.Do exposing (do)

import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)



---- Queries ----


graphQLBackend : String
graphQLBackend =
    "http://localhost:3000/query"


do : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
do query toMsg =
    query
        |> Graphql.Http.queryRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg
