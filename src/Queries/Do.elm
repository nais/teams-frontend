module Queries.Do exposing (send)

import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)



---- Queries ----


graphQLBackend : String
graphQLBackend =
    "http://localhost:3000/query"


send : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
send query toMsg =
    query
        |> Graphql.Http.queryRequest graphQLBackend
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send toMsg
