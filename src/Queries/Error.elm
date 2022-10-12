module Queries.Error exposing (..)

import Graphql.Http exposing (HttpError(..), RawError(..))
import Http exposing (Error(..))
import Json.Decode


errorToString : Graphql.Http.Error a -> String
errorToString error =
    case error of
        Graphql.Http.HttpError err ->
            case err of
                Graphql.Http.BadUrl u ->
                    "bad url: " ++ u

                Graphql.Http.Timeout ->
                    "Timeout"

                Graphql.Http.NetworkError ->
                    "NetworkError"

                Graphql.Http.BadStatus metadata e ->
                    "BadStatus: " ++ metadata.statusText ++ ": " ++ e

                Graphql.Http.BadPayload payload ->
                    "BadPayload: " ++ (payload |> Json.Decode.errorToString)

        Graphql.Http.GraphqlError _ errs ->
            String.join "|" (List.map (\e -> e.message) errs)
