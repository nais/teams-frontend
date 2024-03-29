-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Object.GitHubRepositoryPermission exposing (..)

import Backend.InputObject
import Backend.Interface
import Backend.Object
import Backend.Scalar
import Backend.ScalarCodecs
import Backend.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| Name of the permission.
-}
name : SelectionSet String Backend.Object.GitHubRepositoryPermission
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| Whether or not the permission is granted for the repository.
-}
granted : SelectionSet Bool Backend.Object.GitHubRepositoryPermission
granted =
    Object.selectionForField "Bool" "granted" [] Decode.bool
