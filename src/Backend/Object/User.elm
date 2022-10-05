-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Object.User exposing (..)

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


{-| Unique ID of the user.
-}
id : SelectionSet Backend.ScalarCodecs.Uuid Backend.Object.User
id =
    Object.selectionForField "ScalarCodecs.Uuid" "id" [] (Backend.ScalarCodecs.codecs |> Backend.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


{-| The email address of the user.
-}
email : SelectionSet String Backend.Object.User
email =
    Object.selectionForField "String" "email" [] Decode.string


{-| The name of the user.
-}
name : SelectionSet String Backend.Object.User
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| List of team memberships.
-}
teams :
    SelectionSet decodesTo Backend.Object.TeamMembership
    -> SelectionSet (List decodesTo) Backend.Object.User
teams object____ =
    Object.selectionForCompositeField "teams" [] object____ (Basics.identity >> Decode.list)


{-| Roles attached to the user.
-}
roles :
    SelectionSet decodesTo Backend.Object.Role
    -> SelectionSet (List decodesTo) Backend.Object.User
roles object____ =
    Object.selectionForCompositeField "roles" [] object____ (Basics.identity >> Decode.list)
