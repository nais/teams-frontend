-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Object.ReconcilerConfig exposing (..)

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


{-| Configuration key.
-}
key : SelectionSet Backend.ScalarCodecs.ReconcilerConfigKey Backend.Object.ReconcilerConfig
key =
    Object.selectionForField "ScalarCodecs.ReconcilerConfigKey" "key" [] (Backend.ScalarCodecs.codecs |> Backend.Scalar.unwrapCodecs |> .codecReconcilerConfigKey |> .decoder)


{-| The human-friendly name of the configuration key.
-}
displayName : SelectionSet String Backend.Object.ReconcilerConfig
displayName =
    Object.selectionForField "String" "displayName" [] Decode.string


{-| Configuration description.
-}
description : SelectionSet String Backend.Object.ReconcilerConfig
description =
    Object.selectionForField "String" "description" [] Decode.string


{-| Whether or not the configuration key has a value.
-}
configured : SelectionSet Bool Backend.Object.ReconcilerConfig
configured =
    Object.selectionForField "Bool" "configured" [] Decode.bool
