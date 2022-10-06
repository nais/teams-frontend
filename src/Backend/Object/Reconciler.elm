-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Object.Reconciler exposing (..)

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


{-| The name of the reconciler.
-}
name : SelectionSet Backend.ScalarCodecs.ReconcilerName Backend.Object.Reconciler
name =
    Object.selectionForField "ScalarCodecs.ReconcilerName" "name" [] (Backend.ScalarCodecs.codecs |> Backend.Scalar.unwrapCodecs |> .codecReconcilerName |> .decoder)


{-| The human-friendly name of the reconciler.
-}
displayName : SelectionSet String Backend.Object.Reconciler
displayName =
    Object.selectionForField "String" "displayName" [] Decode.string


{-| Description of what the reconciler is responsible for.
-}
description : SelectionSet String Backend.Object.Reconciler
description =
    Object.selectionForField "String" "description" [] Decode.string


{-| Whether or not the reconciler is available for teams in Console.
-}
enabled : SelectionSet Bool Backend.Object.Reconciler
enabled =
    Object.selectionForField "Bool" "enabled" [] Decode.bool


{-| Reconciler configuration keys and descriptions.
-}
config :
    SelectionSet decodesTo Backend.Object.ReconcilerConfig
    -> SelectionSet (List decodesTo) Backend.Object.Reconciler
config object____ =
    Object.selectionForCompositeField "config" [] object____ (Basics.identity >> Decode.list)


{-| Whether or not the reconciler is fully configured and ready to be enabled.
-}
configured : SelectionSet Bool Backend.Object.Reconciler
configured =
    Object.selectionForField "Bool" "configured" [] Decode.bool


{-| The run order of the reconciler.
-}
runOrder : SelectionSet Int Backend.Object.Reconciler
runOrder =
    Object.selectionForField "Int" "runOrder" [] Decode.int


{-| Audit logs for this reconciler.
-}
auditLogs :
    SelectionSet decodesTo Backend.Object.AuditLog
    -> SelectionSet (List decodesTo) Backend.Object.Reconciler
auditLogs object____ =
    Object.selectionForCompositeField "auditLogs" [] object____ (Basics.identity >> Decode.list)
