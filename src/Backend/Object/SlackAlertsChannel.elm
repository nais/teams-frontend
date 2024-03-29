-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Object.SlackAlertsChannel exposing (..)

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


{-| The environment for the alerts sent to the channel.
-}
environment : SelectionSet String Backend.Object.SlackAlertsChannel
environment =
    Object.selectionForField "String" "environment" [] Decode.string


{-| The name of the Slack channel.
-}
channelName : SelectionSet String Backend.Object.SlackAlertsChannel
channelName =
    Object.selectionForField "String" "channelName" [] Decode.string
