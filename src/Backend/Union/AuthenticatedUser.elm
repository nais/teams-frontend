-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Union.AuthenticatedUser exposing (..)

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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onUser : SelectionSet decodesTo Backend.Object.User
    , onServiceAccount : SelectionSet decodesTo Backend.Object.ServiceAccount
    }


{-| Build up a selection for this Union by passing in a Fragments record.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Backend.Union.AuthenticatedUser
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "User" selections____.onUser
        , Object.buildFragment "ServiceAccount" selections____.onServiceAccount
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onUser = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onServiceAccount = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
