module Queries.ReconcilerQueries exposing (..)

import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.Reconciler as Reconciler
import Backend.Object.ReconcilerConfig as ReconcilerConfig
import Backend.Query as Query
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import Backend.Scalar exposing (ReconcilerName, Map)


type alias ReconcilerConfigData =
    { configured : Bool
    , description : String
    , displayName : String
    , key : String
    }


type alias ReconcilerData =
    { configured : Bool
    , description : String
    , displayname : String
    , enabled : Bool
    , name : ReconcilerName
    , runorder : Int
    , config : List ReconcilerConfigData
    }


getReconcilersQuery : SelectionSet (List ReconcilerData) RootQuery
getReconcilersQuery =
    Query.reconcilers reconcilerDataSelection


updateReconcilerConfigMutation : ReconcilerName -> Map -> SelectionSet ReconcilerData Graphql.Operation.RootMutation
updateReconcilerConfigMutation name config =
    Mutation.configureReconciler { name = name, config = config } reconcilerDataSelection


enableReconcilerMutation : ReconcilerName -> SelectionSet ReconcilerData Graphql.Operation.RootMutation
enableReconcilerMutation name =
    Mutation.enableReconciler { name = name } reconcilerDataSelection

disableReconcilerMutation : ReconcilerName -> SelectionSet ReconcilerData Graphql.Operation.RootMutation
disableReconcilerMutation name =
    Mutation.disableReconciler { name = name } reconcilerDataSelection


reconcilerDataSelection : SelectionSet ReconcilerData Backend.Object.Reconciler
reconcilerDataSelection =
    Graphql.SelectionSet.map7 ReconcilerData
        Reconciler.configured
        Reconciler.description
        Reconciler.displayName
        Reconciler.enabled
        Reconciler.name
        Reconciler.runOrder
        (Reconciler.config reconcilerConfigDataSelection)


reconcilerConfigDataSelection : SelectionSet ReconcilerConfigData Backend.Object.ReconcilerConfig
reconcilerConfigDataSelection =
    Graphql.SelectionSet.map4 ReconcilerConfigData
        ReconcilerConfig.configured
        ReconcilerConfig.description
        ReconcilerConfig.displayName
        ReconcilerConfig.key
