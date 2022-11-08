module Api.Reconciler exposing (..)

import Backend.InputObject exposing (ReconcilerConfigInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.Reconciler as Reconciler
import Backend.Object.ReconcilerConfig as ReconcilerConfig
import Backend.Query as Query
import Backend.Scalar exposing (ReconcilerName)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, mapOrFail)


type alias ReconcilerConfigData =
    { configured : Bool
    , description : String
    , displayName : String
    , key : Backend.Scalar.ReconcilerConfigKey
    , value : Maybe String
    , secret : Bool
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


getReconcilers : SelectionSet (List ReconcilerData) RootQuery
getReconcilers =
    Query.reconcilers reconcilerDataSelection


updateReconcilerConfig : ReconcilerName -> List ReconcilerConfigInput -> SelectionSet ReconcilerData Graphql.Operation.RootMutation
updateReconcilerConfig name config =
    Mutation.configureReconciler { name = name, config = config } reconcilerDataSelection


enableReconciler : ReconcilerName -> SelectionSet ReconcilerData Graphql.Operation.RootMutation
enableReconciler name =
    Mutation.enableReconciler { name = name } reconcilerDataSelection


disableReconciler : ReconcilerName -> SelectionSet ReconcilerData Graphql.Operation.RootMutation
disableReconciler name =
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
    Graphql.SelectionSet.succeed ReconcilerConfigData
        |> Graphql.SelectionSet.with ReconcilerConfig.configured
        |> Graphql.SelectionSet.with ReconcilerConfig.description
        |> Graphql.SelectionSet.with ReconcilerConfig.displayName
        |> Graphql.SelectionSet.with ReconcilerConfig.key
        |> Graphql.SelectionSet.with ReconcilerConfig.value
        |> Graphql.SelectionSet.with ReconcilerConfig.secret
