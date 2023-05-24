module Api.Reconciler exposing (disableReconciler, enableReconciler, getReconcilers, synchronizeAllTeams, updateReconcilerConfig)

import Backend.InputObject exposing (ReconcilerConfigInput)
import Backend.Mutation as Mutation
import Backend.Object
import Backend.Object.Reconciler as Reconciler
import Backend.Object.ReconcilerConfig as ReconcilerConfig
import Backend.Query as Query
import Backend.Scalar exposing (ReconcilerName)
import DataModel exposing (Reconciler, ReconcilerConfig)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)


getReconcilers : SelectionSet (List Reconciler) RootQuery
getReconcilers =
    Query.reconcilers reconcilerDataSelection


updateReconcilerConfig : ReconcilerName -> List ReconcilerConfigInput -> SelectionSet Reconciler Graphql.Operation.RootMutation
updateReconcilerConfig name config =
    Mutation.configureReconciler { name = name, config = config } reconcilerDataSelection


enableReconciler : ReconcilerName -> SelectionSet Reconciler Graphql.Operation.RootMutation
enableReconciler name =
    Mutation.enableReconciler { name = name } reconcilerDataSelection


disableReconciler : ReconcilerName -> SelectionSet Reconciler Graphql.Operation.RootMutation
disableReconciler name =
    Mutation.disableReconciler { name = name } reconcilerDataSelection


reconcilerDataSelection : SelectionSet Reconciler Backend.Object.Reconciler
reconcilerDataSelection =
    Graphql.SelectionSet.map8 Reconciler
        Reconciler.configured
        Reconciler.description
        Reconciler.displayName
        Reconciler.enabled
        Reconciler.name
        Reconciler.runOrder
        (Reconciler.config reconcilerConfigDataSelection)
        Reconciler.usesTeamMemberships


reconcilerConfigDataSelection : SelectionSet ReconcilerConfig Backend.Object.ReconcilerConfig
reconcilerConfigDataSelection =
    Graphql.SelectionSet.succeed ReconcilerConfig
        |> Graphql.SelectionSet.with ReconcilerConfig.configured
        |> Graphql.SelectionSet.with ReconcilerConfig.description
        |> Graphql.SelectionSet.with ReconcilerConfig.displayName
        |> Graphql.SelectionSet.with ReconcilerConfig.key
        |> Graphql.SelectionSet.with ReconcilerConfig.value
        |> Graphql.SelectionSet.with ReconcilerConfig.secret


synchronizeAllTeams : SelectionSet () Graphql.Operation.RootMutation
synchronizeAllTeams =
    Mutation.synchronizeAllTeams emptyDataSelection


emptyDataSelection : SelectionSet () a
emptyDataSelection =
    Graphql.SelectionSet.empty
