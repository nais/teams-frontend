module Admin exposing (..)

import Backend.InputObject exposing (ReconcilerConfigInput)
import Backend.Scalar exposing (Map(..), ReconcilerConfigKey(..), ReconcilerName(..))
import ConfigValue exposing (ConfigValue(..))
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, button, div, form, h2, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Queries.Do exposing (mutate, query)
import Queries.ReconcilerQueries exposing (ReconcilerConfigData, ReconcilerData, getReconcilersQuery, updateReconcilerConfigMutation)
import Session exposing (Session)


type alias Model =
    { session : Session
    , reconcilers : Result String (List ReconcilerData)
    , input : List ReconcilerData
    }


type Msg
    = GotReconcilersResponse (Result (Graphql.Http.Error (List ReconcilerData)) (List ReconcilerData))
    | GotUpdateReconcilerResponse (Result (Graphql.Http.Error ReconcilerData) ReconcilerData)
    | Submit ReconcilerName
    | OnInput ReconcilerName ReconcilerConfigKey String
    | OnToggle ReconcilerName Bool


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , reconcilers = Err "not fetched yet"
      , input = []
      }
    , query getReconcilersQuery GotReconcilersResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit reconcilerName ->
            -- TODO (show some confirmation dialog -> send gql msg)
            let
                config =
                    model.input
                        |> List.filter (\i -> i.name == reconcilerName)
                        |> List.map (\i -> i.config)
                        |> List.head

                inputs =
                    case config of
                        Just cfg ->
                            cfg
                                |> List.filter (\kv -> ConfigValue.hasValue kv.value)
                                |> List.map (\kv -> { key = kv.key, value = ConfigValue.string kv.value })

                        Nothing ->
                            []
            in
            ( model, mutate (updateReconcilerConfigMutation reconcilerName inputs) GotUpdateReconcilerResponse )

        OnInput reconciler configKey value ->
            ( { model | input = List.map (mapReconcilerConfigValue reconciler configKey value) model.input }, Cmd.none )

        GotUpdateReconcilerResponse r ->
            case r of
                Ok rd ->
                    case model.reconcilers of
                        Ok rds ->
                            ( { model | reconcilers = Ok (List.map (mapReconciler rd) rds) }, Cmd.none )

                        Err e ->
                            ( { model | reconcilers = Err e }, Cmd.none )

                Err (Graphql.Http.HttpError _) ->
                    ( { model | reconcilers = Err "graphql http error" }, Cmd.none )

                Err (GraphqlError _ _) ->
                    ( { model | reconcilers = Err "graphql error" }, Cmd.none )

        GotReconcilersResponse r ->
            case r of
                Ok rds ->
                    ( { model | reconcilers = Ok rds, input = rds }, Cmd.none )

                Err (Graphql.Http.HttpError _) ->
                    ( { model | reconcilers = Err "graphql http error" }, Cmd.none )

                Err (GraphqlError _ _) ->
                    ( { model | reconcilers = Err "graphql error" }, Cmd.none )

        OnToggle reconcilerName value ->
            ( { model | input = List.map (mapReconcilerEnabled reconcilerName value) model.input }, Cmd.none )


mapReconcilerEnabled : ReconcilerName -> Bool -> ReconcilerData -> ReconcilerData
mapReconcilerEnabled name enabled reconciler =
    if reconciler.name == name then
        { reconciler | enabled = enabled }

    else
        reconciler


mapReconcilerConfig : ReconcilerConfigKey -> String -> ReconcilerConfigData -> ReconcilerConfigData
mapReconcilerConfig key value input =
    if input.key == key then
        { input | key = key, value = ConfigValue value }

    else
        input


mapReconcilerConfigValue : ReconcilerName -> ReconcilerConfigKey -> String -> ReconcilerData -> ReconcilerData
mapReconcilerConfigValue name key value reconciler =
    if reconciler.name == name then
        { reconciler | config = List.map (mapReconcilerConfig key value) reconciler.config }

    else
        reconciler


mapReconciler : ReconcilerData -> ReconcilerData -> ReconcilerData
mapReconciler new existing =
    if new.name == existing.name then
        new

    else
        existing


view : Model -> Html Msg
view model =
    case model.reconcilers of
        Ok rd ->
            renderForms model.input

        Err e ->
            text e


toggleReconcilerElement : ReconcilerData -> Html Msg
toggleReconcilerElement rd =
    li [ class "checkbox" ]
        [ label [ for (reconcilerEnabledId rd) ] [ text "Enabled" ]
        , input
            [ type_ "checkbox"
            , checked rd.enabled
            , id (reconcilerEnabledId rd)
            , onCheck (OnToggle rd.name)
            ]
            []
        ]


configElement : (ReconcilerConfigKey -> String -> Msg) -> ReconcilerConfigData -> Html Msg
configElement msg rcd =
    let
        (ReconcilerConfigKey idKey) =
            rcd.key
    in
    li
        [ classList
            [ ( "reconcilerConfigured", rcd.configured )
            , ( "reconcilerNotConfigured", not rcd.configured )
            ]
        ]
        [ label [ for idKey ] [ text rcd.displayName ]
        , input [ type_ "text", id idKey, onInput (msg rcd.key) ] []
        , p [] [ text rcd.description ]
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


reconcilerEnabledId : ReconcilerData -> String
reconcilerEnabledId rd =
    let
        (ReconcilerName name) =
            rd.name
    in
    name ++ ":enabled"


viewReconcilerConfig : ReconcilerData -> Html Msg
viewReconcilerConfig rd =
    form
        [ onSubmit (Submit rd.name)
        , classList
            [ ( "reconcilerConfigured", rd.configured )
            , ( "reconcilerNotConfigured", not rd.configured )
            ]
        ]
        [ h3 [] [ text rd.displayname ]
        , p [] [ text rd.description ]
        , ul []
            (toggleReconcilerElement rd
                :: List.map (configElement (OnInput rd.name)) rd.config
            )
        , button [ type_ "submit" ] [ text "Save" ]
        ]


renderForms : List ReconcilerData -> Html Msg
renderForms lrd =
    div []
        (h2 [] [ text "Set up reconcilers" ]
            :: List.map viewReconcilerConfig lrd
        )
