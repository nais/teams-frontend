module Admin exposing (..)

import Backend.Scalar exposing (ReconcilerConfigKey(..), ReconcilerName(..))
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, button, div, form, h2, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Queries.Do exposing (mutate, query)
import Queries.Error
import Queries.ReconcilerQueries exposing (ReconcilerConfigData, ReconcilerData, disableReconcilerMutation, enableReconcilerMutation, getReconcilersQuery, updateReconcilerConfigMutation)
import Session exposing (Session)


type alias Model =
    { session : Session
    , reconcilers : Result String (List ReconcilerData)
    }


type Msg
    = GotReconcilersResponse (Result (Graphql.Http.Error (List ReconcilerData)) (List ReconcilerData))
    | GotUpdateReconcilerResponse (Result (Graphql.Http.Error ReconcilerData) ReconcilerData)
    | GotEnableReconcilerResponse (Result (Graphql.Http.Error ReconcilerData) ReconcilerData)
    | Submit ReconcilerName
    | OnInput ReconcilerName ReconcilerConfigKey String
    | OnToggle ReconcilerName Bool


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , reconcilers = Err "not fetched yet"
      }
    , query getReconcilersQuery GotReconcilersResponse
    )


mapReconcilers fn reconcilers =
    case reconcilers of
        Ok r ->
            Ok (List.map fn r)

        Err e ->
            Err e


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit name ->
            saveReconcilerConfig name model

        OnInput reconciler configKey value ->
            ( { model | reconcilers = mapReconcilers (mapReconcilerConfigValue reconciler configKey value) model.reconcilers }, Cmd.none )

        GotUpdateReconcilerResponse r ->
            case r of
                Ok rd ->
                    let
                        updatedModel =
                            { model | reconcilers = mapReconcilers (mapReconciler rd) model.reconcilers }

                        maybeExisting =
                            filterReconciler rd.name model
                    in
                    case maybeExisting of
                        Just existing ->
                            ( updatedModel, enableDisableReconciler existing )

                        Nothing ->
                            ( updatedModel, Cmd.none )

                Err e ->
                    ( { model | reconcilers = Err (Queries.Error.errorToString e) }, Cmd.none )

        GotEnableReconcilerResponse r ->
            case r of
                Ok rd ->
                    ( { model | reconcilers = mapReconcilers (mapReconciler rd) model.reconcilers }, Cmd.none )

                Err e ->
                    ( { model | reconcilers = Err (Queries.Error.errorToString e) }, Cmd.none )

        GotReconcilersResponse r ->
            case r of
                Ok rds ->
                    ( { model | reconcilers = Ok rds }, Cmd.none )

                Err e ->
                    ( { model | reconcilers = Err (Queries.Error.errorToString e) }, Cmd.none )

        OnToggle name value ->
            ( { model | reconcilers = mapReconcilers (mapReconcilerEnabled name value) model.reconcilers }, Cmd.none )


saveReconcilerConfig : ReconcilerName -> Model -> ( Model, Cmd Msg )
saveReconcilerConfig name model =
    -- TODO (show some confirmation dialog -> send gql msg)
    let
        config =
            case model.reconcilers of
                Ok recs ->
                    recs
                        |> List.filter (\i -> i.name == name)
                        |> List.map (\i -> i.config)
                        |> List.head

                _ ->
                    Nothing

        inputs =
            case config of
                Just cfg ->
                    cfg
                        |> List.filter (\kv -> not (kv.value == Nothing))
                        |> List.map (\kv -> { key = kv.key, value = Maybe.withDefault "" kv.value })

                Nothing ->
                    []
    in
    ( model, mutate (updateReconcilerConfigMutation name inputs) GotUpdateReconcilerResponse )


enableDisableReconciler : ReconcilerData -> Cmd Msg
enableDisableReconciler reconciler =
    let
        func =
            if reconciler.enabled then
                enableReconcilerMutation

            else
                disableReconcilerMutation
    in
    mutate (func reconciler.name) GotEnableReconcilerResponse


mapReconcilerEnabled : ReconcilerName -> Bool -> ReconcilerData -> ReconcilerData
mapReconcilerEnabled name enabled reconciler =
    if reconciler.name == name then
        { reconciler | enabled = enabled }

    else
        reconciler


mapReconcilerConfig : ReconcilerConfigKey -> String -> ReconcilerConfigData -> ReconcilerConfigData
mapReconcilerConfig key value input =
    if input.key == key then
        if value == "" then
            { input | key = key, value = Nothing }

        else
            { input | key = key, value = Just value }

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


filterReconciler : ReconcilerName -> Model -> Maybe ReconcilerData
filterReconciler name model =
    case model.reconcilers of
        Ok reconcilers ->
            List.filter (\rd -> rd.name == name) reconcilers
                |> List.head

        _ ->
            Nothing


reconcilerName : ReconcilerName -> String
reconcilerName (ReconcilerName s) =
    s


view : Model -> Html Msg
view model =
    case model.reconcilers of
        Ok rd ->
            renderForms rd

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


isConfiguredSecret : ReconcilerConfigData -> Bool
isConfiguredSecret rcd =
    rcd.configured && rcd.secret && rcd.value == Nothing


secretHelpText : ReconcilerConfigData -> List (Html msg)
secretHelpText rcd =
    if isConfiguredSecret rcd then
        [ p [ class "secret-help-text" ] [ text "This value is already configured. It is hidden because it is secret." ] ]

    else
        []


placeholderText : ReconcilerConfigData -> String
placeholderText rcd =
    if isConfiguredSecret rcd then
        "********"

    else
        ""


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
        ([ label [ for idKey ] [ text rcd.displayName ]
         , input [ type_ "text", id idKey, onInput (msg rcd.key), value (Maybe.withDefault "" rcd.value), placeholder (placeholderText rcd) ] []
         ]
            ++ secretHelpText rcd
            ++ [ p [] [ text rcd.description ]
               ]
        )


reconcilerEnabledId : ReconcilerData -> String
reconcilerEnabledId rd =
    reconcilerName rd.name ++ ":enabled"


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
