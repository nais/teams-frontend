module Admin exposing (..)

import Backend.Scalar exposing (Map(..), ReconcilerConfigKey(..), ReconcilerName(..))
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, button, div, form, h2, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (classList, for, id, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Queries.Do exposing (mutate, query)
import Queries.ReconcilerQueries exposing (ReconcilerConfigData, ReconcilerData, getReconcilersQuery, updateReconcilerConfigMutation)
import Session exposing (Session)


type alias Model =
    { session : Session
    , reconcilers : Result String (List ReconcilerData)
    , reconcilerFormInputs : List ReconcilerFormInput
    }


type alias ReconcilerFormInput =
    { reconcilerName : ReconcilerName
    , key : ReconcilerConfigKey
    , value : String
    }


type Msg
    = GotReconcilersResponse (Result (Graphql.Http.Error (List ReconcilerData)) (List ReconcilerData))
    | GotUpdateReconcilerResponse (Result (Graphql.Http.Error ReconcilerData) ReconcilerData)
    | Submit ReconcilerName
    | OnInput ReconcilerName ReconcilerConfigKey String
    | OnToggle ReconcilerName String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , reconcilers = Err "not fetched yet"
      , reconcilerFormInputs = []
      }
    , query getReconcilersQuery GotReconcilersResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit reconcilerName ->
            let
                config =
                    model.reconcilerFormInputs
                        |> List.filter (\i -> i.reconcilerName == reconcilerName)
                        |> List.map (\i -> { key = i.key, value = i.value })
            in
            ( model, mutate (updateReconcilerConfigMutation reconcilerName config) GotUpdateReconcilerResponse )

        OnInput reconciler configKey value ->
            ( { model | reconcilerFormInputs = List.map (updateReconcilerFormInput reconciler configKey value) model.reconcilerFormInputs }, Cmd.none )

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
                    ( { model | reconcilers = Ok rds, reconcilerFormInputs = initialConfigData rds }, Cmd.none )

                Err (Graphql.Http.HttpError _) ->
                    ( { model | reconcilers = Err "graphql http error" }, Cmd.none )

                Err (GraphqlError _ _) ->
                    ( { model | reconcilers = Err "graphql error" }, Cmd.none )

        OnToggle reconcilerName t ->
            -- TODO toggle reconcilers (show some confirmation dialog -> send gql msg)
            ( model, Cmd.none )


initialConfigData : List ReconcilerData -> List ReconcilerFormInput
initialConfigData rds =
    List.concatMap (\rd -> List.map (\c -> { reconcilerName = rd.name, key = c.key, value = "" }) rd.config) rds


updateReconcilerFormInput : ReconcilerName -> ReconcilerConfigKey -> String -> ReconcilerFormInput -> ReconcilerFormInput
updateReconcilerFormInput reconciler key value formInput =
    if key == formInput.key && reconciler == formInput.reconcilerName then
        { reconcilerName = reconciler
        , key = key
        , value = value
        }

    else
        formInput


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
            renderForms rd

        Err e ->
            text e


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


reconcilerConfig : ReconcilerData -> Html Msg
reconcilerConfig rd =
    p
        [ classList
            [ ( "reconcilerConfigured", rd.configured )
            , ( "reconcilerNotConfigured", not rd.configured )
            ]
        ]
        [ h3 [] [ text rd.displayname ]
        , p [] [ text rd.description ]
        , form [ onSubmit (Submit rd.name) ]
            [ ul []
                (li []
                    [ label [ for (reconcilerEnabledId rd) ] [ text "enabled" ]
                    , input
                        [ type_ "checkbox"
                        , value (boolToString rd.enabled)
                        , id (reconcilerEnabledId rd)
                        , onInput (OnToggle rd.name)
                        ]
                        []
                    ]
                    :: List.map (configElement (OnInput rd.name)) rd.config
                    ++ [ button [ type_ "submit" ] [ text "Save" ]
                       ]
                )
            ]
        ]


renderForms : List ReconcilerData -> Html Msg
renderForms lrd =
    div []
        (h2 [] [ text "Set up reconcilers" ]
            :: List.map reconcilerConfig lrd
        )
