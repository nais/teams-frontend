module Admin exposing (..)

import Backend.Scalar exposing (Map(..), ReconcilerName(..))
import CreateTeam exposing (Msg(..))
import Dict exposing (Dict)
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, div, form, h2, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (classList, for, id, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Queries.Do exposing (mutate, query)
import Queries.ReconcilerQueries exposing (ReconcilerConfigData, ReconcilerData, getReconcilersQuery, updateReconcilerConfigMutation)
import Session exposing (Session)


type alias Model =
    { session : Session
    , reconcilersData : Result String (List ReconcilerData)
    , reconcilersConfig : Dict String String
    }


type Msg
    = GotReconcilersResponse (Result (Graphql.Http.Error (List ReconcilerData)) (List ReconcilerData))
    | GotUpdateReconcilerResponse (Result (Graphql.Http.Error ReconcilerData) ReconcilerData)
    | Submit ReconcilerName
    | OnInput String String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , reconcilersData = Err "not fetched yet"
      , reconcilersConfig = Dict.empty
      }
    , query getReconcilersQuery GotReconcilersResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit reconcilerName ->
            let
                (ReconcilerName name) =
                    reconcilerName

                config =
                    Map (Dict.filter (\k _ -> String.startsWith name k) model.reconcilersConfig)
            in
            ( model, mutate (updateReconcilerConfigMutation reconcilerName config) GotUpdateReconcilerResponse )

        OnInput k v ->
            ( { model | reconcilersConfig = Dict.insert k v model.reconcilersConfig }, Cmd.none )

        GotUpdateReconcilerResponse r ->
            case r of
                Ok rd ->
                    case model.reconcilersData of
                        Ok rds ->
                            ( { model | reconcilersData = Ok (List.map (mapReconciler rd) rds) }, Cmd.none )

                        Err e ->
                            ( { model | reconcilersData = Err e }, Cmd.none )

                Err (Graphql.Http.HttpError _) ->
                    ( { model | reconcilersData = Err "graphql http error" }, Cmd.none )

                Err (GraphqlError _ _) ->
                    ( { model | reconcilersData = Err "graphql error" }, Cmd.none )

        GotReconcilersResponse r ->
            case r of
                Ok rds ->
                    ( { model | reconcilersData = Ok rds }, Cmd.none )

                Err (Graphql.Http.HttpError _) ->
                    ( { model | reconcilersData = Err "graphql http error" }, Cmd.none )

                Err (GraphqlError _ _) ->
                    ( { model | reconcilersData = Err "graphql error" }, Cmd.none )


mapReconciler : ReconcilerData -> ReconcilerData -> ReconcilerData
mapReconciler new existing =
    if new.name == existing.name then
        new

    else
        existing


view : Model -> Html Msg
view model =
    case model.reconcilersData of
        Ok rd ->
            renderForms rd

        Err e ->
            text e


configElement : (String -> String -> Msg) -> ReconcilerConfigData -> Html Msg
configElement msg rcd =
    li
        [ classList
            [ ( "reconcilerConfigured", rcd.configured )
            , ( "reconcilerNotConfigured", not rcd.configured )
            ]
        ]
        [ label [ for rcd.key ] [ text rcd.displayName ]
        , input [ type_ "text", id rcd.key, onInput (msg rcd.key) ] []
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
                        , onInput (OnInput (reconcilerEnabledId rd))
                        ]
                        []
                    ]
                    :: List.map (configElement OnInput) rd.config
                    ++ [ input
                            [ type_ "submit"
                            , value "Save"
                            ]
                            []
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
