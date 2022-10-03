module Admin exposing (..)

import Backend.Scalar exposing (ReconcilerName(..))
import CreateTeam exposing (Msg(..))
import Dict exposing (Dict)
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, div, form, h2, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (classList, for, id, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Queries.Do exposing (query)
import Queries.ReconcilerQueries exposing (ReconcilerConfigData, ReconcilerData, getReconcilersQuery)
import Session exposing (Session)


type alias Model =
    { session : Session
    , reconcilerData : Result String (List ReconcilerData)
    , reconcilerConfig : Dict String String
    }


type Msg
    = GotReconcilerResponse (Result (Graphql.Http.Error (List ReconcilerData)) (List ReconcilerData))
    | Submit ReconcilerName
    | OnInput String String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , reconcilerData = Err "not fetched yet"
      , reconcilerConfig = Dict.empty
      }
    , query getReconcilersQuery GotReconcilerResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit _ ->
            ( model, Cmd.none )

        OnInput k v ->
            ( { model | reconcilerConfig = Dict.insert k v model.reconcilerConfig }, Cmd.none )

        GotReconcilerResponse r ->
            case r of
                Ok rd ->
                    ( { model | reconcilerData = Ok rd }, Cmd.none )

                Err (Graphql.Http.HttpError _) ->
                    ( { model | reconcilerData = Err "graphql http error" }, Cmd.none )

                Err (GraphqlError _ _) ->
                    ( { model | reconcilerData = Err "graphql error" }, Cmd.none )


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


b2s : Bool -> String
b2s b =
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
                (List.map (configElement OnInput) rd.config
                    ++ [ li []
                            [ label [ for (reconcilerEnabledId rd) ] [ text "enabled" ]
                            , input
                                [ type_ "checkbox"
                                , value (b2s rd.enabled)
                                , id (reconcilerEnabledId rd)
                                , onInput (OnInput (reconcilerEnabledId rd))
                                ]
                                []
                            ]
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


view : Model -> Html Msg
view model =
    case model.reconcilerData of
        Ok rd ->
            renderForms rd

        Err e ->
            text e


reconcilerNameStr : ReconcilerName -> String
reconcilerNameStr (ReconcilerName s) =
    s
