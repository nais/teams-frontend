module Admin exposing (..)

import Backend.Scalar exposing (ReconcilerName(..))
import CreateTeam exposing (Msg(..))
import Dict exposing (Dict)
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, br, div, form, h2, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (for, id, type_)
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


inputGroup : (String -> String -> Msg) -> ReconcilerConfigData -> Html Msg
inputGroup msg rcd =
    li []
        [ label [ for rcd.key ] [ text rcd.displayName ]
        , if rcd.configured then
            text "configured" -- todo checkbox or smt

          else
            text "not configured"
        , input [ type_ "text", id rcd.key, onInput (msg rcd.key) ] []
        , p [] [ text rcd.description ]
        ]


renderForm : ReconcilerData -> Html Msg
renderForm rd =
    div []
        [ h3 [] [ text rd.displayname ]
        , p [] [ text rd.description ]
        , if rd.configured then
            text "configured" -- todo checkbox or smt

          else
            text "not configured"
        , form [ onSubmit (Submit rd.name) ]
            [ ul [] (List.map (inputGroup OnInput) rd.config)
            ]
        ]



{- form [ onSubmit CreateTeamSubmit ]
       (ul []
           [ textbox SlugChanged "slug" "Identifier" "customer-satisfaction"
           , textbox PurposeChanged "purpose" "Purpose of the team" "Making sure customers have a good user experience"
           ]
           :: errorView model.error
           ++ [ input [ type_ "submit", value "Create new team" ] []
              ]
       )
   ]
-}


renderForms : List ReconcilerData -> Html Msg
renderForms lrd =
    div []
        (h2 [] [ text "Set up reconcilers" ]
            :: List.map renderForm lrd
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
