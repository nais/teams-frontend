module Page.Admin exposing (Model, Msg(..), init, update, view)

import Api.Do exposing (mutate, query)
import Api.Error
import Api.Reconciler exposing (disableReconciler, enableReconciler, getReconcilers, synchronizeAllTeams, updateReconcilerConfig)
import Backend.Scalar exposing (ReconcilerConfigKey(..), ReconcilerName(..))
import Component.Buttons exposing (smallButton)
import Component.Card as Card exposing (Card)
import DataModel exposing (Reconciler, ReconcilerConfig)
import Graphql.Http
import Html exposing (Html, a, button, div, form, h2, input, label, li, p, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, for, href, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)


type alias Model =
    { session : Session
    , error : Maybe String
    , reconcilers : RemoteData (Graphql.Http.Error (List Reconciler)) (List Reconciler)
    }


type Msg
    = GotReconcilersResponse (RemoteData (Graphql.Http.Error (List Reconciler)) (List Reconciler))
    | GotUpdateReconcilerResponse (Result (Graphql.Http.Error Reconciler) Reconciler)
    | GotEnableReconcilerResponse (Result (Graphql.Http.Error Reconciler) Reconciler)
    | GotEmptyResponse (Result (Graphql.Http.Error ()) ())
    | Submit ReconcilerName
    | OnInput ReconcilerName ReconcilerConfigKey String
    | OnToggle ReconcilerName Bool
    | OnSynchronizeAllTeams
    | AckError
    | Reload
    | NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , error = Nothing
      , reconcilers = NotAsked
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    query getReconcilers (RemoteData.fromResult >> GotReconcilersResponse)


mapReconcilers : (a -> b) -> RemoteData error (List a) -> RemoteData error (List b)
mapReconcilers fn reconcilers =
    case reconcilers of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure e ->
            Failure e

        Success r ->
            Success (List.map fn r)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AckError ->
            ( { model | error = Nothing }, Cmd.none )

        Reload ->
            ( { model | reconcilers = NotAsked, error = Nothing }, loadData )

        Submit name ->
            saveReconcilerConfig name model

        OnInput reconciler configKey value ->
            ( { model | reconcilers = mapReconcilers (mapReconcilerConfigValue reconciler configKey value) model.reconcilers }, Cmd.none )

        GotUpdateReconcilerResponse r ->
            case r of
                Ok rd ->
                    let
                        updatedModel : Model
                        updatedModel =
                            { model | error = Nothing, reconcilers = mapReconcilers (mapReconciler rd) model.reconcilers }

                        maybeExisting : Maybe Reconciler
                        maybeExisting =
                            filterReconciler rd.name model
                    in
                    case maybeExisting of
                        Just existing ->
                            ( updatedModel, enableDisableReconciler existing )

                        Nothing ->
                            ( updatedModel, Cmd.none )

                Err e ->
                    ( { model | error = Just (Api.Error.errorToString e) }, Cmd.none )

        GotEnableReconcilerResponse r ->
            case r of
                Ok rd ->
                    ( { model | error = Nothing, reconcilers = mapReconcilers (mapReconciler rd) model.reconcilers }, Cmd.none )

                Err e ->
                    ( { model | error = Just (Api.Error.errorToString e) }, Cmd.none )

        GotEmptyResponse r ->
            case r of
                Ok _ ->
                    ( { model | error = Nothing }, Cmd.none )

                Err e ->
                    ( { model | error = Just (Api.Error.errorToString e) }, Cmd.none )

        GotReconcilersResponse r ->
            ( { model | reconcilers = r }, Cmd.none )

        OnToggle name value ->
            ( { model | reconcilers = mapReconcilers (mapReconcilerEnabled name value) model.reconcilers }, Cmd.none )

        OnSynchronizeAllTeams ->
            ( model, mutate synchronizeAllTeams GotEmptyResponse )

        NoOp ->
            ( model, Cmd.none )


saveReconcilerConfig : ReconcilerName -> Model -> ( Model, Cmd Msg )
saveReconcilerConfig name model =
    -- TODO (show some confirmation dialog -> send gql msg)
    let
        config : Maybe (List ReconcilerConfig)
        config =
            case model.reconcilers of
                Success recs ->
                    recs
                        |> List.filter (\i -> i.name == name)
                        |> List.map (\i -> i.config)
                        |> List.head

                _ ->
                    Nothing

        inputs : List { key : ReconcilerConfigKey, value : String }
        inputs =
            case config of
                Just cfg ->
                    cfg
                        |> List.filter (\kv -> not (kv.value == Nothing))
                        |> List.map (\kv -> { key = kv.key, value = Maybe.withDefault "" kv.value })

                Nothing ->
                    []
    in
    ( model, mutate (updateReconcilerConfig name inputs) GotUpdateReconcilerResponse )


enableDisableReconciler : Reconciler -> Cmd Msg
enableDisableReconciler reconciler =
    if reconciler.enabled then
        mutate (enableReconciler reconciler.name) GotEnableReconcilerResponse

    else
        mutate (disableReconciler reconciler.name) GotEnableReconcilerResponse


mapReconcilerEnabled : ReconcilerName -> Bool -> Reconciler -> Reconciler
mapReconcilerEnabled name enabled reconciler =
    if reconciler.name == name then
        { reconciler | enabled = enabled }

    else
        reconciler


mapReconcilerConfig : ReconcilerConfigKey -> String -> ReconcilerConfig -> ReconcilerConfig
mapReconcilerConfig key value input =
    if input.key == key then
        if value == "" then
            { input | key = key, value = Nothing }

        else
            { input | key = key, value = Just value }

    else
        input


mapReconcilerConfigValue : ReconcilerName -> ReconcilerConfigKey -> String -> Reconciler -> Reconciler
mapReconcilerConfigValue name key value reconciler =
    if reconciler.name == name then
        { reconciler | config = List.map (mapReconcilerConfig key value) reconciler.config }

    else
        reconciler


mapReconciler : Reconciler -> Reconciler -> Reconciler
mapReconciler new existing =
    if new.name == existing.name then
        new

    else
        existing


filterReconciler : ReconcilerName -> Model -> Maybe Reconciler
filterReconciler name model =
    case model.reconcilers of
        Success reconcilers ->
            List.filter (\rd -> rd.name == name) reconcilers
                |> List.head

        _ ->
            Nothing


reconcilerName : ReconcilerName -> String
reconcilerName (ReconcilerName s) =
    s


toggleReconcilerElement : Reconciler -> Html Msg
toggleReconcilerElement rd =
    li [ class "checkbox" ]
        [ label [ for (reconcilerEnabledId rd) ] [ text "Enable synchronization" ]
        , input
            [ type_ "checkbox"
            , checked rd.enabled
            , id (reconcilerEnabledId rd)
            , onCheck (OnToggle rd.name)
            ]
            []
        ]


isConfiguredSecret : ReconcilerConfig -> Bool
isConfiguredSecret rcd =
    rcd.configured && rcd.secret && rcd.value == Nothing


secretHelpText : ReconcilerConfig -> List (Html msg)
secretHelpText rcd =
    if isConfiguredSecret rcd then
        [ p [ class "secret-help-text" ] [ text "This value is already configured. It is hidden because it is secret." ] ]

    else
        []


placeholderText : ReconcilerConfig -> String
placeholderText rcd =
    if isConfiguredSecret rcd then
        "********"

    else
        ""


reconcilerDescription : Reconciler -> Html msg
reconcilerDescription rd =
    -- Use this function to override reconciler descriptions
    case rd.name of
        ReconcilerName "google:workspace-admin" ->
            div []
                [ p []
                    [ text "Maintains "
                    , a [ href "https://accounts.google.com" ] [ text "Google Account" ]
                    , text " groups and memberships."
                    ]
                , p []
                    [ text "These groups are used for security features such as user authentication and authorization. "
                    , text "The groups are assigned permissions on Google Cloud Platform and other Google products."
                    ]
                ]

        ReconcilerName "google:gcp:project" ->
            div []
                [ p []
                    [ text "Maintains "
                    , a [ href "https://console.cloud.google.com" ] [ text "Google Cloud Platform" ]
                    , text " projects for teams."
                    ]
                , p []
                    [ text "Each team gets one project per environment. "
                    , text "Environments can be set up in "
                    , a [ href "https://fasit.nais.io" ] [ text "Fasit" ]
                    , text "."
                    ]
                , p []
                    [ text "The project will have its corresponding Google Account team assigned as owner."
                    ]
                ]

        _ ->
            p [] [ text rd.description ]


configDescription : ReconcilerConfig -> Html msg
configDescription rcd =
    -- Use this function to override config descriptions
    p [] [ text rcd.description ]


configElement : (ReconcilerConfigKey -> String -> Msg) -> ReconcilerConfig -> Html Msg
configElement msg rcd =
    let
        (ReconcilerConfigKey idKey) =
            rcd.key

        element : Html Msg
        element =
            case idKey of
                "github:app_private_key" ->
                    textarea [ id idKey, onInput (msg rcd.key), value (Maybe.withDefault "" rcd.value), placeholder (placeholderText rcd) ] []

                _ ->
                    input [ type_ "text", id idKey, onInput (msg rcd.key), value (Maybe.withDefault "" rcd.value), placeholder (placeholderText rcd) ] []
    in
    li
        [ classList
            [ ( "reconcilerConfigured", rcd.configured )
            , ( "reconcilerNotConfigured", not rcd.configured )
            ]
        ]
        ([ label [ for idKey ] [ text rcd.displayName ]
         , element
         ]
            ++ secretHelpText rcd
            ++ [ configDescription rcd ]
        )


reconcilerEnabledId : Reconciler -> String
reconcilerEnabledId rd =
    reconcilerName rd.name ++ ":enabled"


viewReconcilerConfig : Reconciler -> Card Msg
viewReconcilerConfig rd =
    Card.new rd.displayname
        |> Card.withContents
            [ form
                [ onSubmit (Submit rd.name)
                , classList
                    [ ( "reconcilerConfigured", rd.configured )
                    , ( "reconcilerNotConfigured", not rd.configured )
                    ]
                ]
                [ reconcilerDescription rd
                , ul []
                    (toggleReconcilerElement rd
                        :: List.map (configElement (OnInput rd.name)) rd.config
                    )
                , div [ class "button-row" ]
                    [ button [ type_ "submit" ] [ text "Save" ]
                    ]
                ]
            ]


viewForm : List Reconciler -> Html Msg
viewForm lrd =
    div [ class "cards" ] ((viewAdminActions :: List.map viewReconcilerConfig lrd) |> List.map Card.render)


viewAdminActions : Card Msg
viewAdminActions =
    Card.new "Admin actions"
        |> Card.withButtons [ smallButton OnSynchronizeAllTeams "synchronize" "Synchronize teams" ]


onClickStopPropagation : msg -> Html.Attribute msg
onClickStopPropagation msg =
    Html.Events.stopPropagationOn "click" <| Decode.succeed ( msg, True )


modal : String -> Msg -> List (Html Msg) -> Html Msg
modal title hide content =
    div [ class "modal", onClick hide ]
        [ div [ onClickStopPropagation NoOp ] (h2 [] [ text title ] :: content) ]


viewLoadFailure : String -> Html Msg
viewLoadFailure e =
    div []
        [ h2 [] [ text "Failed to load data from server" ]
        , p [ class "server-error-message" ] [ text e ]
        , button [ onClick Reload ] [ text "Try again" ]
        ]


view : Model -> Html Msg
view model =
    case model.reconcilers of
        Success rd ->
            case model.error of
                Nothing ->
                    viewForm rd

                Just error ->
                    modal "Server error"
                        AckError
                        [ div [ class "server-error-message" ] [ text error ]
                        , button [ onClick AckError ] [ text "Got it" ]
                        ]

        NotAsked ->
            h2 [] [ text "No data loaded yet" ]

        Loading ->
            h2 [] [ text "Loading reconcilers..." ]

        Failure e ->
            viewLoadFailure (Api.Error.errorToString e)
