module Page.DeleteTeam exposing (Model, Msg(..), init, update, view)

import Api.Do
import Api.Error exposing (errorToString)
import Api.Str exposing (slugStr, uuidStr)
import Api.Team
import Backend.Query exposing (team)
import Backend.Scalar exposing (Slug(..), Uuid(..))
import Component.ResourceTable as ResourceTable
import DataModel exposing (Team, TeamDeleteConfirmed, TeamDeleteKey)
import Graphql.Http
import Html exposing (Html, button, div, h2, p, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , phase : Phase
    , slug : Slug
    }


type Phase
    = Init
    | Error String
    | Request Team
    | Confirm Team TeamDeleteKey
    | Done Team TeamDeleteConfirmed


type Msg
    = ClickDelete Team
    | ClickConfirm TeamDeleteKey
    | GotTeam (RemoteData (Graphql.Http.Error Team) Team)
    | GotTeamDeleteKey (RemoteData (Graphql.Http.Error TeamDeleteKey) TeamDeleteKey)
    | GotTeamDeleteConfirmed (RemoteData (Graphql.Http.Error TeamDeleteConfirmed) TeamDeleteConfirmed)


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    ( { session = session
      , phase = Init
      , slug = slug
      }
    , Api.Do.query (Api.Team.getTeam slug) (RemoteData.fromResult >> GotTeam)
    )


viewPhase : Phase -> Html Msg
viewPhase phase =
    case phase of
        Error error ->
            div [ class "card error" ] [ text error ]

        Init ->
            div [ class "card" ] [ text "loading team" ]

        Request team ->
            viewRequest team

        Confirm team deleteKey ->
            viewConfirm team deleteKey

        Done team teamDeleteConfirmed ->
            viewDone team teamDeleteConfirmed


card : String -> List (Html msg) -> Html msg
card title elements =
    div [ class "card" ]
        [ div [ class "title" ]
            [ h2 [] [ text title ] ]
        , div [] elements
        ]


viewRequest : Team -> Html Msg
viewRequest team =
    card "Request team team"
        [ div []
            [ p []
                [ text "This will delete the team "
                , strong [] [ text (slugStr team.slug) ]
                , text " permanently."
                ]
            , p [] [ text "Deleting a team is irreversible." ]
            ]
        , div [ class "row" ]
            [ button [ class "button small" ]
                [ Route.link (Route.Team team.slug) [ class "nostyle" ] [ text "no" ]
                ]
            , button [ class "button small", onClick (ClickDelete team) ] [ text "yes" ]
            ]
        ]


viewConfirm : Team -> TeamDeleteKey -> Html Msg
viewConfirm team key =
    card "Confirm team deletion"
        [ div [ class "row" ]
            [ p []
                [ text "Please confirm that you really want to delete the team." ]
            ]
        , ResourceTable.view team.syncState team.metadata
        , div [ class "row" ]
            [ button [ class "button small" ]
                [ Route.link (Route.Team team.slug) [ class "nostyle" ] [ text "no" ]
                ]
            , button [ class "button small", onClick (ClickConfirm key) ] [ text "yes" ]
            ]
        ]


viewDone : Team -> TeamDeleteConfirmed -> Html Msg
viewDone team teamDeleteConfirmed =
    card "team deleted"
        [ p []
            [ text ("Team " ++ slugStr team.slug ++ " has been deleted. This operation has correlation id " ++ uuidStr teamDeleteConfirmed.correlationID)
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "cards" ]
        [ viewPhase model.phase
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickDelete team ->
            ( model, Api.Do.mutate (Api.Team.requestTeamDeletion team) (RemoteData.fromResult >> GotTeamDeleteKey) )

        ClickConfirm key ->
            ( model, Api.Do.mutate (Api.Team.confirmTeamDeletion key) (RemoteData.fromResult >> GotTeamDeleteConfirmed) )

        GotTeam remoteData ->
            case remoteData of
                Failure err ->
                    ( { model | phase = Error (errorToString err) }, Cmd.none )

                Success team ->
                    case model.phase of
                        Init ->
                            ( { model | phase = Request team }, Cmd.none )

                        _ ->
                            ( { model | phase = Error "invalid phase transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTeamDeleteKey remoteData ->
            case remoteData of
                Failure err ->
                    ( { model | phase = Error (errorToString err) }, Cmd.none )

                Success deleteKey ->
                    case model.phase of
                        Request team ->
                            ( { model | phase = Confirm team deleteKey }, Cmd.none )

                        _ ->
                            ( { model | phase = Error "invalid phase transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTeamDeleteConfirmed remoteData ->
            case remoteData of
                Failure err ->
                    ( { model | phase = Error (errorToString err) }, Cmd.none )

                Success teamDeleteConfirmed ->
                    case model.phase of
                        Confirm team _ ->
                            ( { model | phase = Done team teamDeleteConfirmed }, Cmd.none )

                        _ ->
                            ( { model | phase = Error "invalid phase transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
