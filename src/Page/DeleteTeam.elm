module Page.DeleteTeam exposing (Model, Msg(..), confirmTeamDeletion, requestTeamDeletion, update, view)

import Api.DeleteTeam
import Api.Do
import Api.Error exposing (errorToString)
import Api.Str exposing (slugStr, uuidStr)
import Api.Team
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Query exposing (team)
import Backend.Scalar exposing (Slug(..), Uuid(..))
import Component.ResourceTable as ResourceTable
import DataModel exposing (Expandable(..), Team, TeamDeleteConfirmed, TeamDeleteKey, expandableAll)
import Graphql.Http
import Html exposing (Html, button, div, h2, li, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , state : Flow
    , slug : Slug
    }


type Flow
    = RequestDelete Request
    | ConfirmDelete Confirm
    | Error String


type Request
    = RequestInit
    | RequestView Team
    | RequestDone TeamDeleteKey


type Confirm
    = ConfirmInit
    | ConfirmView TeamDeleteKey
    | ConfirmDone TeamDeleteKey TeamDeleteConfirmed


type Msg
    = ClickRequestDelete Team
    | ClickConfirmDelete TeamDeleteKey
    | GotTeam (RemoteData (Graphql.Http.Error Team) Team)
    | GotTeamDeleteKey (RemoteData (Graphql.Http.Error TeamDeleteKey) TeamDeleteKey)
    | GotTeamDeleteConfirmed (RemoteData (Graphql.Http.Error TeamDeleteConfirmed) TeamDeleteConfirmed)


requestTeamDeletion : Session -> Slug -> ( Model, Cmd Msg )
requestTeamDeletion session slug =
    ( { session = session
      , slug = slug
      , state = RequestDelete RequestInit
      }
    , Api.Do.queryRD (Api.Team.getTeam slug) GotTeam
    )


confirmTeamDeletion : Session -> Slug -> Uuid -> ( Model, Cmd Msg )
confirmTeamDeletion session slug teamDeleteKey =
    ( { session = session
      , slug = slug
      , state = ConfirmDelete ConfirmInit
      }
    , Api.Do.queryRD (Api.DeleteTeam.getTeamDeleteKey teamDeleteKey) GotTeamDeleteKey
    )


viewState : Flow -> Html Msg
viewState flow =
    case flow of
        RequestDelete f ->
            case f of
                RequestInit ->
                    div [ class "card" ] [ text "loading team" ]

                RequestView t ->
                    viewRequest t

                RequestDone tdk ->
                    viewRequestDone tdk

        ConfirmDelete f ->
            case f of
                ConfirmInit ->
                    div [ class "card" ] [ text "loading delete request" ]

                ConfirmView tdk ->
                    viewConfirmDelete tdk

                ConfirmDone tdk tdc ->
                    viewConfirmDone tdk tdc

        Error error ->
            div [ class "card error" ] [ text error ]


card : String -> List (Html msg) -> Html msg
card title elements =
    div [ class "card" ]
        [ div [ class "title" ]
            [ h2 [] [ text title ] ]
        , div [] elements
        ]


viewRequest : Team -> Html Msg
viewRequest team =
    card "Confirm team deletion"
        [ div [ class "row" ]
            [ p []
                [ text "Please confirm that you want to delete the following resources, and all resources they contain. Applications in the namespace, databases in the google project, etc will be irreversibly deleted." ]
            ]
        , ResourceTable.view team.syncState team.metadata
        , div [ class "row" ]
            [ button [ class "button small" ]
                [ Route.link (Route.Team team.slug) [ class "nostyle" ] [ text "no" ]
                ]
            , button [ class "button small", onClick (ClickRequestDelete team) ] [ text "yes" ]
            ]
        ]


viewRequestDone : TeamDeleteKey -> Html Msg
viewRequestDone tdk =
    card "Team delete requested"
        [ p []
            [ text ("Deletion of team " ++ slugStr tdk.team.slug ++ " has been requested. To finalize the deletion send this link to another team owner and let them confirm the deletion.")
            , ul []
                (expandableAll tdk.team.members
                    |> List.filter (\m -> m.role == Owner)
                    |> List.map (\m -> li [] [ text m.user.email ])
                )
            ]
        ]


viewConfirmDelete : TeamDeleteKey -> Html Msg
viewConfirmDelete tdk =
    card "Confirm team deletion"
        [ div [ class "row" ]
            [ p []
                [ text "Please confirm that you want to delete the following resources, and all resources they contain. Applications in the namespace, databases in the google project, etc will be irreversibly deleted." ]
            ]
        , ResourceTable.view tdk.team.syncState tdk.team.metadata
        , div [ class "row" ]
            [ button [ class "button small" ]
                [ Route.link (Route.Team tdk.team.slug) [ class "nostyle" ] [ text "no" ]
                ]
            , button [ class "button small", onClick (ClickConfirmDelete tdk) ] [ text "yes" ]
            ]
        ]


viewConfirmDone : TeamDeleteKey -> TeamDeleteConfirmed -> Html Msg
viewConfirmDone tdk tdc =
    card "Team deleted"
        [ p []
            [ text ("Team " ++ slugStr tdk.team.slug ++ " deleted. Correlation id: " ++ uuidStr tdc.correlationID)
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "cards" ]
        [ viewState model.state
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickRequestDelete team ->
            ( model, Api.Do.mutate (Api.DeleteTeam.requestTeamDeletion team) (RemoteData.fromResult >> GotTeamDeleteKey) )

        ClickConfirmDelete key ->
            ( model, Api.Do.mutate (Api.DeleteTeam.confirmTeamDeletion key) (RemoteData.fromResult >> GotTeamDeleteConfirmed) )

        GotTeam remoteData ->
            case remoteData of
                Failure err ->
                    ( { model | state = Error (errorToString err) }, Cmd.none )

                Success team ->
                    case model.state of
                        RequestDelete RequestInit ->
                            ( { model | state = RequestDelete (RequestView team) }, Cmd.none )

                        _ ->
                            ( { model | state = Error "invalid state transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTeamDeleteKey remoteData ->
            case remoteData of
                Failure err ->
                    ( { model | state = Error (errorToString err) }, Cmd.none )

                Success deleteKey ->
                    case model.state of
                        RequestDelete (RequestView _) ->
                            ( { model | state = RequestDelete (RequestDone deleteKey) }, Cmd.none )

                        ConfirmDelete ConfirmInit ->
                            ( { model | state = ConfirmDelete (ConfirmView deleteKey) }, Cmd.none )

                        _ ->
                            ( { model | state = Error "invalid state transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTeamDeleteConfirmed remoteData ->
            case remoteData of
                Failure err ->
                    ( { model | state = Error (errorToString err) }, Cmd.none )

                Success tdc ->
                    case model.state of
                        ConfirmDelete (ConfirmView tdk) ->
                            ( { model | state = ConfirmDelete (ConfirmDone tdk tdc) }, Cmd.none )

                        _ ->
                            ( { model | state = Error "invalid state transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
