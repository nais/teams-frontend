module Page.DeleteTeam exposing (Model, Msg(..), init, update, view)

import Api.Do
import Api.Error exposing (errorToString)
import Api.Team
import Backend.Query exposing (team)
import Backend.Scalar exposing (Slug(..))
import DataModel exposing (Team, TeamDeleteConfirmed, TeamDeleteKey)
import Graphql.Http
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)


type alias Model =
    { session : Session
    , phase : Phase
    }


type Phase
    = Init
    | Error String
    | Request Team
    | Confirm Team TeamDeleteKey
    | Done Team TeamDeleteConfirmed


type Msg
    = ClickDelete
    | ClickConfirm TeamDeleteKey
    | GotTeam (RemoteData (Graphql.Http.Error Team) Team)
    | GotTeamDeleteKey (RemoteData (Graphql.Http.Error TeamDeleteKey) TeamDeleteKey)
    | GotTeamDeleteConfirmed (RemoteData (Graphql.Http.Error TeamDeleteConfirmed) TeamDeleteConfirmed)


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    ( { session = session
      , phase = Init
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

        Done team correlationId ->
            viewDone team correlationId


viewRequest : Team -> Html Msg
viewRequest team =
    div [] []


viewConfirm : Team -> TeamDeleteKey -> Html Msg
viewConfirm team key =
    div [] []


viewDone : Team -> TeamDeleteConfirmed -> Html Msg
viewDone team correlationId =
    div [] []


view : Model -> Html Msg
view model =
    div [ class "cards" ]
        [ viewPhase model.phase
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickDelete ->
            ( model, Cmd.none )

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

                Success correlationId ->
                    case model.phase of
                        Request team ->
                            ( { model | phase = Done team correlationId }, Cmd.none )

                        _ ->
                            ( { model | phase = Error "invalid phase transition" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
