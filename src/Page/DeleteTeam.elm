module Page.DeleteTeam exposing (Flow, Model, Msg(..), confirmTeamDeletion, requestTeamDeletion, update, view)

import Api.DeleteTeam
import Api.Do
import Api.Error exposing (errorToString)
import Api.Str exposing (slugStr, uuidStr)
import Api.Team
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (Slug(..), Uuid)
import Component.Card as Card exposing (Card)
import Component.ResourceTable as ResourceTable
import DataModel exposing (Team, TeamDeleteConfirmed, TeamDeleteKey, expandableAll)
import Graphql.Http
import Html exposing (Html, button, div, input, li, p, text, ul)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick)
import ISO8601
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)
import Task
import Time
import Url


type alias Model =
    { session : Session
    , slug : Slug
    , state : Flow
    , now : Time.Posix
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
    | TimeNow Time.Posix
    | ClickConfirmDelete TeamDeleteKey
    | GotTeam (RemoteData (Graphql.Http.Error Team) Team)
    | GotTeamDeleteKey (RemoteData (Graphql.Http.Error TeamDeleteKey) TeamDeleteKey)
    | GotTeamDeleteConfirmed (RemoteData (Graphql.Http.Error TeamDeleteConfirmed) TeamDeleteConfirmed)


requestTeamDeletion : Session -> Slug -> ( Model, Cmd Msg )
requestTeamDeletion session slug =
    ( { session = session
      , slug = slug
      , now = Time.millisToPosix 0
      , state = RequestDelete RequestInit
      }
    , Api.Do.queryRD (Api.Team.getTeam slug) GotTeam
    )


confirmTeamDeletion : Session -> Uuid -> ( Model, Cmd Msg )
confirmTeamDeletion session teamDeleteKey =
    ( { session = session
      , slug = Slug ""
      , now = Time.millisToPosix 0
      , state = ConfirmDelete ConfirmInit
      }
    , Cmd.batch
        [ Api.Do.queryRD (Api.DeleteTeam.getTeamDeleteKey teamDeleteKey) GotTeamDeleteKey
        , Task.perform TimeNow Time.now
        ]
    )


viewState : Model -> List (Html Msg)
viewState model =
    (case model.state of
        RequestDelete f ->
            case f of
                RequestInit ->
                    [ Card.new "Loading team" ]

                RequestView t ->
                    viewCardRequest t

                RequestDone tdk ->
                    [ viewCardRequestDone model.session tdk ]

        ConfirmDelete f ->
            case f of
                ConfirmInit ->
                    [ Card.new "Loading delete request" ]

                ConfirmView tdk ->
                    viewCardsConfirmDelete model.session tdk model.now

                ConfirmDone tdk tdc ->
                    [ viewCardConfirmDone tdk tdc ]

        Error error ->
            [ Card.new "Error" |> Card.withAttributes [ class "error" ] |> Card.withContents [ text error ] ]
    )
        |> List.map Card.render


viewCardRequest : Team -> List (Card Msg)
viewCardRequest team =
    [ Card.new ("Request team deletion for team " ++ slugStr team.slug)
        |> Card.withContents
            [ div [ class "column" ]
                [ p []
                    [ text "Please confirm that you intend to delete the team and all resources related to it."
                    ]
                ]
            ]
    , Card.new "Resources that would be deleted"
        |> Card.withContents
            [ p []
                [ text "The following resources and all resources within them would be deleted. Applications in the namespace, databases in the google project, etc will be irreversibly deleted."
                ]
            , ResourceTable.view team.syncState
            ]
    , Card.new "Danger zone"
        |> Card.withContents
            [ p [] [ text "When you request deletion a delete key will be generated for this team. It is valid for 5 minutes. Another team-owner will have to confirm the deletion by using a generated link before the team is irreversibly deleted." ]
            , p [] [ text "Current team owners are listed below" ]
            , ul []
                (expandableAll team.members
                    |> List.filter (\m -> m.role == Owner)
                    |> List.map (\m -> li [] [ text m.user.email ])
                )
            , div [ class "button-row" ]
                [ button []
                    [ Route.link (Route.Team team.slug) [ class "nostyle" ] [ text "Abort" ]
                    ]
                , button [ class "red", onClick (ClickRequestDelete team) ] [ text "Request team deletion" ]
                ]
            ]
    ]


baseUrl : Session -> String
baseUrl session =
    let
        url : Url.Url
        url =
            Session.url session

        protocol : String
        protocol =
            case url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        port_ : String
        port_ =
            case url.port_ of
                Just p ->
                    ":" ++ String.fromInt p

                Nothing ->
                    ""
    in
    String.concat [ protocol, url.host, port_ ]


viewCardRequestDone : Session -> TeamDeleteKey -> Card Msg
viewCardRequestDone session tdk =
    Card.new ("Requested team deletion for team " ++ slugStr tdk.team.slug)
        |> Card.withContents
            [ p []
                [ text ("Deletion of team " ++ slugStr tdk.team.slug ++ " has been requested. To finalize the deletion send this link to another team owner and let them confirm the deletion.")
                ]
            , input [ type_ "text", Html.Attributes.disabled True, value (baseUrl session ++ Route.routeToString (Route.DeleteTeamConfirm tdk.key)) ] []
            , p [] [ text "Current owners are listed below" ]
            , ul []
                (expandableAll tdk.team.members
                    |> List.filter (\m -> m.role == Owner)
                    |> List.map (\m -> li [] [ text m.user.email ])
                )
            ]


confirmOwnDeleteRequest : Session -> TeamDeleteKey -> Bool
confirmOwnDeleteRequest session tdk =
    case Session.user (Session.viewer session) of
        Just u ->
            u.email == tdk.createdBy.email

        Nothing ->
            False


expired : ISO8601.Time -> Time.Posix -> Bool
expired expiry now =
    ISO8601.toTime expiry < Time.posixToMillis now


viewCardsConfirmDelete : Session -> TeamDeleteKey -> Time.Posix -> List (Card Msg)
viewCardsConfirmDelete session tdk now =
    (Card.new ("Confirm team deletion for team " ++ slugStr tdk.team.slug)
        |> Card.withContents
            [ p [] [ text ("The deletion was initiated by " ++ tdk.createdBy.email ++ " and expires at " ++ ISO8601.toString tdk.expires) ] ]
    )
        :: (if expired tdk.expires now then
                [ Card.new "Error"
                    |> Card.withAttributes [ class "error" ]
                    |> Card.withContents
                        [ p []
                            [ text "Delete key expired" ]
                        ]
                ]

            else if confirmOwnDeleteRequest session tdk then
                [ Card.new "Error"
                    |> Card.withContents
                        [ p []
                            [ text "You can not confirm your own delete request." ]
                        ]
                ]

            else
                [ Card.new "Resources that will be deleted"
                    |> Card.withContents
                        [ p []
                            [ text "The following resources and all resources within them would be deleted. Applications in the namespace, databases in the google project, etc will be irreversibly deleted." ]
                        , ResourceTable.view tdk.team.syncState
                        ]
                , Card.new "Danger zone"
                    |> Card.withContents
                        [ p [] [ text "When you click delete team there is now way back." ]
                        , div [ class "button-row" ]
                            [ button []
                                [ Route.link (Route.Team tdk.team.slug) [ class "nostyle" ] [ text "Abort" ]
                                ]
                            , button [ class "red", onClick (ClickConfirmDelete tdk) ] [ text "Delete team" ]
                            ]
                        ]
                ]
           )


viewCardConfirmDone : TeamDeleteKey -> TeamDeleteConfirmed -> Card Msg
viewCardConfirmDone tdk tdc =
    Card.new ("Confirmed team deletion for team " ++ slugStr tdk.team.slug)
        |> Card.withContents
            [ p []
                [ text ("Team " ++ slugStr tdk.team.slug ++ " deleted. Correlation id: " ++ uuidStr tdc.correlationID)
                ]
            ]


view : Model -> Html Msg
view model =
    div [ class "cards" ]
        (viewState model)


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
                            ( { model | state = ConfirmDelete (ConfirmView deleteKey), slug = deleteKey.team.slug }, Cmd.none )

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

        TimeNow t ->
            ( { model | now = t }, Cmd.none )
