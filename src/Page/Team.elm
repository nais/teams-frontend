module Page.Team exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (RoleName(..))
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, div, h2, h3, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, colspan, title)
import ISO8601
import Queries.Do exposing (query)
import Queries.Error exposing (errorToString)
import Queries.TeamQueries exposing (AuditLogData, KeyValueData, SyncErrorData, TeamData, TeamMemberData, getTeamQuery, roleString)
import RemoteData exposing (RemoteData(..))
import Route exposing (link)
import Session exposing (Session, User(..))


type alias Model =
    { team : RemoteData (Graphql.Http.Error TeamData) TeamData
    , session : Session
    }


type Msg
    = GotTeamResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)


init : Session -> Backend.Scalar.Uuid -> ( Model, Cmd Msg )
init session id =
    ( { team = NotAsked
      , session = session
      }
    , query (getTeamQuery id) (RemoteData.fromResult >> GotTeamResponse)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamResponse r ->
            ( { model | team = r }, Cmd.none )


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


timestr : ISO8601.Time -> String
timestr u =
    let
        pad len int =
            String.fromInt int |> String.padLeft len '0'
    in
    pad 4 u.year
        ++ "-"
        ++ pad 2 u.month
        ++ "-"
        ++ pad 2 u.day
        ++ " "
        ++ pad 2 u.hour
        ++ ":"
        ++ pad 2 u.minute
        ++ ":"
        ++ pad 2 u.second


actionstr : Backend.Scalar.AuditAction -> String
actionstr (Backend.Scalar.AuditAction u) =
    u


memberRow : TeamMemberData -> Html Msg
memberRow member =
    tr []
        [ td [] [ text member.user.email ]
        , td [ classList [ ( "team-owner", member.role == Owner ) ] ] [ text <| roleString member.role ]
        ]


errorRow : SyncErrorData -> Html Msg
errorRow log =
    tr []
        [ td [] [ text (timestr log.timestamp) ]
        , td [] [ text log.reconcilerName ]
        , td [] [ text log.message ]
        ]


logRow : AuditLogData -> Html Msg
logRow log =
    let
        actor =
            case log.actor of
                Nothing ->
                    span [ title <| "Performed automatically by the console backend module '" ++ actionstr log.action ++ "'." ] [ text "System" ]

                Just s ->
                    span [ title <| "Triggered by '" ++ s ++ "' and performed by '" ++ actionstr log.action ++ "'." ] [ text s ]
    in
    tr []
        [ td [] [ text (timestr log.createdAt) ]
        , td [] [ actor ]
        , td [] [ text log.message ]
        ]


simpleRow : String -> String -> Html msg
simpleRow header content =
    tr []
        [ th [] [ text header ]
        , td [] [ text content ]
        ]


metadataRow : KeyValueData -> Html msg
metadataRow kv =
    case kv.value of
        Just v ->
            simpleRow kv.key v

        Nothing ->
            simpleRow kv.key ""


editorButton : Model -> TeamData -> List (Html msg)
editorButton model team =
    if editor team (Session.user model.session) then
        [ p [] [ link (Route.EditTeam team.id) [ class "button" ] [ text "Edit" ] ] ]

    else
        []


viewProblems syncErrors =
    case syncErrors of
        [] ->
            text ""

        _ ->
            div []
                [ h3 [ class "error" ] [ text "External systems out of sync" ]
                , p [] [ text "Team data is stored in our database, but up-to-date information could not be transferred to some external systems. The messages below may indicate what went wrong." ]
                , p [] [ text "Console will automatically retry any failed synchronization. This will resolve transient errors, but in case one or more systems are misconfigured, these errors will persist. In that case, contact NAIS support." ]
                , table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Timestamp" ]
                            , th [] [ text "Subsystem" ]
                            , th [] [ text "Error message" ]
                            ]
                        ]
                    , tbody [] (List.map errorRow syncErrors)
                    ]
                ]


view : Model -> Html Msg
view model =
    case model.team of
        Success team ->
            div []
                ([ h2 [] [ text ("Viewing team \"" ++ slugstr team.slug ++ "\"") ]
                 , p [] [ text team.purpose ]
                 ]
                    ++ editorButton model team
                    ++ [ table []
                            [ thead []
                                [ tr []
                                    [ th [ colspan 2 ] [ text "Metadata" ]
                                    ]
                                ]
                            , tbody []
                                (List.map metadataRow team.metadata)
                            ]
                       , h3 [] [ text "Members" ]
                       , table []
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Email" ]
                                    , th [] [ text "Role" ]
                                    ]
                                ]
                            , tbody [] (List.map memberRow team.members)
                            ]
                       , viewProblems team.syncErrors
                       , h3 [] [ text "Change and synchronization logs" ]
                       , table []
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Timestamp" ]
                                    , th [] [ text "Changed by" ]
                                    , th [] [ text "Message" ]
                                    ]
                                ]
                            , tbody [] (List.map logRow team.auditLogs)
                            ]
                       ]
                )

        Failure err ->
            div [ class "error" ] [ text <| errorToString err ]

        Loading ->
            div [] [ text "Loading data..." ]

        NotAsked ->
            div [] [ text "No data loaded" ]


teamRoleForUser : TeamData -> User -> Maybe TeamRole
teamRoleForUser team user =
    case user of
        LoggedIn u ->
            List.head (List.filter (\m -> m.user.id == u.id) team.members)
                |> Maybe.map (\m -> m.role)

        Anonymous ->
            Nothing

        Unknown ->
            Nothing


editor : TeamData -> User -> Bool
editor team user =
    List.any (\b -> b)
        [ Session.isGlobalAdmin user
        , teamRoleForUser team user == Just Owner
        ]
