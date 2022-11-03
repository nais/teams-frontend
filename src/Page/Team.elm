module Page.Team exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (RoleName(..))
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, div, h2, h3, p, span, strong, table, tbody, td, text, th, thead, tr)
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
        [ link (Route.EditTeam team.id) [ class "small button" ] [ text "Edit" ] ]

    else
        []


viewProblems : TeamData -> Html Msg
viewProblems team =
    case team.syncErrors of
        [] ->
            text ""

        _ ->
            div [ class "card error" ]
                [ h2 [] [ text "Synchronization error" ]
                , p [] [ text "Console failed to synchronize team ", strong [] [ text (slugstr team.slug) ], text " with external systems. The operations will be automatically retried. The messages below indicate what went wrong." ]
                , p [] [ text "If errors are caused by network outage, they will resolve automatically. If they persist for more than a few hours, please NAIS support." ]
                , table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Timestamp" ]
                            , th [] [ text "Subsystem" ]
                            , th [] [ text "Error message" ]
                            ]
                        ]
                    , tbody [] (List.map errorRow team.syncErrors)
                    ]
                ]


viewTeamOverview : Model -> TeamData -> Html Msg
viewTeamOverview model team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text ("Team " ++ slugstr team.slug) ] ]
                ++ editorButton model team
            )
        , p [] [ text team.purpose ]
        , table []
            [ thead []
                [ tr []
                    [ th [ colspan 2 ] [ text "Metadata" ]
                    ]
                ]
            , tbody []
                (List.map metadataRow team.metadata)
            ]
        ]


viewMembers : Model -> TeamData -> Html Msg
viewMembers model team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text "Members" ] ]
                ++ editorButton model team
            )
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    ]
                ]
            , tbody [] (List.map memberRow team.members)
            ]
        ]


viewLogs : TeamData -> Html Msg
viewLogs team =
    div [ class "card" ]
        [ h2 [] [ text "Logs" ]
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


view : Model -> Html Msg
view model =
    case model.team of
        Success team ->
            div [ class "cards" ]
                [ viewProblems team
                , viewTeamOverview model team
                , viewMembers model team
                , viewLogs team
                ]

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
