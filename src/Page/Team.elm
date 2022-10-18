module Page.Team exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Object exposing (TeamMetadata(..))
import Backend.Scalar exposing (RoleName(..))
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, div, h2, h3, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan)
import Queries.Do exposing (query)
import Queries.Error exposing (errorToString)
import Queries.TeamQueries exposing (AuditLogData, KeyValueData, TeamData, TeamMemberData, getTeamQuery)
import Route exposing (link)
import Session exposing (Session, User(..))


type Team
    = Team TeamData
    | Loading
    | Error String


type alias Model =
    { team : Team
    , session : Session
    }


type Msg
    = GotTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)


init : Session -> Backend.Scalar.Uuid -> ( Model, Cmd Msg )
init session id =
    ( { team = Loading
      , session = session
      }
    , query (getTeamQuery id) GotTeamResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamResponse r ->
            case r of
                Ok team ->
                    ( { model | team = Team team }, Cmd.none )

                Err e ->
                    ( { model | team = Error (errorToString e) }, Cmd.none )


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


timestr : Backend.Scalar.Time -> String
timestr (Backend.Scalar.Time u) =
    u


actionstr : Backend.Scalar.AuditAction -> String
actionstr (Backend.Scalar.AuditAction u) =
    u


memberRow : TeamMemberData -> Html Msg
memberRow member =
    tr []
        [ td [] [ text member.user.email ]
        , td [] [ text (Backend.Enum.TeamRole.toString member.role) ]
        ]


logRow : AuditLogData -> Html Msg
logRow log =
    tr []
        [ td [] [ text (timestr log.createdAt) ]
        , td [] [ text (Maybe.withDefault "" log.actor) ]
        , td [] [ text (actionstr log.action) ]
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


view : Model -> Html Msg
view model =
    case model.team of
        Team team ->
            div []
                (editorButton model team
                    ++ [ h2 [] [ text ("Teams → " ++ slugstr team.slug) ]
                       , p [] [ text (Maybe.withDefault "N/A" team.purpose) ]
                       , table []
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
                       , h3 [] [ text "Logs" ]
                       , table []
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Timestamp" ]
                                    , th [] [ text "Actor" ]
                                    , th [] [ text "Action" ]
                                    , th [] [ text "Message" ]
                                    ]
                                ]
                            , tbody [] (List.map logRow team.auditLogs)
                            ]
                       ]
                )

        Loading ->
            div [] [ text "spinner" ]

        Error msg ->
            div [ class "error" ] [ text msg ]


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
