module Page.Team exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (RoleName(..), Slug, Uuid)
import Browser.Navigation as Nav
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, button, div, em, h2, h3, input, label, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, for, title, type_, value)
import Html.Events exposing (onClick, onInput)
import ISO8601
import Queries.Do exposing (query)
import Queries.Error exposing (errorToString)
import Queries.TeamQueries exposing (AuditLogData, KeyValueData, SyncErrorData, TeamData, TeamMemberData, getTeamQuery, roleString, updateTeamMutation)
import Queries.UserQueries exposing (UserData)
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session, User(..))


type EditMode
    = View
    | EditMain (Maybe (Graphql.Http.Error TeamData))
    | EditMembers (Maybe (Graphql.Http.Error TeamData))


type alias Model =
    { team : RemoteData (Graphql.Http.Error TeamData) TeamData
    , edit : EditMode
    , membersToAdd : List TeamMemberData
    , membersToRemove : List UserData
    , session : Session
    }


type Msg
    = GotTeamResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | GotSaveOverviewResponse (RemoteData (Graphql.Http.Error TeamData) TeamData)
    | ClickedEditMain
    | ClickedEditMembers TeamData
    | ClickedSaveOverview TeamData
    | PurposeChanged String


init : Session -> Backend.Scalar.Slug -> ( Model, Cmd Msg )
init session slug =
    ( { team = NotAsked
      , session = session
      , edit = View
      , membersToAdd = []
      , membersToRemove = []
      }
    , fetchTeam slug
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamResponse r ->
            ( { model | team = r }, Cmd.none )

        GotSaveOverviewResponse r ->
            case r of
                Success _ ->
                    ( { model | team = r, edit = View }, Cmd.none )

                Failure error ->
                    ( { model | edit = EditMain (Just error) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedEditMain ->
            ( { model | edit = EditMain Nothing }, Cmd.none )

        ClickedEditMembers team ->
            ( { model | edit = EditMembers Nothing }
            , Nav.pushUrl (Session.navKey model.session) (Route.routeToString (Route.EditTeam team.slug))
            )

        PurposeChanged s ->
            ( mapTeam (\team -> { team | purpose = s }) model, Cmd.none )

        ClickedSaveOverview team ->
            ( model, saveOverview team )


saveOverview : TeamData -> Cmd Msg
saveOverview team =
    Queries.Do.mutate
        (updateTeamMutation
            team.slug
            { purpose = Graphql.OptionalArgument.Present team.purpose
            }
        )
        (GotSaveOverviewResponse << RemoteData.fromResult)


mapTeam : (TeamData -> TeamData) -> Model -> Model
mapTeam fn model =
    case model.team of
        Success team ->
            { model | team = Success <| fn team }

        _ ->
            model


fetchTeam : Slug -> Cmd Msg
fetchTeam slug =
    query (getTeamQuery slug) (RemoteData.fromResult >> GotTeamResponse)


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


logLine : ISO8601.Time -> String -> String -> Html msg
logLine ts actor message =
    li []
        [ p [] [ text message ]
        , div [ class "meta" ]
            [ p [] [ text (timestr ts) ]
            , p [] [ text actor ]
            ]
        ]


errorLine : SyncErrorData -> Html Msg
errorLine log =
    logLine log.timestamp log.reconcilerName log.message


auditLogLine : AuditLogData -> Html Msg
auditLogLine log =
    let
        actor =
            case log.actor of
                Nothing ->
                    actionstr log.action

                Just s ->
                    s
    in
    logLine log.createdAt actor log.message


simpleRow : String -> String -> Html msg
simpleRow header content =
    tr []
        [ td [] [ text header ]
        , td [] [ text content ]
        ]


metadataRow : KeyValueData -> Html msg
metadataRow kv =
    case kv.value of
        Just v ->
            simpleRow kv.key v

        Nothing ->
            simpleRow kv.key ""


editorButton : Msg -> Model -> TeamData -> List (Html Msg)
editorButton msg model team =
    if editor team (Session.user model.session) then
        [ div [ class "small button", onClick msg ] [ text "Edit" ] ]

    else
        []


viewSyncErrors : TeamData -> Html Msg
viewSyncErrors team =
    let
        syncSuccess =
            case team.lastSuccessfulSync of
                Nothing ->
                    Html.text ""

                Just ts ->
                    p []
                        [ em [] [ text <| "The last successful synchronization was on " ++ timestr ts ++ "." ]
                        ]
    in
    case team.syncErrors of
        [] ->
            text ""

        _ ->
            div [ class "card error" ]
                [ h2 [] [ text "Synchronization error" ]
                , p [] [ text "Console failed to synchronize team ", strong [] [ text (slugstr team.slug) ], text " with external systems. The operations will be automatically retried. The messages below indicate what went wrong." ]
                , p [] [ text "If errors are caused by network outage, they will resolve automatically. If they persist for more than a few hours, please NAIS support." ]
                , syncSuccess
                , h3 [] [ text "Error messages" ]
                , ul [ class "logs" ] (List.map errorLine team.syncErrors)
                ]


viewTeamMetaTable : List KeyValueData -> Html msg
viewTeamMetaTable metadata =
    case metadata of
        [] ->
            text ""

        _ ->
            table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Key" ]
                        , th [] [ text "Value" ]
                        ]
                    ]
                , tbody []
                    (List.map metadataRow metadata)
                ]


viewTeamOverview : Model -> TeamData -> Html Msg
viewTeamOverview model team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text ("Team " ++ slugstr team.slug) ] ]
                ++ editorButton ClickedEditMain model team
            )
        , p [] [ text team.purpose ]
        , viewTeamMetaTable team.metadata
        ]


viewEditTeamOverview : TeamData -> Maybe (Graphql.Http.Error TeamData) -> Html Msg
viewEditTeamOverview team error =
    let
        errorMessage =
            case error of
                Nothing ->
                    text ""

                Just err ->
                    div [ class "error" ] [ text <| Queries.Error.errorToString err ]
    in
    div [ class "card" ]
        [ h2 [] [ text ("Team " ++ slugstr team.slug) ]
        , input [ type_ "text", Html.Attributes.placeholder "Describe team's purpose", onInput PurposeChanged, value team.purpose ] []
        , errorMessage
        , button [ onClick (ClickedSaveOverview team) ] [ text "Save changes" ]
        , viewTeamMetaTable team.metadata
        ]


viewMembers : Model -> TeamData -> Html Msg
viewMembers model team =
    div [ class "card" ]
        [ div [ class "title" ]
            ([ h2 [] [ text "Members" ] ]
                ++ editorButton (ClickedEditMembers team) model team
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


viewEditMembers =
    viewMembers


viewLogs : TeamData -> Html Msg
viewLogs team =
    div [ class "card" ]
        [ h2 [] [ text "Logs" ]
        , ul [ class "logs" ] (List.map auditLogLine team.auditLogs)
        ]


viewCards : Model -> TeamData -> Html Msg
viewCards model team =
    div [ class "cards" ]
        (case model.edit of
            View ->
                [ viewTeamOverview model team
                , viewSyncErrors team
                , viewMembers model team
                , viewLogs team
                ]

            EditMain err ->
                [ viewEditTeamOverview team err
                , viewSyncErrors team
                , viewMembers model team
                , viewLogs team
                ]

            EditMembers err ->
                [ viewTeamOverview model team
                , viewSyncErrors team
                , viewEditMembers model team
                , viewLogs team
                ]
        )


view : Model -> Html Msg
view model =
    case model.team of
        Success team ->
            viewCards model team

        Failure err ->
            div [ class "card error" ] [ text <| errorToString err ]

        Loading ->
            div [ class "card" ] [ text "Loading data..." ]

        NotAsked ->
            div [ class "card" ] [ text "No data loaded" ]


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
