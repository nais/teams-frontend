module Page.Teams exposing (..)

import Api.Do exposing (query)
import Api.Error exposing (errorToString)
import Api.Team exposing (TeamData, getTeams)
import Backend.Scalar
import Graphql.Http
import Html exposing (Html, div, h2, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import List exposing (filter, map, member)
import RemoteData exposing (RemoteData(..))
import Route exposing (link)
import Session exposing (Session, User(..))


type ViewMode
    = AllTeams
    | MyTeams


type alias Model =
    { session : Session
    , teams : RemoteData (Graphql.Http.Error (List TeamData)) (List TeamData)
    , viewMode : ViewMode
    }


type Msg
    = NoOp
    | GotTeamsResponse (RemoteData (Graphql.Http.Error (List TeamData)) (List TeamData))


init : Session -> ViewMode -> ( Model, Cmd Msg )
init session viewMode =
    ( { teams = Loading
      , session = session
      , viewMode = viewMode
      }
    , query getTeams (RemoteData.fromResult >> GotTeamsResponse)
    )


myTeams : Session.User -> List TeamData -> List TeamData
myTeams user teams =
    let
        teamSlugs =
            case user of
                LoggedIn u ->
                    map (\x -> x.team.slug) u.teamMemberships

                _ ->
                    []
    in
    filter (\x -> member x.slug teamSlugs) teams


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTeamsResponse r ->
            ( { model | teams = r }, Cmd.none )


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


row : TeamData -> Html Msg
row team =
    tr []
        [ td []
            [ link (Route.Team team.slug) [] [ text (slugstr team.slug) ]
            ]
        , td [] [ text team.purpose ]
        ]


teamTable : List TeamData -> Html Msg
teamTable teams =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Identifier" ]
                , th [] [ text "Purpose" ]
                ]
            ]
        , tbody [] (List.map row teams)
        ]


view : Model -> Html Msg
view model =
    let
        title =
            case model.viewMode of
                AllTeams ->
                    "All teams"

                MyTeams ->
                    "My teams"

        switcher =
            case model.viewMode of
                AllTeams ->
                    link Route.MyTeams [] [ text "Show only my teams" ]

                MyTeams ->
                    link Route.AllTeams [] [ text "Show all teams" ]
    in
    div [ class "card" ]
        [ div [ class "title" ]
            [ h2 [] [ text title ]
            , link Route.CreateTeam [ class "button small" ] [ text "Create" ]
            ]
        , p []
            [ switcher
            ]
        , div []
            [ case model.teams of
                Success teams ->
                    case model.viewMode of
                        AllTeams ->
                            teamTable teams

                        MyTeams ->
                            teamTable (myTeams (Session.user model.session) teams)

                Failure err ->
                    text <| errorToString err

                Loading ->
                    text <| "Loading teams..."

                NotAsked ->
                    text <| "Data not loaded"
            ]
        ]
