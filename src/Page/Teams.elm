module Page.Teams exposing (Model, Msg(..), ViewMode(..), init, update, view)

import Api.Do exposing (query)
import Api.Error exposing (errorToString)
import Api.Team exposing (getTeams)
import Backend.Scalar
import DataModel exposing (..)
import Graphql.Http
import Html exposing (Html, div, h2, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import List exposing (filter, map, member)
import RemoteData exposing (RemoteData(..))
import Route exposing (link)
import Session exposing (Session, Viewer(..))


type ViewMode
    = AllTeams
    | MyTeams


type alias Model =
    { session : Session
    , teams : RemoteData (Graphql.Http.Error (List Team)) (List Team)
    , viewMode : ViewMode
    }


type Msg
    = GotTeamsResponse (RemoteData (Graphql.Http.Error (List Team)) (List Team))


init : Session -> ViewMode -> ( Model, Cmd Msg )
init session viewMode =
    ( { teams = Loading
      , session = session
      , viewMode = viewMode
      }
    , query getTeams (RemoteData.fromResult >> GotTeamsResponse)
    )


myTeams : Session.Viewer -> List Team -> List Team
myTeams user teams =
    let
        teamSlugs =
            case user of
                LoggedIn u ->
                    map (\x -> x.team.slug) u.teamMemberships

                Anonymous ->
                    []
    in
    filter (\x -> member x.slug teamSlugs) teams


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeamsResponse r ->
            ( { model | teams = r }, Cmd.none )


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


row : Team -> Html Msg
row team =
    tr []
        [ td []
            [ link (Route.Team team.slug) [] [ text (slugstr team.slug) ]
            ]
        , td [] [ text team.purpose ]
        ]


teamTable : List Team -> Html Msg
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
                            teamTable (myTeams (Session.viewer model.session) teams)

                Failure err ->
                    text <| errorToString err

                Loading ->
                    text <| "Loading teams..."

                NotAsked ->
                    text <| "Data not loaded"
            ]
        ]
