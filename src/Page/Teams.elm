module Page.Teams exposing (..)

import Backend.Scalar
import Graphql.Http
import Html exposing (Html, div, h2, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Queries.Do exposing (query)
import Queries.Error exposing (errorToString)
import Queries.TeamQueries exposing (TeamData, getTeamsQuery)
import RemoteData exposing (RemoteData(..))
import Route exposing (link)
import Session exposing (Session)


type alias Model =
    { session : Session
    , teams : RemoteData (Graphql.Http.Error (List TeamData)) (List TeamData)
    }


type Msg
    = NoOp
    | GotTeamsResponse (RemoteData (Graphql.Http.Error (List TeamData)) (List TeamData))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { teams = Loading
      , session = session
      }
    , query getTeamsQuery (RemoteData.fromResult >> GotTeamsResponse)
    )


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
            [ link (Route.Team team.id) [] [ text (slugstr team.slug) ]
            ]
        , td [] [ text team.purpose ]
        ]


teamTable : List TeamData -> Html Msg
teamTable teams =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Slug" ]
                , th [] [ text "Purpose" ]
                ]
            ]
        , tbody [] (List.map row teams)
        ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Operations" ]
        , p [] [ link Route.CreateTeam [ class "button" ] [ text "Create new" ] ]
        , h2 [] [ text "List of teams" ]
        , div []
            [ case model.teams of
                Success teams ->
                    teamTable teams

                Failure err ->
                    text <| errorToString err

                Loading ->
                    text <| "Loading teams..."

                NotAsked ->
                    text <| "Data not loaded"
            ]
        ]
