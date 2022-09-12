module Teams exposing (..)

import Backend.Query exposing (me)
import Backend.Scalar exposing (Uuid)
import Browser.Navigation
import Graphql.Http
import Html exposing (Html, a, button, div, h1, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Queries.Do exposing (send)
import Queries.TeamQueries exposing (TeamData, getTeamsQuery)
import Route
import Route exposing (link)


type alias Model =
    { teams : List TeamData
    , navKey : Browser.Navigation.Key
    }


type Msg
    = NoOp
    | GotTeamsResponse (Result (Graphql.Http.Error (List TeamData)) (List TeamData))


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init navigationKey =
    ( { teams = []
      , navKey = navigationKey
      }
    , send getTeamsQuery GotTeamsResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTeamsResponse r ->
            case r of
                Ok teams ->
                    ( { model | teams = teams }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


uuidstr : Backend.Scalar.Uuid -> String
uuidstr (Backend.Scalar.Uuid u) =
    u


row : TeamData -> Html Msg
row team =
    tr []
        [ td []
            [ link (Route.Team (uuidstr team.id)) [] [ text (slugstr team.slug) ]
            ]
        , td [] [ text team.name ]
        , td [] [ text (Maybe.withDefault "" team.purpose) ]
        ]


teamTable : List TeamData -> Html Msg
teamTable teams =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Slug" ]
                , th [] [ text "Team name" ]
                , th [] [ text "Purpose" ]
                ]
            ]
        , tbody [] (List.map row teams)
        ]


view : Model -> Html Msg
view model =
    teamTable model.teams


navKey : Model -> Browser.Navigation.Key
navKey model =
    model.navKey
