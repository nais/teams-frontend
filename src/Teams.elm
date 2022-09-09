module Teams exposing (..)

import Backend.Query exposing (me)
import Backend.Scalar exposing (Uuid)
import Browser.Navigation
import Graphql.Http
import Html exposing (Html, button, div, h1, p, table, tbody, td, text, th, thead, tr)
import Html.Events exposing (onClick)
import Queries.Do exposing (send)
import Queries.TeamQueries exposing (TeamData, getTeamsQuery)


type alias Model =
    { teams : List TeamData
    }


type Msg
    = NoOp
    | GotTeamsResponse (Result (Graphql.Http.Error (List TeamData)) (List TeamData))


init : ( Model, Cmd Msg )
init =
    ( { teams = []
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
        [ td [] [ text (uuidstr team.id) ]
        , td [] [ text (slugstr team.slug) ]
        , td [] [ text team.name ]
        , td [] [ text (Maybe.withDefault "" team.purpose) ]
        ]


teamTable : List TeamData -> Html Msg
teamTable teams =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "id" ]
                , th [] [ text "slug" ]
                , th [] [ text "name" ]
                , th [] [ text "purpose" ]
                ]
            ]
        , tbody [] (List.map row teams)
        ]


view : Model -> Html Msg
view model =
    teamTable model.teams
