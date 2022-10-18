module Page.Teams exposing (..)

import Backend.Scalar
import Graphql.Http
import Html exposing (Html, div, h2, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Queries.Do exposing (query)
import Queries.Error exposing (errorToString)
import Queries.TeamQueries exposing (TeamData, getTeamsQuery)
import Route exposing (link)
import Session exposing (Session)


type alias Model =
    { session : Session
    , teams : List TeamData
    , error : Maybe String
    }


type Msg
    = NoOp
    | GotTeamsResponse (Result (Graphql.Http.Error (List TeamData)) (List TeamData))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { teams = []
      , session = session
      , error = Nothing
      }
    , query getTeamsQuery GotTeamsResponse
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

                Err e ->
                    ( { model | error = Just (errorToString e) }, Cmd.none )


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
            [ link (Route.Team team.id) [] [ text (slugstr team.slug) ]
            ]
        , td [] [ text (Maybe.withDefault "" team.purpose) ]
        ]


teamTable : List TeamData -> Html Msg
teamTable teams =
    div []
        [ h2 [] [ text "Operations" ]
        , p [] [ link Route.CreateTeam [ class "button" ] [ text "Create new" ] ]
        , h2 [] [ text "List of teams" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Slug" ]
                    , th [] [ text "Purpose" ]
                    ]
                ]
            , tbody [] (List.map row teams)
            ]
        ]


view : Model -> Html Msg
view model =
    case model.error of
        Nothing ->
            teamTable model.teams

        Just err ->
            text err
