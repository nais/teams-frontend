module Page.Teams exposing (..)

import Api.Do exposing (query)
import Api.Error exposing (errorToString)
import Api.Team exposing (TeamData, getTeams)
import Backend.Scalar
import Graphql.Http
import Html exposing (Html, div, h2, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
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
    , query getTeams (RemoteData.fromResult >> GotTeamsResponse)
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
    div [ class "card" ]
        [ div [ class "title" ]
            [ h2 [] [ text "Teams" ]
            , link Route.CreateTeam [ class "button small" ] [ text "Create" ]
            ]
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
