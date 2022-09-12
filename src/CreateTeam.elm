module CreateTeam exposing (..)

import Backend.Scalar
import Graphql.Http
import Html exposing (Html, a, button, div, form, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (href, placeholder, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Queries.Do exposing (send)
import Queries.TeamQueries exposing (TeamData, getTeamsQuery)
import String exposing (startsWith)


type alias Model =
    {}


type Msg
    = CreateTeamSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTeamSubmit ->
            ( model, Cmd.none )


createTeamForm : Html Msg
createTeamForm =
    div []
        [ form [ onSubmit CreateTeamSubmit ]
            [ input [ type_ "text", placeholder "slug" ] []
            , input [ type_ "text", placeholder "name" ] []
            , input [ type_ "text", placeholder "purpose" ] []
            , input [ type_ "submit" ] [ text "create team" ]
            ]
        ]


view : Model -> Html Msg
view model =
    createTeamForm
