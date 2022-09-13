module Team exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar
import Browser.Navigation
import Graphql.Http exposing (RawError(..))
import Html exposing (Html, div, em, h2, h3, li, p, text, ul)
import Html.Attributes exposing (class)
import Queries.Do exposing (query)
import Queries.TeamQueries exposing (TeamData, TeamMemberData, getTeamQuery)


type Team
    = Team TeamData
    | Loading
    | Error String


type alias Model =
    { team : Team
    , navKey : Browser.Navigation.Key
    }


type Msg
    = GotTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)


init : Browser.Navigation.Key -> Backend.Scalar.Uuid -> ( Model, Cmd Msg )
init navigationKey id =
    ( { team = Loading
      , navKey = navigationKey
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

                Err (Graphql.Http.HttpError e) ->
                    ( { model | team = Error "Can't talk to server, are we connected?" }, Cmd.none )

                Err (GraphqlError data errors) ->
                    let
                        errstr =
                            List.map (\error -> error.message) errors
                                |> String.join ","
                    in
                    ( { model | team = Error errstr }, Cmd.none )


slugstr : Backend.Scalar.Slug -> String
slugstr (Backend.Scalar.Slug u) =
    u


memberRow : TeamMemberData -> Html Msg
memberRow member =
    li []
        [ text member.user.email
        , text " | "
        , text (Backend.Enum.TeamRole.toString member.role)
        ]


view : Model -> Html Msg
view model =
    case model.team of
        Team team ->
            div []
                [ h2 []
                    [ text "Team "
                    , em [] [ text (slugstr team.slug) ]
                    ]
                , p [] [ text ("Slug: " ++ slugstr team.slug) ]
                , p [] [ text ("Name: " ++ slugstr team.slug) ]
                , p [] [ text ("Purpose: " ++ Maybe.withDefault "" team.purpose) ]
                , text "members:"
                , ul [] (List.map memberRow team.members)
                ]

        Loading ->
            div [] [ text "spinner" ]

        Error msg ->
            div [ class "error" ] [ text msg ]
