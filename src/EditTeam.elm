module EditTeam exposing (..)

import Backend.Scalar exposing (Slug(..), Uuid)
import Browser.Navigation
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, div, form, h2, input, label, li, p, text, ul)
import Html.Attributes exposing (class, disabled, for, placeholder, readonly, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Queries.Do
import Queries.TeamQueries exposing (TeamData, createTeamMutation, updateTeamMutation)
import Session exposing (Session)


type alias Model =
    { session : Session
    , id : Uuid
    , slug : String
    , originalName : Maybe String
    , name : Maybe String
    , purpose : Maybe String
    , error : Maybe String
    }


type Msg
    = SubmitForm
    | GotTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotUpdateTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | NameChanged String
    | PurposeChanged String


init : Session -> Uuid -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , originalName = Nothing
      , name = Nothing
      , purpose = Nothing
      , slug = ""
      , error = Nothing
      }
    , getTeam id
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitForm ->
            ( model
            , Queries.Do.mutate
                (updateTeamMutation
                    model.id
                    { name = Graphql.OptionalArgument.fromMaybe model.name
                    , purpose = Graphql.OptionalArgument.fromMaybe model.purpose
                    }
                )
                GotUpdateTeamResponse
            )

        GotTeamResponse (Ok team) ->
            ( mapTeam team { model | error = Nothing }, Cmd.none )

        GotTeamResponse (Err (Graphql.Http.HttpError e)) ->
            ( { model | error = Just "Can't talk to server, are we connected?" }, Cmd.none )

        GotTeamResponse (Err (GraphqlError _ errors)) ->
            let
                errstr =
                    List.map (\error -> error.message) errors
                        |> String.join ","
            in
            ( { model | error = Just errstr }, Cmd.none )

        GotUpdateTeamResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none )

        GotUpdateTeamResponse (Err (Graphql.Http.HttpError e)) ->
            ( { model | error = Just "Can't talk to server, are we connected?" }, Cmd.none )

        GotUpdateTeamResponse (Err (GraphqlError _ errors)) ->
            let
                errstr =
                    List.map (\error -> error.message) errors
                        |> String.join ","
            in
            ( { model | error = Just errstr }, Cmd.none )

        NameChanged s ->
            ( { model | name = stringOrBlank s }, Cmd.none )

        PurposeChanged s ->
            ( { model | purpose = stringOrBlank s }, Cmd.none )


stringOrBlank : String -> Maybe String
stringOrBlank s =
    if s == "" then
        Nothing

    else
        Just s


textbox : (String -> Msg) -> Maybe String -> String -> String -> String -> Html Msg
textbox msg val id lbl placeholder =
    li []
        [ label [ for id ] [ text lbl ]
        , input [ type_ "text", Html.Attributes.placeholder placeholder, onInput msg, value (Maybe.withDefault "" val) ] []
        ]


errorView : Maybe String -> List (Html msg)
errorView maybeString =
    case maybeString of
        Nothing ->
            []

        Just s ->
            [ div [ class "error" ] [ text s ] ]


formView : Model -> Html Msg
formView model =
    div []
        [ h2 [] [ text ("Teams → Change " ++ Maybe.withDefault "team" model.originalName) ]
        , form [ onSubmit SubmitForm ]
            ([ ul []
                [ li []
                    [ label [ for "slug" ] [ text "Identifier" ]
                    , input [ type_ "text", readonly True, disabled True, value model.slug ] []
                    ]
                , textbox NameChanged model.name "name" "Team name" "Customer satisfaction"
                , textbox PurposeChanged model.purpose "purpose" "Purpose of the team" "Making sure customers have a good user experience"
                ]
             ]
                ++ errorView model.error
                ++ [ input [ type_ "submit", value "Save changes" ] []
                   ]
            )
        ]


mapTeam : TeamData -> Model -> Model
mapTeam teamData model =
    let
        slug =
            case teamData.slug of
                Slug s ->
                    s
    in
    { model
        | name = Just teamData.name
        , originalName = Just teamData.name
        , purpose = teamData.purpose
        , slug = slug
        , id = teamData.id
    }


getTeam : Uuid -> Cmd Msg
getTeam uuid =
    Queries.Do.query
        (Queries.TeamQueries.getTeamQuery uuid)
        GotTeamResponse


view : Model -> Html Msg
view model =
    formView model
