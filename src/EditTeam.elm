module EditTeam exposing (..)

import Backend.Enum.TeamRole exposing (TeamRole)
import Backend.Object.User exposing (roles)
import Backend.Scalar exposing (Slug(..), Uuid)
import Browser.Navigation
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, div, form, h2, input, label, li, option, p, select, table, tbody, td, text, th, tr, ul)
import Html.Attributes exposing (class, disabled, for, placeholder, readonly, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List exposing (member)
import Queries.Do
import Queries.TeamQueries exposing (TeamData, TeamMemberData, createTeamMutation, updateTeamMutation)
import Queries.UserQueries exposing (UserData)
import Session exposing (Session)


type alias Model =
    { session : Session
    , id : Uuid
    , slug : String
    , originalName : Maybe String
    , name : Maybe String
    , purpose : Maybe String
    , error : Maybe String
    , members : List TeamMemberData
    }


type Msg
    = SubmitForm
    | GotTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotUpdateTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotSetTeamMemberRoleResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | NameChanged String
    | PurposeChanged String
    | RoleDropDownClicked TeamMemberData TeamRole


init : Session -> Uuid -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , originalName = Nothing
      , name = Nothing
      , purpose = Nothing
      , slug = ""
      , error = Nothing
      , members = []
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
            ( { model | error = Nothing } |> mapTeam team, Cmd.none )

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

        GotSetTeamMemberRoleResponse (Ok team) ->
            ( { model | error = Nothing } |> mapTeam team, Cmd.none )

        GotSetTeamMemberRoleResponse (Err (Graphql.Http.HttpError e)) ->
            ( { model | error = Just "Can't talk to server, are we connected?" }, Cmd.none )

        GotSetTeamMemberRoleResponse (Err (GraphqlError _ errors)) ->
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

        RoleDropDownClicked member role ->
            if member.role == role then
                ( model, Cmd.none )
            else
                (model, setTeamMemberRole member.user.id model.id role)


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
        , members = teamData.members
    }


getTeam : Uuid -> Cmd Msg
getTeam uuid =
    Queries.Do.query
        (Queries.TeamQueries.getTeamQuery uuid)
        GotTeamResponse


setTeamMemberRole : Uuid -> Uuid -> TeamRole -> Cmd Msg
setTeamMemberRole teamUuid memberUuid role =
    Queries.Do.mutate
        (Queries.TeamQueries.setTeamMemberRoleMutation teamUuid memberUuid role)
        GotSetTeamMemberRoleResponse


view : Model -> Html Msg
view model =
    div []
        [ formView model
        , memberView model
        ]


memberView : Model -> Html Msg
memberView model =
    table []
        [ tr []
            [ th [] [ text "Email" ]
            , th [] [ text "Role" ]
            , th [] [ text "Delete" ]
            ]
        , tbody [] (List.map memberRow model.members)
        ]


roleOption : TeamMemberData -> TeamRole -> Html Msg
roleOption member role =
    let
        roleStr =
            Backend.Enum.TeamRole.toString role
    in
    option
        [ onClick (RoleDropDownClicked member role)
        , selected (role == member.role)
        , value roleStr
        ]
        [ text roleStr ]


roleSelector : TeamMemberData -> Html Msg
roleSelector active =
    select [] (Backend.Enum.TeamRole.list |> List.map (roleOption active))


memberRow : TeamMemberData -> Html Msg
memberRow member =
    tr []
        [ td [] [ text member.user.email ]
        , td [] [ roleSelector member ]
        , td [] [ text (Backend.Enum.TeamRole.toString member.role) ]
        ]
