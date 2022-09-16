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
import Session exposing (Session, User(..))


type alias Model =
    { session : Session
    , originalName : Maybe String
    , error : Maybe String
    , team : TeamData
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
      , originalName = Nothing
      , error = Nothing
      , team =
            { id = id
            , name = ""
            , slug = Slug ""
            , purpose = Nothing
            , members = []
            , auditLogs = []
            }
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
                    model.team.id
                    { name = Graphql.OptionalArgument.Present model.team.name
                    , purpose = Graphql.OptionalArgument.fromMaybe model.team.purpose
                    }
                )
                GotUpdateTeamResponse
            )

        GotTeamResponse (Ok team) ->
            ( { model | error = Nothing, team = team, originalName = Just team.name }, Cmd.none )

        GotTeamResponse (Err e) ->
            handleGraphQLError model e

        GotUpdateTeamResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none )

        GotUpdateTeamResponse (Err e) ->
            handleGraphQLError model e

        GotSetTeamMemberRoleResponse (Ok team) ->
            ( { model | error = Nothing, team = team }, Cmd.none )

        GotSetTeamMemberRoleResponse (Err e) ->
            handleGraphQLError model e

        NameChanged name ->
            ( { model | team = mapTeamName name model.team }, Cmd.none )

        PurposeChanged purpose ->
            ( { model | team = mapTeamPurpose purpose model.team }, Cmd.none )

        RoleDropDownClicked member role ->
            if member.role == role then
                ( model, Cmd.none )

            else
                ( model, setTeamMemberRole model.team member role )


handleGraphQLError : Model -> RawError parsedData httpError -> ( Model, Cmd msg )
handleGraphQLError model err =
    case err of
        Graphql.Http.HttpError e ->
            ( { model | error = Just "Can't talk to server, are we connected?" }, Cmd.none )

        GraphqlError _ errors ->
            let
                errstr =
                    List.map (\error -> error.message) errors
                        |> String.join ","
            in
            ( { model | error = Just errstr }, Cmd.none )


stringOrNothing : String -> Maybe String
stringOrNothing s =
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
            (ul []
                [ li []
                    [ label [ for "slug" ] [ text "Identifier" ]
                    , input [ type_ "text", readonly True, disabled True, value (slugstr model.team.slug) ] []
                    ]
                , textbox NameChanged (stringOrNothing model.team.name) "name" "Team name" "Customer satisfaction"
                , textbox PurposeChanged model.team.purpose "purpose" "Purpose of the team" "Making sure customers have a good user experience"
                ]
                :: errorView model.error
                ++ [ input [ type_ "submit", value "Save changes" ] []
                   ]
            )
        ]


slugstr : Slug -> String
slugstr (Slug s) =
    s


mapTeamName : String -> TeamData -> TeamData
mapTeamName name team =
    { team | name = name }


mapTeamPurpose : String -> TeamData -> TeamData
mapTeamPurpose purpose team =
    { team | purpose = Just purpose }


getTeam : Uuid -> Cmd Msg
getTeam uuid =
    Queries.Do.query
        (Queries.TeamQueries.getTeamQuery uuid)
        GotTeamResponse


setTeamMemberRole : TeamData -> TeamMemberData -> TeamRole -> Cmd Msg
setTeamMemberRole team member role =
    Queries.Do.mutate
        (Queries.TeamQueries.setTeamMemberRoleMutation team member role)
        GotSetTeamMemberRoleResponse


view : Model -> Html Msg
view model =
    div []
        [ formView model
        , memberView (Session.user model.session)model.team.members 
        ]


memberView : User -> List TeamMemberData -> Html Msg
memberView currentUser members =
    table []
        [ tr []
            [ th [] [ text "Email" ]
            , th [] [ text "Role" ]
            , th [] [ text "Delete" ]
            ]
        , tbody [] (List.map (memberRow currentUser) members)
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


roleSelector : User -> TeamMemberData -> Html Msg
roleSelector currentUser member =
    select [ disabled (isActiveUser currentUser member) ] (Backend.Enum.TeamRole.list |> List.map (roleOption member))


isActiveUser : User -> TeamMemberData -> Bool
isActiveUser currentUser member =
    case currentUser of
        LoggedIn u ->
            u.id == member.user.id

        _ ->
            False


memberRow : User -> TeamMemberData -> Html Msg
memberRow currentUser member =
    tr []
        [ td [] [ text member.user.email ]
        , td [] [ roleSelector currentUser member ]
        , td [] [ text (Backend.Enum.TeamRole.toString member.role) ]
        ]
