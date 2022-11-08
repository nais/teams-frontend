module Page.EditTeam exposing (..)

import Api.Do
import Api.Error exposing (errorToString)
import Api.Team exposing (TeamData, TeamMemberData, roleString, updateTeam)
import Api.User exposing (UserData)
import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (Slug(..), Uuid)
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, button, datalist, div, form, h2, input, label, li, option, p, select, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, disabled, for, id, list, placeholder, readonly, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Session exposing (Session, User(..))


type alias Model =
    { session : Session
    , error : Maybe String
    , team : TeamData
    , userList : List UserData
    , addMemberSearch : String
    }


type Msg
    = SubmitForm
    | GotTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotUpdateTeamResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotSetTeamMemberRoleResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotRemoveTeamMemberResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotAddTeamMemberResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | GotUserListResponse (Result (Graphql.Http.Error (List UserData)) (List UserData))
    | PurposeChanged String
    | RoleDropDownClicked TeamMemberData TeamRole
    | RemoveMemberClicked TeamMemberData
    | AddMemberSearchChanged String


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    ( { session = session
      , error = Nothing
      , userList = []
      , addMemberSearch = ""
      , team =
            { slug = Slug ""
            , purpose = ""
            , members = []
            , auditLogs = []
            , metadata = []
            , syncErrors = []
            , lastSuccessfulSync = Nothing
            }
      }
    , Cmd.batch
        [ getTeam slug
        , getUserList
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitForm ->
            ( model
            , Api.Do.mutate
                (updateTeam
                    model.team.slug
                    { purpose = Graphql.OptionalArgument.Present model.team.purpose
                    }
                )
                GotUpdateTeamResponse
            )

        GotTeamResponse (Ok team) ->
            ( { model | error = Nothing, team = team }, Cmd.none )

        GotTeamResponse (Err e) ->
            ( { model | error = Just (errorToString e) }, Cmd.none )

        GotUpdateTeamResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none )

        GotUpdateTeamResponse (Err e) ->
            ( { model | error = Just (errorToString e) }, Cmd.none )

        GotSetTeamMemberRoleResponse (Ok team) ->
            ( { model | error = Nothing, team = team }, Cmd.none )

        GotSetTeamMemberRoleResponse (Err e) ->
            ( { model | error = Just (errorToString e) }, Cmd.none )

        GotRemoveTeamMemberResponse (Ok team) ->
            ( { model | error = Nothing, team = team }, Cmd.none )

        GotRemoveTeamMemberResponse (Err e) ->
            ( { model | error = Just (errorToString e) }, Cmd.none )

        GotAddTeamMemberResponse (Ok team) ->
            ( { model | error = Nothing, team = team }, Cmd.none )

        GotAddTeamMemberResponse (Err e) ->
            ( { model | error = Just (errorToString e) }, Cmd.none )

        GotUserListResponse (Ok userList) ->
            ( { model | error = Nothing, userList = userList }, Cmd.none )

        GotUserListResponse (Err e) ->
            ( { model | error = Just (errorToString e) }, Cmd.none )

        PurposeChanged purpose ->
            ( { model | team = mapTeamPurpose purpose model.team }, Cmd.none )

        RoleDropDownClicked member role ->
            if member.role == role then
                ( model, Cmd.none )

            else
                ( model, setTeamMemberRole model.team member role )

        RemoveMemberClicked member ->
            ( model, removeTeamMember model.team member.user )

        AddMemberSearchChanged query ->
            case List.head (List.filter (matchExactUser query) model.userList) of
                Just u ->
                    ( { model | addMemberSearch = "" }, addTeamMember model.team u )

                Nothing ->
                    ( { model | addMemberSearch = query }, Cmd.none )


matchExactUser : String -> UserData -> Bool
matchExactUser query user =
    nameAndEmail user == query


textbox : (String -> Msg) -> String -> String -> String -> String -> Html Msg
textbox msg val id lbl placeholder =
    li []
        [ label [ for id ] [ text lbl ]
        , input [ type_ "text", Html.Attributes.placeholder placeholder, onInput msg, value val ] []
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
        [ h2 [] [ text ("Edit team \"" ++ slugstr model.team.slug ++ "\"") ]
        , form [ onSubmit SubmitForm ]
            (ul []
                [ li []
                    [ label [ for "slug" ] [ text "Identifier" ]
                    , p [] [ text <| slugstr model.team.slug ]
                    ]
                , textbox PurposeChanged model.team.purpose "purpose" "Purpose of the team" "Making sure customers have a good user experience"
                ]
                :: errorView model.error
                ++ [ button [ type_ "submit" ] [ text "Save changes" ]
                   ]
            )
        ]


slugstr : Slug -> String
slugstr (Slug s) =
    s


mapTeamPurpose : String -> TeamData -> TeamData
mapTeamPurpose purpose team =
    { team | purpose = purpose }


getTeam : Slug -> Cmd Msg
getTeam slug =
    Api.Do.query
        (Api.Team.getTeam slug)
        GotTeamResponse


getUserList : Cmd Msg
getUserList =
    Api.Do.query
        Api.User.getAllUsers
        GotUserListResponse


setTeamMemberRole : TeamData -> TeamMemberData -> TeamRole -> Cmd Msg
setTeamMemberRole team member role =
    Api.Do.mutate
        (Api.Team.setTeamMemberRole team member role)
        GotSetTeamMemberRoleResponse


removeTeamMember : TeamData -> UserData -> Cmd Msg
removeTeamMember team user =
    Api.Do.mutate
        (Api.Team.removeMemberFromTeam team user)
        GotRemoveTeamMemberResponse


addTeamMember : TeamData -> UserData -> Cmd Msg
addTeamMember team user =
    Api.Do.mutate
        (Api.Team.addMemberToTeam team user)
        GotAddTeamMemberResponse


view : Model -> Html Msg
view model =
    div []
        [ formView model
        , memberView model
        ]


memberView : Model -> Html Msg
memberView model =
    let
        currentUser =
            Session.user model.session
    in
    div []
        [ h2 [] [ text "Membership administration" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    , th [] [ text "" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td []
                        [ input [ list "teams", type_ "text", value model.addMemberSearch, onInput AddMemberSearchChanged ] []
                        , datalist [ id "teams" ] (List.map addUserCandidateRow model.userList)
                        ]
                    , td [ colspan 2 ] [ text "(new member)" ]
                    ]
                ]
            , tbody [] (List.map (memberRow currentUser) model.team.members)
            ]
        ]


roleOption : TeamMemberData -> TeamRole -> Html Msg
roleOption member role =
    let
        roleID =
            Backend.Enum.TeamRole.toString role

        roleStr =
            roleString role
    in
    option
        [ onClick (RoleDropDownClicked member role)
        , selected (role == member.role)
        , value roleID
        ]
        [ text roleStr ]


roleSelector : User -> TeamMemberData -> Html Msg
roleSelector currentUser member =
    let
        isMember =
            userIsMember currentUser member

        isAdmin =
            member.role == Owner

        isGlobalAdmin =
            Session.isGlobalAdmin currentUser
    in
    select [ disabled (isMember && isAdmin && not isGlobalAdmin) ] (Backend.Enum.TeamRole.list |> List.map (roleOption member))


userIsMember : User -> TeamMemberData -> Bool
userIsMember currentUser member =
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
        , td [] [ button [ class "red", onClick (RemoveMemberClicked member) ] [ text "Remove" ] ]
        ]


nameAndEmail : UserData -> String
nameAndEmail user =
    user.name ++ " <" ++ user.email ++ ">"


addUserCandidateRow : UserData -> Html msg
addUserCandidateRow user =
    option [] [ text (nameAndEmail user) ]
