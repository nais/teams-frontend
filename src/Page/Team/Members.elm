module Page.Team.Members exposing (AddMember, MemberChange(..), Mode(..), Model, Msg(..), init, update, view)

import Api.Do exposing (mutateRD)
import Api.Str exposing (roleStr)
import Api.Team exposing (addMemberToTeam, addOwnerToTeam, removeMemberFromTeam, setTeamMemberRole)
import Backend.Enum.TeamRole as TeamRole exposing (TeamRole(..))
import Component.Buttons exposing (smallButton)
import DataModel exposing (Expandable(..), Team, TeamMember, User, expandableAll, expandableTake, flipExpanded)
import Graphql.Http
import Html exposing (Html, button, datalist, div, form, h2, input, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, colspan, disabled, id, list, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import RemoteData exposing (RemoteData(..))
import Util exposing (appendMaybe, conditionalElement)


type Mode
    = View
    | Edit


type MemberChange
    = Unchanged TeamMember
    | Remove TeamMember
    | Add TeamRole TeamMember
    | ChangeRole TeamRole TeamMember


type alias Model =
    { team : Team
    , allUsers : List User
    , isEditor : Bool
    , mode : Mode
    , addMember : AddMember
    , changes : List MemberChange
    }


type alias AddMember =
    { email : String
    , role : TeamRole
    }


mapAddMember : (a -> a) -> { b | addMember : a } -> { b | addMember : a }
mapAddMember a b =
    { b | addMember = a b.addMember }


setRole : a -> { b | role : a } -> { b | role : a }
setRole a b =
    { b | role = a }


setEmail : a -> { b | email : a } -> { b | email : a }
setEmail a b =
    { b | email = a }


mapChanges : (a -> a) -> { b | changes : a } -> { b | changes : a }
mapChanges a b =
    { b | changes = a b.changes }


mapTeam : (a -> a) -> { b | team : a } -> { b | team : a }
mapTeam a b =
    { b | team = a b.team }


type Msg
    = ClickedFormSave
    | ClickedFormEdit
    | ClickedFormCancel
    | ClickedNewMemberAdd
    | ClickedNewMemberRole String
    | ClickedMemberRole MemberChange String
    | ClickedMemberRemove MemberChange
    | ClickedMemberUndo MemberChange
    | ClickedShowMore
    | GotSaveTeamMembersResponse (RemoteData (Graphql.Http.Error Team) Team)
    | InputChangedNewMember String


init : Team -> List User -> Bool -> Model
init team allUsers isEditor =
    { team = team
    , allUsers = allUsers
    , isEditor = isEditor
    , mode = View
    , addMember = { email = "", role = TeamRole.Member }
    , changes = mcInit team.members
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFormSave ->
            ( model, Cmd.batch (List.concatMap (mapMemberChangeToCmds model.team) model.changes) )

        ClickedFormEdit ->
            ( { model | mode = Edit }, Cmd.none )

        ClickedFormCancel ->
            ( { model
                | mode = View
                , changes = mcInit model.team.members
              }
            , Cmd.none
            )

        ClickedNewMemberRole role ->
            ( model |> mapAddMember (setRole (teamRoleFromString role)), Cmd.none )

        ClickedNewMemberAdd ->
            ( model
                |> mapChanges (mcAdd model.addMember model.allUsers)
                |> mapAddMember (setEmail "")
            , Cmd.none
            )

        ClickedMemberRole change roleString ->
            ( model |> mapChanges (mcChangeRole change roleString), Cmd.none )

        ClickedMemberRemove change ->
            ( model |> mapChanges (mcRemove change), Cmd.none )

        ClickedMemberUndo change ->
            ( model |> mapChanges (mcUndo change), Cmd.none )

        ClickedShowMore ->
            ( model |> mapTeam (\t -> { t | members = flipExpanded t.members }), Cmd.none )

        GotSaveTeamMembersResponse r ->
            case r of
                Success team ->
                    ( { model | mode = View, team = team }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InputChangedNewMember email ->
            ( model |> mapAddMember (setEmail email), Cmd.none )


view : Model -> Html Msg
view model =
    case model.mode of
        Edit ->
            viewEditMembers model

        View ->
            viewMembers model.team.members model.isEditor


viewMembers : Expandable (List TeamMember) -> Bool -> Html Msg
viewMembers members isEditor =
    div [ class "card" ]
        ([ div [ class "title" ]
            ([ h2 [] [ text "Members" ] ]
                |> appendMaybe (smallButton ClickedFormEdit "edit" "Edit" |> conditionalElement isEditor)
            )
         , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    ]
                ]
            , tbody []
                (if List.length (expandableTake 10 members) == 0 then
                    [ tr [] [ td [ colspan 2 ] [ text "This team has no members" ] ] ]

                 else
                    List.map viewMemberRow (expandableTake 10 members)
                )
            ]
         ]
            |> appendMaybe (showMoreButton members 10 ClickedShowMore)
        )


viewEditMembers : Model -> Html Msg
viewEditMembers model =
    div [ class "card" ]
        [ h2 [] [ text "Members" ]
        , form [ id "addMemberForm", onSubmit ClickedNewMemberAdd ] []
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] [ text "Role" ]
                    , th [] [ text "" ]
                    ]
                ]
            , tbody []
                (tr []
                    [ td []
                        [ input
                            [ list "userCandidates"
                            , Html.Attributes.form "addMemberForm"
                            , type_ "text"
                            , value model.addMember.email
                            , onInput InputChangedNewMember
                            ]
                            []
                        , datalist [ id "userCandidates" ] (List.map (\u -> option [] [ text u.email ]) model.allUsers)
                        ]

                    --
                    , td [] [ viewRoleSelector model.addMember.role ClickedNewMemberRole False ]
                    , td [ colspan 2 ]
                        [ button [ type_ "submit", class "small button", Html.Attributes.form "addMemberForm", disabled (queryUserList model.addMember.email model.allUsers == Nothing) ]
                            [ div [ class "icon add" ] []
                            , text "Add"
                            ]
                        ]
                    ]
                    :: List.map editMemberRow model.changes
                )
            ]
        , div [ class "button-row" ]
            [ button [ disabled (noMemberChanges model.changes || not (model.addMember.email == "")), onClick ClickedFormSave ] [ text "Save members" ]
            , button [ class "transparent", onClick ClickedFormCancel ] [ text "Cancel changes" ]
            ]
        ]


mapMemberChangeToCmds : Team -> MemberChange -> List (Cmd Msg)
mapMemberChangeToCmds team change =
    case change of
        Add r m ->
            case r of
                TeamRole.Member ->
                    [ mutateRD (addMemberToTeam team m.user) GotSaveTeamMembersResponse ]

                TeamRole.Owner ->
                    [ mutateRD (addOwnerToTeam team m.user) GotSaveTeamMembersResponse ]

        Remove m ->
            [ mutateRD (removeMemberFromTeam team m.user) GotSaveTeamMembersResponse ]

        ChangeRole r m ->
            [ mutateRD (setTeamMemberRole team m r) GotSaveTeamMembersResponse ]

        Unchanged _ ->
            []


noMemberChanges : List MemberChange -> Bool
noMemberChanges changes =
    List.filter
        (\c ->
            case c of
                Unchanged _ ->
                    False

                _ ->
                    True
        )
        changes
        |> List.isEmpty


editMemberRow : MemberChange -> Html Msg
editMemberRow member =
    let
        role : TeamRole
        role =
            case member of
                Unchanged m ->
                    m.role

                Add r _ ->
                    r

                ChangeRole r _ ->
                    r

                Remove m ->
                    m.role

        roleSelector : Bool -> Html Msg
        roleSelector =
            viewRoleSelector role (ClickedMemberRole member)

        viewButton : String -> String -> String -> msg -> Html msg
        viewButton cls svg txt msg =
            button [ class <| "button small " ++ cls, onClick msg ]
                [ div [ class <| "icon " ++ svg ] []
                , text txt
                ]
    in
    case member of
        Unchanged m ->
            tr []
                [ td [] [ text m.user.email ]
                , td [] [ roleSelector False ]
                , td [] [ viewButton "" "delete" "Remove" (ClickedMemberRemove member) ]
                ]

        Remove m ->
            tr []
                [ td [ class "strikethrough" ] [ text m.user.email ]
                , td [] [ roleSelector True ]
                , td [] [ viewButton "transparent" "cancel" "Undo" (ClickedMemberUndo member) ]
                ]

        Add _ m ->
            tr []
                [ td [] [ text m.user.email ]
                , td [] [ roleSelector False ]
                , td [] [ viewButton "transparent" "cancel" "Undo" (ClickedMemberUndo member) ]
                ]

        ChangeRole _ m ->
            tr []
                [ td [] [ text m.user.email ]
                , td [] [ roleSelector False, text " *" ]
                , td [] [ viewButton "transparent" "cancel" "Undo" (ClickedMemberUndo member) ]
                ]


viewRoleSelector : TeamRole -> (String -> Msg) -> Bool -> Html Msg
viewRoleSelector currentRole action disable =
    select
        [ value (roleStr currentRole)
        , onInput action
        , disabled disable
        ]
        (TeamRole.list |> List.map (roleOption currentRole))


roleOption : TeamRole -> TeamRole -> Html Msg
roleOption currentRole role =
    option
        [ selected (role == currentRole)
        , value (roleStr role)
        ]
        [ text (roleStr role) ]


queryUserList : String -> List User -> Maybe User
queryUserList query userList =
    List.filter (\u -> u.email == query) userList
        |> List.head


viewMemberRow : TeamMember -> Html Msg
viewMemberRow member =
    tr []
        [ td [] [ text member.user.email ]
        , td [ classList [ ( "team-owner", member.role == Owner ) ] ] [ text <| roleStr member.role ]
        ]


teamRoleFromString : String -> TeamRole
teamRoleFromString s =
    Maybe.withDefault TeamRole.Member <| TeamRole.fromString (String.toUpper s)


showMoreButton : Expandable (List a) -> Int -> Msg -> Maybe (Html Msg)
showMoreButton expandable previewSize msg =
    let
        ( belowPreview, t ) =
            case expandable of
                Preview list ->
                    if List.length list > previewSize then
                        ( False, "show more" )

                    else
                        ( True, "" )

                Expanded _ ->
                    ( False, "show less" )
    in
    if belowPreview then
        Nothing

    else
        div []
            [ button
                [ class "text", onClick msg ]
                [ text t ]
            ]
            |> Just


mcGetTeamMember : MemberChange -> TeamMember
mcGetTeamMember change =
    case change of
        Unchanged m ->
            m

        Remove m ->
            m

        Add _ m ->
            m

        ChangeRole _ m ->
            m


mcMapChange : MemberChange -> MemberChange -> MemberChange
mcMapChange new existing =
    if (mcGetTeamMember new).user.email == (mcGetTeamMember existing).user.email then
        new

    else
        existing


mcUndo : MemberChange -> List MemberChange -> List MemberChange
mcUndo change changes =
    changes
        |> (case change of
                Add _ _ ->
                    List.filter (\c -> not (mcGetTeamMember c == mcGetTeamMember change))

                _ ->
                    List.map (mcMapChange (Unchanged (mcGetTeamMember change)))
           )


mcRemove : MemberChange -> List MemberChange -> List MemberChange
mcRemove change changes =
    List.map (mcMapChange (Remove (mcGetTeamMember change))) changes


mcChangeRole : MemberChange -> String -> List MemberChange -> List MemberChange
mcChangeRole change roleStr changes =
    let
        role : TeamRole
        role =
            teamRoleFromString roleStr

        new : MemberChange
        new =
            case change of
                Add _ m ->
                    Add role m

                Remove m ->
                    Remove m

                Unchanged m ->
                    ChangeRole role m

                ChangeRole _ m ->
                    ChangeRole role m
    in
    List.map (mcMapChange new) changes


mcAdd : AddMember -> List User -> List MemberChange -> List MemberChange
mcAdd new allUsers changes =
    if (changes |> List.filter (\c -> (mcGetTeamMember c).user.email == new.email) |> List.length) > 0 then
        changes

    else
        case queryUserList new.email allUsers of
            Just u ->
                Add new.role (TeamMember u new.role) :: changes

            Nothing ->
                changes


mcInit : Expandable (List TeamMember) -> List MemberChange
mcInit members =
    List.map Unchanged (expandableAll members)
