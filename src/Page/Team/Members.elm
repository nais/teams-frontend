module Page.Team.Members exposing (AddMember, Mode(..), Model, Msg(..), Row, RowState, init, update, view)

import Api.Do exposing (mutateRD)
import Api.Error exposing (errorToString)
import Api.Str exposing (roleStr)
import Api.Team exposing (addMemberToTeam, addOwnerToTeam, removeMemberFromTeam, setTeamMemberRole)
import Backend.Enum.TeamRole as TeamRole exposing (TeamRole(..))
import Component.Buttons exposing (smallButton)
import Component.Icons exposing (spinnerDone, spinnerError, spinnerLoading)
import DataModel exposing (Expandable(..), Team, TeamMember, User, expandableAll, expandableTake, flipExpanded)
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, datalist, div, form, h2, input, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, colspan, disabled, id, list, selected, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import RemoteData exposing (RemoteData(..))
import Util exposing (appendMaybe, conditionalElement)


type Mode
    = View
    | Edit


type RowState
    = PendingRemove
    | PendingChange
    | Unchanged
    | Updated
    | Error String


type alias Row =
    { member : TeamMember
    , state : RowState
    }


type alias Model =
    { team : Team
    , allUsers : List User
    , isEditor : Bool
    , mode : Mode
    , addMember : AddMember
    , members : List Row
    }


type alias AddMember =
    { email : String
    , role : TeamRole
    }


mapAddMember : (AddMember -> AddMember) -> Model -> Model
mapAddMember fn model =
    { model | addMember = fn model.addMember }


setRole : TeamRole -> AddMember -> AddMember
setRole role addMember =
    { addMember | role = role }


setEmail : String -> AddMember -> AddMember
setEmail email addMember =
    { addMember | email = email }


mapMembers : (List Row -> List Row) -> Model -> Model
mapMembers fn model =
    { model | members = fn model.members }


mapTeam : (Team -> Team) -> Model -> Model
mapTeam fn model =
    { model | team = fn model.team }


type Msg
    = ClickedFormEdit
    | ClickedFormCancel
    | ClickedNewMemberAdd
    | ClickedNewMemberRole String
    | ClickedMemberRole Row String
    | ClickedMemberRemove Row
    | ClickedMemberRemoveConfirm Row
    | ClickedShowMore
    | GotSaveTeamMemberResponse Row (RemoteData (Graphql.Http.Error Team) Team)
    | InputChangedNewMember String


init : Team -> List User -> Bool -> Model
init team allUsers isEditor =
    { team = team
    , allUsers = allUsers
    , isEditor = isEditor
    , mode = View
    , addMember = { email = "", role = TeamRole.Member }
    , members = toRows team.members
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFormEdit ->
            ( { model | mode = Edit }, Cmd.none )

        ClickedFormCancel ->
            ( { model
                | mode = View
                , members = toRows model.team.members
              }
            , Cmd.none
            )

        ClickedNewMemberRole role ->
            ( model |> mapAddMember (setRole (teamRoleFromString role)), Cmd.none )

        ClickedNewMemberAdd ->
            case queryUserList model.addMember.email model.allUsers of
                Just user ->
                    let
                        mutation : Team -> User -> SelectionSet Team RootMutation
                        mutation =
                            case model.addMember.role of
                                TeamRole.Owner ->
                                    addOwnerToTeam

                                TeamRole.Member ->
                                    addMemberToTeam

                        member : TeamMember
                        member =
                            TeamMember user model.addMember.role
                    in
                    ( model
                        |> mapMembers (addRow (Row member PendingChange))
                        |> mapAddMember (setEmail "")
                    , mutateRD (mutation model.team user) (GotSaveTeamMemberResponse (Row member PendingChange))
                    )

                Nothing ->
                    -- TODO: render error
                    ( model, Cmd.none )

        ClickedMemberRole row roleString ->
            let
                r : TeamRole
                r =
                    teamRoleFromString roleString
            in
            ( model |> mapMembers (mapRow (Row row.member PendingChange)), mutateRD (setTeamMemberRole model.team row.member r) (GotSaveTeamMemberResponse row) )

        ClickedMemberRemove row ->
            ( model |> mapMembers (mapRow (Row row.member PendingRemove)), Cmd.none )

        ClickedMemberRemoveConfirm row ->
            ( model |> mapMembers (mapRow (Row row.member PendingChange)), mutateRD (removeMemberFromTeam model.team row.member.user) (GotSaveTeamMemberResponse row) )

        ClickedShowMore ->
            ( model |> mapTeam (\t -> { t | members = flipExpanded t.members }), Cmd.none )

        GotSaveTeamMemberResponse row resp ->
            case resp of
                Success team ->
                    ( { model | team = team } |> mapMembers (updateRows team.members), Cmd.none )

                Failure err ->
                    ( model |> mapMembers (updateRowsError row (errorToString err)), Cmd.none )

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
                |> appendMaybe (smallButton ClickedFormEdit "edit" "Edit mode" |> conditionalElement isEditor)
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
                    List.map viewRow (expandableTake 10 members)
                )
            ]
         ]
            |> appendMaybe (showMoreButton members 10 ClickedShowMore)
        )


viewEditMembers : Model -> Html Msg
viewEditMembers model =
    div [ class "card" ]
        [ div [ class "title" ]
            [ h2 [] [ text "Members" ]
            , smallButton ClickedFormCancel "edit" "View mode"
            ]
        , form [ id "addMemberForm", onSubmit ClickedNewMemberAdd ] []
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Email" ]
                    , th [] []
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
                        , datalist [ id "userCandidates" ] (candidates model)
                        ]
                    , td [] []
                    , td [] [ viewRoleSelector model.addMember.role ClickedNewMemberRole False ]
                    , td []
                        [ button [ type_ "submit", class "small button", Html.Attributes.form "addMemberForm", disabled (queryUserList model.addMember.email model.allUsers == Nothing) ]
                            [ div [ class "icon add" ] []
                            , text "Add"
                            ]
                        ]
                    ]
                    :: List.map viewEditRow model.members
                )
            ]
        ]


candidates : Model -> List (Html Msg)
candidates model =
    let
        memberEmails : List String
        memberEmails =
            model.team.members
                |> expandableAll
                |> List.map (\m -> m.user.email)
    in
    model.allUsers
        |> List.map (\u -> u.email)
        |> List.filter (\email -> not (List.member email memberEmails))
        |> List.map (\email -> option [] [ text email ])


viewEditRow : Row -> Html Msg
viewEditRow row =
    let
        roleSelector : Html Msg
        roleSelector =
            viewRoleSelector row.member.role (ClickedMemberRole row) (row.state == PendingChange)

        phase : Html msg
        phase =
            case row.state of
                PendingChange ->
                    span [ title "Updating" ] [ spinnerLoading ]

                PendingRemove ->
                    span [ title "Are you sure you want to remove this user from the team?" ] [ text "😱" ]

                Unchanged ->
                    text ""

                Updated ->
                    span [ title "Saved" ] [ spinnerDone ]

                Error err ->
                    span [ title err ] [ spinnerError ]

        btn : Html Msg
        btn =
            case row.state of
                PendingChange ->
                    button [ class "small button", disabled True ] [ div [ class "icon delete" ] [], text "Remove" ]

                PendingRemove ->
                    smallButton (ClickedMemberRemoveConfirm row) "delete" "Confirm"

                Unchanged ->
                    smallButton (ClickedMemberRemove row) "delete" "Remove"

                Updated ->
                    smallButton (ClickedMemberRemove row) "delete" "Remove"

                Error _ ->
                    smallButton (ClickedMemberRemove row) "delete" "Remove"
    in
    tr []
        [ td [] [ text row.member.user.email ]
        , td [] [ phase ]
        , td [] [ roleSelector ]
        , td [] [ btn ]
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


viewRow : TeamMember -> Html Msg
viewRow member =
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


mapMember : Row -> Row -> Row
mapMember new existing =
    if new.member.user.email == existing.member.user.email then
        new

    else
        existing


mapRow : Row -> List Row -> List Row
mapRow new existing =
    List.map (mapMember new) existing


addRow : Row -> List Row -> List Row
addRow new list =
    case list of
        [] ->
            [ new ]

        [ row ] ->
            if sortRows new row then
                [ new, row ]

            else
                [ row, new ]

        row :: tail ->
            if sortRows new row then
                new :: row :: tail

            else
                row :: addRow new tail


toRows : Expandable (List TeamMember) -> List Row
toRows members =
    List.map (\m -> Row m Unchanged) (expandableAll members)


sortRows : Row -> Row -> Bool
sortRows r1 r2 =
    r1.member.user.email < r2.member.user.email


updateRowsError : Row -> String -> List Row -> List Row
updateRowsError row error allRows =
    mapRow (Row row.member (Error error)) allRows


updateRows : Expandable (List TeamMember) -> List Row -> List Row
updateRows expandableMembers allRows =
    let
        findMember : String -> List TeamMember -> Maybe TeamMember
        findMember email members =
            members
                |> List.filter (\m -> m.user.email == email)
                |> List.head

        updateRowState : Row -> TeamMember -> Row
        updateRowState row member =
            case row.state of
                PendingChange ->
                    Row member Updated

                _ ->
                    row

        rec : List TeamMember -> List Row -> List Row
        rec members rows =
            case rows of
                [] ->
                    []

                [ row ] ->
                    case findMember row.member.user.email members of
                        Just member ->
                            [ updateRowState row member ]

                        Nothing ->
                            []

                row :: tail ->
                    case findMember row.member.user.email members of
                        Just member ->
                            updateRowState row member :: rec members tail

                        Nothing ->
                            rec members tail
    in
    rec (expandableAll expandableMembers) allRows
