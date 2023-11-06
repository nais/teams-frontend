module Page.Team.Members exposing (AddMember, Modal, Mode(..), Model, Msg(..), Row, RowState, init, update, view)

import Api.Do exposing (mutateRD)
import Api.Error exposing (errorToString)
import Api.Str exposing (roleStr)
import Api.Team exposing (addTeamMember, removeMemberFromTeam, setTeamMemberRole)
import Backend.Enum.TeamRole as TeamRole exposing (TeamRole(..))
import Backend.Scalar exposing (ReconcilerName)
import Component.Buttons exposing (smallButton, smallButtonWithAttrs)
import Component.Card as Card
import Component.Icons exposing (spinnerDone, spinnerError, spinnerLoading)
import Component.Modal as Modal
import DataModel exposing (Expandable(..), Reconciler, Team, TeamMember(..), TeamMemberReconciler(..), User, expandableAll, expandableTake, flipExpanded, tmRole, tmUser)
import Graphql.Http
import Html exposing (Html, button, datalist, div, form, h3, input, label, li, option, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, classList, colspan, disabled, for, id, list, selected, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import RemoteData exposing (RemoteData(..))
import Util exposing (appendMaybe, conditionalElement, flattenMaybe)


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
    , reconcilers : List Reconciler
    }


type alias AddMember =
    { email : String
    , role : TeamRole
    , reconcilerOptOuts : List ReconcilerName
    }


type Modal
    = AddNewMember


init : Team -> List User -> Bool -> List Reconciler -> Model
init team allUsers isEditor reconcilers =
    { team = team
    , allUsers = allUsers
    , isEditor = isEditor
    , mode = View
    , addMember = initialAddMember
    , members = toRows team.members
    , reconcilers = reconcilers
    }


initialAddMember : AddMember
initialAddMember =
    { email = "", role = TeamRole.Member, reconcilerOptOuts = [] }


modalId : Modal -> String
modalId modal =
    case modal of
        AddNewMember ->
            "modalAddMember"


mapAddMember : (AddMember -> AddMember) -> Model -> Model
mapAddMember fn model =
    { model | addMember = fn model.addMember }


setRole : TeamRole -> AddMember -> AddMember
setRole role addMember =
    { addMember | role = role }


setEmail : String -> AddMember -> AddMember
setEmail email addMember =
    { addMember | email = email }


addReconcilerOptOut : ReconcilerName -> AddMember -> AddMember
addReconcilerOptOut reconcilerName addMember =
    { addMember | reconcilerOptOuts = reconcilerName :: addMember.reconcilerOptOuts }


removeReconcilerOptOut : ReconcilerName -> AddMember -> AddMember
removeReconcilerOptOut reconcilerName addMember =
    { addMember | reconcilerOptOuts = List.filter (\r -> r /= reconcilerName) addMember.reconcilerOptOuts }


mapMembers : (List Row -> List Row) -> Model -> Model
mapMembers fn model =
    { model | members = fn model.members }


mapTeam : (Team -> Team) -> Model -> Model
mapTeam fn model =
    { model | team = fn model.team }


type Msg
    = ClickedEditMode
    | ClickedViewMode
    | ClickedNewMemberAdd
    | ClickedNewMemberRole String
    | ClickedMemberRole Row String
    | ClickedMemberRemove Row
    | ClickedMemberRemoveConfirm Row
    | ClickedMemberToggleReconciler Row Reconciler Bool
    | ClickedShowMore
    | ClickedToggleReconciler ReconcilerName Bool
    | GotSaveTeamResponse Row (RemoteData (Graphql.Http.Error Team) Team)
    | GotSaveTeamMemberResponse Row (RemoteData (Graphql.Http.Error TeamMember) TeamMember)
    | InputChangedNewMember String
    | CloseModal Modal
    | ShowModal Modal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedEditMode ->
            ( { model | mode = Edit }, Cmd.none )

        ClickedViewMode ->
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
                        member : TeamMember
                        -- TODO add reconcilers
                        member =
                            TeamMember user model.team model.addMember.role []

                        optOuts : Maybe (List ReconcilerName)
                        optOuts =
                            if List.isEmpty model.addMember.reconcilerOptOuts then
                                Nothing

                            else
                                Just model.addMember.reconcilerOptOuts
                    in
                    ( model
                        |> mapMembers (addRow (Row member PendingChange))
                        |> mapAddMember (\_ -> initialAddMember)
                    , Cmd.batch
                        [ Modal.close (modalId AddNewMember)
                        , mutateRD (addTeamMember model.team user model.addMember.role optOuts) (GotSaveTeamResponse (Row member PendingChange))
                        ]
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
            ( model |> mapMembers (mapRow (Row row.member PendingChange)), mutateRD (setTeamMemberRole model.team row.member r) (GotSaveTeamResponse row) )

        ClickedMemberRemove row ->
            ( model |> mapMembers (mapRow (Row row.member PendingRemove)), Cmd.none )

        ClickedMemberRemoveConfirm row ->
            ( model |> mapMembers (mapRow (Row row.member PendingChange)), mutateRD (removeMemberFromTeam model.team (tmUser row.member)) (GotSaveTeamResponse row) )

        ClickedShowMore ->
            ( model |> mapTeam (\t -> { t | members = flipExpanded t.members }), Cmd.none )

        GotSaveTeamMemberResponse row resp ->
            case resp of
                Success member ->
                    ( model |> mapMembers (mapRow (Row member Updated)), Cmd.none )

                Failure err ->
                    ( model |> mapMembers (updateRowsError row (errorToString err)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotSaveTeamResponse row resp ->
            case resp of
                Success team ->
                    ( { model | team = team } |> mapMembers (updateRows team.members), Cmd.none )

                Failure err ->
                    ( model |> mapMembers (updateRowsError row (errorToString err)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InputChangedNewMember email ->
            ( model |> mapAddMember (setEmail email), Cmd.none )

        CloseModal modal ->
            ( model, Modal.close (modalId modal) )

        ShowModal modal ->
            ( model, Modal.open (modalId modal) )

        ClickedToggleReconciler reconcilerName checked ->
            if checked then
                ( model |> mapAddMember (removeReconcilerOptOut reconcilerName), Cmd.none )

            else
                ( model |> mapAddMember (addReconcilerOptOut reconcilerName), Cmd.none )

        ClickedMemberToggleReconciler row reconciler checked ->
            if checked then
                ( model, mutateRD (Api.Team.removeReconcilerOptOut row.member reconciler) (GotSaveTeamMemberResponse row) )

            else
                ( model, mutateRD (Api.Team.addReconcilerOptOut row.member reconciler) (GotSaveTeamMemberResponse row) )


view : Model -> Html Msg
view model =
    case model.mode of
        Edit ->
            viewEditMembers model

        View ->
            viewMembers model


viewMembers : Model -> Html Msg
viewMembers model =
    Card.new "Members"
        |> Card.withButtons
            ([ smallButtonWithAttrs ClickedEditMode "edit" "Edit" [ disabled model.team.deletionInProgress ]
                |> conditionalElement model.isEditor
             ]
                |> flattenMaybe
            )
        |> Card.withContents
            ([ table [ class "first-column-wide" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Email" ]
                        , th [] [ text "Role" ]
                        ]
                    ]
                , tbody []
                    (if List.length (expandableTake 10 model.team.members) == 0 then
                        [ tr [] [ td [ colspan 2 ] [ text "This team has no members" ] ] ]

                     else
                        List.map viewRow (expandableTake 10 model.team.members)
                    )
                ]
             ]
                |> appendMaybe (showMoreButton model.team.members 10 ClickedShowMore)
            )
        |> Card.render


viewAddMemberModal : Model -> List (Html Msg)
viewAddMemberModal model =
    [ smallButton (ShowModal AddNewMember) "add" "Add member"
    , Modal.view (modalId AddNewMember)
        [ Card.new "Add member"
            |> Card.withContents
                [ form [ onSubmit ClickedNewMemberAdd ]
                    [ ul []
                        (List.concat
                            [ [ label [ for "addUserEmail" ] [ text "Email:" ]
                              , li []
                                    [ input
                                        [ list "userCandidates"
                                        , type_ "text"
                                        , value model.addMember.email
                                        , onInput InputChangedNewMember
                                        ]
                                        []
                                    , datalist [ id "userCandidates" ] (candidates model)
                                    ]
                              , label [ for "addUserRole" ] [ text "Role:" ]
                              , li []
                                    [ viewRoleSelector "addUserRole" model.addMember.role ClickedNewMemberRole False
                                    ]
                              , li [] [ h3 [] [ text "Reconcilers" ] ]
                              ]
                            , List.filterMap (viewReconcilerOption model) model.reconcilers
                            , [ li [ class "row" ]
                                    [ button [ type_ "reset", onClick (CloseModal AddNewMember), class "transparent" ] [ text "Cancel" ]
                                    , button [ type_ "submit", disabled (queryUserList model.addMember.email model.allUsers == Nothing) ]
                                        [ text "Add"
                                        ]
                                    ]
                              ]
                            ]
                        )
                    ]
                ]
            |> Card.render
        ]
    ]


viewReconcilerOption : Model -> Reconciler -> Maybe (Html Msg)
viewReconcilerOption model r =
    if r.enabled && r.usesTeamMemberships then
        let
            elementId : String
            elementId =
                Api.Str.reconcilerNameStr r.name

            optedOut : Bool
            optedOut =
                List.member r.name model.addMember.reconcilerOptOuts
        in
        Just
            (li [ class "checkbox" ]
                [ label [ for elementId ] [ text r.displayName ]
                , input [ type_ "checkbox", checked (not optedOut), id elementId, onCheck (ClickedToggleReconciler r.name) ] []
                ]
            )

    else
        Nothing


viewEditMembers : Model -> Html Msg
viewEditMembers model =
    let
        hasLessThanTwoOwners = countOwners model <= 1
    in
    Card.new "Members"
        |> Card.withButtons (viewAddMemberModal model ++ [ smallButton ClickedViewMode "edit" "View" ])
        |> Card.withContents
            [ table [ class "first-column-wide" ]
                [ thead []
                    [ tr []
                        (List.concat
                            [ [ th [] [ text "Email" ]
                              , th [] []
                              ]
                            , List.filterMap
                                (\r ->
                                    if r.usesTeamMemberships && r.enabled then
                                        Just (th [] [ text r.displayName ])

                                    else
                                        Nothing
                                )
                                model.reconcilers
                            , [ th [] [ text "Role" ]
                              , th [] [ text "" ]
                              ]
                            ]
                        )
                    ]
                , tbody [] (List.map (viewEditRow hasLessThanTwoOwners model.reconcilers) model.members) 
                ]
            ]
        |> Card.render


candidates : Model -> List (Html Msg)
candidates model =
    let
        memberEmails : List String
        memberEmails =
            model.team.members
                |> expandableAll
                |> List.map (\m -> (tmUser m).email)
    in
    model.allUsers
        |> List.map (\u -> u.email)
        |> List.filter (\email -> not (List.member email memberEmails))
        |> List.map (\email -> option [] [ text email ])


viewEditRow : Bool -> List Reconciler -> Row -> Html Msg
viewEditRow hasLessThan2Owners reconcilers row =
    let
        roleSelector : Html Msg
        roleSelector =
            viewRoleSelector "" (tmRole row.member) (ClickedMemberRole row) (row.state == PendingChange)

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
                    button [ class "small", disabled True ] [ div [ class "icon delete" ] [], text "Remove" ]

                PendingRemove ->
                    smallButton (ClickedMemberRemoveConfirm row) "delete" "Confirm"

                Unchanged ->
                    smallButtonWithAttrs (ClickedMemberRemove row) "delete" "Remove" [disabled hasLessThan2Owners]

                Updated ->
                    smallButton (ClickedMemberRemove row) "delete" "Remove"

                Error _ ->
                    smallButton (ClickedMemberRemove row) "delete" "Remove"
    in
    tr []
        (List.concat
            [ [ td [] [ text (tmUser row.member).email ]
              , td [] [ phase ]
              ]
            , viewToggleReconciler reconcilers row
            , [ td [] [ roleSelector ]
              , td [] [ btn ]
              ]
            ]
        )


viewToggleReconciler : List Reconciler -> Row -> List (Html Msg)
viewToggleReconciler globalReconcilers row =
    let
        (TeamMember _ _ _ memberReconcilers) =
            row.member
    in
    List.concat
        (globalReconcilers
            |> List.filterMap
                (\globalReconciler ->
                    if not globalReconciler.enabled || not globalReconciler.usesTeamMemberships then
                        Nothing

                    else
                        Just
                            (memberReconcilers
                                |> List.filterMap
                                    (\(TeamMemberReconciler optedIn memberReconciler) ->
                                        if memberReconciler.name == globalReconciler.name then
                                            Just (td [] [ input [ type_ "checkbox", checked optedIn, onCheck (ClickedMemberToggleReconciler row globalReconciler) ] [] ])

                                        else
                                            Nothing
                                    )
                            )
                )
        )


viewRoleSelector : String -> TeamRole -> (String -> Msg) -> Bool -> Html Msg
viewRoleSelector inputId currentRole action disable =
    select
        [ id inputId
        , value (roleStr currentRole)
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
        [ td [] [ text (tmUser member).email ]
        , td [ classList [ ( "team-owner", tmRole member == Owner ) ] ] [ text <| roleStr (tmRole member) ]
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
    if (new.member |> tmUser).email == (existing.member |> tmUser).email then
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
    (r1.member |> tmUser).email < (r2.member |> tmUser).email


updateRowsError : Row -> String -> List Row -> List Row
updateRowsError row error allRows =
    mapRow (Row row.member (Error error)) allRows


updateRows : Expandable (List TeamMember) -> List Row -> List Row
updateRows expandableMembers allRows =
    let
        findMember : String -> List TeamMember -> Maybe TeamMember
        findMember email members =
            members
                |> List.filter (\m -> (tmUser m).email == email)
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
                    case findMember (tmUser row.member).email members of
                        Just member ->
                            [ updateRowState row member ]

                        Nothing ->
                            []

                row :: tail ->
                    case findMember (tmUser row.member).email members of
                        Just member ->
                            updateRowState row member :: rec members tail

                        Nothing ->
                            rec members tail
    in
    rec (expandableAll expandableMembers) allRows

countOwners : Model -> Int
countOwners model = List.filter isOwner model.members |> List.length

isOwner : Row -> Bool
isOwner row = case row.member of 
    TeamMember _ _ Owner _  -> True
    _ -> False