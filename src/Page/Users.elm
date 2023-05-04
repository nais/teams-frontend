module Page.Users exposing (Model, Msg(..), init, update, view)

import Api.Do exposing (mutateRD)
import Api.Error
import Api.Str exposing (uuidStr)
import Api.User
import Backend.Mutation as Mutation
import Backend.Scalar exposing (RoleName(..), Slug(..), Uuid)
import Component.Buttons exposing (smallButton)
import DataModel exposing (Role, User)
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, h2, input, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, type_, value)
import Page.Team exposing (copy)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)


type alias Model =
    { session : Session
    , error : Maybe String
    , users : RemoteData (Graphql.Http.Error (List User)) (List User)
    , synchronizeUsersCorrelationID : RemoteData (Graphql.Http.Error Uuid) Uuid
    }


type Msg
    = GotUsers (RemoteData (Graphql.Http.Error (List User)) (List User))
    | Copy String
    | GotSynchronizeUsersResponse (RemoteData (Graphql.Http.Error Uuid) Uuid)
    | SynchronizeUsersClicked


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , error = Nothing
      , users = Loading
      , synchronizeUsersCorrelationID = NotAsked
      }
    , getUsers
    )


getUsers : Cmd Msg
getUsers =
    Api.Do.queryRD Api.User.getAllUsersWithRoles GotUsers


view : Model -> Html Msg
view model =
    case model.users of
        Loading ->
            p [] [ text "loading" ]

        Success users ->
            div [ class "cards" ]
                [ viewAdminActions model.synchronizeUsersCorrelationID
                , div [ class "card" ]
                    [ div [ class "title" ]
                        [ h2 [] [ text "Users" ]
                        ]
                    , table []
                        [ thead []
                            [ tr []
                                [ th [] [ text "Full name" ]
                                , th [] [ text "Email" ]
                                , th [] [ text "External ID" ]
                                , th [] [ text "Roles" ]
                                ]
                            ]
                        , tbody [] (List.map viewUser users)
                        ]
                    ]
                ]

        NotAsked ->
            p [] [ text "NotAsked" ]

        Failure f ->
            p [] [ text (f |> Api.Error.errorToString) ]


viewAdminActions : RemoteData (Graphql.Http.Error Uuid) Uuid -> Html Msg
viewAdminActions synchronizeUsersCorrelationID =
    div [ class "card" ]
        [ div [ class "title" ]
            [ h2 [] [ text "Admin actions" ]
            , smallButton SynchronizeUsersClicked "synchronize" "Synchronize users"
            ]
        , div []
            (case synchronizeUsersCorrelationID of
                Success uuid ->
                    [ text "User sync triggered. Correlation ID: "
                    , input [ type_ "text", class "synchronizeUsersCorrelationID", disabled True, value (uuidStr uuid) ] []
                    , smallButton (Copy (uuidStr uuid)) "copy" "copy"
                    ]

                _ ->
                    [ text "" ]
            )
        ]


viewUser : User -> Html Msg
viewUser user =
    tr []
        [ td [] [ text user.name ]
        , td [] [ text user.email ]
        , td [] [ text user.externalId ]
        , td [] [ viewRoleDatas user.roles ]
        ]


roleNameToString : RoleName -> String
roleNameToString (RoleName s) =
    s


slugToString : Slug -> String
slugToString (Slug s) =
    s


viewRoleData : Role -> Html Msg
viewRoleData r =
    tr []
        [ td [] [ text (roleNameToString r.name) ]
        , td []
            [ text
                (if r.isGlobal then
                    "global"

                 else
                    case r.targetTeamSlug of
                        Just slug ->
                            slugToString slug

                        Nothing ->
                            "no target slug - bug?"
                )
            ]
        ]


viewRoleDatas : List Role -> Html Msg
viewRoleDatas roleDatas =
    table [] [ tbody [] (List.map viewRoleData roleDatas) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsers r ->
            ( { model | users = r }, Cmd.none )

        GotSynchronizeUsersResponse r ->
            ( { model | synchronizeUsersCorrelationID = r }, Cmd.none )

        SynchronizeUsersClicked ->
            ( model, mutateRD synchronizeUsers GotSynchronizeUsersResponse )

        Copy s ->
            ( model, copy s )


synchronizeUsers : SelectionSet Uuid RootMutation
synchronizeUsers =
    Mutation.synchronizeUsers
