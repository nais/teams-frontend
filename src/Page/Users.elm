module Page.Users exposing (..)

import Api.Do
import Api.Error
import Api.User
import Backend.Scalar exposing (RoleName(..), Slug(..))
import DataModel exposing (..)
import Graphql.Http
import Html exposing (Html, p, table, tbody, td, text, th, thead, tr)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)


type alias Model =
    { session : Session
    , error : Maybe String
    , users : RemoteData (Graphql.Http.Error (List User)) (List User)
    }


type Msg
    = NoOp
    | GotUsers (RemoteData (Graphql.Http.Error (List User)) (List User))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , error = Nothing
      , users = Loading
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
            table []
                [ thead []
                    [ tr []
                        [ th [] [ text "name" ]
                        , th [] [ text "email" ]
                        , th [] [ text "external id" ]
                        , th [] [ text "more" ]
                        ]
                    ]
                , tbody [] (List.map viewUser users)
                ]

        NotAsked ->
            p [] [ text "NotAsked" ]

        Failure f ->
            p [] [ text (f |> Api.Error.errorToString) ]


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
        NoOp ->
            ( model, Cmd.none )

        GotUsers r ->
            ( { model | users = r }, Cmd.none )
