module Api.User exposing (..)

import Backend.Object
import Backend.Object.Role as Role
import Backend.Object.User as User
import Backend.Query as Query
import Backend.Scalar exposing (RoleName, Slug, Uuid)
import Backend.Union
import Backend.Union.AuthenticatedUser as AuthenticatedUser
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)



---- MODEL ----


type alias RoleData =
    { name : RoleName
    , isGlobal : Bool
    , targetTeamSlug : Maybe Slug
    }


type alias UserData =
    { id : Uuid
    , email : String
    , name : String
    , roles : List RoleData
    }


getMe : SelectionSet (Maybe UserData) RootQuery
getMe =
    Query.me meSelection


getAllUsers : SelectionSet (List UserData) RootQuery
getAllUsers =
    Query.users userDataSelection


userDataWithRoleSelection : SelectionSet UserData Backend.Object.User
userDataWithRoleSelection =
    SelectionSet.map4 UserData
        User.id
        User.email
        User.name
        (User.roles roleDataSelection)


userDataSelection : SelectionSet UserData Backend.Object.User
userDataSelection =
    SelectionSet.succeed UserData
        |> SelectionSet.with User.id
        |> SelectionSet.with User.email
        |> SelectionSet.with User.name
        |> SelectionSet.hardcoded []


roleDataSelection : SelectionSet RoleData Backend.Object.Role
roleDataSelection =
    SelectionSet.map3 RoleData
        Role.name
        Role.isGlobal
        Role.targetTeamSlug


meSelection : SelectionSet (Maybe UserData) Backend.Union.AuthenticatedUser
meSelection =
    AuthenticatedUser.fragments
        { onUser = SelectionSet.map Just userDataWithRoleSelection
        , onServiceAccount = SelectionSet.map (\_ -> Nothing) SelectionSet.empty
        }
