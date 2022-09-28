module Queries.UserQueries exposing (..)

import Backend.Object
import Backend.Object.Role as Role
import Backend.Object.User as User
import Backend.Query as Query
import Backend.Scalar exposing (Uuid)
import Backend.Union
import Backend.Union.AuthenticatedUser as AuthenticatedUser
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Backend.Scalar exposing (RoleName)



---- MODEL ----


type alias RoleData =
    { name : RoleName
    , isGlobal : Bool
    , targetId : Maybe Uuid
    }


type alias UserData =
    { id : Uuid
    , email : String
    , name : String
    , roles : List RoleData
    }


getMeQuery : SelectionSet (Maybe UserData) RootQuery
getMeQuery =
    Query.me meSelection


getUserQuery : String -> SelectionSet UserData RootQuery
getUserQuery email =
    Query.userByEmail { email = email } userDataSelection


getAllUsers : SelectionSet (List UserData) RootQuery
getAllUsers =
    Query.users userDataSelection


userDataSelection : SelectionSet UserData Backend.Object.User
userDataSelection =
    SelectionSet.map4 UserData
        User.id
        User.email
        User.name
        (User.roles roleDataSelection)


roleDataSelection : SelectionSet RoleData Backend.Object.Role
roleDataSelection =
    SelectionSet.map3 RoleData
        Role.name
        Role.isGlobal
        Role.targetId


meSelection : SelectionSet (Maybe UserData) Backend.Union.AuthenticatedUser
meSelection =
    AuthenticatedUser.fragments
        { onUser = SelectionSet.map Just userDataSelection
        , onServiceAccount = SelectionSet.map (\_ -> Nothing) SelectionSet.empty
        }
