module Queries.UserQueries exposing (..)

import Backend.Object
import Backend.Object.User as User
import Backend.Query as Query
import Backend.Scalar exposing (Uuid)
import Backend.Union
import Backend.Union.AuthenticatedUser as AuthenticatedUser
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)



---- MODEL ----


type alias UserData =
    { id : Uuid
    , email : String
    , name : String
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
    SelectionSet.map3 UserData
        User.id
        User.email
        User.name


meSelection : SelectionSet (Maybe UserData) Backend.Union.AuthenticatedUser
meSelection =
    AuthenticatedUser.fragments
        { onUser = SelectionSet.map Just userDataSelection
        , onServiceAccount = SelectionSet.map (\_ -> Nothing) SelectionSet.empty
        }
