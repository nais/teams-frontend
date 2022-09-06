module Queries.UserQueries exposing (..)

import Backend.Object
import Backend.Object.User as User
import Backend.Query as Query
import Backend.Scalar exposing (Uuid)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)



---- MODEL ----


type alias UserData =
    { id : Uuid
    , email : String
    , name : String
    }


getMeQuery : SelectionSet UserData RootQuery
getMeQuery =
    Query.me userDataSelection

getUserQuery : String -> SelectionSet UserData RootQuery
getUserQuery email =
    Query.userByEmail {email=email} userDataSelection


userDataSelection : SelectionSet UserData Backend.Object.User
userDataSelection =
    Graphql.SelectionSet.map3 UserData
        User.id
        User.email
        User.name
