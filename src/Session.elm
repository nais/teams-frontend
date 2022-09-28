module Session exposing (Session, User(..), init, isGlobalAdmin, mapUser, navKey, user, username)

import Backend.Scalar exposing (RoleName(..))
import Browser.Navigation as Nav
import Queries.UserQueries exposing (UserData)


type User
    = LoggedIn UserData
    | Anonymous
    | Unknown


type Session
    = Session Nav.Key User


init : Nav.Key -> Session
init nk =
    Session nk Unknown


mapUser : User -> Session -> Session
mapUser u (Session nk _) =
    Session nk u


navKey : Session -> Nav.Key
navKey (Session nk _) =
    nk


user : Session -> User
user (Session _ u) =
    u


username : User -> String
username u =
    case u of
        LoggedIn loggedIn ->
            loggedIn.email

        _ ->
            "Not logged in"


isGlobalAdmin : User -> Bool
isGlobalAdmin u =
    case u of
        LoggedIn lu ->
            List.any (\r -> r.isGlobal && r.name == RoleName "Admin") lu.roles

        Unknown ->
            False

        Anonymous ->
            False
