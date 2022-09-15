module Session exposing (Session, User(..), init, mapUser, navKey, user)

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
