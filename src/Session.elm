module Session exposing (Session, Viewer(..), init, isGlobalAdmin, mapViewer, navKey, username, viewer)

import Backend.Scalar exposing (RoleName(..))
import Browser.Navigation as Nav
import DataModel exposing (User)


type Viewer
    = LoggedIn User
    | Anonymous
    | Unknown


type Session
    = Session Nav.Key Viewer


init : Nav.Key -> Session
init nk =
    Session nk Unknown


mapViewer : Viewer -> Session -> Session
mapViewer v (Session nk _) =
    Session nk v


navKey : Session -> Nav.Key
navKey (Session nk _) =
    nk


viewer : Session -> Viewer
viewer (Session _ v) =
    v


username : Viewer -> String
username v =
    case v of
        LoggedIn user ->
            user.email

        _ ->
            "Not logged in"


isGlobalAdmin : Viewer -> Bool
isGlobalAdmin v =
    case v of
        LoggedIn u ->
            List.any (\r -> r.isGlobal && r.name == RoleName "Admin") u.roles

        Unknown ->
            False

        Anonymous ->
            False
