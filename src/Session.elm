module Session exposing (Session, Viewer(..), init, isGlobalAdmin, mapViewer, navKey, user, viewer)

import Backend.Scalar exposing (RoleName(..))
import Browser.Navigation as Nav
import DataModel exposing (User)


type Viewer
    = LoggedIn User
    | Anonymous


type Session
    = Session Nav.Key Viewer


init : Nav.Key -> Session
init nk =
    Session nk Anonymous


mapViewer : Viewer -> Session -> Session
mapViewer v (Session nk _) =
    Session nk v


navKey : Session -> Nav.Key
navKey (Session nk _) =
    nk


viewer : Session -> Viewer
viewer (Session _ v) =
    v


user : Viewer -> Maybe User
user v =
    case v of
        LoggedIn u ->
            Just u

        Anonymous ->
            Nothing


isGlobalAdmin : Viewer -> Bool
isGlobalAdmin v =
    case v of
        LoggedIn u ->
            List.any (\r -> r.isGlobal && r.name == RoleName "Admin") u.roles

        Anonymous ->
            False
