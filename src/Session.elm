module Session exposing (Session, Viewer(..), init, isGlobalAdmin, mapViewer, navKey, url, user, viewer)

import Backend.Scalar exposing (RoleName(..))
import Browser.Navigation as Nav
import DataModel exposing (User)
import Url


type Viewer
    = LoggedIn User
    | Anonymous


type Session
    = Session Nav.Key Url.Url Viewer


init : Nav.Key -> Url.Url -> Session
init nk u =
    Session nk u Anonymous


mapViewer : Viewer -> Session -> Session
mapViewer v (Session nk u _) =
    Session nk u v


navKey : Session -> Nav.Key
navKey (Session nk _ _) =
    nk


viewer : Session -> Viewer
viewer (Session _ _ v) =
    v


url : Session -> Url.Url
url (Session _ u _) =
    u


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
