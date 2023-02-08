module Api.User exposing (..)

import Backend.Object exposing (TeamMembership(..))
import Backend.Object.Role as Role
import Backend.Object.Team
import Backend.Object.TeamMembership as TeamMembership
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


type alias TeamSlugData =
    { slug : Slug }


type alias TeamMembershipData =
    { team : TeamSlugData }


type alias UserData =
    { id : Uuid
    , email : String
    , name : String
    , externalId : String
    , teamMemberships : List TeamMembershipData
    , roles : List RoleData
    }


getMe : SelectionSet (Maybe UserData) RootQuery
getMe =
    Query.me meSelection


getAllUsers : SelectionSet (List UserData) RootQuery
getAllUsers =
    Query.users userDataSelection


getAllUsersWithRoles : SelectionSet (List UserData) RootQuery
getAllUsersWithRoles =
    Query.users userDataWithRoleSelection


userDataWithRoleSelection : SelectionSet UserData Backend.Object.User
userDataWithRoleSelection =
    SelectionSet.succeed UserData
        |> SelectionSet.with User.id
        |> SelectionSet.with User.email
        |> SelectionSet.with User.name
        |> SelectionSet.with User.externalId
        |> SelectionSet.with (User.teams teamMembershipSelection)
        |> SelectionSet.with (User.roles roleDataSelection)


userDataSelection : SelectionSet UserData Backend.Object.User
userDataSelection =
    SelectionSet.succeed UserData
        |> SelectionSet.with User.id
        |> SelectionSet.with User.email
        |> SelectionSet.with User.name
        |> SelectionSet.with User.externalId
        |> SelectionSet.hardcoded []
        |> SelectionSet.hardcoded []


roleDataSelection : SelectionSet RoleData Backend.Object.Role
roleDataSelection =
    SelectionSet.map3 RoleData
        Role.name
        Role.isGlobal
        Role.targetTeamSlug


teamMembershipSelection : SelectionSet TeamMembershipData Backend.Object.TeamMembership
teamMembershipSelection =
    SelectionSet.succeed TeamMembershipData
        |> SelectionSet.with (TeamMembership.team teamDataSelection)


teamDataSelection : SelectionSet TeamSlugData Backend.Object.Team
teamDataSelection =
    SelectionSet.succeed TeamSlugData
        |> SelectionSet.with Backend.Object.Team.slug


meSelection : SelectionSet (Maybe UserData) Backend.Union.AuthenticatedUser
meSelection =
    AuthenticatedUser.fragments
        { onUser = SelectionSet.map Just userDataWithRoleSelection
        , onServiceAccount = SelectionSet.map (\_ -> Nothing) SelectionSet.empty
        }
