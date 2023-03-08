module Api.User exposing (getAllUsers, getAllUsersWithRoles, getMe, userSelection)

import Backend.Object
import Backend.Object.Role as BORole
import Backend.Object.Team as BOTeam
import Backend.Object.TeamMembership as BOTeamMembership
import Backend.Object.User as BOUser
import Backend.Query as Query
import Backend.Union
import Backend.Union.AuthenticatedUser
import DataModel exposing (Role, TeamMembership, TeamSlug, User)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)



---- MODEL ----


getMe : SelectionSet (Maybe User) RootQuery
getMe =
    Query.me meSelection


getAllUsers : SelectionSet (List User) RootQuery
getAllUsers =
    Query.users userSelection


getAllUsersWithRoles : SelectionSet (List User) RootQuery
getAllUsersWithRoles =
    Query.users userWithRoleSelection


userWithRoleSelection : SelectionSet User Backend.Object.User
userWithRoleSelection =
    SelectionSet.succeed User
        |> SelectionSet.with BOUser.id
        |> SelectionSet.with BOUser.email
        |> SelectionSet.with BOUser.name
        |> SelectionSet.with BOUser.externalId
        |> SelectionSet.with (BOUser.teams teamMembershipSelection)
        |> SelectionSet.with (BOUser.roles roleSelection)


userSelection : SelectionSet User Backend.Object.User
userSelection =
    SelectionSet.succeed User
        |> SelectionSet.with BOUser.id
        |> SelectionSet.with BOUser.email
        |> SelectionSet.with BOUser.name
        |> SelectionSet.with BOUser.externalId
        |> SelectionSet.hardcoded []
        |> SelectionSet.hardcoded []


roleSelection : SelectionSet Role Backend.Object.Role
roleSelection =
    SelectionSet.map3 Role
        BORole.name
        BORole.isGlobal
        BORole.targetTeamSlug


teamMembershipSelection : SelectionSet TeamMembership Backend.Object.TeamMembership
teamMembershipSelection =
    SelectionSet.succeed TeamMembership
        |> SelectionSet.with (BOTeamMembership.team teamSelection)


teamSelection : SelectionSet TeamSlug Backend.Object.Team
teamSelection =
    SelectionSet.succeed TeamSlug
        |> SelectionSet.with BOTeam.slug


meSelection : SelectionSet (Maybe User) Backend.Union.AuthenticatedUser
meSelection =
    Backend.Union.AuthenticatedUser.fragments
        { onUser = SelectionSet.map Just userWithRoleSelection
        , onServiceAccount = SelectionSet.map (\_ -> Nothing) SelectionSet.empty
        }
