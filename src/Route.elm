module Route exposing (Route(..), ViewMode(..), fromUrl, link, routeToString)

import Backend.Scalar exposing (Slug(..), Uuid(..))
import Html exposing (Html, a)
import Html.Attributes exposing (href)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Admin
    | CreateTeam
    | DeleteTeam Slug
    | DeleteTeamConfirm Uuid
    | Home
    | Team Slug
    | Teams ViewMode
    | Users


type ViewMode
    = MyTeams
    | AllTeams


parseViewMode : Maybe String -> Route
parseViewMode s =
    case s of
        Just selection ->
            case selection of
                "my" ->
                    Teams MyTeams

                "all" ->
                    Teams AllTeams

                _ ->
                    Teams AllTeams

        Nothing ->
            Teams AllTeams


viewModeToQueryString : ViewMode -> String
viewModeToQueryString m =
    if m == MyTeams then
        "selection=my"

    else
        "selection=all"


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Admin (s "admin" </> s "synchronization")
        , Parser.map CreateTeam (s "teams" </> s "create")
        , Parser.map (\s -> DeleteTeam (Slug s)) (s "teams" </> s "delete" </> string)
        , Parser.map (\s -> DeleteTeamConfirm (Uuid s)) (s "teams" </> s "delete" </> s "confirm" </> string)
        , Parser.map Home Parser.top
        , Parser.map parseViewMode (s "teams" <?> Query.string "selection")
        , Parser.map (\s -> Team (Slug s)) (s "teams" </> string)
        , Parser.map Users (s "users")
        ]



-- PUBLIC HELPERS


link : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link targetRoute attr body =
    a (href (routeToString targetRoute) :: attr) body


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


routeToString : Route -> String
routeToString page =
    case page of
        Admin ->
            "/admin/synchronization"

        CreateTeam ->
            "/teams/create"

        DeleteTeam (Slug id) ->
            "/teams/delete/" ++ id

        DeleteTeamConfirm (Uuid key) ->
            "/teams/delete/confirm/" ++ key

        Home ->
            "/"

        Team (Slug id) ->
            "/teams/" ++ id

        Teams m ->
            "/teams?" ++ viewModeToQueryString m

        Users ->
            "/users"
