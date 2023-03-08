module Route exposing (Route(..), fromUrl, link, routeToString)

import Backend.Scalar exposing (Slug(..), Uuid(..))
import Html exposing (Html, a)
import Html.Attributes exposing (href)
import String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | ReconcilerAdmin
    | MyTeams
    | AllTeams
    | CreateTeam
    | Users
    | Team Slug
    | DeleteTeam Slug
    | DeleteTeamConfirm Uuid


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map ReconcilerAdmin (s "admin" </> s "synchronization")
        , Parser.map AllTeams (s "teams")
        , Parser.map MyTeams (s "teams" </> s "my")
        , Parser.map CreateTeam (s "teams" </> s "create")
        , Parser.map Users (s "users")
        , Parser.map (\s -> Team (Slug s)) (s "teams" </> string)
        , Parser.map (\s -> DeleteTeam (Slug s)) (s "teams" </> s "delete" </> string)
        , Parser.map (\s -> DeleteTeamConfirm (Uuid s)) (s "teams" </> s "delete" </> s "confirm" </> string)
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
    let
        pieces : List String
        pieces =
            case page of
                Home ->
                    [ "" ]

                ReconcilerAdmin ->
                    [ "admin", "synchronization" ]

                AllTeams ->
                    [ "teams" ]

                MyTeams ->
                    [ "teams", "my" ]

                CreateTeam ->
                    [ "teams", "create" ]

                Users ->
                    [ "users" ]

                Team (Slug id) ->
                    [ "teams", id ]

                DeleteTeam (Slug id) ->
                    [ "teams", "delete", id ]

                DeleteTeamConfirm (Uuid key) ->
                    [ "teams", "delete", "confirm", key ]
    in
    "/" ++ String.join "/" pieces
