module Route exposing (Route(..), fromUrl, link, routeToString)

import Backend.Scalar exposing (Slug(..))
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
    | Team Backend.Scalar.Slug


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
    in
    "/" ++ String.join "/" pieces
