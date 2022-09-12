module Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Html, a)
import Html.Attributes exposing (href)
import String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Backend.Scalar exposing (Uuid(..))


type Route
    = Home
    | Teams
    | CreateTeam
    | Team Backend.Scalar.Uuid


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map CreateTeam (s "teams" </> s "create")
        , Parser.map Teams (s "teams")
        , Parser.map (\s -> Team (Uuid s)) (s "teams" </> string)
        ]

-- PUBLIC HELPERS


link : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link targetRoute attr body =
    a (href (routeToString targetRoute) :: attr) body


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


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

                CreateTeam ->
                    [ "teams", "create" ]

                Teams ->
                    [ "teams" ]

                Team (Uuid id) ->
                    [ "teams", id ]
    in
    "/" ++ String.join "/" pieces
