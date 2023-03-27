module Component.Navigation exposing (view)

import Api.Str exposing (slugStr)
import Html exposing (Html, li, nav, text, ul)
import Html.Attributes exposing (classList)
import Route exposing (Route(..), link)
import Session exposing (Session, Viewer(..))


view : Route -> Session -> Html msg
view route session =
    case Session.viewer session of
        LoggedIn _ ->
            let
                teamsButton : List (Html msg)
                teamsButton =
                    [ menuItem route Route.MyTeams False "Teams" ]

                adminButtons : List (Html msg)
                adminButtons =
                    if Session.isGlobalAdmin (Session.viewer session) then
                        [ menuItem route Route.ReconcilerAdmin False "Synchronizers"
                        , menuItem route Route.Users False "Users"
                        ]

                    else
                        []

                ephemeralButtons : List (Html msg)
                ephemeralButtons =
                    case route of
                        Team slug ->
                            [ menuItem route route True (slugStr slug) ]

                        DeleteTeam _ ->
                            [ menuItem route route True "Delete team" ]

                        CreateTeam ->
                            [ menuItem route route True "Create team"
                            ]

                        _ ->
                            []
            in
            nav [] [ ul [] (teamsButton ++ ephemeralButtons ++ adminButtons) ]

        _ ->
            nav [] []


menuItem : Route -> Route -> Bool -> String -> Html.Html msg
menuItem currentRoute targetRoute indent title =
    let
        classes : List ( String, Bool )
        classes =
            [ ( "active", currentRoute == targetRoute )
            , ( "indent", indent )
            ]
    in
    li [ classList classes ] [ link targetRoute [ Html.Attributes.title title ] [ text title ] ]
