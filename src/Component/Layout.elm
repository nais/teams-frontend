module Component.Layout exposing (view)

import Component.Navigation
import Html exposing (Html, a, div, h1, header, main_, p, text)
import Html.Attributes exposing (class, href, id)
import Route exposing (link)
import Session exposing (Viewer(..))
import Url.Builder


view : Route.Route -> Session.Session -> Html msg -> List (Html msg)
view route session html =
    let
        user : Viewer
        user =
            Session.viewer session

        auth : Html msg
        auth =
            case user of
                LoggedIn loggedInUser ->
                    let
                        logoutURL : String
                        logoutURL =
                            Url.Builder.absolute [ "oauth2", "logout" ] []
                    in
                    div [ class "user-info" ]
                        [ p [] [ text loggedInUser.name ]
                        , a [ href logoutURL, class "button small" ] [ text "Logout" ]
                        ]

                _ ->
                    let
                        loginURL : String
                        loginURL =
                            Url.Builder.absolute [ "oauth2", "login" ] []
                    in
                    a [ href loginURL, class "button small" ] [ text "Login" ]
    in
    [ header []
        [ div [ class "content" ]
            [ div []
                [ div [ id "logo" ] []
                , h1 []
                    [ link Route.Home [] [ text "Console" ]
                    ]
                ]
            , auth
            ]
        ]
    , div [ id "layout" ]
        [ Component.Navigation.view route session
        , main_ []
            [ html ]
        , div [] []
        ]
    ]
