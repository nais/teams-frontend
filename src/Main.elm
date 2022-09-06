module Main exposing (..)

import Backend.Object
import Backend.Object.User as User
import Backend.Query as Query
import Backend.Scalar exposing (Uuid)
import Browser
import Browser.Navigation
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, empty)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type Actor
    = LoggedIn UserData
    | Unauthorized
    | Unknown


type alias Model =
    { user : Actor }


type alias UserData =
    { id : Uuid
    , email : String
    , name : String
    }


init : ( Model, Cmd Msg )
init =
    ( { user = Unknown }, sendGetMeRequest )



---- UPDATE ----


type Msg
    = ClickedLogin
    | GotMeResponse (Result (Graphql.Http.Error UserData) UserData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLogin ->
            ( model, Browser.Navigation.load "http://localhost:3000/oauth2/login" )

        GotMeResponse (Ok user) ->
            ( { model | user = LoggedIn user }, Cmd.none )

        GotMeResponse (Err _) ->
            ( { model | user = Unauthorized }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "nais console" ]
        , button [ onClick ClickedLogin ] [ text "Single sign-on" ]
        ]



---- PROGRAM ----


sendGetMeRequest : Cmd Msg
sendGetMeRequest =
    getMeQuery
        |> Graphql.Http.queryRequest "http://localhost:3000/query"
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send GotMeResponse


getMeQuery : SelectionSet UserData RootQuery
getMeQuery =
    Query.me meSelection


meSelection : SelectionSet UserData Backend.Object.User
meSelection =
    Graphql.SelectionSet.map3 UserData
        User.id
        User.email
        User.name


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
