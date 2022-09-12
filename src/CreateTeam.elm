module CreateTeam exposing (..)

import Browser.Navigation
import Html exposing (Html, div, form, input, label, li, text, ul)
import Html.Attributes exposing (for, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)


type alias Model =
    { navKey : Browser.Navigation.Key
    , slug : String
    , name : String
    , purpose : String
    }


type Msg
    = CreateTeamSubmit
    | SlugChanged String
    | NameChanged String
    | PurposeChanged String


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init navigationKey =
    ( { navKey = navigationKey
      , name = ""
      , purpose = ""
      , slug = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTeamSubmit ->
            ( model, Cmd.none )

        SlugChanged s ->
            ( { model | slug = s }, Cmd.none )

        NameChanged s ->
            ( { model | name = s }, Cmd.none )

        PurposeChanged s ->
            ( { model | purpose = s }, Cmd.none )


textbox : (String -> Msg) -> String -> String -> Html Msg
textbox msg id placeholder =
    li []
        [ label [ for id ] [ text placeholder ]
        , input [ type_ "text", Html.Attributes.placeholder placeholder, onInput msg ] []
        ]


createTeamForm : Html Msg
createTeamForm =
    div []
        [ form [ onSubmit CreateTeamSubmit ]
            [ ul []
                [ textbox SlugChanged "slug" "Unique identifier"
                , textbox NameChanged "name" "Human readable team name"
                , textbox PurposeChanged "purpose" "Purpose of the team"
                ]
            , input [ type_ "submit", value "Create new team" ] []
            ]
        ]


view : Model -> Html Msg
view model =
    createTeamForm
