module CreateTeam exposing (..)

import Backend.Scalar
import Graphql.Http exposing (RawError(..))
import Graphql.OptionalArgument
import Html exposing (Html, div, form, h2, input, label, li, p, text, ul)
import Html.Attributes exposing (class, for, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Queries.Do
import Queries.TeamQueries exposing (TeamData, createTeamMutation)
import Session exposing (Session)


type alias Model =
    { session : Session
    , slug : String
    , purpose : Maybe String
    , error : Maybe String
    }


type Msg
    = CreateTeamSubmit
    | GotTeamCreatedResponse (Result (Graphql.Http.Error TeamData) TeamData)
    | SlugChanged String
    | PurposeChanged String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , purpose = Nothing
      , slug = ""
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTeamSubmit ->
            ( model
            , Queries.Do.mutate
                (createTeamMutation
                    { name = model.slug
                    , purpose = Graphql.OptionalArgument.fromMaybe model.purpose
                    , slug = Backend.Scalar.Slug model.slug
                    }
                )
                GotTeamCreatedResponse
            )

        GotTeamCreatedResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none )

        GotTeamCreatedResponse (Err (Graphql.Http.HttpError _)) ->
            ( { model | error = Just "Can't talk to server, are we connected?" }, Cmd.none )

        GotTeamCreatedResponse (Err (GraphqlError _ errors)) ->
            let
                errstr =
                    List.map (\error -> error.message) errors
                        |> String.join ","
            in
            ( { model | error = Just errstr }, Cmd.none )

        SlugChanged s ->
            ( { model | slug = s }, Cmd.none )

        PurposeChanged s ->
            ( { model | purpose = stringOrBlank s }, Cmd.none )


stringOrBlank : String -> Maybe String
stringOrBlank s =
    if s == "" then
        Nothing

    else
        Just s


textbox : (String -> Msg) -> String -> String -> String -> Html Msg
textbox msg id lbl placeholder =
    li []
        [ label [ for id ] [ text lbl ]
        , input [ type_ "text", Html.Attributes.placeholder placeholder, onInput msg ] []
        ]


errorView : Maybe String -> List (Html msg)
errorView maybeString =
    case maybeString of
        Nothing ->
            []

        Just s ->
            [ div [ class "error" ] [ text s ] ]


createTeamForm : Model -> Html Msg
createTeamForm model =
    div []
        [ h2 [] [ text "Create a new team" ]
        , p [] [ text "Use this form to create a new team. You will become the administrator of the team." ]
        , p [] [ text "The identifier will be propagated to other systems and cannot be changed after creation." ]
        , form [ onSubmit CreateTeamSubmit ]
            (ul []
                [ textbox SlugChanged "slug" "Identifier" "customer-satisfaction"
                , textbox PurposeChanged "purpose" "Purpose of the team" "Making sure customers have a good user experience"
                ]
                :: errorView model.error
                ++ [ input [ type_ "submit", value "Create new team" ] []
                   ]
            )
        ]


view : Model -> Html Msg
view model =
    createTeamForm model
