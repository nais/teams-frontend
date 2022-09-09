module Main exposing (..)

import Browser exposing (Document)
import Home
import Html exposing (Html)
import Teams



---- MODEL ----


type Model
    = Home Home.Model
    | Teams Teams.Model



---- UPDATE ----


type Msg
    = GotHomeMsg Home.Msg
    | GotTeamsMsg Teams.Msg


init : ( Model, Cmd Msg )
init =
    --Home.init |> updateWith Home GotHomeMsg
    Teams.init |> updateWith Teams GotTeamsMsg



-- just use the home model's init in main (should be route based later on)


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- Add handler here when we add new modules
        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel |> updateWith Home GotHomeMsg

        ( GotTeamsMsg subMsg, Teams subModel ) ->
            Teams.update subMsg subModel |> updateWith Teams GotTeamsMsg

        ( _, _ ) ->
            ( model, Cmd.none )



-- (_, _) ->
--     Debug.log "got update from invalid location"
---- VIEW ----


view : Model -> Document Msg
view model =
    let
        html =
            case model of
                -- Add new view here when we add new modules
                Home subModel ->
                    Home.view subModel |> Html.map GotHomeMsg

                Teams subModel ->
                    Teams.view subModel |> Html.map GotTeamsMsg
    in
    { title = "NAIS console", body = [ html ] }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
