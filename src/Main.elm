module Main exposing (..)

import Browser
import Home
import Html exposing (Html)



---- MODEL ----


type Model
    = Home Home.Model



---- UPDATE ----


type Msg
    = GotHomeMsg Home.Msg


init : (Model, Cmd Msg)
init =
    (Home.init |> updateWith Home GotHomeMsg ) -- just use the home model's init in main (should be route based later on)


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of -- Add handler here when we add new modules
        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel |> updateWith Home GotHomeMsg



-- (_, _) ->
--     Debug.log "got update from invalid location"
---- VIEW ----


view : Model -> Html Msg
view model =
    case model of -- Add new view here when we add new modules
        Home subModel ->
            Home.view subModel |> Html.map GotHomeMsg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
