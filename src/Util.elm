module Util exposing (appendMaybe, conditionalElement, updateWith)

import Browser exposing (element)
import DataModel exposing (Expandable(..))


appendMaybe : Maybe a -> List a -> List a
appendMaybe maybe list =
    case maybe of
        Just toConcat ->
            list ++ List.singleton toConcat

        Nothing ->
            list


conditionalElement : Bool -> a -> Maybe a
conditionalElement condition element =
    if condition then
        Just element

    else
        Nothing


updateWith : (subModel -> a) -> (subMsg -> b) -> ( subModel, Cmd subMsg ) -> ( a, Cmd b )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
