module Util exposing (appendMaybe, conditionalElement)


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
