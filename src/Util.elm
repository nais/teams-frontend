module Util exposing (appendMaybe, conditionalElement, flattenMaybe)


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


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just v ->
            [ v ]

        Nothing ->
            []


flattenMaybe : List (Maybe a) -> List a
flattenMaybe l =
    List.concatMap maybeToList l
