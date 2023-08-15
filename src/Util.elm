module Util exposing (appendMaybe, conditionalElement, flattenMaybe, formatForDisplay)

import ISO8601
import Time exposing (Month(..), Posix, Zone)
import TimeZone


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


formatForDisplay : ISO8601.Time -> String
formatForDisplay isoTime =
    let
        posix : Posix
        posix =
            ISO8601.toPosix isoTime

        zone : Zone
        zone =
            TimeZone.europe__berlin ()
    in
    ensureTwoDigits (Time.toDay zone posix)
        ++ " "
        ++ toNorMonthShort (Time.toMonth zone posix)
        ++ " "
        ++ ensureTwoDigits (Time.toYear zone posix)
        ++ " "
        ++ ensureTwoDigits (Time.toHour zone posix)
        ++ ":"
        ++ ensureTwoDigits (Time.toMinute zone posix)


ensureTwoDigits : Int -> String
ensureTwoDigits nr =
    String.padLeft 2 '0' <| String.fromInt nr


toNorMonthShort : Month -> String
toNorMonthShort month =
    case month of
        Jan ->
            "jan"

        Feb ->
            "feb"

        Mar ->
            "mar"

        Apr ->
            "apr"

        May ->
            "mai"

        Jun ->
            "jun"

        Jul ->
            "jul"

        Aug ->
            "aug"

        Sep ->
            "sep"

        Oct ->
            "okt"

        Nov ->
            "nov"

        Dec ->
            "des"
