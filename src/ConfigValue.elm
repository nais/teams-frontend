module ConfigValue exposing (..)


type ConfigValue
    = ConfigValue String
    | NotConfigured
    | Secret


hasValue : ConfigValue -> Bool
hasValue cv =
    case cv of
        ConfigValue _ ->
            True

        _ ->
            False


string : ConfigValue -> String
string cv =
    case cv of
        ConfigValue s ->
            s

        _ ->
            ""
