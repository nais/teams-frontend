-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Backend.Scalar exposing (AuditAction(..), AuditLogsTargetType(..), Codecs, Id(..), Map(..), ReconcilerConfigKey(..), ReconcilerName(..), RoleName(..), Slug(..), SystemName(..), Time(..), Uuid(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type AuditAction
    = AuditAction String


type AuditLogsTargetType
    = AuditLogsTargetType String


type Id
    = Id String


type Map
    = Map String


type ReconcilerConfigKey
    = ReconcilerConfigKey String


type ReconcilerName
    = ReconcilerName String


type RoleName
    = RoleName String


type Slug
    = Slug String


type SystemName
    = SystemName String


type Time
    = Time String


type Uuid
    = Uuid String


defineCodecs :
    { codecAuditAction : Codec valueAuditAction
    , codecAuditLogsTargetType : Codec valueAuditLogsTargetType
    , codecId : Codec valueId
    , codecMap : Codec valueMap
    , codecReconcilerConfigKey : Codec valueReconcilerConfigKey
    , codecReconcilerName : Codec valueReconcilerName
    , codecRoleName : Codec valueRoleName
    , codecSlug : Codec valueSlug
    , codecSystemName : Codec valueSystemName
    , codecTime : Codec valueTime
    , codecUuid : Codec valueUuid
    }
    -> Codecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid
    ->
        { codecAuditAction : Codec valueAuditAction
        , codecAuditLogsTargetType : Codec valueAuditLogsTargetType
        , codecId : Codec valueId
        , codecMap : Codec valueMap
        , codecReconcilerConfigKey : Codec valueReconcilerConfigKey
        , codecReconcilerName : Codec valueReconcilerName
        , codecRoleName : Codec valueRoleName
        , codecSlug : Codec valueSlug
        , codecSystemName : Codec valueSystemName
        , codecTime : Codec valueTime
        , codecUuid : Codec valueUuid
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder :
    (RawCodecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid -> Codec getterValue)
    -> Codecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid
    -> getterValue
    -> Graphql.Internal.Encode.Value
unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid
    = Codecs (RawCodecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid)


type alias RawCodecs valueAuditAction valueAuditLogsTargetType valueId valueMap valueReconcilerConfigKey valueReconcilerName valueRoleName valueSlug valueSystemName valueTime valueUuid =
    { codecAuditAction : Codec valueAuditAction
    , codecAuditLogsTargetType : Codec valueAuditLogsTargetType
    , codecId : Codec valueId
    , codecMap : Codec valueMap
    , codecReconcilerConfigKey : Codec valueReconcilerConfigKey
    , codecReconcilerName : Codec valueReconcilerName
    , codecRoleName : Codec valueRoleName
    , codecSlug : Codec valueSlug
    , codecSystemName : Codec valueSystemName
    , codecTime : Codec valueTime
    , codecUuid : Codec valueUuid
    }


defaultCodecs : RawCodecs AuditAction AuditLogsTargetType Id Map ReconcilerConfigKey ReconcilerName RoleName Slug SystemName Time Uuid
defaultCodecs =
    { codecAuditAction =
        { encoder = \(AuditAction raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map AuditAction
        }
    , codecAuditLogsTargetType =
        { encoder = \(AuditLogsTargetType raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map AuditLogsTargetType
        }
    , codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    , codecMap =
        { encoder = \(Map raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Map
        }
    , codecReconcilerConfigKey =
        { encoder = \(ReconcilerConfigKey raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map ReconcilerConfigKey
        }
    , codecReconcilerName =
        { encoder = \(ReconcilerName raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map ReconcilerName
        }
    , codecRoleName =
        { encoder = \(RoleName raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map RoleName
        }
    , codecSlug =
        { encoder = \(Slug raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Slug
        }
    , codecSystemName =
        { encoder = \(SystemName raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map SystemName
        }
    , codecTime =
        { encoder = \(Time raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Time
        }
    , codecUuid =
        { encoder = \(Uuid raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Uuid
        }
    }
