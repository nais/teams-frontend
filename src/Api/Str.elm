module Api.Str exposing (auditActionStr, reconcilerNameStr, roleStr, slugStr, uuidStr)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar


slugStr : Backend.Scalar.Slug -> String
slugStr (Backend.Scalar.Slug s) =
    s


uuidStr : Backend.Scalar.Uuid -> String
uuidStr (Backend.Scalar.Uuid s) =
    s


reconcilerNameStr : Backend.Scalar.ReconcilerName -> String
reconcilerNameStr (Backend.Scalar.ReconcilerName r) =
    r


auditActionStr : Backend.Scalar.AuditAction -> String
auditActionStr (Backend.Scalar.AuditAction a) =
    a


roleStr : TeamRole -> String
roleStr teamRole =
    case teamRole of
        Member ->
            "Member"

        Owner ->
            "Owner"
