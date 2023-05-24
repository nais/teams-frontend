module Api.Str exposing (auditActionStr, deployKeyStr, reconcilerNameStr, roleStr, slugStr, uuidStr)

import Backend.Enum.TeamRole exposing (TeamRole(..))
import Backend.Scalar


slugStr : Backend.Scalar.Slug -> String
slugStr (Backend.Scalar.Slug s) =
    s


uuidStr : Backend.Scalar.Uuid -> String
uuidStr (Backend.Scalar.Uuid s) =
    s


deployKeyStr : Backend.Scalar.DeployKey -> String
deployKeyStr (Backend.Scalar.DeployKey s) =
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
