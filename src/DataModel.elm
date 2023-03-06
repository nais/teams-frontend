module DataModel exposing (AuditLog, Expandable(..), GCPProject, GitHubRepository, GitHubRepositoryPermission, KeyValue, NaisNamespace, ReconcilerConfigData, ReconcilerData, Role, SlackAlertsChannel, SyncError, Team, TeamDeleteConfirmed, TeamDeleteKey, TeamMember, TeamMembership, TeamSlug, TeamSync, TeamSyncState, User)

import Backend.Enum.TeamRole exposing (TeamRole)
import Backend.Scalar exposing (AuditAction, ReconcilerName, RoleName, Slug, Uuid)
import ISO8601


type Expandable a
    = Preview a
    | Expanded a


type alias AuditLog =
    { action : AuditAction
    , actor : Maybe String
    , message : String
    , createdAt : ISO8601.Time
    }


type alias GCPProject =
    { environment : String
    , projectID : String
    }


type alias GitHubRepository =
    { name : String
    , permissions : List GitHubRepositoryPermission
    }


type alias GitHubRepositoryPermission =
    { name : String
    , granted : Bool
    }


type alias KeyValue =
    { key : String
    , value : Maybe String
    }


type alias NaisNamespace =
    { environment : String
    , namespace : Slug
    }


type alias ReconcilerConfigData =
    { configured : Bool
    , description : String
    , displayName : String
    , key : Backend.Scalar.ReconcilerConfigKey
    , value : Maybe String
    , secret : Bool
    }


type alias ReconcilerData =
    { configured : Bool
    , description : String
    , displayname : String
    , enabled : Bool
    , name : ReconcilerName
    , runorder : Int
    , config : List ReconcilerConfigData
    }


type alias Role =
    { name : RoleName
    , isGlobal : Bool
    , targetTeamSlug : Maybe Slug
    }


type alias SlackAlertsChannel =
    { environment : String
    , channelName : Maybe String
    }


type alias SyncError =
    { timestamp : ISO8601.Time
    , reconcilerName : String
    , message : String
    }


type alias Team =
    { slug : Slug
    , purpose : String
    , slackChannel : String
    , slackAlertsChannels : List SlackAlertsChannel
    , members : Expandable (List TeamMember)
    , auditLogs : Expandable (List AuditLog)
    , metadata : List KeyValue
    , syncErrors : List SyncError
    , lastSuccessfulSync : Maybe ISO8601.Time
    , syncState : Maybe TeamSyncState
    , repositories : Expandable (List GitHubRepository)
    , enabled : Bool
    }


type alias TeamDeleteKey =
    { key : Uuid
    , expires : ISO8601.Time
    }


type alias TeamDeleteConfirmed =
    { correlationID : Uuid
    }


type alias TeamMember =
    { user : User
    , role : TeamRole
    }


type alias TeamMembership =
    { team : TeamSlug }


type alias TeamSlug =
    { slug : Slug }


type alias TeamSync =
    { correlationID : Uuid
    }


type alias TeamSyncState =
    { githubTeamSlug : Maybe Slug
    , googleWorkspaceGroupEmail : Maybe String
    , gcpProjects : List GCPProject
    , naisNamespaces : List NaisNamespace
    , azureADGroupID : Maybe Uuid
    , garRepository : Maybe String
    }


type alias User =
    { id : Uuid
    , email : String
    , name : String
    , externalId : String
    , teamMemberships : List TeamMembership
    , roles : List Role
    }
