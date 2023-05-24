module DataModel exposing (AuditLog, DeployKey, Expandable(..), GCPProject, GitHubRepository, GitHubRepositoryPermission, NaisNamespace, Reconciler, ReconcilerConfig, Role, SlackAlertsChannel, SyncError, Team, TeamDeleteConfirmed, TeamDeleteKey, TeamMember(..), TeamMemberReconciler(..), TeamSync, TeamSyncState, User, UserSyncRun, expandableAll, expandableTake, flipExpanded, tmRole, tmTeam, tmUser)

import Backend.Enum.TeamRole exposing (TeamRole)
import Backend.Enum.UserSyncRunStatus exposing (UserSyncRunStatus)
import Backend.Scalar exposing (AuditAction, ReconcilerName, RoleName, Slug, Uuid)
import ISO8601


type Expandable a
    = Preview a
    | Expanded a


expandableTake : Int -> Expandable (List a) -> List a
expandableTake n l =
    case l of
        Preview p ->
            List.take n p

        Expanded e ->
            e


expandableAll : Expandable a -> a
expandableAll e =
    case e of
        Preview i ->
            i

        Expanded i ->
            i


flipExpanded : Expandable a -> Expandable a
flipExpanded e =
    case e of
        Preview i ->
            Expanded i

        Expanded i ->
            Preview i


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


type alias NaisNamespace =
    { environment : String
    , namespace : Slug
    }


type alias ReconcilerConfig =
    { configured : Bool
    , description : String
    , displayName : String
    , key : Backend.Scalar.ReconcilerConfigKey
    , value : Maybe String
    , secret : Bool
    }


type alias Reconciler =
    { configured : Bool
    , description : String
    , displayname : String
    , enabled : Bool
    , name : ReconcilerName
    , runorder : Int
    , config : List ReconcilerConfig
    , usesTeamMemberships : Bool
    }


type alias Role =
    { name : RoleName
    , isGlobal : Bool
    , targetTeamSlug : Maybe Slug
    }


type alias SlackAlertsChannel =
    { environment : String
    , channelName : String
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
    , syncErrors : List SyncError
    , lastSuccessfulSync : Maybe ISO8601.Time
    , syncState : Maybe TeamSyncState
    , repositories : Expandable (List GitHubRepository)
    }


type alias TeamDeleteKey =
    { key : Uuid
    , expires : ISO8601.Time
    , team : Team
    , createdBy : User
    }


type alias TeamDeleteConfirmed =
    { correlationID : Uuid
    }


type TeamMember
    = TeamMember User Team TeamRole (List TeamMemberReconciler)


type alias TeamSync =
    { correlationID : Uuid
    }


type TeamMemberReconciler
    = TeamMemberReconciler Bool Reconciler


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
    , teamMemberships : List TeamMember
    , roles : List Role
    }


type alias DeployKey =
    { key : Backend.Scalar.DeployKey
    }


type alias UserSyncRun =
    { correlationID : Uuid
    , startedAt : ISO8601.Time
    , finishedAt : Maybe ISO8601.Time
    , status : UserSyncRunStatus
    , logEntries : List AuditLog
    , error : Maybe String
    }


tmUser : TeamMember -> User
tmUser (TeamMember user _ _ _) =
    user


tmTeam : TeamMember -> Team
tmTeam (TeamMember _ team _ _) =
    team


tmRole : TeamMember -> TeamRole
tmRole (TeamMember _ _ role _) =
    role


tmReconciler : TeamMember -> List TeamMemberReconciler
tmReconciler (TeamMember _ _ _ userReconcilers) =
    userReconcilers


tmrEnabled : TeamMemberReconciler -> Bool
tmrEnabled (TeamMemberReconciler enabled _) =
    enabled
