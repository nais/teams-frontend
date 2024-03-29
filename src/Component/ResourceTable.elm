module Component.ResourceTable exposing (view)

import Api.Str exposing (slugStr, uuidStr)
import DataModel exposing (GCPProject, NaisNamespace, TeamSyncState)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (colspan)


simpleRow : String -> String -> List (Html msg)
simpleRow title description =
    [ tr []
        [ td [] [ text title ]
        , td [] [ text description ]
        ]
    ]


namespaceRow : NaisNamespace -> List (Html msg)
namespaceRow ns =
    simpleRow ("NAIS namespace (" ++ ns.environment ++ ")") (slugStr ns.namespace)


projectRow : GCPProject -> List (Html msg)
projectRow p =
    simpleRow ("GCP project ID (" ++ p.environment ++ ")") p.projectID


syncStateRows : TeamSyncState -> List (Html msg)
syncStateRows state =
    List.concatMap (Maybe.withDefault [])
        [ Maybe.map (simpleRow "GitHub team slug" << slugStr) state.githubTeamSlug
        , Maybe.map (simpleRow "Google group email") state.googleWorkspaceGroupEmail
        , Maybe.map (simpleRow "Azure AD group ID" << uuidStr) state.azureADGroupID
        , Maybe.map (simpleRow "Artifact Registry repository") (garRepoToLink state.garRepository)
        ]
        ++ List.concatMap projectRow state.gcpProjects
        ++ List.concatMap namespaceRow state.naisNamespaces



-- from: projects/nais-management-233d/locations/europe-north1/repositories/aura
-- to: europe-north1-docker.pkg.dev/nais-management-233d/aura


garRepoToLink : Maybe String -> Maybe String
garRepoToLink id =
    id
        |> Maybe.map (String.split "/")
        |> Maybe.map (List.take 6)
        |> (\l ->
                case l of
                    Just [ _, projectId, _, location, _, repository ] ->
                        Just (location ++ "-docker.pkg.dev/" ++ projectId ++ "/" ++ repository)

                    _ ->
                        Nothing
           )


view : Maybe TeamSyncState -> Html msg
view syncState =
    let
        rows : List (Html msg)
        rows =
            case syncState of
                Nothing ->
                    []

                Just s ->
                    syncStateRows s
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Description" ]
                , th [] [ text "Value" ]
                ]
            ]
        , tbody []
            (if List.length rows == 0 then
                [ tr [] [ td [ colspan 2 ] [ text "NAIS Teams has not created any resources yet" ] ] ]

             else
                rows
            )
        ]
