#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$inputFileName = Replace[ $InputFileName, "" :> NotebookFileName[ ] ];
$buildDir      = GeneralUtilities`EnsureDirectory @ {
                     DirectoryName[ $inputFileName, 2 ],
                     "build"
                 };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*env*)
env[ key_ ] := Enclose[
    ConfirmBy[ Lookup[ GetEnvironment[ key ], key ], StringQ ],
    (
        printError[ "The environment variable ", key, " is undefined." ];
        Exit[ 1 ]
    ) &
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*printError*)
printError[ args___ ] := Print[ "::error::", args ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*printWarning*)
printWarning[ args___ ] := Print[ "::warning::", args ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*printDebug*)
printDebug[ args___ ] := Print[ "::debug::", args ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*printHint*)
printHint[ KeyValuePattern @ {
    "Level"   -> "Error",
    "Message" -> message_,
    "Tag"     -> tag_
} ] :=
    printError[ hintTag @ tag, message ];

printHint[ KeyValuePattern @ {
    "Level"   -> "Warning"|"Suggestion",
    "Message" -> message_,
    "Tag"     -> tag_
} ] :=
    printWarning[ hintTag @ tag, message ];

printHint[ KeyValuePattern @ { "Message" -> message_, "Tag" -> tag_ } ] :=
    Print[ hintTag @ tag, message ];

hintTag[ tag_ ] := Sequence[ "[DefinitionNotebookClient:", tag, "] " ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*downloadBuildArtifacts*)
downloadBuildArtifacts[ KeyValuePattern[ "BuildArtifacts" -> a_ ] ] :=
    downloadBuildArtifacts @ a;

downloadBuildArtifacts[ KeyValuePattern[ "PacletArchive" -> url_ ] ] := Enclose[
    Module[ { downloaded, extracted },
        downloaded = ConfirmBy[ URLDownload @ url, FileExistsQ ];
        DeleteDirectory[ $buildDir, DeleteContents -> True ];
        CreateDirectory @ $buildDir;
        extracted = ExtractPacletArchive[ downloaded, $buildDir ];
        ConfirmBy[ CreatePacletArchive @ extracted, FileExistsQ ]
    ],
    (
        printError[ "Failed: ", downloadBuildArtifacts ];
        Exit[ 1 ]
    ) &
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$checkAPI        = CloudObject[ "https://www.wolframcloud.com/obj/rhennigan/CheckGitHubPaclet" ];
$permissionsKey  = env[ "CHECKGITHUBPACLET_PERMISSIONS_KEY" ];
$ref             = env[ "GITHUB_REF" ];
$branch          = StringDelete[ $ref, StartOfString ~~ "refs/heads/" ];
$fullRepo        = env[ "GITHUB_REPOSITORY" ];
{ $user, $repo } = StringSplit[ $fullRepo, "/" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Run*)

result = Enclose[
    ConfirmBy[
        URLExecute[
            $checkAPI,
            {
                "User"          -> ConfirmBy[ $user, StringQ ],
                "Repository"    -> ConfirmBy[ $repo, StringQ ],
                "Branch"        -> ConfirmBy[ $branch, StringQ ],
                "Properties"    -> "All",
                "DeployPreview" -> "True"
            },
            "WL",
            Authentication -> PermissionsKey @ $permissionsKey
        ],
        AssociationQ
    ],
    (
        printError[ "Something happened: ", # ];
        Exit[ 1 ]
    ) &
];


(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Print hints*)
printHint /@ result[ "Hints" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Export files*)
downloadBuildArtifacts @ result;

Export[ FileNameJoin @ { $buildDir, "DefinitionNotebook.nb" },
        result[ "Notebook" ],
        "Binary"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Finish*)
If[ ! MatchQ[ result[ "Result" ], _Success ],
    printError[ "Inspection failed: ", result[ "Result" ] ];
    Exit[ 1 ],
    Exit[ 0 ]
];

(* :!CodeAnalysis::EndBlock:: *)