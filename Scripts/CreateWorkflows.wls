#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$inputFileName = Replace[ $InputFileName, "" :> NotebookFileName[ ] ];
$pacletDir     = DirectoryName[ $inputFileName, 2 ];

PacletInstall[ "Wolfram/PacletCICD" ];
Needs[ "Wolfram`PacletCICD`" ];

$check = Workflow[
    "Check",
    <|
        "On" -> <|
            "push"     -> None,
            "schedule" -> { <| "cron" -> "30 12 * * *" |> }
        |>,
        "Jobs" -> {
            WorkflowJob[
                {
                    "Checkout",
                    WorkflowStep @ File[ "Scripts/BuildMX.wls" ],
                    "Build",
                    "UploadArtifact"
                },
                <| "Name" -> "Check" |>,
                TimeConstraint -> Quantity[ 15, "Minutes" ]
            ],
            WorkflowJob[
                {
                    "Checkout",
                    WorkflowStep @ File[ "Scripts/BuildMX.wls" ],
                    "Test",
                    "UploadTestResults"
                },
                <| "Name" -> "Test" |>,
                TimeConstraint -> Quantity[ 30, "Minutes" ]
            ]
        }
    |>,
    "CancelInProgress" -> True
];

WorkflowExport[ $pacletDir, $check ];

$release =
    Workflow[
        "Release",
        <|
            "Jobs" -> {
                WorkflowJob[
                    {
                        "Checkout",
                        WorkflowStep @ File[ "Scripts/BuildMX.wls" ],
                        "Check"
                    },
                    <| "Name" -> "Check" |>,
                    TimeConstraint -> Quantity[ 15, "Minutes" ]
                ],
                WorkflowJob[
                    {
                        "Checkout",
                        WorkflowStep @ File[ "Scripts/BuildMX.wls" ],
                        "Test",
                        "UploadTestResults"
                    },
                    <| "Name" -> "Test" |>,
                    TimeConstraint -> Quantity[ 30, "Minutes" ]
                ],
                WorkflowJob[
                    {
                        "Checkout",
                        WorkflowStep @ File[ "Scripts/BuildMX.wls" ],
                        "Build",
                        "UploadArtifact",
                        "CreateRelease",
                        "UploadRelease"
                    },
                    <|
                        "Name"  -> "Release",
                        "Needs" -> { "Check", "Test" }
                    |>,
                    TimeConstraint -> Quantity[ 30, "Minutes" ]
                ],
                WorkflowJob[
                    {
                        "Checkout",
                        WorkflowStep @ File[ "Scripts/BuildMX.wls" ],
                        "Submit",
                        "UploadArtifact"
                    },
                    <|
                        "Name"  -> "Submit",
                        "Needs" -> { "Check", "Test" }
                    |>,
                    TimeConstraint -> Quantity[ 30, "Minutes" ]
                ]
            }
        |>,
        "CancelInProgress" -> True
    ];

WorkflowExport[ $pacletDir, $release ];

(* :!CodeAnalysis::EndBlock:: *)