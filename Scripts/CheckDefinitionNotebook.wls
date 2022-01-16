#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

Print[ "::notice::", "GITHUB_WORKFLOW: ", Environment[ "GITHUB_WORKFLOW" ] ];
Print[ "::notice::", "$EvaluationEnvironment: ", $EvaluationEnvironment ];

PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet" ];

CloudObject;
Block[ { CloudObject`Private`promptForWebLogin = $Failed & },
    Quiet[
        DefinitionNotebookClient`CheckDefinitionNotebook[
            File[ "DefinitionNotebook.nb" ],
            "ConsoleType"   -> Automatic,
            "ClickedButton" -> "Submit"
        ],
        CloudObject::notauth
    ]
];

(* :!CodeAnalysis::EndBlock:: *)