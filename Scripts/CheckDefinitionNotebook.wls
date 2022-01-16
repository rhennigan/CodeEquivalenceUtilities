#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet" ];

CloudObject;
Block[ { CloudObject`Private`promptForWebLogin = $Canceled & },
    Quiet[
        DefinitionNotebookClient`CheckDefinitionNotebook[
            File[ "DefinitionNotebook.nb" ],
            "ConsoleType"   -> Automatic,
            "ClickedButton" -> "Submit",
            "DisabledHints" -> { "PacletRequiresBuild" }
        ],
        CloudObject::notauth
    ]
];

(* :!CodeAnalysis::EndBlock:: *)