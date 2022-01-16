#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

Print[ "::notice::", "GITHUB_WORKFLOW: ", Environment[ "GITHUB_WORKFLOW" ] ];

PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet" ];

DefinitionNotebookClient`CheckDefinitionNotebook[
    File[ "DefinitionNotebook.nb" ],
    "ConsoleType"   -> Automatic,
    "ClickedButton" -> "Submit"
];

(* :!CodeAnalysis::EndBlock:: *)