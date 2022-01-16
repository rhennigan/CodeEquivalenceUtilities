#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet" ];

Print @ DefinitionNotebookClient`CheckDefinitionNotebook[
    File[ "CodeEquivalenceUtilities/DefinitionNotebook.nb" ],
    "ConsoleType"   -> Automatic,
    "ClickedButton" -> "Submit"
];

(* :!CodeAnalysis::EndBlock:: *)