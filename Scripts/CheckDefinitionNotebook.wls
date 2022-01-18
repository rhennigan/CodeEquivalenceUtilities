#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

Quiet[
(* Temporary *)
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet" ];
];

checked = DefinitionNotebookClient`CheckDefinitionNotebook[
    File[ "DefinitionNotebook.nb" ],
    "ConsoleType"   -> Automatic,
    "ClickedButton" -> "Submit",
    "DisabledHints" -> {
        <|
            "MessageTag" -> "PacletRequiresBuild",
            "Level"      -> "Error",
            "ID"         -> All
        |>
    }
];

If[ ! FreeQ[ checked, KeyValuePattern[ "Level" -> "Error" ], { 1 } ],
    Exit[ 1 ]
];

(* :!CodeAnalysis::EndBlock:: *)