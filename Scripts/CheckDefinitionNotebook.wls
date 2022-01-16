#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet" ];

CloudObject;
checked = Block[ { CloudObject`Private`promptForWebLogin = $Canceled & },
    Quiet[
        DefinitionNotebookClient`CheckDefinitionNotebook[
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
        ],
        CloudObject::notauth
    ]
];

If[ ! FreeQ[ checked, KeyValuePattern[ "Level" -> "Error" ], { 1 } ],
    Exit[ 1 ]
];

(* :!CodeAnalysis::EndBlock:: *)