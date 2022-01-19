#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

pacletInstall[ url_ ] :=
    Module[ { installed },
        Print[ "::notice:: Installing paclet from: ", url ];
        installed = PacletInstall[ url, ForceVersionInstall -> True ];
        If[ PacletObjectQ @ installed,
            Print[ "::notice:: Paclet installed to: ", installed[ "Location" ] ],
            Print[ "::error:: Paclet installation failed: ", installed ]
        ]
    ];

pacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
pacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet"           ];
pacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/ResourceSystemClient.paclet"     ];

(* :!CodeAnalysis::EndBlock:: *)