PreemptProtect[ BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ]; EndPackage[ ] ];

Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile = FileNameJoin @ {
    DirectoryName @ $InputFileName,
    ToString @ $SystemWordLength <> "Bit",
    "CodeEquivalenceUtilities.mx"
};

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SymbolVersionTooNew:: *)
Quiet[
    If[ FileExistsQ @ Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile,
        Get @ Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile,
        WithCleanup[
            Get[ "Wolfram`CodeEquivalenceUtilities`Package`" ],
            { $Context, $ContextPath, System`$ContextAliases } = { ## }
        ] & [ $Context, $ContextPath, System`$ContextAliases ]
    ],
    General::shdw
];
(* :!CodeAnalysis::EndBlock:: *)