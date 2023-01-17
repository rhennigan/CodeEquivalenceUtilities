PreemptProtect[ BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ]; EndPackage[ ] ];

Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile = FileNameJoin @ {
    DirectoryName @ $InputFileName,
    ToString @ $SystemWordLength <> "Bit",
    "CodeEquivalenceUtilities.mx"
};

Quiet[
    If[ FileExistsQ @ Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile,
        Get @ Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile,
        WithCleanup[
            Get[ "Wolfram`CodeEquivalenceUtilities`Package`" ],
            { $Context, $ContextPath, $ContextAliases } = { ## }
        ] & [ $Context, $ContextPath, $ContextAliases ]
    ],
    General::shdw
];