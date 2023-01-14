PreemptProtect[ BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ]; EndPackage[ ] ];

Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile = FileNameJoin @ {
    DirectoryName @ $InputFileName,
    ToString @ $SystemWordLength <> "Bit",
    "CodeEquivalenceUtilities.mx"
};

Block[ { $ContextPath },
    Quiet[
        If[ FileExistsQ @ Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile,
            Get @ Wolfram`CodeEquivalenceUtilitiesLoader`$MXFile,
            Get[ "Wolfram`CodeEquivalenceUtilities`Package`" ]
        ],
        General::shdw
    ]
];