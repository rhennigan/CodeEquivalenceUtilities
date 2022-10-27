BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

`Internal`$KernelRoot = DirectoryName @ $InputFileName;

`Internal`$MXFile =
    FileNameJoin @ {
        `Internal`$KernelRoot,
        ToString @ $SystemWordLength <> "Bit",
        "CodeEquivalenceUtilities.mx"
    };

Quiet[
    If[ FileExistsQ @ `Internal`$MXFile
        ,
        `Internal`$MX = True;
        Get @ `Internal`$MXFile
        ,
        `Internal`$MX = False;
        Scan[
            Get @ FileNameJoin @ { `Internal`$KernelRoot, # } &,
            {
                "Config.wl",
                "Utilities.wl",
                "CachedValues.wl",
                "EvaluationControl.wl",
                "Types.wl",
                "CanonicalForms/Common.wl",
                "CanonicalForms/Scope.wl",
                "CanonicalForms/Attributes.wl",
                "CanonicalForms/Graphics.wl",
                "CanonicalForms/Structural.wl",
                "CanonicalForms/Rules.wl",
                "Equivalence.wl",
                "Formatting.wl",
                "Legacy.wl"
            }
        ]
    ],
    General::shdw
];

Off[ Wolfram`CodeEquivalenceUtilities`EvaluateSafely::unsafe ];

EndPackage[ ];