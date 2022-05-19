BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
EndPackage[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Load package*)
Wolfram`CodeEquivalenceUtilities`Internal`$MXFile =
    FileNameJoin @ {
        DirectoryName @ $InputFileName,
        ToString @ $SystemWordLength <> "Bit",
        "CodeEquivalenceUtilities.mx"
    };

If[ FileExistsQ[ Wolfram`CodeEquivalenceUtilities`Internal`$MXFile ]
    ,
    Wolfram`CodeEquivalenceUtilities`Internal`$MX = True;
    Get[ Wolfram`CodeEquivalenceUtilities`Internal`$MXFile ]
    ,
    Wolfram`CodeEquivalenceUtilities`Internal`$MX = False;
    Quiet[
        Block[ { $ContextPath },
            Get[ "Wolfram`CodeEquivalenceUtilities`Config`"                    ];
            Get[ "Wolfram`CodeEquivalenceUtilities`Utilities`"                 ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CachedValues`"              ];
            Get[ "Wolfram`CodeEquivalenceUtilities`EvaluationControl`"         ];
            Get[ "Wolfram`CodeEquivalenceUtilities`Types`"                     ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`"     ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`"      ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Attributes`" ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Graphics`"   ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Structural`" ];
            Get[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Rules`"      ];
            Get[ "Wolfram`CodeEquivalenceUtilities`Equivalence`"               ];
            Get[ "Wolfram`CodeEquivalenceUtilities`Formatting`"                ];
            Get[ "Wolfram`CodeEquivalenceUtilities`Legacy`"                    ];
        ],
        General::shdw
    ]
];
