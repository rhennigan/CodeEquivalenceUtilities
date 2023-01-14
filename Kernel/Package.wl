(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Needs[ "GeneralUtilities`" -> None ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Clear Previous Definitions*)
Unprotect[ "Wolfram`CodeEquivalenceUtilities`*"   ];
Unprotect[ "Wolfram`CodeEquivalenceUtilities`*`*" ];
ClearAll[ "Wolfram`CodeEquivalenceUtilities`*"    ];
ClearAll[ "Wolfram`CodeEquivalenceUtilities`*`*"  ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Load Files*)
<<`Symbols`;
<<`Definitions`;
<<`Config`;
<<`Utilities`;
<<`CachedValues`;
<<`EvaluationControl`;
<<`Types`;
<<`CanonicalForms`Common`;
<<`CanonicalForms`Scope`;
<<`CanonicalForms`Attributes`;
<<`CanonicalForms`Graphics`;
<<`CanonicalForms`Structural`;
<<`CanonicalForms`RuleSets`;
<<`CanonicalForms`Rules`;
<<`Equivalence`;
<<`Formatting`;
<<`Legacy`;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*End Package*)
EndPackage[ ];
