(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Default Values*)
SetRuleDefaults @ <|
    "Description" -> "Transform image processing operations into a form that is equivalent to the original.",
    "Usage"       -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*bitMatrixQ*)
bitMatrixQ // ClearAll;
bitMatrixQ // Attributes = { HoldAllComplete };
bitMatrixQ[ m_ ] := MatrixQ[ Unevaluated @ m, bitQ ];

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*bitQ*)
bitQ // ClearAll;
bitQ // Attributes = { HoldAllComplete };
bitQ[ 0|1 ] := True;
bitQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*combineDilationKernels*)
combineDilationKernels // ClearAll;
combineDilationKernels[ m1_, m2_ ] :=
    Module[ { a1, a2 },
        a1 = arrayUnpad @ m1;
        a2 = arrayUnpad @ m2;
        SelectFirst[ HoldComplete[ Dot[ a1, a2 ], Dot[ a2, a1 ] ], bitMatrixQ, $Failed ]
    ];

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*arrayUnpad*)
arrayUnpad // ClearAll;
arrayUnpad[ m_ ] := Quiet @ arrayUnpadCF @ m;

arrayUnpadCF := arrayUnpadCF =
    Compile[ { { m, _Integer, 2 } },
        Block[ { a, b, c, d, i, len, top, bottom, left, right },
            a = Abs @ m;
            b = Reverse @ a;
            c = Transpose @ a;
            d = Reverse @ c;

            { top, bottom, left, right } = Table[
                i = 0;
                len = Length @ x;
                While[ ++i < len && Total @ x[[ i ]] === 0 ];
                i,
                { x, { a, b, c, d } }
            ];

            m[[ top;;-bottom, left;;-right ]]
        ]
    ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
HoldComplete[
    (f: Erosion | Dilation)[ img_, r_? IntTypeQ ] :> f[ img, BoxMatrix @ r ],
    (f: Erosion | Dilation)[ (f: Erosion | Dilation)[ img_, m1_? bitMatrixQ ], m2_? bitMatrixQ ] :>
        With[ { m = combineDilationKernels[ m1, m2 ] }, f[ img, m ] /; bitMatrixQ @ m ]
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)