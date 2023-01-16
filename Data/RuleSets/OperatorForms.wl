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
    "Description" -> "Transform operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

$$forwardOps = HoldPattern @ Alternatives[
    Apply,
    Map,
    System`MapApply
];

$$reverseOps = HoldPattern @ Alternatives[
    Replace,
    Select,
    Append,
    Prepend,
    ReplacePart,
    SelectFirst,
    FirstCase,
    Cases,
    DeleteCases,
    Position,
    Extract,
    Delete,
    AnyTrue,
    AllTrue,
    NoneTrue,
    SortBy,
    MaximalBy,
    MinimalBy,
    DeleteDuplicatesBy,
    CurryApplied
];

$$reverseOps2 = HoldPattern @ Alternatives[
    Insert
];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
Inline[ { $$forwardOps, $$reverseOps, $$reverseOps2 }, HoldComplete[
    (h:$$forwardOps)[ a_ ][ b_ ] :> h[ a, b ],
    (h:$$reverseOps)[ a_ ][ b_ ] :> h[ b, a ],
    (h:$$reverseOps2)[ a_, b_ ][ c_ ] :> h[ c, a, b ],
    (h:$$forwardOps)[ a_ ] :> Function[ h[ a, # ] ],
    (h:$$reverseOps)[ a_ ] :> Function[ h[ #, a ] ],
    (h:$$reverseOps2)[ a_, b_ ] :> Function[ h[ #, a, b ] ]
] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)