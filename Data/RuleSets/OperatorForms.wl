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

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-QxjJX",
    "Description" -> "Transform forward operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$forwardOps },
    "Rule"        :> (h: $$forwardOps)[ a_ ][ b_ ] :> h[ a, b ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-eyvTQ1",
    "Description" -> "Transform reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps },
    "Rule"        :> (h: $$reverseOps)[ a_ ][ b_ ] :> h[ b, a ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-dFqVpP",
    "Description" -> "Transform reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps2 },
    "Rule"        :> (h: $$reverseOps2)[ a_, b_ ][ c_ ] :> h[ c, a, b ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-eEcR9I",
    "Description" -> "Transform forward operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$forwardOps },
    "Rule"        :> (h: $$forwardOps)[ a_ ] :> (h[ a, #1 ] &)
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-cuHSzf",
    "Description" -> "Transform reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps },
    "Rule"        :> (h: $$reverseOps)[ a_ ] :> (h[ #1, a ] &)
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-zZeRC",
    "Description" -> "Transform reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps2 },
    "Rule"        :> (h: $$reverseOps2)[ a_, b_ ] :> (h[ #1, a, b ] &)
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)