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
    System`Apply,
    System`ApplyReaction,
    System`AssociationMap,
    System`Map,
    System`MapApply,
    System`MapThread,
    System`Scan
];

$$reverseOps = HoldPattern @ Alternatives[
    System`AllTrue,
    System`Between,
    System`BioSequenceModify,
    System`AnyTrue,
    System`Append,
    System`Cases,
    System`CurryApplied,
    System`Delete,
    System`DeleteCases,
    System`DeleteDuplicatesBy,
    System`Extract,
    System`FirstCase,
    System`MatchQ,
    System`MaximalBy,
    System`MinimalBy,
    System`NoneTrue,
    System`Position,
    System`Prepend,
    System`Replace,
    System`ReplacePart,
    System`Select,
    System`SelectFirst,
    System`SortBy,
    System`StringCases,
    System`StringDelete,
    System`StringMatchQ,
    System`StringPosition
];

$$forwardOps2 = HoldPattern @ Alternatives[
    System`MapAt
];

$$reverseOps2 = HoldPattern @ Alternatives[
    System`Insert,
    System`ReplaceAt
];

(* TODO: ensure all of these are in the appropriate list
    * AllTrue
    * AlphabeticOrder (weird)
    * AnyTrue
    * Append
    * Apply
    * ApplyReaction
    * AssociationMap
    * Between
    * BioSequenceModify
    * Cases
    * CellularAutomaton
    * ContainsAll
    * ContainsAny
    * ContainsExactly
    * ContainsNone
    * ContainsOnly
    * Count
    * CountDistinctBy
    * CountsBy
    * Curry (weird)
    * CurryApplied (weird)
    * DateSelect
    * Delete
    * DeleteCases
    * DeleteDuplicatesBy
    * EqualTo
    * ExternalEvaluate
    * Extract
    * FeatureDistance
    * FirstCase
    * FirstPosition
    * FlattenAt
    * Fold
    * FoldList
    * FoldWhile
    * FoldWhileList
    * FreeQ
    * GenerateDigitalSignature
    * GenerateFileSignature
    * GeoWithinQ
    * GreaterEqualThan
    * GreaterThan
    * GroupBy
    * Insert
    * IntersectionQ
    * IntervalMemberQ
    * KeyDrop
    * KeyExistsQ
    * KeyFreeQ
    * KeyMap
    * KeyMemberQ
    * KeySelect
    * KeySortBy
    * KeyTake
    * KeyValueMap
    * LessEqualThan
    * LessThan
    * LexicographicOrder
    * Lookup
    * Map
    * MapApply
    * MapAt
    * MapIndexed
    * MapThread
    * MatchQ
    * MaximalBy
    * MemberQ
    * Merge
    * MinimalBy
    * MoleculeContainsQ
    * MoleculeFreeQ
    * MoleculeMatchQ
    * MoleculeModify
    * MoleculeSubstructureCount
    * NearestTo (weird)
    * NoneTrue
    * OperatorApplied (weird)
    * Position
    * Prepend
    * Replace
    * ReplaceAll
    * ReplaceAt
    * ReplaceList
    * ReplacePart
    * ReplaceRepeated
    * ReverseSortBy
    * SameAs
    * Scan
    * Select
    * SelectFirst
    * SequenceReplace
    * SortBy
    * SpeakerMatchQ
    * StringCases
    * StringContainsQ
    * StringDelete
    * StringEndsQ
    * StringFreeQ
    * StringMatchQ
    * StringPosition
    * StringReplace
    * StringReplacePart
    * StringStartsQ
    * SubsetMap
    * SubsetReplace
    * SubstitutionSystem
    * TakeLargest
    * TakeLargestBy
    * TakeSmallest
    * TakeSmallestBy
    * TreeCases
    * TreeCount
    * TreeDelete
    * TreeExtract
    * TreeFold
    * TreeInsert
    * TreeLevel
    * TreeMap
    * TreeMapAt
    * TreePosition
    * TreeReplacePart
    * TreeScan
    * TreeSelect
    * TuringMachine
    * UnequalTo
    * VerifyDigitalSignature
    * VerifyFileSignature
*)

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Forward-1",
    "Description" -> "Transform applied forward operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$forwardOps },
    "Rule"        :> (h: $$forwardOps)[ a_ ][ b_ ] :> h[ a, b ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Reverse-1",
    "Description" -> "Transform applied reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps },
    "Rule"        :> (h: $$reverseOps)[ a_ ][ b_ ] :> h[ b, a ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Forward-2",
    "Description" -> "Transform applied forward operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$forwardOps2 },
    "Rule"        :> (h: $$forwardOps2)[ a_, b_ ][ c_ ] :> h[ a, c, b ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Reverse-2",
    "Description" -> "Transform applied reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps2 },
    "Rule"        :> (h: $$reverseOps2)[ a_, b_ ][ c_ ] :> h[ c, a, b ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Forward-Function-1",
    "Description" -> "Transform forward operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$forwardOps },
    "Rule"        :> (h: $$forwardOps)[ a_ ] :> (h[ a, #1 ] &)
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Reverse-Function-1",
    "Description" -> "Transform reverse operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$reverseOps },
    "Rule"        :> (h: $$reverseOps)[ a_ ] :> (h[ #1, a ] &)
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Forward-Function-2",
    "Description" -> "Transform forward operator forms into canonical forms",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Inline"      :> { $$forwardOps2 },
    "Rule"        :> (h: $$forwardOps2)[ a_, b_ ] :> (h[ a, #1, b ] &)
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "OperatorForms-Reverse-Function-2",
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