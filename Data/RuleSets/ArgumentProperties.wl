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
    "Usage" -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)
$orderlessLevel2Symbols       = Blend | Length | Max | Mean | Min | RandomChoice | RandomSample | Sort | Total;
$orderlessLevel2AnyArgSymbols = Complement | Intersection | Union | Graph;
$flatLevel2Symbols            = Min | Max;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
<|
    "Description" -> "Sort list arguments of functions that are not order-dependent",
    "Symbols"     :> { Sort, OrderedQ },
    "Rule"        ->
        (f: $orderlessLevel2Symbols)[ l: { a___ }, b___ ] /; ! OrderedQ @ HoldComplete @ a :>
            With[ { sorted = Sort @ TempHold @ a }, f[ { sorted }, b ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Sort list arguments of functions that are not order-dependent",
    "Symbols"     :> { Sort, OrderedQ },
    "Rule"        ->
        (f: $orderlessLevel2AnyArgSymbols)[ a___, l: { b___ }, c___ ] /; ! OrderedQ @ HoldComplete @ b :>
            With[ { sorted = Sort @ TempHold @ b }, f[ a, { sorted }, c ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Flatten arguments of functions that are not sensitive to nesting",
    "Rule"        ->
        f: $flatLevel2Symbols[ a_ ] /; ! UFlatQ @ a :> \
            With[ { flat = HFlatten[ f, HoldApply ] }, flat /; True ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Flatten arguments of functions that are not sensitive to nesting",
    "Rule"        -> (f: $flatLevel2Symbols)[ { a___ } ] :> f @ a
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)