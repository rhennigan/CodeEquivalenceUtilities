(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)
(* :!CodeAnalysis::Disable::UnscopedObjectError:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Default Values*)
SetRuleDefaults @ <|
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "PureFunctions-cvKAmS",
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function, Slot, SlotSequence },
    "Rule"        :> (fun: (expr_ &))[ ___ ] /; FreeQ[ fun, _Slot | _SlotSequence ] :> expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "PureFunctions-Bn4go",
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function, Slot },
    "Rule"        :>
        (Function[ x: Except[ Null, _? SymbolQ ], expr_ ]) /; ! FreeQ[ HoldComplete @ expr, HoldPattern @ x ] :>
            With[ { new = TempHold[ expr & ] /. HoldPattern @ x -> #1 }, new /; True ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "PureFunctions-bEFuC5",
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function },
    "Rule"        :> (expr_ &)[ n_? SafeEvaluatedQ ] :> With[ { eval$ = (TempHold @ expr &)[ n ] }, eval$ /; True ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "PureFunctions-eP0V8y",
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function, Composition, Slot },
    "Rule"        :>
        Composition[ a___, f_ &, g_ &, b___ ] /; FreeQ[ HoldComplete[ f, g ], HoldPattern[ Slot ][ Except[ 1 ] ] ] :>
            With[ { composed = TempHold @ f /. HoldPattern[ Slot ][ 1 ] :> g },
                Composition[ a, composed &, b ] /; True
            ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "PureFunctions-b4Tfzu",
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function, Slot },
    "Rule"        :> (#1 &)[ a_ ] :> a
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)