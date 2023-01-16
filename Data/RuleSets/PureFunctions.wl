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
    "Description" -> "Standardize pure functions",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Function }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(* :!CodeAnalysis::Disable::UnscopedObjectError:: *)

HoldComplete[

    (fun : Function[ expr_ ])[ ___ ] /; FreeQ[ fun, _Slot | _SlotSequence ] :>
      expr,



    Function[x : Except[ Null, _? SymbolQ ], expr_] /;
      ! FreeQ[ HoldComplete @ expr, HoldPattern @ x ] :>
        WithHolding[
            {
                new = TempHold @ Function @ expr /. HoldPattern @ x -> Slot @ 1
            },
            new
            ]
    ,

    Function[ expr_ ][ n_? SafeEvaluatedQ ] :>
      TrEval @ Function[ TempHold @ expr ][ n ]
    ,

    Composition[ a___, Function[ f_ ], Function[ g_ ], b___ ] /;
      FreeQ[ HoldComplete[ f, g ], HoldPattern[ Slot ][ Except @ 1 ] ] :>
        With[ { composed = TempHold @ f /. HoldPattern[ Slot ][ 1 ] :> g },
            Composition[ a, Function @ composed, b ] /; True
            ]
    ,

    Function[ # ][ a_ ] :> a
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)