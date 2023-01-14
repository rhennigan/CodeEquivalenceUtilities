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
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Convert SetDelayed to a typed expression",
    "Symbols"     :> { SetDelayed },
    "Rule"        :> sd_SetDelayed /; ! TypedDefinitionQ @ sd :> TrEval @ ToTypedBinding @ sd
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Convert RuleDelayed to a typed expression",
    "Symbols"     :> { RuleDelayed },
    "Rule"        :> rd_RuleDelayed /; ! TypedDefinitionQ @ rd :> TrEval @ ToTypedBinding @ rd
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)