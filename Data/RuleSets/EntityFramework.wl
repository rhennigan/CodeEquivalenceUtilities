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
    "Description" -> "Transform entity operations into an equivalent form.",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Entity }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
HoldComplete[
    Entity[ s_String, f_ ][ "Image" ] :> Entity[ s, f ][ EntityProperty[ s, "Image" ] ],
    (e: _Entity | _EntityClass)[ a_ ] :> EntityValue[ e, a ],
    EntityValue[ Entity[ a_String, b___ ], c_String ] :> EntityValue[ Entity[ a, b ], EntityProperty[ a, c ] ]
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)