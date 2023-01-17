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
    "Name"        -> "EntityPropertyString",
    "Description" -> "Transform Entity property SubValues into an equivalent EntityProperty form",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Entity, EntityProperty },
    "Rule"        :> Entity[ s_String, f_ ][ property_String ] :> Entity[ s, f ][ EntityProperty[ s, property ] ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "EntitySubValuesToEntityValue",
    "Description" -> "Transform Entity/EntityClass SubValues into an equivalent EntityValue form",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Entity, EntityClass, EntityValue },
    "Rule"        :> (e: _Entity|_EntityClass)[ a_ ] :> EntityValue[ e, a ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "EntityPropertyStringToEntityProperty",
    "Description" -> "Transform entity property strings into an equivalent EntityProperty form",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Entity, EntityValue, EntityProperty },
    "Rule"        :>
        EntityValue[ Entity[ a_String, b___ ], c_String ] :>
            EntityValue[ Entity[ a, b ], EntityProperty[ a, c ] ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)