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
    "Description" -> "Standardize Table scoping",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Table }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
<|
    "Rule" :>
        Table[ exp_, { i_? SymbolQ /; ! LocalContextQ @ i, ii__ } ] :>
            With[ { j = NewLocalSymbol[ ] },
                  { held = ReplaceRepeated[ TempHold @ exp, HoldPattern @ i :> j ] },
                  Table[ held, { j, ii } ] /; True
            ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)