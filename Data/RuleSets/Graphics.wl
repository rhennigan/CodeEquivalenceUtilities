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

$gfx = HoldPattern @ Alternatives[
    Graphics,
    Graphics3D,
    Canonical @ Graphics,
    Canonical @ Graphics3D
];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "Graphics-cN4AkJ",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { ListLinePlot, Joined, ListPlot },
    "Rule"        :>
        ListLinePlot[ expr_, opts___ ] /; FreeQ[ HoldComplete @ opts, HoldPattern[ Joined -> _ ], { 1 } ] :>
            ListPlot[ expr, Joined -> True, opts ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Graphics-cFDWz6",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Graphics, Graphics3D, Style },
    "Inline"      :> { $gfx },
    "Rule"        :>
        (gfx: $gfx)[ p_, opts___ ] /; ! FreeQ[ HoldComplete @ p, Style[ _, _ ] ] :>
            With[ { held = TempHold @ p /. Style[ a_, b_ ] :> { b, a } }, gfx[ held, opts ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Graphics-cL6Lxu",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Graphics, Graphics3D, List },
    "Inline"      :> { $gfx },
    "Rule"        :>
        (gfx: $gfx)[ { a___, b_List, c___ }, opts___ ] :>
            With[ { held = { Flatten[ TempHold @@@ TempHold[ a, b, c ] ] } }, gfx[ held, opts ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Graphics-cV75xt",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Graphics, Graphics3D, Show },
    "Inline"      :> { $gfx },
    "Rule"        :> Show[ a: $gfx[ ___ ], b: $gfx[ ___ ].., opts: OptionsPattern[ ] ] :> Show[ { a, b }, opts ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)