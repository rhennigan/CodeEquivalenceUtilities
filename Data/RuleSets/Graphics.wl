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
Inline[ $gfx, HoldComplete[

    ListLinePlot[ expr_, opts___ ] /;
      FreeQ[ HoldComplete @ opts,
             HoldPattern[ Joined -> _ ],
             { 1 }
      ] :> ListPlot[ expr, Joined -> True, opts ]
    ,

    (gfx: $gfx)[ p_, opts___ ] /; ! FreeQ[ HoldComplete @ p, Style[ _, _ ] ] :>
      With[
          {
              held = TempHold @ p /. Style[ a_, b_ ] :> { b, a }
          },
          gfx[ held, opts ] /; True
      ]
    ,

    (gfx: $gfx)[ { a___, b_List, c___ }, opts___ ] :>
      With[
          {
              held = { Flatten[ TempHold @@@ TempHold[ a, b, c ] ] }
          },
          gfx[ held, opts ] /; True
      ]
    ,

    Show[ a: $gfx[ ___ ],
          b: $gfx[ ___ ]..,
          opts: OptionsPattern[ ]
    ] :>
      Show[ { a, b }, opts ]
] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)