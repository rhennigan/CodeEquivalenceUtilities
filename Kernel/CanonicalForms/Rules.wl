(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* TODO:
    Tuples -> Outer
*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
$TransformationRules;
TransformHold;
TransformRelease;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Declarations*)
ListableQ;
TypedSymbol;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TransformHold*)
TransformHold // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TransformRelease*)
TransformRelease // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$TransformationRules*)
$intAtom = (_Integer | TypedSymbol[ _, Verbatim[ _Integer ] ]);

$intType = Inline[ $intAtom,
    HoldPattern @ Alternatives[
        (Plus | Times)[ $intAtom .. ],
        $intAtom,
        RandomValue[ _DiscreteUniformDistribution ]
    ]
];

$reaAtom = (_Real | TypedSymbol[ _, Verbatim[ _Real ] ]);

$reaType = Inline[ $reaAtom,
    HoldPattern[ (Plus | Times)[
        $intAtom..., $reaAtom .., $intAtom...
    ] | $reaAtom
    ]
];



Int // ClearAll;
Int // Attributes = { HoldAllComplete };
Int[ i_ ] := TypedSymbol[ i, _Integer ];


Rea // ClearAll;
Rea // Attributes = { HoldAllComplete };
Rea[ i_ ] := TypedSymbol[ i, _Real ];


Str // ClearAll;
Str // Attributes = { HoldAllComplete };
Str[ i_ ] := TypedSymbol[ i, _String ];



(******************************************************************************)



simplify // ClearAll;
simplify // Attributes = {HoldAllComplete};
simplify // Options = {"Timeout" -> 0.3};

simplifiedQ // ClearAll;
simplifiedQ // Attributes = { HoldAllComplete };

simplifiedQ[ expr_ /; ! SafeExpressionQ @ expr ] := True;
simplifiedQ[ ___ ] := False;

simplify[expr_, opts : OptionsPattern[]] :=
  With[{simplified =
    TimeConstrained[Expand@Simplify@expr, OptionValue@"Timeout",
        Expand@expr]},
      If[SameQ[HoldComplete@simplified , HoldComplete@expr],
      simplifiedQ[Verbatim[expr]] = True];
  simplified];



(******************************************************************************)



$unrollLimit = 20;

integerRules = HoldComplete[
    FromDigits[IntegerDigits[n_]] :> n
];







associationQ // Attributes = { HoldAllComplete };
associationQ[ Association[ a___ ] ] :=
    AllTrue[ Unevaluated @ { a }, associationRulesQ ];

associationRulesQ // Attributes = { HoldAllComplete };
associationRulesQ[ (Rule|RuleDelayed)[ _, _ ] ] := True;
associationRulesQ[ a_List ] := AllTrue[ Unevaluated @ a, associationRulesQ ];
associationRulesQ[ a_Association ] := associationQ @ a;
associationRulesQ[ ___ ] := False;


atomQ // Attributes = { HoldAllComplete };
atomQ[ _? UAtomQ       ] := True;
atomQ[ _? IntTypeQ     ] := True;
atomQ[ _? RealTypeQ    ] := True;
atomQ[ _? StringTypeQ  ] := True;
atomQ[ _? associationQ ] := True;
atomQ[ ___             ] := False;


inertAtomQ // Attributes = { HoldAllComplete };
inertAtomQ[ _Symbol ] := False;
inertAtomQ[ expr_   ] := AtomQ @ Unevaluated @ expr;
inertAtomQ[ ___     ] := False;


(* TODO:
    If[ False|True, ... ],
    StringQ[ _? StringTypeQ ] -> True,
    IntegerQ[ _? IntTypeQ ] -> True,
*)

urlQ // ClearAll;
urlQ // Attributes = { HoldAllComplete };

urlQ[ url_String /; StringQ @ Unevaluated @ url ] :=
  StringStartsQ[ url, "http://"|"https://" ];

urlQ[ _URLBuild ] :=
  True;

urlQ[ ___ ] :=
  False;


expandURL // ClearAll;

expandURL[ url_String ] :=
  expandURL[ url, 1 ];

expandURL[ url_String, iter_ ] :=
  Catch[ FixedPoint[ iExpandURL, url, iter ], $tag ];

iExpandURL[ url_String ] :=
  If[ StringStartsQ[ url, $CloudBase ],
      Throw[ url, $tag ],
      Cached @ Quiet @ Replace[
          Check[ URLExpand @ url, url ],
          Except[ _String? StringQ ] :> Throw[ url, $tag ]
      ]
  ];



expandedQ // ClearAll;
expandedQ // Attributes = { HoldAllComplete };
expandedQ[ e_ ] := (expandedQ[ e ] = True; False);
expandedQ[ ___ ] := False;

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
expandBlocked // ClearAll;
expandBlocked // Attributes = { HoldAllComplete };
expandBlocked[ e_ ] :=
    Block[ { Echo },
        Protect @ Echo;
        TempHold @@ { Expand @ e }
    ];
(* :!CodeAnalysis::EndBlock:: *)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SymbolVersionTooNew:: *)

(* :!CodeAnalysis::EndBlock:: *)



LoadTransformationRules[ All ];

$TransformationRules = GetRules[ "EquivalenceTesting" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)