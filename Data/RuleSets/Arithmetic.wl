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
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)
$zero   =  0 |  0.0;
$posOne =  1 |  1.0;
$negOne = -1 | -1.0;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "EvaluateArithmetic",
    "Description" -> "Evaluate simple arithmetic expressions",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization", "CodeStyle" },
    "Symbols"     :> { },
    "Rule"        :>
        x_? HoldNumericQ /; SafeEvaluatedQ @ x === False && FreeQ[ HoldComplete @ x, $UnsafeSymbolPattern ] :>
            RuleCondition @ x
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Rewrite Total using Plus",
    "Symbols"     :> { Total, Plus },
    "Rule"        :> Total[ list_ ] :> Plus @@ list
|>

(**********************************************************************************************************************)
Inline[ { $zero, $posOne, $negOne }, HoldComplete[

    Verbatim[ Plus  ][ ] :> 0,
    Verbatim[ Times ][ ] :> 1,

    Power[ x_ ] :> x,
    Power[ Except[ $zero ], 0   ] :> 1,
    Power[ Except[ $zero ], 0.0 ] :> 1.0,
    Power[ o: $posOne, $negOne ] :> o,

    Verbatim[ Times ][ a___, 1, b___ ] :> Times[ a, b ],
    Verbatim[ Plus  ][ a___, 0, b___ ] :> Plus[  a, b ],

    Verbatim[ Times ][ OrderlessPatternSequence[ 1.0, _? RealTypeQ, a___ ] ] :> Times @ a,
    Verbatim[ Plus  ][ OrderlessPatternSequence[ 0.0, _? RealTypeQ, a___ ] ] :>  Plus @ a,

    Verbatim[ Times ][ -1, DirectedInfinity[ 1 ] ] :> DirectedInfinity[ -1 ],

    (* distribute Times over Plus *)
    Verbatim[ Times ][ x_, Verbatim[ Plus ][ a___ ] ] :>
      With[
          {
              dist = Replace[ TempHold @ Plus @ a,
                              e_ :> Times[ x, e ],
                              { 2 }
                     ]
          },
          dist /; True
      ]

] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)