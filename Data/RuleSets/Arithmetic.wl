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
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "EvaluateArithmetic",
    "Description" -> "Evaluate simple arithmetic expressions",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization", "CodeStyle" },
    "Symbols"     :> { NumericQ, RuleCondition },
    "Rule"        :>
        x_? HoldNumericQ /; SafeEvaluatedQ @ x === False && FreeQ[ HoldComplete @ x, $UnsafeSymbolPattern ] :>
            RuleCondition @ x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-enBgfX",
    "Description" -> "Rewrite Total using Plus",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Total, Plus },
    "Rule"        :> Total[ list_ ] :> Plus @@ list
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Plus-0",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Plus },
    "Rule"        :> Verbatim[ Plus ][ ] :> 0
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Plus-1",
    "Description" -> "Simplify Plus expressions with one argument",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Plus },
    "Rule"        :> Verbatim[ Plus ][ x_ ] :> x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Times-0",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Times },
    "Rule"        :> Verbatim[ Times ][ ] :> 1
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Times-1",
    "Description" -> "Simplify Times expressions with one argument",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Times },
    "Rule"        :> Verbatim[ Times ][ x_ ] :> x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Power-1",
    "Description" -> "Simplify Power expressions with one argument",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Power },
    "Rule"        :> Power[ x_ ] :> x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-3okbs",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Power },
    "Rule"        :> Except[ 0 | 0.0 ] ^ 0 :> 1
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-cBfVay",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Power },
    "Rule"        :> Except[ 0 | 0.0 ] ^ 0.0 :> 1.0
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-extZL7",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Power },
    "Rule"        :> (o: 1 | 1.0)^(-1 | -1.0) :> o
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-b02jrr",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Times },
    "Rule"        :> Verbatim[ Times ][ a___, 1, b___ ] :> a * b
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-ZbgcH",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Plus },
    "Rule"        :> Verbatim[ Plus ][ a___, 0, b___ ] :> a + b
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-dZBNGC",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Times, Real },
    "Rule"        :> Verbatim[ Times ][ OrderlessPatternSequence[ 1.0, _? RealTypeQ, a___ ] ] :> Times @ a
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-1r2rh",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Plus, Real },
    "Rule"        :> Verbatim[ Plus ][ OrderlessPatternSequence[ 0.0, _? RealTypeQ, a___ ] ] :> Plus @ a
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-ewmXga",
    "Description" -> "Transform using basic arithmetic rules",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { Times, Infinity, DirectedInfinity },
    "Rule"        :> Verbatim[ Times ][ -1, DirectedInfinity[ 1 ] ] :> DirectedInfinity[ -1 ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Arithmetic-JHpgn",
    "Description" -> "Distribute Times over Plus",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Times, Plus },
    "Rule"        :>
        Verbatim[ Times ][ x_, Verbatim[ Plus ][ a___ ] ] :>
            With[ { dist = Replace[ TempHold @ Plus @ a, e_ :> x * e, { 2 } ] }, dist /; True ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)