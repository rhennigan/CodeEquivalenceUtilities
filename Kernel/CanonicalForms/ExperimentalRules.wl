(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Priority Rules*)


(******************************************************************************)
<|
    "Category"    -> "PriorityRules",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Simplify rounding of integers",
    "Symbols"     :> { Floor, Ceiling, Round, Integer },
    "Rule"        :>
        (Floor|Ceiling|Round)[ i_? IntTypeQ ]
        :>
        i
|>
(******************************************************************************)
<|
    "Category"    -> "PriorityRules",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Expand numeric expressions",
    "Symbols"     :> { NumericQ, NumericFunction, Expand },
    "Rule"        :>
        x_? HoldNumericQ /; ! expandedQ @ x
        :>
        RuleCondition @ expandBlocked @ x
|>
(******************************************************************************)
<|
    "Category"    -> "PriorityRules",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Simplify numeric expressions",
    "Symbols"     :> { NumericQ, NumericFunction, Simplify },
    "Rule"        :>
        x_? HoldNumericQ /; ! simplifiedQ @ x
        :>
        RuleCondition @ simplify @ x
|>
(******************************************************************************)
<|
    "Category"    -> "PriorityRules",
    "Usage"       -> { "CanonicalForm" },
    "Internal"    -> True,
    "Description" -> "Avoid unnecessary canonicalization of inner contents of large graphics expressions",
    "Symbols"     :> { Graphics, Graphics3D },
    "Rule"        :>
        (gfx: Graphics|Graphics3D)[ a_, b___ ]
        :>
        With[ { c = TempHold @ a /. { ints__Integer } /;
                        Length @ HoldComplete @ ints >= 4 :>
                            { Canonical @ ints }
              },
              Canonical[ gfx ][ c, b ] /; True
        ]
|>


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Attributes*)


(******************************************************************************)
<|
    "Category"    -> "Attributes",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Flatten arguments of functions that have the Flat attribute.",
    "Symbols"     :> { Flat, Flatten },
    "Rule"        :>
        (f_? FlatQ)[ a___, f_[ b___ ], c___ ]
        :>
        f[ a, b, c ]
|>
(******************************************************************************)
<|
    "Category"    -> "Attributes",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Simplify OneIdentity expressions.",
    "Symbols"     :> { OneIdentity, AtomQ },
    "Rule"        :>
        (f_? OneIdentityQ)[ x: Except[ _? SymbolQ, _? UAtomQ ] ]
        :>
        x
|>
(******************************************************************************)
<|
    "Category"    -> "Attributes",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Sort arguments of functions that have the Orderless attribute.",
    "Symbols"     :> { Orderless, OrderedQ, Sort },
    "Rule"        :>
        (f_? OrderlessQ)[ a___ ] /; ! OrderedQ @ Unevaluated @ { a }
        :>
        With[ { sorted = Sort @ TempHold @ a }, f @ sorted /; True ]
|>
(******************************************************************************)
<|
    "Category"    -> "Attributes",
    "Usage"       -> { "CanonicalForm", "Simplification", "Optimization" },
    "Description" -> "Simplify Apply where appropriate.",
    "Symbols"     :> { Apply, $HoldingAttributes },
    "Rule"        :>
        f_? SymbolQ @@ { a___ } /; And[
            NonHoldingQ @ f,
            FreeQ[ Unevaluated @ { a },
                   s_? SymbolQ /; ! FreeQ[ UpValueSymbols @ s, Apply, { 1 } ],
                   { 1 }
            ]
        ]
        :>
        f @ a
|>


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tables*)


(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "CanonicalForm" },
    "Description" -> "Convert tables with multiple iterators to nested tables.",
    "Rule"        :>
        Table[ exp_, ii__, jj_ ]
        :>
        Table[ Table[ exp, jj ], ii ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "Simplification" },
    "Description" -> "Convert nested tables to tables with multiple iterators.",
    "Rule"        :>
        Table[ Table[ exp_, jj_ ], ii__ ]
        :>
        Table[ exp, ii, jj ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "CanonicalForm" },
    "Description" -> "Expand numeric table iterators to their full canonical form.",
    "Rule"        :>
        Table[ exp_, imax_? HoldNumericQ ]
        :>
        Table[ exp, { i, 1, imax, 1 } ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "CanonicalForm" },
    "Description" -> "Expand numeric table iterators to their full canonical form.",
    "Rule"        :>
        Table[ exp_, { i_, imax_? HoldNumericQ } ]
        :>
        Table[ exp, { i, 1, imax, 1 } ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "Simplification" },
    "Description" -> "Simplify numeric iterators.",
    "Rule"        :>
        Table[ exp_, ii___, { j, 1, jmax_, 1 }, kk___ ]
        :>
        Table[ exp, ii, { j, jmax }, kk ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "CanonicalForm" },
    "Description" -> "Expand numeric table iterators to their full canonical form.",
    "Rule"        :>
        Table[ exp_, { imax_? HoldNumericQ } ]
        :>
        Table[ exp, { i, 1, imax, 1 } ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "CanonicalForm" },
    "Description" -> "Expand numeric table iterators to their full canonical form.",
    "Rule"        :>
        Table[ exp_, { i_, imin_, imax_ } ]
        :>
        Table[ exp, { i, imin, imax, 1 } ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "Simplification" },
    "Description" -> "Simplify numeric iterators.",
    "Rule"        :>
        Table[ exp_, ii___, { j_, jmin_, jmax_, 1 }, kk___ ]
        :>
        Table[ exp, ii, { j, jmin, jmax }, kk ]
|>
(******************************************************************************)
<|
    "Category"    -> "Tables",
    "Usage"       -> { "Simplification" },
    "Description" -> "Simplify numeric iterators.",
    "Rule"        :>
        Table[ exp_, ii___, { j_, 1, jmax_ }, kk___ ]
        :>
        Table[ exp, ii, { j, jmax }, kk ]
|>
(******************************************************************************)
<|
    "Category"     -> "General",
    "Usage"        -> { "CanonicalForm", "Simplification", "Optimization" },
    "Strict"       -> False,
    "Experimental" -> True,
    "Description"  -> "Evaluate arithmetic expressions.",
    "Rule"         :>
        x_? HoldNumericQ /; SafeEvaluatedQ @ x === False
        :>
        RuleCondition @ x
|>


(* :!CodeAnalysis::EndBlock:: *)