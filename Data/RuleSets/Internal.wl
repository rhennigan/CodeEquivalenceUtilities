(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)
(* :!CodeAnalysis::Disable::SymbolVersionTooNew:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Default Values*)
SetRuleDefaults @ <|
    "Usage"    -> { "EquivalenceTesting", "Optimization", "Scoping", "Simplification", "TypeSystem" },
    "Priority" -> -1000
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Strip TempHold wrappers",
    "Symbols"     :> { TempHold },
    "Priority"    -> -Infinity,
    "Internal"    -> True,
    "Rule"        :> (f_)[ a1___, TempHold[ a2___ ], a3___ ] :> f[ a1, a2, a3 ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Prevent transformation of TransformHold contents",
    "Symbols"     :> { TransformHold },
    "Priority"    -> -10000,
    "Internal"    -> True,
    "Rule"        :> TransformHold[ a___ ] :> TransformHold @ a
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Handle PacletSymbol wrappers for pasted documentation examples",
    "Symbols"     :> { PacletSymbol },
    "Internal"    -> True,
    "Rule"        :>
        Verbatim[ System`PacletSymbol ][ "Wolfram/CodeEquivalenceUtilities", sym_String ] :>
            RuleCondition @
                If[ StringContainsQ[ sym, "`" ],
                    ToExpression[ sym, InputForm, TempHold ],
                    ToExpression[
                        "Wolfram`CodeEquivalenceUtilities`"<>sym,
                        InputForm,
                        TempHold
                    ]
                ]
|>

(**********************************************************************************************************************)
(* TODO: combine these rules: *)
<|
    "Description" -> "Simplify trivial integer operations",
    "Usage"       -> { "EquivalenceTesting", "Optimization", "Simplification" },
    "Symbols"     :> { Ceiling },
    "Rule"        :> Ceiling[ i_? IntTypeQ ] :> i
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Simplify trivial integer operations",
    "Usage"       -> { "EquivalenceTesting", "Optimization", "Simplification" },
    "Symbols"     :> { Floor },
    "Rule"        :> Floor[ i_? IntTypeQ ] :> i
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Simplify trivial integer operations",
    "Usage"       -> { "EquivalenceTesting", "Optimization", "Simplification" },
    "Symbols"     :> { Round },
    "Rule"        :> Round[ i_? IntTypeQ ] :> i
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Hide large integer lists in graphics from transformations",
    "Usage"       -> { "EquivalenceTesting", "Optimization" },
    "Symbols"     :> { Graphics, Graphics3D },
    "Internal"    -> True,
    "Rule"        :>
        (gfx: Graphics|Graphics3D)[ a_, b___ ] :>
            WithHolding[
                {
                    c = TempHold @ a /.
                        { ints__Integer } /; Length @ HoldComplete @ ints >= 4 :>
                        { Canonical @ ints }
                },
                Canonical[ gfx ][ c, b ]
            ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Flatten TypedSymbol expressions",
    "Symbols"     :> { TypedSymbol },
    "Internal"    -> True,
    "Priority"    -> 9000,
    "Rule"        :> TypedSymbol[ TypedSymbol[ s_, t_ ], t_ ] :> TypedSymbol[ s, t ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Generate a cycle detection warning for testing",
    "Symbols"     :> { },
    "Internal"    -> True,
    "Priority"    -> 10000,
    "Rule"        :>
        Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ a_ ] :>
            Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ a, a ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Generate a cycle detection warning for testing",
    "Symbols"     :> { },
    "Internal"    -> True,
    "Priority"    -> 10000,
    "Rule"        :>
        Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ a_, a_ ] :>
            Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ a ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)