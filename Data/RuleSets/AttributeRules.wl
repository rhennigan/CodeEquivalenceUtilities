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
    "Usage"    -> { "EquivalenceTesting" },
    "Symbols"  :> { Attributes }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Flatten arguments of functions that have the Flat attribute",
    "Usage"       -> { Inherited, "Simplification", "Optimization" },
    "Symbols"     :> { Attributes, Flat },
    "Rule"        :>
        (f_Symbol? SymbolQ)[ a1___, f_[ a2___ ], a3___ ] /; MemberQ[ Attributes @ f, Flat ] :>
            f[ a1, a2, a3 ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Sort arguments of functions that have the Orderless attribute",
    "Symbols"     :> { Attributes, Orderless },
    "Rule"        :>
        (f_Symbol? SymbolQ)[ a___ ] /; MemberQ[ Attributes @ f, Orderless ] && ! OrderedQ @ Unevaluated @ { a } :>
            With[ { sorted = Sort @ TempHold @ a }, f @ sorted /; True ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Resolve single-argument forms of functions with the OneIdentity attribute",
    "Symbols"     :> { Attributes, OneIdentity },
    "Rule"        :>
        (f_Symbol? SymbolQ)[ x: Except[ _Symbol? SymbolQ, _? UAtomQ ] ] /; MemberQ[ Attributes @ f, OneIdentity ] :>
            x
|>

(**********************************************************************************************************************)
<|
    (* TODO: this doesn't quite belong in attribute rules *)
    "Description" -> "Resolve Apply when the specified function is not holding",
    "Symbols"     :> { Attributes, HoldFirst, HoldRest, HoldAll, HoldAllComplete, Apply },
    "Rule"        :>
        (f_? SymbolQ) @@ { a___ } /; And[
            NonHoldingQ @ f,
            FreeQ[ Unevaluated @ { a }, s_? SymbolQ /; ! FreeQ[ UpValueSymbols @ s, Apply, { 1 } ], { 1 } ]
        ] :>
            f @ a
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Remove order-modifying functions if the result is being passed to an Orderless function",
    "Usage"       -> { Inherited, "Simplification", "Optimization" },
    "Symbols"     :> { Attributes, HoldFirst, HoldRest, HoldAll, HoldAllComplete, Apply, Reverse },
    "Rule"        :>
        (f_? OrderlessQ) @@ (Reverse)[ h_[ a___ ] ] /; NonHoldingQ @ f :>
            f @@ h @ a
|>

(* TODO: combine with next rule *)

(**********************************************************************************************************************)
<|
    "Description" -> "Remove order-modifying functions if the result is being passed to an Orderless function",
    "Usage"       -> { Inherited, "Simplification", "Optimization" },
    "Symbols"     :> { Attributes, HoldFirst, HoldRest, HoldAll, HoldAllComplete, Apply, Sort },
    "Rule"        :>
        (f_? OrderlessQ) @@ (Sort)[ h_[ a___ ] ] /; NonHoldingQ @ f :>
            f @@ h @ a
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Thread listable functions over equal-length tables",
    "Usage"       -> { Inherited, "Simplification", "Optimization" },
    "Symbols"     :> { Attributes, Listable, Table },
    "Rule"        :>
        (f_? ListableQ)[ Table[ ei_, { i_, i1_, i2_, di_ } ], Table[ ej_, { j_, i1_, i2_, di_ } ] ] :>
            With[ { ej2 = TempHold @ ej /. HoldPattern @ Verbatim @ j :> i },
                Table[ f[ ei, ej2 ], { i, i1, i2, di } ] /; True
            ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Apply listable functions after taking parts of lists instead of before",
    "Usage"       -> { Inherited, "Optimization" },
    "Symbols"     :> { Attributes, Listable, Part, Span },
    "Rule"        :>
        (f_? ListableQ)[ list_? ListTypeQ ][[ s_Span ]] :>
            f @ list[[ s ]]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)