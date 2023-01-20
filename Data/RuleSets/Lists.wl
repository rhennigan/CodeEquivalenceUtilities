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
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)
$assocOuter = HoldPattern[ Reverse | RandomSample ];
$assocInner = HoldPattern[ Reverse | Sort | RandomSample ];
$joinable   = HoldPattern[ List | Association ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*simpleListQ*)
simpleListQ // ClearAll;
simpleListQ // Attributes = { HoldAllComplete };
simpleListQ[ { (_String? UStringQ | _Integer? UIntegerQ)... } ] := True;
simpleListQ[ { other___ } ] := AllTrue[ HoldComplete @ other, simpleListQ ];
simpleListQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-emNz9h",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Reverse },
    "Rule"        :> Reverse @ Reverse[ expr_ ] :> expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-AmTYF",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Sort, Reverse },
    "Rule"        :> Sort @ Reverse[ expr_ ] :> Sort @ expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-emv4Cv",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Reverse, SortBy, Function },
    "Rule"        :> Reverse @ SortBy[ list_, f_ ] :> SortBy[ list, -f[ #1 ] & ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-da5rLt",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Reverse, Sort, SortBy, NumericQ },
    "Rule"        :> Reverse @ Sort[ list: { __? HoldNumericQ } ] :> SortBy[ list, -#1 & ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-bTqlh",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Take, Part, Span },
    "Rule"        :> Take[ list_, n_Integer ] :> list[[ 1;;n ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-cKozi4",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Reverse, Join },
    "Rule"        :> Reverse @ Join[ Reverse[ list1_ ], list2_ ] :> Join[ Reverse @ list2, list1 ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-bz3y7J",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Reverse, Join },
    "Rule"        :> Reverse @ Join[ list1_, Reverse[ list2_ ] ] :> Join[ list2, Reverse @ list1 ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dfvnKj",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, First, Rest, Part },
    "Rule"        :> First @ Rest[ list_ ] :> list[[ 2 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dm8w8B",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Last, Most, Part },
    "Rule"        :> Last @ Most[ list_ ] :> list[[ -2 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dbDM8h",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, First, Most },
    "Rule"        :> First @ Most[ list_ ] :> First @ list
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-bl95n1",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Rest, Last },
    "Rule"        :> Last @ Rest[ list_ ] :> Last @ list
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-8vziO",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, First, Part },
    "Rule"        :> First[ list_ ] :> list[[ 1 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-i6fDO",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Last, Part },
    "Rule"        :> Last[ list_ ] :> list[[ -1 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-cP4NoY",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Most, Part, Span },
    "Rule"        :> Most[ list_ ] :> list[[ 1;;-2 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dEp6US",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Rest, Part, Span, All },
    "Rule"        :> Rest[ list_ ] :> list[[ 2;;All ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dVpDP0",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Span, Part },
    "Rule"        :>
        (str_List? simpleListQ)[[ span: (_? IntTypeQ | Span[ __? IntTypeQ ]).. ]] :>
            With[ { part = Quiet @ str[[ span ]] }, part /; ! MatchQ[ part, _Part ] ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-esMhyg",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Part, All, Span },
    "Rule"        :> (expr_)[[ a___, All, b___ ]] :> expr[[ a, 1;;-1, b ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-cHy9J3",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting", "Simplification", "Optimization" },
    "Symbols"     :> { List, Part, Span },
    "Rule"        :> (expr_)[[ 1;;-1 ]] :> expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-UncJf",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Part, Span, All },
    "Rule"        :> (expr_)[[ a___, (i_);;All, b___ ]] :> expr[[ a, i;;-1, b ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-b7wJWl",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Part },
    "Rule"        :> (expr_)[[ 0 ]] :> With[ { h = HeldHead @ expr }, h /; MatchQ[ h, _TempHold ] ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-cijJMX",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, TakeSmallest, Sort, Part, Span },
    "Rule"        :> TakeSmallest[ expr_, i_? IntTypeQ ] :> (Sort @ expr)[[ 1;;i ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-b6IhnF",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, TakeLargest, Sort, Part, Span },
    "Rule"        :> TakeLargest[ expr_, i_? IntTypeQ ] :> (Sort @ expr)[[ -i;;All ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-c2CglB",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Reverse, Sort, Part, Span },
    "Rule"        :> Reverse[ (Sort[ expr_ ])[[ a_? IntTypeQ;;b_? IntTypeQ ]] ] :> (Sort @ expr)[[ b;;-a ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-eMzsYF",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, ReverseSort, Reverse, Sort },
    "Rule"        :> ReverseSort[ expr_ ] :> Reverse @ Sort @ expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-c6Mf8z",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Reverse, Keys, Sort },
    "Rule"        :> Reverse @ Keys @ Sort[ expr_ ] :> Keys @ Reverse @ Sort @ expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dXaLFE",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Reverse, RandomSample, Keys, Values, Sort },
    "Inline"      :> { $assocOuter, $assocInner },
    "Rule"        :>
        (out: $assocOuter)[ (f: Keys|Values)[ (in: $assocInner)[ expr_ ] ] ] :>
            f @ out @ in @ expr
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-0bqsz",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Reverse, RandomSample, Keys, Values },
    "Inline"      :> { $assocOuter },
    "Rule"        :>
        (out: $assocOuter)[ (f: Keys|Values)[ (expr_)[[ a_? IntTypeQ ;; b_? IntTypeQ ]] ] ] :>
            f @ out @ expr[[ a;;b ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dxYYKC",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Part, Span, All },
    "Rule"        :> (list_)[[ n_? IntTypeQ ;; All ]][[ 1 ]] :> list[[ n ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-enbQfQ",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Cases },
    "Rule"        :> Cases[ a_, b_ ] :> Cases[ a, b, { 1 } ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-c2AVd6",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Cases, PatternTest, Blank, Select },
    "Rule"        :> Cases[ list_, Verbatim[ PatternTest ][ Verbatim[ Blank ][ ], f_ ], { 1 } ] :> Select[ list, f ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-coraxl",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Select, TrueQ, Function },
    "Rule"        :> Select[ expr_, TrueQ[ cond_ ] & ] :> Select[ expr, cond & ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-el3fJ",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Select, Equal, SameQ, Function },
    "Rule"        :> Select[ expr_, Equal[ a___, b_? StringTypeQ, c___ ] & ] :> Select[ expr, a === b === c & ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-f7DWF",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Association },
    "Inline"      :> { $joinable },
    "Rule"        :>
        Join[ (f: $joinable)[ a___ ], (f_)[ b___ ], c___f ] :>
            With[
                {
                    held = Flatten @ Replace[
                        TempHold[ f @ a, f @ b, c ],
                        HoldPattern[ f ][ args___ ] :> TempHold @ args,
                        { 1 }
                    ]
                },
                f @ held /; True
            ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-bDLEWJ",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Map },
    "Rule"        :> (f_) /@ (expr_) :> Map[ f, expr, { 1 } ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-pSFxu",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Map },
    "Rule"        :> Map[ f_, expr_, 1 ] :> Map[ f, expr, { 1 } ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-ddRXYw",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Map, Composition },
    "Rule"        :>
        Map[ f_? NonHoldingQ, Map[ g_? NonHoldingQ, expr_, { 1 } ], { 1 } ] :>
            Map[ Composition[ f, g ], expr, { 1 } ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-clwXQw",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Map, First, Part },
    "Rule"        :> Map[ First, expr_, { 1 } ] :> expr[[ All, 1 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dz2AAr",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Map, Last, Part, All },
    "Rule"        :> Map[ Last, expr_, { 1 } ] :> expr[[ All, -1 ]]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-cf6oGF",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, TakeLargestBy, Take, SortBy },
    "Rule"        :> TakeLargestBy[ expr_, fun_, i_ ] :> Take[ SortBy[ expr, fun ], -i ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-eFjoZW",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, RandomInteger, Table },
    "Rule"        :>
        { a: (same_RandomInteger), b: (same_RandomInteger).. } /; Length @ HoldComplete[ a, b ] >= 4 :>
            With[ { len = Length @ HoldComplete[ a, b ], i = NewLocalSymbol[ ] },
                  Table[ same, { i, 1, len, 1 } ] /; True
            ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "Lists-dJrJWn",
    "Description" -> "Transform lists",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { List, Length },
    "Rule"        :> Length @ { a___? inertAtomQ } :> RuleCondition @ Length @ HoldComplete @ a
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)