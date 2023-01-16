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
    "Usage"       -> { "EquivalenceTesting" }
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
Inline[ { $assocOuter, $assocInner, $joinable }, HoldComplete[
    Reverse[Reverse[expr_]] :> expr,
    Sort[Reverse[expr_]] :> Sort[expr],
    Reverse[SortBy[list_, f_]] :> SortBy[list, -f[#] &],
    Reverse[Sort[list : {__?HoldNumericQ}]] :> SortBy[list, -# &],
    Take[list_, n_Integer] :> list[[;; n]],
    Reverse[Join[Reverse[list1_], list2_]] :>
      Join[Reverse[list2], list1],
    Reverse[Join[list1_, Reverse[list2_]]] :>
      Join[list2, Reverse[list1]],

    First@Rest@list_ :> list[[2]],
    Last@Most@list_ :> list[[-2]],
    First@Most@list_ :> First@list,
    Last@Rest@list_ :> Last@list,

    First[list_] :> list[[1]],
    Last[list_] :> list[[-1]],
    Most[list_] :> list[[;; -2]],
    Rest[list_] :> list[[2 ;;]],

    Part[ str_List? simpleListQ, span: (_? IntTypeQ | Span[ __? IntTypeQ ]).. ] :>
        With[ { part = Quiet[ Part[ str, span ] ] },
            part /; ! MatchQ[ part, _Part ]
        ],
    Part[expr_, a___, All, b___] :> Part[expr, a, 1 ;; -1, b],
    Part[expr_, 1 ;; -1] :> expr,
    Part[expr_, a___, i_ ;; All, b___] :> Part[expr, a, i ;; -1, b],
    Part[ expr_, 0 ] :>
        With[ { h = HeldHead @ expr }, h /; MatchQ[ h, _TempHold ] ],

    TakeSmallest[expr_, i_?IntTypeQ] :> Sort[expr][[;; i]],
    TakeLargest[expr_, i_?IntTypeQ] :> Sort[expr][[-i ;;]],
    Reverse[Sort[expr_][[a_?IntTypeQ ;; b_?IntTypeQ]]] :> Sort[expr][[b ;; -a]],
    ReverseSort[expr_] :> Reverse[Sort[expr]],

    Reverse[Keys[Sort[expr_]]] :> Keys[Reverse[Sort[expr]]],

    (out:$assocOuter)[ (f:Keys|Values)[ (in:$assocInner)[ expr_ ] ] ] :>
      f @ out @ in @ expr,

    (out:$assocOuter)[ (f:Keys|Values)[
        Part[ expr_, a_?IntTypeQ ;; b_?IntTypeQ ]
    ] ] :>
      f @ out @ Part[ expr, a ;; b ],

    (*expr: _[ e_ ][[ 1 ]] /; SafeEvaluatedQ @ expr :> e,*)

    list_[[ n_?IntTypeQ ;; All ]][[ 1 ]] :> list[[ n ]],

    Cases[a_, b_] :> Cases[a, b, {1}],
    Cases[list_, Verbatim[PatternTest][Verbatim[Blank][], f_], {1}] :>
      Select[list, f]
    ,
    Select[ expr_, TrueQ[ cond_ ] & ] :>
      Select[ expr, cond & ],

    Select[ expr_, Equal[ a___, b_? StringTypeQ, c___ ] & ] :>
      Select[ expr, SameQ[ a, b, c ] & ],

    Join[ (f:$joinable)[ a___ ], f_[ b___ ], c___f ] :> WithHolding[
        {
            held = Flatten @ Replace[
                TempHold[ f @ a, f @ b, c ],
                HoldPattern[ f ][ args___ ] :> TempHold @ args,
                { 1 }
            ]
        },
        f @ held
    ],

    Map[ f_, expr_ ] :> Map[ f, expr, { 1 } ],
    Map[ f_, expr_, 1 ] :> Map[ f, expr, { 1 } ],
    Map[ f_? NonHoldingQ, Map[ g_? NonHoldingQ, expr_, { 1 } ], { 1 } ] :>
      Map[ Composition[ f, g ], expr, { 1 } ],

    Map[First, expr_, {1}] :> expr[[All, 1]],
    Map[Last, expr_, {1}] :> expr[[All, -1]],

    TakeLargestBy[ expr_, fun_, i_ ] :> Take[ SortBy[ expr, fun ], -i ],

    { a: same_RandomInteger, b: same_RandomInteger .. } /;
        Length @ HoldComplete[ a, b ] >= 4 :>
            WithHolding[
                {
                    len = Length @ HoldComplete[ a, b ],
                    i = NewLocalSymbol[ ]
                },
                Table[ same, { i, 1, len, 1 } ]
            ],

    Length[ { a___? inertAtomQ } ] :>
        RuleCondition @ Length @ HoldComplete[ a ]
] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)