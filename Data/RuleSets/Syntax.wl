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

$byFunction = Alternatives[
    CountDistinctBy,
    CountsBy,
    DeleteDuplicatesBy,
    GatherBy,
    GroupBy,
    KeySortBy,
    MaximalBy,
    MinimalBy,
    OrderingBy,
    ReverseSortBy,
    SortBy,
    SplitBy,
    TakeLargestBy,
    TakeSmallestBy
];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rule*)
rule // ClearAll;
rule[ a_, b_ ] := (Rule|RuleDelayed)[ a, b ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
Inline[ { rule, $byFunction }, HoldComplete[
    Verbatim[ And ][ a_ ] :> a,
    Verbatim[ And ][ ] :> True,
    Verbatim[ And ][ ___, False, ___ ] :> False,
    Verbatim[ And ][ a___, True, b___ ] :> And[ a, b ],

    Verbatim[ Or ][ a_ ] :> a,
    Verbatim[ Or ][ ] :> False,
    Verbatim[ Or ][ ___, True, ___ ] :> True,
    Verbatim[ Or ][ a___, False, b___ ] :> Or[ a, b ],

    And[ a___, Equal[ b1___, x_, b2___ ], c___, Equal[ d1___, x_, d2___ ], e___ ] :>
        And[ a, Equal[ b1, b2, d1, d2, x ], c, e ],

    And[ a___, SameQ[ b1___, x_, b2___ ], c___, SameQ[ d1___, x_, d2___ ], e___ ] :>
        And[ a, SameQ[ b1, b2, d1, d2, x ], c, e ],

    (eq:Equal|SameQ|Unequal|UnsameQ)[ a___ ] /; ! OrderedQ @ TempHold @ a :>
        With[ { b = Sort @ TempHold @ a },
            eq[ b ] /; True
        ],

    (eq:Equal|SameQ)[ a___ ] /; ! DuplicateFreeQ @ TempHold @ a :>
        With[ { b = Union @ TempHold @ a },
            eq[ b ] /; True
        ],

    (Equal|SameQ|Unequal|UnsameQ)[ a_ ] :> True,
    (Equal|SameQ|Unequal|UnsameQ)[ ] :> True,
    Unequal[ a_, b_ ] :> ! Equal[ a, b ],
    UnsameQ[ a_, b_ ] :> ! SameQ[ a, b ],
    Unequal[ a_, b_, c__ ] :> ! Equal[ a, b ] && Unequal[ b, c ],
    UnsameQ[ a_, b_, c__ ] :> ! SameQ[ a, b ] && UnsameQ[ b, c ],

    Select[ Select[ a_, Equal[ b1___, x_, b2___ ] & ], Equal[ c1___, x_, c2___ ] & ] :>
        Select[ a, Equal[ b1, b2, c1, c2, x ] & ],

    TrueQ[ Equal[ a___, b_? inertAtomQ, c___ ] ] :>
        SameQ[ a, b, c ],

    (Equal|SameQ)[ x_ .. ] /; inertAtomQ @ x :> True,
    (Unequal|UnsameQ)[ x_ .. ] /; inertAtomQ @ x :> False,

    (Equal|SameQ)[ x_? inertAtomQ, y_? inertAtomQ ] /; x =!= y :> False,

    Not[ True ] :> False,
    Not[ False ] :> True,

    ApplyTo[ x_, f_ ] :> (x = f @ x),
    AddTo[ a_, b_ ] :> (a = a + b),
    SubtractFrom[ a_, b_ ] :> (a = a - b),
    TimesBy[ a_, b_ ] :> (a = a * b),
    DivideBy[ a_, b_ ] :> (a = a / b),
    PreIncrement[ x_ ] :> (x = x + 1),
    PreDecrement[ x_ ] :> (x = x - 1),
    AppendTo[ a_, b_ ] :> (a = Append[ a, b ]),
    PrependTo[ a_, b_ ] :> (a = Prepend[ a, b ]),

    Construct[ f_, x___ ] :> f @ x,
    OperatorApplied[ f_ ][ x_ ][ y_ ] :> f[ y, x ],
    ReverseApplied[ f_ ][ a___ ] :> With[ { r = Reverse @ TempHold @ a }, f @ r /; True ],
    Operate[ p_, f_[ x_, y_ ] ] :> p[ f ][ x, y ],

    Composition[ fs__, f_? NonHoldingQ ][ a___ ] :> Composition[ fs ][ f @ a ],
    Composition[ f_ ][ a___ ] :> f @ a,
    a_[ b___, c_Composition, d___ ] :> a[ b, c[ ## ] &, d ]
    ,
    RightComposition[ a___ ] :> With[ { r = Reverse @ TempHold @ a }, Composition @ r /; True ]
    ,
    Identity[ e_ ] :> e
    ,
    TrueQ @ TrueQ[ expr_ ] :> TrueQ @ expr
    ,
    If[ a_, b_ ] :> If[ a, b, Null ],
    If[ TrueQ[ a_ ], b_, c_ ] :> If[ a, b, c, c ],
    If[ True, a_, _ ] :> a,
    If[ True, a_, _, _ ] :> a,
    If[ False, _, a_ ] :> a,
    If[ False, _, a_, _ ] :> a
    ,
    Which[ a_, b_ ] :> If[ a, b ],
    Which[ a_, b_, c_, d___ ] :> If[ a, b, Which[ c, d ] ],
    HoldPattern[ Which ][ ] :> Null
    ,
    Switch[ a_, b_, c__ ] /; EvenQ @ Length @ HoldComplete[ b, c ] :>
        With[
            {
                r = Replace[
                        Partition[ TempHold[ b, c ], 2 ],
                        TempHold[ d_, e_ ] :> d :> e,
                        { 1 }
                    ]
            },
            Replace[ a, { r } ] /; True
        ]
    ,
    Replace[ a_, b_, o: OptionsPattern[ ] ] :>
        Replace[ a, b, { 0, 0 }, o ],

    Replace[ a_, b_, c_? IntTypeQ, o: OptionsPattern[ ] ] :>
        Replace[ a, b, { 1, c }, o ],

    Replace[ a_, b_, { c_? IntTypeQ }, o: OptionsPattern[ ] ] :>
        Replace[ a, b, { c, c }, o ],

    Replace[ a_, b_, DirectedInfinity[ 1 ], o: OptionsPattern[ ] ] :>
        Replace[ a, b, { 1, DirectedInfinity[ 1 ] }, o ],

    Replace[ a_, b_, { DirectedInfinity[ 1 ] }, o: OptionsPattern[ ] ] :>
        Replace[ a, b, { 1, DirectedInfinity[ 1 ] }, o ],

    Replace[ a_, b: _Rule|_RuleDelayed, c_, o: OptionsPattern[ ] ] :>
        Replace[ a, { b }, c, o ],

    Replace[ a_, b_, All, o: OptionsPattern[ ] ] :>
        Replace[ a, b, { 0, DirectedInfinity[ 1 ] }, o ],

    Replace[ a_, b_, c__, rule[ Heads, False ], d___ ] :>
        Replace[ a, b, c, d ],

    ReplaceAll[ a_, b_ ] :>
        Replace[ a, b, { 0, DirectedInfinity[ 1 ] }, Heads -> True ],

    System`MapApply[ f_, expr_ ] :>
        Apply[ f, expr, { 1 } ],

    ReplacePart[
        expr_,
        (r:Rule|RuleDelayed)[ i_? IntTypeQ, new_ ],
        o: OptionsPattern[ ]
    ] :> ReplacePart[ expr, { r[ { i }, new ] }, o ],

    ReplacePart[
        expr_,
        (r:Rule|RuleDelayed)[ { i__? IntTypeQ }, new_ ],
        o: OptionsPattern[ ]
    ] :> ReplacePart[ expr, { r[ { i }, new ] }, o ],

    ReplacePart[
        expr_,
        rules: { ___, rule[ _? IntTypeQ, _ ], ___ },
        o: OptionsPattern[ ]
    ] :>
        With[
            {
                rules1 = Replace[
                    TempHold @ rules,
                    HoldPattern[ (r:Rule|RuleDelayed)[ i_? IntTypeQ, new_ ] ] :>
                        r[ { i }, new ],
                    { 2 }
                ]
            },
            ReplacePart[ expr, rules1, o ] /; True
        ],

    (* default option value *)
    ReplacePart[ a_, b_, c___, rule[ Heads, Automatic ], d___ ] :>
        ReplacePart[ a, b, c, d ],

    (* operator *)
    ReplacePart[ (r:Rule|RuleDelayed)[ i_? IntTypeQ, new_ ] ] :>
        ReplacePart[ { r[ { i }, new ] } ],

    ReplacePart[ (r:Rule|RuleDelayed)[ { i__? IntTypeQ }, new_ ] ] :>
        ReplacePart[ { r[ { i }, new ] } ],

    ReplacePart[ rules: { ___, rule[ _? IntTypeQ, _ ], ___ } ] :>
        With[
            {
                rules1 = Replace[
                    TempHold @ rules,
                    HoldPattern[ (r:Rule|RuleDelayed)[ i_? IntTypeQ, new_ ] ] :>
                        r[ { i }, new ],
                    { 2 }
                ]
            },
            ReplacePart[ rules1 ] /; True
        ],

    (* xxByFunctionRules *)
    Select[ thing_, And[ this_, that__ ] & ] :>
      Select[ Select[ thing, this & ], And[ that ] & ]
    ,
    Select[ thing_, And[ this_ ] & ] :> Select[ thing, this & ]
    ,
    (select: $byFunction)[ thing_, Function[ f_[ #1 ] ], args___ ] :>
      select[ thing, f, args ]
] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)