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

attributeRules = HoldComplete[
    (f_Symbol? SymbolQ)[a1___, f_@a2___, a3___] /;
      Attributes@f ~ MemberQ ~ Flat :> f[a1, a2, a3]
    ,
    (f_Symbol? SymbolQ)[a___] /;
      Attributes @ f ~MemberQ~ Orderless && ! OrderedQ[Unevaluated[{a}]] :>
      WithHolding[
          {
              sorted = Sort[TempHold[a]]
          },
          f[sorted]
      ]
    ,
    (f_Symbol? SymbolQ)[x : Except[_Symbol? SymbolQ, _?UAtomQ]] /;
      Attributes@f ~ MemberQ ~ OneIdentity :> x
    ,
    (f_? SymbolQ) @@ { a___ } /;
      NonHoldingQ @ f &&
        FreeQ[ Unevaluated @ { a },
               s_? SymbolQ /; ! FreeQ[ UpValueSymbols @ s, Apply, { 1 } ],
               { 1 }
        ] :> f @ a,
(*        FreeQ[Unevaluated[{a}],
            s_Symbol /; Context[Unevaluated[s]] =!= "System`",
            {1, Infinity}] :> f[a],*)

    (f_? OrderlessQ) @@ Reverse[ h_[ a___ ] ] /; NonHoldingQ[ f ] :> f @@ h[a]
    ,

    (f_? OrderlessQ) @@ Sort[ h_[ a___ ] ] /; NonHoldingQ[ f ] :> f @@ h[a],



    ( f_? ListableQ )[ Table[ ei_, { i_, i1_, i2_, di_ } ],
                       Table[ ej_, { j_, i1_, i2_, di_ } ]
    ] :>
      WithHolding[
          {
              ej2 = TempHold @ ej /. HoldPattern @ Verbatim @ j :> i
          },
          Table[ f[ ei, ej2 ], { i, i1, i2, di } ]
      ]
    ,

    Part[ (f_? ListableQ)[ list_? ListTypeQ ], s_Span ] :>
      f @ Part[ list, s ]
];


(******************************************************************************)


hasSeqFunQ[ rng: { __Integer | __Rational } ] := hasSeqFunQ[ rng ] =
    And[ Length @ rng >= 4,
         ! constantArrayQ @ rng,
         MatchQ[
             Replace[
                 FindSequenceFunction @ rng,
                 f_Function :> (seqFun[ rng ] = f)
             ],
             _Function
         ]
    ];


(******************************************************************************)


$atomicNumber = (_Integer|_Real|_Rational)? UAtomQ;

tableIteratorRules = Inline[ { $intType, $atomicNumber }, HoldComplete[
    Table[exp_, ii_, jj_] :> Table[Table[exp, jj], ii],

    Table[exp_, imax_?HoldNumericQ] :>
      WithHolding[ { i = NewLocalSymbol[] }, Table[exp, {i, 1, imax, 1}]],

    Table[exp_, {imax_?HoldNumericQ}] :>
      WithHolding[ { i = NewLocalSymbol[] }, Table[exp, {i, 1, imax, 1}]],

    Table[exp_, {i_, imax_?HoldNumericQ}] :> Table[exp, {i, 1, imax, 1}],
    Table[exp_, {i_, imin_, imax_}] :> Table[exp, {i, imin, imax, 1}]
    ,

    (* remove redundant Tables in iterator *)
    Table[ expr_, { i_, Table[ j_, { j_, j1_, j2_, dj_ } ] } ] :>
      Table[ expr, { i, j1, j2, dj } ]
    ,


    (* Reverse tables that use a negative step size *)
(*    Table[ expr_, { i_, imin_, imax_, Times[ -1, di_ ] } ] :>
      Reverse @ Table[ expr, { i, imax, imin, di } ]
    ,*)

(* Evaluate deterministic numeric iterators *)
(*    Table[exp_, {i_, i1___, ii_ /; !UAtomQ[ii] && HoldNumericQ[ii], i2___}] :>
      PartialEvaluation[{j}, j = ii; Table[exp, {i, i1, j, i2}]],*)

(* unroll small range tables *)
   (* Table[i_Symbol, {i_Symbol, a_?HoldNumericQ, b_?HoldNumericQ,
        d_?HoldNumericQ}] /; (b - a) / d <= $unrollLimit :>
      PartialEvaluation[{rng}, rng = Range[a, b, d]; rng],

    Table[TypedSymbol[i_, Verbatim[_Integer]],
        {TypedSymbol[i_, Verbatim[_Integer]],
            a_?HoldNumericQ, b_?HoldNumericQ, d_?HoldNumericQ}
    ] /; (b - a) / d <= $unrollLimit :>
      PartialEvaluation[{rng}, rng = Range[a, b, d]; rng],*)

    (* on second thought, let's roll up everything *)
    rng : { $atomicNumber.. } /;
      Length @ rng >= 4 && OrderedQ[ rng ] && Equal @@ Differences @ rng :>
      TrEval @
        With[
            {
                imin = First @ rng,
                imax = Last @ rng,
                di = First @ Differences @ rng
            },
            TempHold @ Range[ imin, imax, di ]
        ],


    (****************************************************)
    (* EXPERIMENTAL *)
    (****************************************************)
    (* normalize imin offset to 1 for integer iterators *)
    Table[ exp_, { i_, imin : Except[ 1, _Integer ], imax_Integer, di_Integer } ] :>
      WithHolding[
          {
              sub   = imin - 1,
              imax2 = imax - sub,
              exp2  = TempHold[ exp ] /. HoldPattern[ Verbatim[ i ] ] :> i + sub
          },
          Table[ exp2, { i, 1, imax2, di } ]
      ],

    (* normalize di step size to 1 for integer iterators *)
    Table[ exp_, { i_, 1, imax_? HoldNumericQ, di : Except[ 1, _Integer ] } ] :>
      WithHolding[
          {
              imax2 = Floor[ (imax - 1)/di ] + 1,
              exp2  = TempHold[ exp ] /. HoldPattern[ Verbatim[ i ] ] :>
                                           1 + di*(i - 1)
          },
          Table[ exp2, {  i, 1, imax2, 1 } ]
      ]


(*
    Table[ exp_, { i_, 1, imax_, di : Except[ 1, _Integer ] } ] :>
      WithHolding[
          {
              imax2 = (imax - 1) / di,
              exp2  = TempHold[ exp ] /.
                HoldPattern[ Verbatim[ i ] ] :> i * di + 1
          },
          Table[ exp2, { i, 0, imax2, 1 } ]
      ]*)
    ,
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::UnscopedObjectError:: *)
    rng : { __Integer | __Rational } /; hasSeqFunQ[rng] :>
      WithHolding[
          {
              sf = seqFun[rng],
              s = NewLocalSymbol[ ],
              h = Replace[sf, HoldPattern[Function[expr_]] :> TempHold[expr]] /.
                Slot[1] :> s,
              len = Length[rng]
          },
          Table[h, {s, 1, len, 1}]
      ],
(* :!CodeAnalysis::EndBlock:: *)

    f_[a_?SafeEvaluatedQ, Table[exp_, ii_]] /;
      Length[Intersection[
          GetAttributes[f], {NumericFunction, Listable}]] === 2 :>
      Table[f[a, exp], ii],

    f_[Table[exp_, ii_], b_?SafeEvaluatedQ] /;
      Length[Intersection[
          GetAttributes[f], {NumericFunction, Listable}]] === 2 :>
      Table[f[exp, b], ii],

    Reverse[Table[exp_, {i_, 1, n_, di_}]] :>
      WithHolding[
          {
              imax = di Floor[(n - 1)/di] + 1,
              h = TempHold[exp] /. HoldPattern[Verbatim @ i ] :> imax - i + 1
          },
          Table[h, {i, 1, imax, di}]
      ]
    ,
    Reverse @ Table[ exp_, { i_, i1_, i2_, 1 } ] :>
      Table[ exp, { i, i2, i1, -1 } ]
    ,

    (* tables with only one item *)
    Table[ expr_, { i_, 1, 1, 1 } ][[ 1 | -1 ]] :>
      RuleCondition[ TempHold @ expr /. HoldPattern[ i ] :> 1 ],


    Verbatim[ Plus ][ a___, b_?HoldNumericQ, c___, d_?HoldNumericQ, e___ ] /;
      Length[ HoldComplete[ a, c, e ] ] > 0 &&
      ! SafeEvaluatedQ[b + d] && ! SafeEvaluatedQ[d + b] :>
        WithHolding[ { bd = b + d },
                     Plus[ a, bd, c, e ]
        ],

    p: Verbatim[ Plus ][ x_ ] /; Length @ Unevaluated @ p === 1 :> x

]];


(******************************************************************************)


tableScopingRules = HoldComplete[
    Table[ exp_, { i_? SymbolQ /; ! LocalContextQ @ i, ii__ } ] :>
      WithHolding[
          {
              j = NewLocalSymbol[ ],
              held = TempHold @ exp //. HoldPattern[ i ] :> j
          },
          Table[ held, { j, ii } ]
      ]
];


(******************************************************************************)


tableFromOtherFunctions = Inline[ $intType, HoldComplete[
    Range[ args___ ] :>
      WithHolding[ { i = NewLocalSymbol[ ] },
          Table[ i, { i, args } ]
      ],

    Array[ f_, n_? IntTypeQ ] :>
      WithHolding[ { i = NewLocalSymbol[ ] },
          Table[ f[i], { i, n } ]
      ],


    Array[f_, n_? IntTypeQ, r_] :>
      WithHolding[
          {
              nn = n + r - 1,
              i = NewLocalSymbol[ ]
          },
          Table[f[i], {i, r, nn}]
      ],

    Map[ f_, Table[ exp_, iter___ ], { 1 } ] :> Table[ f @ exp, iter ],
    Map[ f_, list_List, { 1 } ] :>
        With[ { i = NewLocalSymbol[ ] },
            Table[ f @ i, { i, list } ]
        ],

    (f_)[a1___? UAtomQ, Table[exp_, iter___], a2___? UAtomQ] /;
      ListableQ[ f ] :> Table[f[a1, exp, a2], iter],


    ca : { Repeated[ x_, { 4, Infinity } ] } :>
      WithHolding[
          {
              i = NewLocalSymbol[ ],
              n = Length @ Unevaluated @ ca
          },
          Table[ x, { i, 1, n, 1 } ]
      ],

    Do[ args___ ] :> (Table[ args ];),

    Scan[ f_, args__ ] :> (Map[ f, args ];)
]];


(******************************************************************************)


$orderlessLevel2Symbols =
  Alternatives[
      Blend,
      Length,
      Max,
      Mean,
      Min,
      RandomChoice,
      RandomSample,
      Sort,
      Total
  ];

$orderlessLevel2AnyArgSymbols =
  Alternatives[
      Complement,
      Intersection,
      Union,
      Graph
  ];


orderlessLevel2 = Inline[
    { $orderlessLevel2Symbols, $orderlessLevel2AnyArgSymbols },
    HoldComplete[
        (f: $orderlessLevel2Symbols)[ l: { a___ }, b___ ] /;
          ! OrderedQ @ HoldComplete @ a :>
            WithHolding[ { sorted = Sort @ TempHold @ a },
                f[ { sorted }, b ]
            ]
        ,
        (f: $orderlessLevel2AnyArgSymbols)[ a___, l: { b___ }, c___ ] /;
          ! OrderedQ @ HoldComplete @ b :>
            WithHolding[ { sorted = Sort @ TempHold @ b },
                f[ a, { sorted }, c ]
            ]
    ]
];


(******************************************************************************)


$flatLevel2Symbols =
  Alternatives[
      Min,
      Max
  ];


flatLevel2 = Inline[ $flatLevel2Symbols, HoldComplete[

    f : $flatLevel2Symbols[a_] /; ! UFlatQ[a] :>
      WithHolding[
          {
              flat = HFlatten[f, HoldApply]
          },
          flat
      ],

    (f : $flatLevel2Symbols)[{a___}] :> f[a]


] ];



(******************************************************************************)



typingPatterns = HoldComplete[
    sd_SetDelayed /; ! TypedDefinitionQ[sd] :>
      TrEval[ToTypedBinding[sd]],
    rd_RuleDelayed /; ! TypedDefinitionQ[rd] :>
      TrEval[ToTypedBinding[rd]]
];



(******************************************************************************)

constantArrayQ // Attributes = { HoldAllComplete };

constantArrayQ[ m_List ] :=
  TrueQ @ And[
      ArrayQ @ Unevaluated @ m,
      Times @@ Dimensions @ Unevaluated @ m >= 4,
      SameQ @@ Map[ HoldComplete, Unevaluated @ m ]
  ];

constantArrayQ[ ___ ] := False;



newTypedLocal // ClearAll;
newTypedLocal[ Integer ] := Int @@ { NewLocalSymbol[ ] };
newTypedLocal[ Real    ] := Rea @@ { NewLocalSymbol[ ] };
newTypedLocal[ String  ] := Str @@ { NewLocalSymbol[ ] };
newTypedLocal[ ___     ] := $Failed;


replaceTableFromListType // Attributes = { HoldAllComplete };

replaceTableFromListType[ exp_, i_, ii_ ] :=
    With[ { local = newTypedLocal @ ListType @ ii },
        If[ FailureQ @ local,
            $Failed,
            replaceTableFromListType[ exp, i, ii, local ]
        ]
    ];

replaceTableFromListType[ exp_, i_, ii_, j_ ] :=
    With[ { held = TempHold[ exp ] /. HoldPattern[ i ] -> j },
        TempHold @ Table[ held, { j, ii } ]
    ];

replaceTableFromListType[ ___ ] := $Failed;



typedRules = Inline[ { $intType, $reaType }, HoldComplete[

    (* Table iterators *)

    Table[ exp_, n_? IntTypeQ ] :>
      WithHolding[
          {
              i = NewLocalSymbol[ ]
          },
          Table[ exp, { i, 1, n, 1 } ]
      ],



    Table[ exp_, { i_Symbol? SymbolQ, i2_? IntTypeQ } ] :>
      WithHolding[
          {
              j = Int @@ { NewLocalSymbol[ ] },
              held = TempHold[ exp ] /. HoldPattern[ i ] -> j
          },
          Table[ held, { j, 1, i2, 1 } ]
      ],



    Table[ exp_, { i_Symbol? SymbolQ, i1_? IntTypeQ, i2_? IntTypeQ } ] :>
      WithHolding[
          {
              j = Int @@ { NewLocalSymbol[ ] },
              held = TempHold[ exp ] /. HoldPattern[ i ] -> j
          },
          Table[ held, { j, i1, i2, 1 } ]
      ],



    Table[ exp_, { i_Symbol? SymbolQ, i1_? IntTypeQ, i2_? IntTypeQ, di_? IntTypeQ } ] :>
      WithHolding[
          {
              j = Int @@ { NewLocalSymbol[ ] },
              held = TempHold[ exp ] /. HoldPattern[ i ] -> j
          },
          Table[ held, { j, i1, i2, di } ]
      ],

     Table[ exp_, { i_Symbol? SymbolQ, ii: _List|_Table } ] :>
        With[ { new = replaceTableFromListType[ exp, i, ii ] },
            new /; ! FailureQ @ new
        ],

    Table[ exp_, { i: Except[ _Integer, _? IntTypeQ ], n_? IntTypeQ } ] :>
      Table[ exp, { i, 1, n, 1 } ],


    Table[ exp_, { i_Symbol? SymbolQ, i1 : $reaType, i2 : $reaType, di_ } ] :>
      WithHolding[
          {
              held = TempHold[ exp ] /. HoldPattern[ i ] -> Rea @ i
          },
          Table[ held, { Rea @ i, i1, i2, di } ]
      ],

    Table[ exp_,
           { i_Symbol? SymbolQ, i1_? HoldNumericQ, i2_? HoldNumericQ, di : $reaType }
    ] :>
      WithHolding[ { held = TempHold[ exp ] /. HoldPattern[ i ] -> Rea @ i },
                   Table[ held, { Rea @ i, i1, i2, di } ]
      ],



    Array[ f_, n_? IntTypeQ ] :>
      Table[ f[i], { i, 1, n, 1 } ],


    Array[ fun_, { iter__? IntTypeQ } ] :>
      WithHolding[
          {
              iterators = Replace[ TempHold @ iter,
                                   e_ :> WithHolding[ { i = NewLocalSymbol[ ] },
                                                      { i, 1, e, 1 }
                                         ],
                                   { 1 }
                          ]
              ,
              args = TempHold @@ Cases[ iterators, { sym_, _, _, _ } :>
                                                     TempHold @ sym
                                 ]
          },
          Table[ fun @ args, iterators ]
      ],

    BoxMatrix[ r_? IntTypeQ ] /; Positive @ Unevaluated @ r :>
      WithHolding[
          {
              n = 2 r + 1
          },
          Table[ 1, n, n ]
      ],

    ConstantArray[ x_, n_? IntTypeQ ] :>
      WithHolding[
          {
              i = NewLocalSymbol[ ]
          },
          Table[ x, { i, 1, n, 1 } ]
      ],

    ConstantArray[ x_, { n___? IntTypeQ } ] :>
      Table[ x, n ]
    ,

    arr_List? constantArrayQ :>
      WithHolding[
        {
            dims = Dimensions @ Unevaluated @ arr,
            elem = Extract[ Unevaluated @ arr, 1 & /@ dims, TempHold ]
        },
        ConstantArray[ elem, dims ]
    ],

    Reverse[ exp_ ][[ i_? IntTypeQ ]] :> exp[[ -i ]]
    ,


    RandomColor[ { x__? IntTypeQ } ] :>
      Table[RandomColor[], x]
    ,


    i_Int :> RuleCondition @ i,
    i_Rea :> RuleCondition @ i
]];



(******************************************************************************)


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::UnscopedObjectError:: *)
pureFunctionRules = HoldComplete[

    (fun : Function[ expr_ ])[ ___ ] /; FreeQ[ fun, _Slot | _SlotSequence ] :>
      expr,



    Function[x : Except[ Null, _? SymbolQ ], expr_] /;
      ! FreeQ[ HoldComplete @ expr, HoldPattern @ x ] :>
        WithHolding[
            {
                new = TempHold @ Function @ expr /. HoldPattern @ x -> Slot @ 1
            },
            new
            ]
    ,

    Function[ expr_ ][ n_? SafeEvaluatedQ ] :>
      TrEval @ Function[ TempHold @ expr ][ n ]
    ,

    Composition[ a___, Function[ f_ ], Function[ g_ ], b___ ] /;
      FreeQ[ HoldComplete[ f, g ], HoldPattern[ Slot ][ Except @ 1 ] ] :>
        With[ { composed = TempHold @ f /. HoldPattern[ Slot ][ 1 ] :> g },
            Composition[ a, Function @ composed, b ] /; True
            ]
    ,

    Function[ # ][ a_ ] :> a
];


(******************************************************************************)


systemSymbols = HoldComplete[
    Circle @ { 0, 0 } :> Circle[ { 0, 0 }, 1 ],
    Dashed            :> Dashing @ { Small, Small },
    DotDashed         :> Dashing @ { 0, Small, Small, Small },
    Dotted            :> Dashing @ { 0, Small },
    Thick             :> Thickness @ Large,
    Thin              :> Thickness @ Tiny,
    Yesterday         :> DateObject[ Take[ DateList[ ], 3 ] - { 0, 0, 1 } ],
    Today             :> DateObject @ Take[ DateList[ ], 3 ],
    Tomorrow          :> DateObject[ Take[ DateList[ ], 3 ] + { 0, 0, 1 } ],
    Now               :> DateObject @ DateList[ ],
    InfiniteFuture    :> DateObject[ { Infinity }, "Eternity", "Gregorian", None ],
    InfinitePast      :> DateObject[ { -Infinity }, "Eternity", "Gregorian", None ],
    Infinity          :> DirectedInfinity[ 1 ],
    Here              :> $GeoLocation,
    Black             :> GrayLevel[ 0 ],
    Gray              :> GrayLevel[ 0.5 ],
    LightGray         :> GrayLevel[ 0.85 ],
    White             :> GrayLevel[ 1 ],
    Transparent       :> GrayLevel[ 0, 0 ],
    Blue              :> RGBColor[ 0, 0, 1 ],
    Green             :> RGBColor[ 0, 1, 0 ],
    Cyan              :> RGBColor[ 0, 1, 1 ],
    Purple            :> RGBColor[ 0.5, 0, 0.5 ],
    LightGreen        :> RGBColor[ 0.88, 1, 0.88 ],
    Red               :> RGBColor[ 1, 0, 0 ],
    Magenta           :> RGBColor[ 1, 0, 1 ],
    Orange            :> RGBColor[ 1, 0.5, 0 ],
    Pink              :> RGBColor[ 1, 0.5, 0.5 ],
    Yellow            :> RGBColor[ 1, 1, 0 ],
    LightYellow       :> RGBColor[ 1, 1, 0.85 ],
    $CloudCreditsAvailable :> If[ TrueQ @ $CloudConnected,
                                  CloudAccountData[ "CloudCreditsAvailable" ],
                                  Indeterminate
                              ],
    $Linked                 :> $ParentLink =!= Null,
    $PerformanceGoal        :> If[ $ControlActiveSetting, "Speed", "Quality" ]
];

systemDownvaluesNull = HoldComplete[
    Circle[ ]     :> Circle @ { 0, 0 },
    Cone[ ]       :> Cone @ { { 0, 0, -1 }, { 0, 0, 1 } },
    Cylinder[ ]   :> Cylinder @ { { 0, 0, -1 }, { 0, 0, 1 } },
    Disk[ ]       :> Disk @ { 0, 0 },
    Graphics[ ]   :> Graphics @ { },
    Graphics3D[ ] :> Graphics3D @ { },
    Rectangle[ ]  :> Rectangle @ { 0, 0 },
    Sphere[ ]     :> Sphere @ { 0, 0, 0 },
    DateObject[ ] :> DateObject @ DateList[ ]
];

$assocOuter = HoldPattern @ Alternatives[
    Reverse,
    RandomSample
];

$assocInner = HoldPattern @ Alternatives[
    Reverse,
    Sort,
    RandomSample
];

$joinable = HoldPattern @ Alternatives[
    List,
    Association
];


simpleListQ // Attributes = { HoldAllComplete };
simpleListQ[ { (_String? UStringQ | _Integer? UIntegerQ)... } ] := True;
simpleListQ[ { other___ } ] := AllTrue[ HoldComplete @ other, simpleListQ ];
simpleListQ[ ___ ] := False;


listRules = Inline[ { $assocOuter, $assocInner, $joinable }, HoldComplete[
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

    { a: same_RandomInteger, b: same_RandomInteger .. } :>
      WithHolding[
          {
              len = Length @ HoldComplete[ a, b ],
              i = NewLocalSymbol[ ]
          },
          Table[ same, { i, 1, len, 1 } ]
      ]
] ];

integerRules = HoldComplete[
    FromDigits[IntegerDigits[n_]] :> n
];

colorRules = HoldComplete[
    GrayLevel[p_] :> RGBColor[p, p, p],
    RGBColor[r_, g_, b_] :> RGBColor[r, g, b, 1],
    Hue[p_?UNumericQ] :> Hue[p, 1, 1],
    Hue[h_, s_, v_] :> Hue[h, s, v, 1],
    Hue[a___?UNumericQ] :>
      PartialEvaluation[{c},
          c = Check[ColorConvert[Hue[a], RGBColor], HoldApply[Hue, {a}]];
          c],
    RGBColor[a___?UNumericQ] /; Round[{a}, 1 / 255] =!= {a} :>
      PartialEvaluation[{r, c}, r = Round[{a}, 1 / 255];
      c = RGBColor @@ r; c]
];


stringRules = HoldComplete[
    WordCount[ s_? StringTypeQ ] :>
        Length @ TextCases[ s, "Word" ],

    StringLength[ s_? StringTypeQ ] :>
        Length @ Characters @ s,

    Characters[ s_String? UStringQ ] :>
        RuleCondition @ Characters @ s,

    StringTake[ s_? StringTypeQ, spec_ ] :>
        StringJoin @ Take[ Characters @ s, spec ],

    StringDrop[ s_? StringTypeQ, spec_ ] :>
        StringJoin @ Drop[ Characters @ s, spec ],

    StringJoin[ a___, "", b___ ] :>
        StringJoin[ a, b ],

    StringJoin[ a1___, { a2___ }, a3___ ] :>
        StringJoin[ a1, a2, a3 ],

    StringLength /@ TextCases[ e_, "Word" ] :>
        StringLength @ TextCases[ e, "Word" ],

    HoldPattern[ StringJoin ][ str__? UStringQ ] :>
        RuleCondition @ StringJoin @ str,

    HoldPattern[ StringJoin ][ a1___, a2_String? UStringQ, a3_String? UStringQ, a4___ ] :>
        With[ { joined = StringJoin[ a2, a3 ] },
            StringJoin[ a1, joined, a4 ] /; UStringQ @ joined
        ],

    StringLength[ s_String? UStringQ ] :>
        RuleCondition @ StringLength @ s,

    StringReplace[ a_, b_ -> c_String? UStringQ, d___ ] :>
        StringReplace[ a, b :> c, d ],

    StringReplace[ a_, { b___String? UStringQ } :> c_, d___ ] :>
        StringReplace[ a, Alternatives @ b :> c, d ],

    StringTake[ s_, 1 ] :>
        StringPart[ s, 1 ],

    StringTake[ s_, -1 ] :>
        StringPart[ s, -1 ],

    StringTake[ Part[ list_, span_Span ], i_? IntTypeQ ] :>
        Part[ Characters @ list, span, i ],

    TextSentences[ str_? StringTypeQ ] :>
        TextCases[ str, "Sentence" ],

    TextSentences[ str_? StringTypeQ, n_? IntTypeQ ] :>
        TextCases[ str, "Sentence", n ],

    TextWords[ str_? StringTypeQ ] :>
        TextCases[ str, "Word" ],

    TextWords[ str_? StringTypeQ, n_? IntTypeQ ] :>
        TextCases[ str, "Word", n ],

    HoldPattern[ StringJoin ][ str_? StringTypeQ ] :>
        str,

    StringReverse[ str_ ] :>
        StringJoin @ Reverse @ Characters @ str
];

entityFrameworkRules = HoldComplete[
    Entity[s_String, f_]["Image"] :>
      Entity[s, f][EntityProperty[s, "Image"]]
    ,

    Entity[ a___ ][ b_ ] :>
      EntityValue[ Entity @ a, b ]
    ,

    EntityValue[ Entity[ a_String, b___ ], c_String ] :>
      EntityValue[ Entity[ a, b ], EntityProperty[ a, c ] ]
];


randomColorByteQ // Attributes = { HoldAllComplete };
randomColorByteQ[ RandomValue @ UniformDistribution @ { 0, 1 } ] := True;
randomColorByteQ[ a_ * RandomValue[ DiscreteUniformDistribution @ { 0, 255 } ] ] :=
    With[ { b = Rational[ 1, 255 ] },
        HoldComplete[ a ] === HoldComplete[ b ]
    ];


randomRules = Inline[ $intType, HoldComplete[
    RandomInteger[ ] :> RandomInteger[ { 0, 1 } ],
    RandomInteger[ n_? IntTypeQ ] :> RandomInteger[ { 0, n } ]
    ,
    { r1: RandomInteger[ n_? IntTypeQ ], r2: RandomInteger[ n_ ] .. } :>
      WithHolding[ { len = Length @ HoldComplete[ r1, r2 ] },
                   RandomInteger[ n, len ]
      ]
    ,
    RandomReal[ ] :> RandomReal[ 1 ],
    RandomReal[ n_? HoldNumericQ ] :> RandomReal[ { 0, n } ],


    (r: RandomReal|RandomInteger)[ range_, { n_? IntTypeQ, rest___ } ] :>
      Table[ r[ range, { rest } ], n ],

    (r: RandomReal|RandomInteger)[ range_, n_? IntTypeQ ] :>
      r[ range, { n } ],

    (r: RandomReal|RandomInteger)[ range_, { } ] :>
      r @ range,


    RandomColor[ n_? IntTypeQ ] :>
      Table[ RandomColor[ ], n ],

    RandomColor[ ] :>
      RGBColor[ RandomInteger[ { 0, 255 } ] / 255,
                RandomInteger[ { 0, 255 } ] / 255,
                RandomInteger[ { 0, 255 } ] / 255,
                1
      ],

    RGBColor @ Table[ x_? randomColorByteQ, { i_, 1, 3, 1 } ] /;
        FreeQ[ HoldComplete @ x, HoldPattern @ i ] :>
            RandomColor[ ],

    RGBColor[ a1___, RandomReal[ { 0, 1 } ], a2___ ] :>
      RGBColor[ a1, RandomInteger[ { 0, 255 } ] / 255, a2 ],

(*    RandomInteger[range_,
        n : _Integer | {__Integer} /; Times @@ n <= $unrollLimit] :>
      TrEval@ConstantArray[TempHold[RandomInteger[range]], n],
    RandomReal[range_,
        n : _Integer | {__Integer} /; Times @@ n <= $unrollLimit] :>
      TrEval@ConstantArray[TempHold[RandomReal[range]], n],*)

    RandomInteger[{a_? IntTypeQ, b_? IntTypeQ}, n_? IntTypeQ] :>
      Table[RandomInteger[{a, b}], n],

    RandomInteger[b_? IntTypeQ] + (a_? IntTypeQ) :>
      RandomInteger[{a, a + b}],

    RandomSample @ Table[ rv : RandomValue[ DiscreteUniformDistribution[ { _Integer, _Integer } ] ], iters___ ] :>
      Table[ rv, iters ],


    (* sandbox evaluated forms *)
    Unsafe["System`RandomInteger", {{a_Integer, b_Integer}}] :>
      WithHolding[{c = b - a},
          Unsafe["System`RandomInteger", {{0, c}}]
      ],

    RandomInteger[ { a_, b_ } ] :>
      RandomValue @
          DiscreteUniformDistribution[ { a, b } ]
    ,
    RandomReal[ { a_, b_ } ] :>
      RandomValue @
          UniformDistribution[ { a, b } ]
    ,

    Part[ { a___ },
          RandomValue @ DiscreteUniformDistribution @ { 1, n_Integer }
    ] /; Length @ HoldComplete @ a === n && ! OrderedQ @ Unevaluated @ { a } :>
      WithHolding[ { sorted = Sort @ TempHold @ a },
          Part[ { sorted }, RandomValue @ DiscreteUniformDistribution @ {1, n}]
      ]
    ,


    Plus[ a___,
          i_? IntTypeQ,
          RandomValue @ DiscreteUniformDistribution @ {
              imin_? IntTypeQ,
              imax_? IntTypeQ
            },
          b___
    ] :>
      Plus[ a,
            RandomValue @ DiscreteUniformDistribution @ { imin + i, imax + i },
            b
      ]
]];

extraRules = HoldComplete[
    Total[list_] :> Plus @@ list,

(*Subtract[a_,b_]\[RuleDelayed]TrEval[a-b],
   Divide[a_,b_]\[RuleDelayed]TrEval[a/b],*)
(*    Except[-1, x_?UNumericQ /; x < 0] :>
      PartialEvaluation[{x$}, x$ = -x; Times[-1, x$]],*)
(*    With[{s_ = val_, vars___}, exp_] /;
      FreeQ[Unevaluated[exp], HoldPattern[s]] :> With[{vars}, val; exp],
    With[{s_ = val_, vars___}, exp_] :>
      PartialEvaluation[{h},
          h = TempHold[exp] //. HoldPattern[s] -> TempHold[val];
          With[{vars}, h]],
    With[{}, exp_] :> exp
    ,*)
    x_? HoldNumericQ /; And[ SafeEvaluatedQ @ x === False,
                             FreeQ[ HoldComplete @ x, $UnsafeSymbolPattern ]
                        ] :> RuleCondition @ x
    ,
    Symbol[ name_String? UStringQ ] /;
        NameQ @ name && Context @ name === "System`" :>
            RuleCondition @ ToExpression[ name, InputForm, TempHold ]
];



syntaxRules = HoldComplete[
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
    a_[ b___, c_Composition, d___ ] :> a[ b, c[ # ] &, d ]
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

    Replace[ a_, b_, c__, (Rule|RuleDelayed)[ Heads, False ], d___ ] :>
        Replace[ a, b, c, d ],

    ReplaceAll[ a_, b_ ] :>
        Replace[ a, b, { 0, DirectedInfinity[ 1 ] }, Heads -> True ],

    System`MapApply[ f_, expr_ ] :>
        Apply[ f, expr, { 1 } ]
];


$$forwardOps = HoldPattern @ Alternatives[
    Apply,
    Map,
    System`MapApply
];

$$reverseOps = HoldPattern @ Alternatives[
    Replace,
    Select,
    Append,
    Prepend,
    ReplacePart,
    SelectFirst,
    FirstCase,
    Cases,
    DeleteCases,
    Position,
    Extract,
    Delete,
    AnyTrue,
    AllTrue,
    NoneTrue,
    SortBy,
    MaximalBy,
    MinimalBy,
    DeleteDuplicatesBy,
    CurryApplied
];

$$reverseOps2 = HoldPattern @ Alternatives[
    Insert
];

operatorFormRules = Inline[ { $$forwardOps, $$reverseOps }, HoldComplete[
    (h:$$forwardOps)[ a_ ][ b_ ] :> h[ a, b ],
    (h:$$reverseOps)[ a_ ][ b_ ] :> h[ b, a ],
    (h:$$reverseOps2)[ a_, b_ ][ c_ ] :> h[ c, a, b ]
] ];



$booleanFunctions1 = HoldPattern @ Alternatives[
    AcyclicGraphQ, AlgebraicIntegerQ, AlgebraicUnitQ, \
    AntihermitianMatrixQ, AntisymmetricMatrixQ, ArrayQ, AssociationQ, \
    AtomQ, AudioQ, BinaryImageQ, BioSequenceQ, BipartiteGraphQ, BooleanQ, \
    BoundaryMeshRegionQ, BoundedRegionQ, BusinessDayQ, ByteArrayQ, \
    ColorQ, CompatibleUnitQ, CompleteGraphQ, CompositeQ, ConnectedGraphQ, \
    ConnectedMoleculeQ, ConstantRegionQ, ContinuousTimeModelQ, \
    ConvexPolygonQ, ConvexPolyhedronQ, ConvexRegionQ, CoprimeQ, \
    CSGRegionQ, DataStructureQ, DateObjectQ, DeviceOpenQ, \
    DiagonalizableMatrixQ, DiagonalMatrixQ, DirectedGraphQ, DirectoryQ, \
    DiscreteTimeModelQ, DispatchQ, EdgeTaggedGraphQ, \
    EdgeTransitiveGraphQ, EdgeWeightedGraphQ, EmptyGraphQ, \
    EulerianGraphQ, EvenQ, ExactNumberQ, FailureQ, FileExistsQ, GraphQ, \
    HamiltonianGraphQ, HermitianMatrixQ, ImageQ, IndefiniteMatrixQ, \
    InexactNumberQ, IntegerQ, IrreduciblePolynomialQ, KnownUnitQ, ListQ, \
    LoopFreeGraphQ, LowerTriangularMatrixQ, MachineNumberQ, \
    ManagedLibraryExpressionQ, MatrixQ, MersennePrimeExponentQ, \
    MeshRegionQ, MissingQ, MixedGraphQ, MoleculeEquivalentQ, MoleculeQ, \
    MultigraphQ, NegativeDefiniteMatrixQ, NegativeSemidefiniteMatrixQ, \
    NormalMatrixQ, NumberQ, NumericArrayQ, NumericQ, OddQ, OptionQ, \
    OrthogonalMatrixQ, PacletObjectQ, PathGraphQ, PerfectNumberQ, \
    PermutationCyclesQ, PermutationListQ, PlanarGraphQ, PolynomialQ, \
    PositiveDefiniteMatrixQ, PositiveSemidefiniteMatrixQ, PossibleZeroQ, \
    PrimePowerQ, PrimeQ, QuadraticIrrationalQ, QuantityQ, \
    ReactionBalancedQ, RegionQ, RootOfUnityQ, SameQ, SatisfiableQ, \
    SimpleGraphQ, SimplePolygonQ, SimplePolyhedronQ, SolidRegionQ, \
    SparseArrayQ, SpatialObservationRegionQ, SquareFreeQ, SquareMatrixQ, \
    StringQ, StructuredArrayHeadQ, SymmetricMatrixQ, TautologyQ, TensorQ, \
    TimeObjectQ, TreeGraphQ, TreeLeafQ, TreeQ, TrueQ, UnateQ, \
    UndirectedGraphQ, UnitaryMatrixQ, UnsameQ, UpperTriangularMatrixQ, \
    ValueQ, VectorQ, VertexTransitiveGraphQ, VertexWeightedGraphQ, \
    VideoQ, WeaklyConnectedGraphQ, WeightedGraphQ
];

$booleanFunctionsN = HoldPattern @ Alternatives[
    SameQ, UnsameQ
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


booleanFunctionRules = Inline[ { $booleanFunctions1, $booleanFunctionsN },
    HoldComplete[
        TrueQ[ (h:$booleanFunctions1)[ x_ ] ] :> h @ x,
        TrueQ[ (h:$booleanFunctionsN)[ x___ ] ] :> h @ x,
        SameQ[ x_, True ] :> TrueQ @ x,
        SameQ[ True, x_ ] :> TrueQ @ x,
        AtomQ[ _? atomQ ] :> True,
        IntegerQ[ _? IntTypeQ ] :> True,
        StringQ[ _? StringTypeQ ] :> True,
        AssociationQ[ _? associationQ ] :> True
    ]
];


(* TODO:
    If[ False|True, ... ],
    StringQ[ _? StringTypeQ ] -> True,
    IntegerQ[ _? IntTypeQ ] -> True,
*)

$zero   =  0 |  0.0;
$posOne =  1 |  1.0;
$negOne = -1 | -1.0;

arithmeticRules = Inline[ { $zero, $posOne, $negOne }, HoldComplete[

    Verbatim[ Plus  ][ ] :> 0,
    Verbatim[ Times ][ ] :> 1,

    Power[ x_ ] :> x,
    Power[ Except[ $zero ], 0   ] :> 1,
    Power[ Except[ $zero ], 0.0 ] :> 1.0,
    Power[ o: $posOne, $negOne ] :> o,

    Verbatim[ Times ][ a___, $posOne, b___ ] :> Times[ a, b ],
    Verbatim[ Plus  ][ a___, $zero  , b___ ] :> Plus[  a, b ],

    Verbatim[ Times ][ -1, DirectedInfinity[ 1 ] ] :> DirectedInfinity[ -1 ],

    (* distribute Times over Plus *)
    Verbatim[ Times ][ x_, Verbatim[ Plus ][ a___ ] ] :>
      WithHolding[
          {
              dist = Replace[ TempHold @ Plus @ a,
                              e_ :> Times[ x, e ],
                              { 2 }
                     ]
          },
          dist
      ]

] ];


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
      Wolfram`CodeEquivalenceUtilities`Cached @ Quiet @ Replace[
          Check[ URLExpand @ url, url ],
          Except[ _String? StringQ ] :> Throw[ url, $tag ]
      ]
  ];

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

xxByFunctionRules = Inline[ $byFunction, HoldComplete[
    Select[ thing_, And[ this_, that__ ] & ] :>
      Select[ Select[ thing, this & ], And[ that ] & ]
    ,
    Select[ thing_, And[ this_ ] & ] :> Select[ thing, this & ]
    ,
    Select[ expr_, Equal[ args___ ] & ] :> Select[ expr, SameQ @ args & ]
    ,
    (select: $byFunction)[ thing_, Function[ f_[ #1 ] ], args___ ] :>
      select[ thing, f, args ]
] ];

eiwlRules = Inline[ { $intType, $byFunction }, HoldComplete[

    Import[ url_? urlQ, element: "Images"|"Hyperlinks" ] :>
      Import[ url, { "HTML", element } ]
    ,

    url_String? urlQ :> WithHolding[
        {
            expanded = FixedPoint[ expandURL, url, 5 ],
            urlData = KeySort @ Join[ URLParse @ expanded,
                                      <|
                                          "Scheme"   -> "https",
                                          "User"     -> None,
                                          "Port"     -> None,
                                          "Fragment" -> None
                                      |>
                                ]
        },
        URLBuild @ urlData
    ]
    ,

    Entity[ "HistoricalSite", "GreatPyramidOfGiza::kbgx6" ][ "Image"|EntityProperty[ "HistoricalSite", "Image" ] ] :>
      Entity[ "Building", "GreatPyramidOfGiza::jbm66" ][ EntityProperty[ "Building", "Image" ] ]
    ,

    EntityValue[
        Entity[ "HistoricalSite", "GreatPyramidOfGiza::kbgx6" ],
        EntityProperty[ "HistoricalSite", "Image" ]
    ] :>
      EntityValue[
          Entity[ "Building", "GreatPyramidOfGiza::jbm66" ],
          EntityProperty[ "Building", "Image" ]
      ]
    ,


    CountryData[ "G5" ] :> EntityList @ EntityClass[ "Country", "GroupOf5" ],

    Column[ c_ ] :> Column[ c, ItemSize -> { Automatic, Automatic } ]
    ,

    Circle[ pt_ ] :> Circle[ pt, 1 ]
    ,

    CurrencyConvert[ a_, Quantity[ b_, "USDollars" ] ] :>
      UnitConvert[ a, Quantity[ b, "USDollars" ] ]
    ,
    Style[ s_String, a___, n_? IntTypeQ, b___ ] :>
      Style[ s, a, FontSize -> n, b ]
    ,

    FromLetterNumber[ Table[ expr_, iter_ ] ] :>
      Table[ FromLetterNumber @ expr, iter ]
    ,

    Style[ t_? IntTypeQ, Plain ] :> t
    ,
    UnitConvert[ q_, unit_String ] :> UnitConvert[ q, Quantity @ unit ]
    ,
    Style[ a_, b___, n_? IntTypeQ, c___ ] :> Style[ a, b, FontSize -> n, c ]
    ,
    Round[ N[ a_ ], 1 ] :> Round[ a, 1 ]
    ,
    Round[ a_ ] :> Round[ a, 1 ]
    ,
    {items__}[[RandomValue[DiscreteUniformDistribution[{1, n_?IntTypeQ}]]]] /; (HoldComplete[#] === HoldComplete[n] &[Length[HoldComplete[items]]]) :>
      RandomChoice[{items}]
    ,
    Style[ a_, args__ ] /; ! OrderedQ[Unevaluated[{args}]] :>
      WithHolding[
          {
              sorted = Sort[ TempHold[ args ] ]
          },
          Style[ a, sorted ]
      ]
    ,
    Alphabet[ ][[ n_? IntTypeQ ]] :> FromLetterNumber[ n ]
    ,
    Alphabet[ ] :> CharacterRange[ "a", "z" ]
    ,
    ToUpperCase[ CharacterRange[ a_String, b_String ] ] :>
      WithHolding[ { au = ToUpperCase[ a ], bu = ToUpperCase[ b ] },
                   CharacterRange[ au, bu ]
      ]
    ,
    CharacterRange[ "a", "z" ][[ n_? IntTypeQ ]] :> FromLetterNumber[ n ]
    ,
    Quantity[ 7, "Days" ] :> Quantity[ 1, "Weeks" ]
    ,
    x_?IntTypeQ * RandomValue[DiscreteUniformDistribution[{0, y_}]] :>
      RandomValue[DiscreteUniformDistribution[{0, x * y}]]
    ,
    x_?HoldNumericQ * RandomValue[UniformDistribution[{0, y_}]] :>
      RandomValue[UniformDistribution[{0, x * y}]]
    ,
    360 * Degree :> 2 * Pi
    ,
    LetterNumber /@ Characters[ x_ ] :> LetterNumber[ x ]
    ,
    LetterNumber[ Characters[ x_ ] ] :> LetterNumber[ x ]
    ,
    HoldPattern[
        (f: FormFunction|FormPage|APIFunction)[
            { a___, (Rule|RuleDelayed)[ x_String, t_ ], b___ },
            body_,
            rest___
        ]
    ] /; ! LocalContextQ @ x :> WithHolding[
        {
            xx = NewLocalSymbol[ ],
            xn = StringArg @ xx,
            body2 = TempHold @ body /. x -> xn
        },
        f[ { a, xn -> t, b }, body2, rest ]
    ]
    ,
    Delayed[ (f:FormFunction|FormPage)[ args___ ] ] :> f[ args ]
    ,
    WordList[ ] :> WordList[ Language -> "English" ]
    ,
    Style[ a_ ] :> a
    ,
    Quantity[u_String] :> Quantity[1, u]
    ,
(*
    BarChart[ t : Table[ _? HoldNumericQ, { __? IntTypeQ } ] ] :>
      WithHolding[ { list = ReleaseHold @ CodeEquivalenceUtilities`FromCanonicalForm @ Hold @t },
                   BarChart @ list
      ]*)

    t : Table[ Fibonacci @ TypedSymbol[ i_, Verbatim[ _Integer ] ],
               { TypedSymbol[ i_, Verbatim[ _Integer ] ], _Integer, _Integer, _Integer }
        ] :>
      TrEval @ ReleaseHold @ Wolfram`CodeEquivalenceUtilities`FromCanonicalForm @ Hold @ t
    ,



    (* Hardcoded Overrides: *)

    (* x11.10 *)
    HoldPattern[Table[expr_, {c_, CharacterRange["a", "z"]|CharacterRange["A", "Z"]}]] :>
      WithHolding[
          {
              n = NewLocalSymbol[],
              expr2 =
                TempHold[expr] /.
                  HoldPattern[c] :> ToUpperCase[FromLetterNumber[n]]
          },
          Table[expr2, {n, 1, 26, 1}]
      ]
    ,
    CharacterRange["A", "Z"][[ n_ ]] :>
      ToUpperCase[FromLetterNumber[n]]
    ,
    Length[CharacterRange["a", "z"]] :> 26
    ,
    Length[CharacterRange["A", "Z"]] :> 26
    ,
    ToUpperCase[ ToUpperCase[ arg_ ] ] :> ToUpperCase[ arg ]
    ,
    DateObject[ d_DateObject ] :> d
    ,


    (* x19.1 *)
    UnitConvert[-DateObject[{1900, 1, 1}] +
      DateObject[DateList[][[1 ;; 3]]], Quantity[1, "Weeks"]] :>
      UnitConvert[DateObject[DateList[]] - DateObject[{1900, 1, 1}],
          Quantity[1, "Weeks"]]
    ,
    N[UnitConvert[DateObject[DateList[]] - DateObject[{1900, 1, 1}],
        Quantity[1, "Weeks"]]] :>
      UnitConvert[DateObject[DateList[]] - DateObject[{1900, 1, 1}],
          Quantity[1, "Weeks"]]
    ,
    DateObject[DateObject[DateList[][[1 ;; 3]]] - Quantity[1, "Weeks"]] :>
      DateObject[DateList[][[1 ;; 3]]] - Quantity[1, "Weeks"]
    ,

    (* 19.11 *)
    AirTemperatureData[
        Entity["Building",
            "EiffelTower::5h9w8"], {DateObject[DateList[]] -
      Quantity[1, "Weeks"], DateObject[DateList[]]}] :>
      AirTemperatureData[
          Entity["Building",
              "EiffelTower::5h9w8"], {DateObject[DateList[][[1 ;; 3]]] -
        Quantity[1, "Weeks"], DateObject[DateList[][[1 ;; 3]]]}]
    ,
    DatePlus[DateObject[d_], q_] :> DateObject[d] + q
    ,
    AirTemperatureData[
        Entity["Building",
            "EiffelTower::5h9w8"], {DateObject[DateList[][[1 ;; 3]]] -
      Quantity[1, "Weeks"], DateObject[DateList[]]}] :>
      AirTemperatureData[
          Entity["Building",
              "EiffelTower::5h9w8"], {DateObject[DateList[][[1 ;; 3]]] -
        Quantity[1, "Weeks"], DateObject[DateList[][[1 ;; 3]]]}]
    ,


    (* 35.7 *)
    Permutations[{"l", "i", "m", "a"}] :> Permutations[{"a", "i", "l", "m"}]
    ,
    Cases[(Interpreter["City"][#1] &) /@
      StringJoin /@ Permutations[{"a", "i", "l", "m"}], _Entity, {1}] :>
      Cases[Interpreter["City"][
          StringJoin /@ Permutations[{"a", "i", "l", "m"}]], _Entity, {1}]

    ,

    WikipediaData[ str_String? UStringQ ] :>
        With[ { lower = ToLowerCase @ str },
            WikipediaData @ lower /; lower =!= str
        ]
] ];


$gfx = HoldPattern @ Alternatives[
    Graphics,
    Graphics3D,
    Canonical @ Graphics,
    Canonical @ Graphics3D
];

graphicsRules = Inline[ $gfx, HoldComplete[

    ListLinePlot[ expr_, opts___ ] /;
      FreeQ[ HoldComplete @ opts,
             HoldPattern[ Joined -> _ ],
             { 1 }
      ] :> ListPlot[ expr, Joined -> True, opts ]
    ,

    (gfx: $gfx)[ p_, opts___ ] /; ! FreeQ[ HoldComplete @ p, Style[ _, _ ] ] :>
      WithHolding[
          {
              held = TempHold @ p /. Style[ a_, b_ ] :> { b, a }
          },
          gfx[ held, opts ]
      ]
    ,

    (gfx: $gfx)[ { a___, b_List, c___ }, opts___ ] :>
      WithHolding[
          {
              held = { Flatten[ TempHold @@@ TempHold[ a, b, c ] ] }
          },
          gfx[ held, opts ]
      ]
    ,

    Show[ a: $gfx[ ___ ],
          b: $gfx[ ___ ]..,
          opts: OptionsPattern[ ]
    ] :>
      Show[ { a, b }, opts ]
] ];

bitQ // Attributes = { HoldAllComplete };
bitQ[ 0|1 ] := True;
bitQ[ ___ ] := False;

bitMatrixQ // Attributes = { HoldAllComplete };
bitMatrixQ[ m_ ] := MatrixQ[ Unevaluated @ m, bitQ ];

arrayUnpadCF := arrayUnpadCF =
  Compile[ { { m, _Integer, 2 } },
      Block[ { a, b, c, d, i, len, top, bottom, left, right },
          a = Abs @ m;
          b = Reverse @ a;
          c = Transpose @ a;
          d = Reverse @ c;

          { top, bottom, left, right } = Table[
              i = 0;
              len = Length @ x;
              While[ ++i < len && Total @ x[[ i ]] === 0 ];
              i,
              { x, { a, b, c, d } }
          ];

          m[[ top ;; -bottom, left ;; -right ]]
      ]
  ];

arrayUnpad[ m_ ] := Quiet @ arrayUnpadCF @ m;

combineDilationKernels[ m1_, m2_ ] :=
  Module[ { a1, a2 },
      a1 = arrayUnpad @ m1;
      a2 = arrayUnpad @ m2;
      SelectFirst[ HoldComplete[ a1.a2, a2.a1 ],
                   bitMatrixQ[ # ] &,
                   $Failed
      ]
  ];


imageRules = HoldComplete[
    (f: Erosion|Dilation)[ img_, r_? IntTypeQ ] :> f[ img, BoxMatrix @ r ],
    (f: Erosion|Dilation)[
        (f: Erosion|Dilation)[ img_, m1_? bitMatrixQ ],
        m2_? bitMatrixQ
    ] :>
      With[ { m = combineDilationKernels[ m1, m2 ] },
          f[ img, m ] /; bitMatrixQ @ m
      ]
];

cloudRules = HoldComplete[

    (* standardize permissions *)
    HoldPattern[
        Permissions -> permissions: Except[ _Canonical, _? SafeEvaluatedQ ]
    ] :> With[
        {
            normal = (
                CloudObject;
                CloudObject`Private`normalizePermissions[
                    permissions,
                    "application/mathematica",
                    CloudObject
                ]
            )
        },
        (Permissions -> Canonical @ normal) /; True
    ]
    ,
    HoldPattern[ Permissions -> $Permissions ] :>
      Permissions -> "Private"
    ,
    HoldPattern[ Permissions :> permissions_ ] :>
      Permissions -> permissions
    ,
    (h: CloudPut|CloudDeploy|CloudPublish)[
        a: Except[ Permissions -> _ ]...
    ] :> h[ a, Permissions -> $Permissions ]
    ,

    (* standardize anonymous cloud objects *)
    CloudObject[ url_String? UStringQ, opts___ ] :>
      With[ { new = canonicalizeCloudObjectURL @ url },
          CloudObject[ Canonical @ new, opts ] /; True
      ]
    ,

    (* standardize deploy targets *)
    (h: CloudPut|CloudDeploy|CloudPublish)[
        a_, b: Except[ _Canonical|_Rule|_RuleDelayed ], c___
    ] :>
      With[ { std = canonicalizeCloudObjectURL @ b },
          h[ a, Canonical @ std, c ] /; True
      ]
    ,
    (h: CloudPut|CloudDeploy|CloudPublish)[ a_, b: (_Rule|_RuleDelayed)... ] :>
      h[ a, CloudObject[ ], b ]
    ,

    (* EmbedCode *)
    EmbedCode[ code_ ] :> EmbedCode[ code, "HTML" ]
];


canonicalizeCloudObjectURL[ CloudObject[ url_String, opts___ ] ] :=
  CloudObject[ canonicalizeCloudObjectURL @ url, opts ];

canonicalizeCloudObjectURL[ url_String ] :=
  Module[ { obj, expanded, normal },
      obj       = CloudObject @ url;
      expanded  = First @ obj;

      If[ StringEndsQ[ expanded, "/"~~uuid__~~EndOfString /; UUIDStringQ @ uuid ],
          Canonical[ ],
          expanded

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

priority1Rules = HoldComplete[
    Floor[ i_? IntTypeQ ] :> i
    ,
    Ceiling[ i_? IntTypeQ ] :> i
    ,
    Round[ i_? IntTypeQ ] :> i
    (* ,
    x_? HoldNumericQ /; ! expandedQ @ x :> TrEval @ expandBlocked @ x,
    x_? HoldNumericQ /; ! simplifiedQ @ x :> TrEval @ simplify @ x *)
    ,
    (gfx: Graphics|Graphics3D)[ a_, b___ ] :>
      WithHolding[
          {
              c = TempHold @ a /.
                { ints__Integer } /; Length @ HoldComplete @ ints >= 4 :>
                  { Canonical @ ints }
          },
          Canonical[ gfx ][ c, b ]
      ]
];


cleanupRules = HoldComplete[
    TypedSymbol[ TypedSymbol[ s_, t_ ], t_ ] :> TypedSymbol[ s, t ]
];


rules = {
    HoldComplete[(f_)[a1___, TempHold[a2___], a3___] :> f[a1, a2, a3]],
    priority1Rules,
    attributeRules,
    typingPatterns,
    tableFromOtherFunctions,
    tableIteratorRules,
    orderlessLevel2,
    flatLevel2,
    tableScopingRules,
    typedRules,
    systemSymbols,
    pureFunctionRules,
    systemDownvaluesNull,
    listRules,
    colorRules,
    stringRules,
    entityFrameworkRules,
    randomRules,
    imageRules,
    cloudRules,
    xxByFunctionRules,
    eiwlRules,
    extraRules,
    syntaxRules,
    operatorFormRules,
    arithmeticRules,
    booleanFunctionRules,
    graphicsRules,
    cleanupRules
} /. HoldPattern[$unrollLimit] -> $unrollLimit;

$TransformationRules = rules;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)