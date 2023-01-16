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
    "Description" -> "Rewrite Table iterators",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Table }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

hasSeqFunQ[ rng: { __Integer | __Rational } ] := hasSeqFunQ[ rng ] = TrueQ @
    And[ Length @ rng >= 4,
         ! constantArrayQ @ rng,
         MatchQ[
             Replace[
                 findSequenceFunction @ rng,
                 f_Function :> (seqFun[ rng ] = f)
             ],
             _Function
         ]
    ];


findSequenceFunction[ rng_ ] :=
    Quiet @ Module[ { n, f },
        f = TimeConstrained[ FindSequenceFunction[ rng, n ], 1, $Failed ];
        If[ MatchQ[ f, _FindSequenceFunction | _? FailureQ ],
            $Failed,
            With[ { expr = f },
                Function @ Evaluate[ expr /. n -> # ]
            ]
        ]
    ];


$atomicNumber = (_Integer|_Real|_Rational)? UAtomQ;
$heldNumeric = Alternatives[
    _? HoldNumericQ,
    (TransformHold|TransformRelease)[ _? HoldNumericQ ]
];


singleArgumentQ // Attributes = { HoldAllComplete };
singleArgumentQ[ _SlotSequence|_Sequence ] := False;
singleArgumentQ[ _                       ] := True;
singleArgumentQ[ ___                     ] := False;


(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

Inline[ { $atomicNumber, $heldNumeric }, HoldComplete[
    Table[exp_, ii_, jj_] :> Table[Table[exp, jj], ii],

    Table[exp_, imax: $heldNumeric] :>
      WithHolding[ { i = NewLocalSymbol[] }, Table[exp, {i, 1, imax, 1}]],

    Table[exp_, {imax: $heldNumeric}] :>
      WithHolding[ { i = NewLocalSymbol[] }, Table[exp, {i, 1, imax, 1}]],

    Table[exp_, {i_, imax: $heldNumeric}] :> Table[exp, {i, 1, imax, 1}],
    Table[exp_, {i_, imin_, imax_}] :> Table[exp, {i, imin, imax, 1}]
    ,

    Table[ expr_, { i_, 1, imax: (_Rational|_Real)? UAtomQ, 1 } ] :>
        With[ { n = Floor @ imax }, Table[ expr, { i, 1, n, 1 } ] /; True ],

    (* remove redundant Tables in iterator *)
    Table[ expr_, { i_, Table[ j_, { j_, j1_, j2_, dj_ } ] } ] :>
      Table[ expr, { i, j1, j2, dj } ],
    Table[ a_, { i1_, Table[ b_, { i2_, imin_, imax_, di_ } ] } ] :>
        With[ { aNew = TempHold @ a /. HoldPattern @ Verbatim @ i1 :> b },
            Table[ aNew, { i2, imin, imax, di } ] /; True
        ]
    ,

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


    (* unused list iterators *)
    Table[ expr_, { i_, ii_List } ] /;
        FreeQ[ HoldComplete @ expr, HoldPattern @ Verbatim @ i ] :>
        WithHolding[ { j = NewLocalSymbol[ ], len = Length @ Unevaluated @ ii },
            Table[ expr, { j, len } ]
        ],

    (* empty tables *)
    Table[ _, { _TypedSymbol, 1, 0, 1 } ] :> { },
    Table[ _, { _, { } } ] :> { },

    (* short tables *)
    Table[ expr_, { i: _Symbol|_TypedSymbol, 1, n: 1|2|3, 1 } ] :>
        RuleCondition @ Table[
            TempHold @ expr /. HoldPattern @ Verbatim @ i -> j,
            { j, n }
        ],

    Table[ expr_, { i: _Symbol|_TypedSymbol, { ii___ } } ] /;
        Length @ HoldComplete @ ii < 4 :>
        RuleCondition @ With[
            { len = Length @ HoldComplete @ ii },
            Table[
                TempHold @ expr /. HoldPattern @ Verbatim @ i -> j,
                { j, TempHold /@ Unevaluated[ { ii } ] }
            ]
        ],

    Verbatim[ Plus ][ a___, b_?HoldNumericQ, c___, d_?HoldNumericQ, e___ ] /;
      Length[ HoldComplete[ a, c, e ] ] > 0 &&
      ! SafeEvaluatedQ[b + d] && ! SafeEvaluatedQ[d + b] :>
        WithHolding[ { bd = b + d },
                     Plus[ a, bd, c, e ]
        ],

    p: Verbatim[ Plus ][ x_? singleArgumentQ ] :> x,

    (* Table properties *)
    Length @ Table[ _, { _, 1, n_Integer, 1 } ] /;
        IntegerQ @ Unevaluated @ n :> RuleCondition @ Max[ 0, n ],

    Count[ list_, Verbatim[ _ ] | Verbatim[ Pattern ][ _, Verbatim[ Blank[ ] ] ] ] :>
        Length @ list

    (* TODO: account for possible `Nothing` values *)
] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)