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

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*randomColorByteQ*)
randomColorByteQ // ClearAll;
randomColorByteQ // Attributes = { HoldAllComplete };
randomColorByteQ[ RandomValue @ UniformDistribution @ { 0, 1 } ] := True;
randomColorByteQ[ Verbatim[ Times ][ a_, RandomValue @ DiscreteUniformDistribution @ { 0, 255 } ] ] :=
    With[ { b = Rational[ 1, 255 ] }, HoldComplete @ a === HoldComplete @ b ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
Inline[ $intType, HoldComplete[
    RandomInteger[ ] :> RandomInteger[ { 0, 1 } ],
    RandomInteger[ n_? IntTypeQ ] :> RandomInteger[ { 0, n } ]
    ,
    { r1: RandomInteger[ n_? IntTypeQ ], r2: RandomInteger[ n_ ] .. }  :>
        With[ { len = Length @ HoldComplete[ r1, r2 ] },
            RandomInteger[ n, len ] /; len >= 4
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
]]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)