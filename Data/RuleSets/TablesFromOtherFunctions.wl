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
    "Description" -> "Convert expressions to Table where possible",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { Table }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

HoldComplete[
    Range[ args: Repeated[ _? HoldNumericQ, { 1, 3 } ] ] :>
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
    Map[ f_, list_, { 1 } ] :>
        With[ { i = NewLocalSymbol[ ] },
            Table[ f @ i, { i, list } ] /; True
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
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)