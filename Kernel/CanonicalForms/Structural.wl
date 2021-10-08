Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


BeginPackage[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Structural`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`"
    }
];


Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)
$StructuralTransformations              ::usage = "";
$RestorationReplacements                ::usage = "";
$OrderlessLevel2Functions               ::usage = "";
$FlatLevel2Functions                    ::usage = "";
CanonicalTable                          ::usage = "";
CanonicalTransformTables                ::usage = "";
CanonicalCompoundExpression             ::usage = "";
CanonicalTransformCompoundExpressions   ::usage = "";
CanonicalTransformLevel2Orderless       ::usage = "";
CanonicalTransformLevel2Flat            ::usage = "";
CanonicalTransformFromStructure         ::usage = "";



Begin["`Private`"];



(******************************************************************************)



$StructuralTransformations =
  {
      CanonicalTransformTables,
      CanonicalTransformCompoundExpressions,
      CanonicalTransformLevel2Orderless,
      CanonicalTransformLevel2Flat
  };


$RestorationReplacements =
  {
      CanonicalCompoundExpression   -> CompoundExpression,
      CanonicalTable                -> Table
  };


$OrderlessLevel2Functions =
  {
      Length,
      Max,
      Mean,
      Min,
      Sort,
      Total
  };


$FlatLevel2Functions =
  {
      Flatten,
      Max,
      Min
  };



(******************************************************************************)



CanonicalTable // Attributes = { HoldAllComplete };
CanonicalTable // Options    = { };


CanonicalTable /:
  Normal @ CanonicalTable[ args___ ] :=
    HoldComplete @ Table @ args;



(******************************************************************************)



rangeToTable // ClearAll;
rangeToTable // Attributes = { HoldFirst };
rangeToTable // Options    = { };


rangeToTable[ HoldPattern @ Range[ args___? NumericQ ] ] :=
  With[ { i = NewLocalSymbol[ ] },
      fillTableIterators @ Table[ i, { i, args } ]
  ];


rangeToTable[ HoldPattern @ Range[ args___ ] ] :=
  TempHold[ Range ][ TempHold @ args ];



allRangeToTable // Attributes = { };
allRangeToTable // Options    = { };

allRangeToTable[ exp_ ] :=
  exp //. r_Range :> TrEval @ rangeToTable @ r //.
    {
        CanonicalTable -> Table,
        TempHold[ a___ ] :> a
    };



(******************************************************************************)



arrayToTable // ClearAll;
arrayToTable // Attributes = { HoldFirst };
arrayToTable // Options    = { };


arrayToTable[ HoldPattern @ Array[ f_, n_Integer ] ] :=
  With[ { i = NewLocalSymbol[ ] },
      CanonicalTable[ f @ i, { i, 1, n, 1 } ]
  ];


arrayToTable[ HoldPattern @ Array[ f_, n_Integer, r_ ] ] :=
  With[ { i = NewLocalSymbol[ ], nn = n + r - 1 },
      CanonicalTable[ f @ i, { i, r, nn, 1 } ]
  ];


arrayToTable[ HoldPattern @ Array[ args___ ] ] :=
  TempHold[ Array ][ TempHold @ args ];



allArrayToTable // Attributes = { };
allArrayToTable // Options    = { };

allArrayToTable[ exp_ ] :=
  exp //. a_Array :> TrEval @ arrayToTable @ a //.
    {
        CanonicalTable -> Table,
        TempHold[ a___ ] :> a
    };



(******************************************************************************)



distributeMapOverTable // ClearAll;
distributeMapOverTable // Attributes = { HoldFirst };
distributeMapOverTable // Options    = { };


distributeMapOverTable[ HoldPattern @ Map[ f_, Table[ exp_, iter___ ] ] ] :=
  TempHold @ Table[ f @ exp, iter ];



$tableMapPatt = HoldPattern @ Map[ _, Table[ _, ___ ] ];



distributeAllMapsOverTables // Attributes = { };
distributeAllMapsOverTables // Options    = { };


distributeAllMapsOverTables[ exp_ ] :=
  Inline[ $tableMapPatt,
      exp //. m : $tableMapPatt :> TrEval @ distributeMapOverTable @ m //.
        TempHold[ a___ ] :> a
  ];



(******************************************************************************)



distributeListableOverTable // ClearAll;
distributeListableOverTable // Attributes = { HoldFirst };
distributeListableOverTable // Options    = { };


reallyAtomQ =
  Function[ x,
      If[ SymbolQ @ x,
          ReleaseHold[ HoldComplete @ UAtomQ @ x /. OwnValues @ x ],
          UAtomQ @ x
      ],
      HoldAllComplete
  ];


distributeListableOverTable[
    HoldPattern @ (f_)[ a_, Table[ exp_, iter___ ] ] /;
      reallyAtomQ @ a && Attributes @ f ~MemberQ~ Listable
] :=
  TempHold @ Table[ f[ a, exp ], iter ];


distributeListableOverTable[
    HoldPattern @ (f_)[ Table[ exp_, iter___ ], a_ ] /;
      reallyAtomQ @ a && Attributes @ f ~MemberQ~ Listable
] :=
  TempHold @ Table[ f[ exp, a ], iter ];



$tableListablePatt =
  Alternatives[
      HoldPattern @ (f_)[ a_, Table[ _, ___ ] ] /;
        reallyAtomQ @ a && Attributes @ f ~ MemberQ ~ Listable,
      HoldPattern @ (f_)[ Table[ _, ___ ], a_ ] /;
        reallyAtomQ @ a && Attributes @ f ~ MemberQ ~ Listable
  ];



iDistributeAllListableOverTable // Attributes = { };
iDistributeAllListableOverTable // Options    = { };


iDistributeAllListableOverTable[ exp_ ] :=
  Inline[ $tableListablePatt,
      exp //. t : $tableListablePatt :>
        TrEval @ distributeListableOverTable @ t //.
          TempHold[ a___ ] :> a
  ];


distributeAllListableOverTable // Attributes = { };
distributeAllListableOverTable // Options    = { };


distributeAllListableOverTable[ exp_ ] :=
  iDistributeAllListableOverTable ~FixedPoint~ exp;



(******************************************************************************)



fillTableIterators // ClearAll;
fillTableIterators // Attributes = { HoldFirst };
fillTableIterators // Options    = { };

fillTableIterators::unroll =
  "Warning: Tables should be unrolled before filling iterators.";


fillTableIterators[ HoldPattern @ Table[ exp_, imax_? UNumericQ ] ] :=
  With[ { i = NewLocalSymbol[ ] },
      CanonicalTable[ exp, { i, 1, imax, 1 } ]
  ];


fillTableIterators[ HoldPattern @ Table[ exp_, { imax_? UNumericQ } ] ] :=
  With[ { i = NewLocalSymbol[ ] },
      CanonicalTable[ exp, { i, 1, imax, 1 } ]
  ];


fillTableIterators[ HoldPattern @ Table[ exp_, { i_, imax_? UNumericQ } ] ] :=
  CanonicalTable[ exp, { i, 1, imax, 1 } ];


fillTableIterators[ HoldPattern @ Table[ exp_, { i_, imin_, imax_ } ] ] :=
  CanonicalTable[ exp, { i, imin, imax, 1 } ];


fillTableIterators[ HoldPattern @ Table[ exp_, { i_, imin_, imax_, di_ } ] ] :=
  CanonicalTable[ exp, { i, imin, imax, di } ];


fillTableIterators[ HoldPattern @ Table[ e_, i_, ii__ ] ] :=
  (
      (* TODO: Unroll table and try again? *)
      Message[ fillTableIterators::unroll ];
      FailedTransform[ Table ][ e, i, ii ]
  );


fillTableIterators[ HoldPattern @ Table[ args___ ] ] :=
  TempHold[ TempHold[ Table ][ args ] ];



fillAllTableIterators // ClearAll;
fillAllTableIterators // Attributes = { };
fillAllTableIterators // Options    = { };


fillAllTableIterators[ expression_ ] :=
  expression //. t_Table :> TrEval @ fillTableIterators @ t //.
    {
        CanonicalTable -> Table,
        TempHold[ a___ ] :> a
    };



(******************************************************************************)



unrollTable // ClearAll;
unrollTable // Attributes = { HoldFirst };
unrollTable // Options    = { };


unrollTable[ HoldPattern @ Table[ exp_, i_ ] ] :=
  TempHold @ Table[ exp, i ];


unrollTable[ HoldPattern @ Table[ exp_, i_, ii__ ] ] :=
  With[ { inner = unrollTable @ Table[ exp, ii ] },
      TempHold @ Table[ inner, i ]
  ];



unrollAllTables // ClearAll;
unrollAllTables // Attributes = { };
unrollAllTables // Options    = { };


unrollAllTables[ exp_ ] :=
  exp //. t : HoldPattern @ Table[ _, _, __ ] :> TrEval @ unrollTable @ t //.
      TempHold[ a___ ] :> a;



(******************************************************************************)



CanonicalTransformTables // Attributes = { };
CanonicalTransformTables // Options    = { };


CanonicalTransformTables[ expression_ ] :=
  expression // RightComposition[
      allRangeToTable,
      allArrayToTable,
      distributeAllMapsOverTables,
      (*distributeAllListableOverTable,*)
      unrollAllTables,
      fillAllTableIterators
      (* TODO: add more transformations *)
  ];



(******************************************************************************)



CanonicalCompoundExpression // Attributes = { HoldAllComplete };
CanonicalCompoundExpression // Options    = { };


CanonicalCompoundExpression /:
  Normal @ CanonicalCompoundExpression[ args___ ] :=
    HoldComplete @ CompoundExpression @ args;



(******************************************************************************)



extractCE // ClearAll;
extractCE // Attributes = { HoldAllComplete };
extractCE // Options    = { };


extractCE[ HoldPattern @ CompoundExpression[ args___ ] ] :=
  TempHold[ args ];



depGraph // ClearAll;
depGraph // Attributes = { HoldAllComplete };
depGraph // Options    = { };


depGraph[ c_CompoundExpression ] :=
  With[ { th = extractCE @ c }, depGraph @ th ];


depGraph[ th : TempHold[ __, _ ] ] :=

  Module[
      {
          result, init, resIdx, hidden, symbols,
          thPatt, symPatt, isFree, depIdx, graphNext
      },

      result  = Last   @ th;
      init    = Most   @ th;
      resIdx  = Length @ th;

      symbols = ExtractSymbols[ result, HoldPattern, Infinity, Heads -> True ];
      thPatt  = Verbatim[ HoldPattern ][ TempHold ];
      symPatt = Alternatives @@ DeleteCases[ symbols, thPatt ];

      hidden  = HideLocalSymbols @ init;
      isFree  = e_ /; ! FreeQ[ Hold @ e, symPatt ];
      depIdx  = Flatten @ Position[ hidden, isFree, { 1 } ];

      Sow @ Thread[ resIdx -> depIdx ];

      graphNext = With[ { next = init[[ ;; # ]] }, depGraph @ next ] &;

      graphNext /@ depIdx
  ];


depGraph[ th : TempHold[ _ ] ] := th;



elimCompExpArgs // ClearAll;
elimCompExpArgs // Attributes = { HoldFirst };
elimCompExpArgs // Options    = { };


elimCompExpArgs[ th_TempHold ] :=
  Module[ { reap, parts },

      reap = Last @ Reap @ depGraph @ th;

      parts = If[ Flatten @ reap === {  }
                  ,
                  { Length @ th }
                  ,
                  reap //
                    RightComposition[
                        Last,
                        First,
                        Flatten,
                        Graph,
                        TopologicalSort,
                        Reverse
                    ]
              ];

      CanonicalCompoundExpression @@ th[[ parts ]]
  ];


elimCompExpArgs[ ce_CompoundExpression ] :=
  With[ { th = extractCE @ ce },
      elimCompExpArgs @ th
  ];



eliminateAllExtraCEArgs // ClearAll;
eliminateAllExtraCEArgs // Attributes = { };
eliminateAllExtraCEArgs // Options    = { };


eliminateAllExtraCEArgs[ exp_ ] :=
  Module[ { pos, folder },
      pos = Position[ exp, _CompoundExpression ] ~SortBy~ Length // Reverse;
      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, elimCompExpArgs ] ] &;
      Fold[ folder, exp, pos ] //.
        CanonicalCompoundExpression -> CompoundExpression
  ];



(******************************************************************************)



ceIdentity // ClearAll;
ceIdentity // Attributes = { };
ceIdentity // Options    = { };


ceIdentity[ exp_ ] :=
  exp //. HoldPattern @ CompoundExpression[ e_ ] :> e;



(******************************************************************************)



CanonicalTransformCompoundExpressions // Attributes = { };
CanonicalTransformCompoundExpressions // Options    = { };


CanonicalTransformCompoundExpressions[ expression_ ] :=
  expression // RightComposition[
      eliminateAllExtraCEArgs,
      ceIdentity
      (* TODO: add more transformations *)
  ];



(******************************************************************************)


orderlessLevel2 // ClearAll;
orderlessLevel2 // Attributes = { };
orderlessLevel2 // Options    = { };


orderlessLevel2[ function_ ] :=
  Function[ expression,
            expression //.
              HoldPattern @ function @ l : h_[ a___ ] /; ! UOrderedQ @ l :>
                TrEval @ TempHold[ h ] @ Sort @ TempHold @ a //.
                  TempHold[ f_ ] @ TempHold[ a___ ] :> function @ f @ a
  ];



(******************************************************************************)



CanonicalTransformLevel2Orderless // Attributes = { };
CanonicalTransformLevel2Orderless // Options    = { };


CanonicalTransformLevel2Orderless[ expression_ ] :=
  expression // RightComposition @@
    orderlessLevel2 /@ $OrderlessLevel2Functions;



(******************************************************************************)



flatLevel2 // ClearAll;
flatLevel2 // Attributes = { };
flatLevel2 // Options    = { };


flatLevel2[ function_ ] :=
  Function[ expression,
            expression //.
              f : HoldPattern @ function[ a_ ] /; ! UFlatQ @ a :>
                TrEval @ HFlatten @ f //.
                  TempHold[ a___ ] :> a
  ];



(******************************************************************************)



CanonicalTransformLevel2Flat // Attributes = { };
CanonicalTransformLevel2Flat // Options    = { };


CanonicalTransformLevel2Flat[ expression_ ] :=
  expression // RightComposition @@
    flatLevel2 /@ $FlatLevel2Functions;



(******************************************************************************)



CanonicalTransformFromStructure // Attributes = { };
CanonicalTransformFromStructure // Options    = { };


CanonicalTransformFromStructure[ exp_ ] :=
  Module[ { transformed },
      transformed = exp // RightComposition @@ $StructuralTransformations;
      transformed //. CanonicalCompoundExpression -> CompoundExpression
  ];



(******************************************************************************)



End[ ];

EndPackage[ ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
