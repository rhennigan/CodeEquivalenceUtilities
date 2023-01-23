(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

$FlatLevel2Functions;
$OrderlessLevel2Functions;
$RestorationReplacements;
$StructuralTransformations;
CanonicalCompoundExpression;
CanonicalTable;
CanonicalTransformCompoundExpressions;
CanonicalTransformFromStructure;
CanonicalTransformLevel2Flat;
CanonicalTransformLevel2Orderless;
CanonicalTransformTables;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Patterns*)

$tableMapPatt = HoldPattern @ Map[ _, Table[ _, ___ ] ];

$tableListablePatt =
    Alternatives[
        HoldPattern[ (f_)[ a_, Table[ _, ___ ] ] ] /;
            reallyAtomQ @ a && MemberQ[ Attributes @ f, Listable ],
        HoldPattern[ (f_)[ Table[ _, ___ ], a_ ] ] /;
            reallyAtomQ @ a && MemberQ[ Attributes @ f, Listable ]
    ];

reallyAtomQ =
  Function[ x,
      If[ SymbolQ @ x,
          ReleaseHold[ HoldComplete @ UAtomQ @ x /. OwnValues @ x ],
          UAtomQ @ x
      ],
      HoldAllComplete
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$StructuralTransformations*)
$StructuralTransformations = {
    CanonicalTransformTables,
    CanonicalTransformCompoundExpressions,
    CanonicalTransformLevel2Orderless,
    CanonicalTransformLevel2Flat
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$RestorationReplacements*)
$RestorationReplacements := {
    CanonicalCompoundExpression -> CompoundExpression,
    CanonicalTable              -> Table
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$OrderlessLevel2Functions*)
$OrderlessLevel2Functions := { Length, Max, Mean, Min, Sort, Total };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$FlatLevel2Functions*)
$FlatLevel2Functions := { Flatten, Max, Min };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalCompoundExpression*)
CanonicalCompoundExpression // Attributes = { HoldAllComplete };
CanonicalCompoundExpression /:
  Normal @ CanonicalCompoundExpression[ args___ ] :=
    HoldComplete @ CompoundExpression @ args;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTable*)
CanonicalTable // Attributes = { HoldAllComplete };
CanonicalTable /:
    Normal @ CanonicalTable[ args___ ] := HoldComplete @ Table @ args;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformCompoundExpressions*)
CanonicalTransformCompoundExpressions[ expression_ ] :=
  expression // RightComposition[
      eliminateAllExtraCEArgs,
      ceIdentity
      (* TODO: add more transformations *)
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*eliminateAllExtraCEArgs*)
eliminateAllExtraCEArgs[ exp_ ] :=
  Module[ { pos, folder },
      pos = Position[ exp, _CompoundExpression ] ~SortBy~ Length // Reverse;
      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, elimCompExpArgs ] ] &;
      Fold[ folder, exp, pos ] //.
        CanonicalCompoundExpression -> CompoundExpression
  ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*elimCompExpArgs*)
elimCompExpArgs // Attributes = { HoldFirst };

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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*depGraph*)
depGraph // Attributes = { HoldAllComplete };

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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*extractCE*)
extractCE // Attributes = { HoldAllComplete };
extractCE[ HoldPattern @ CompoundExpression[ args___ ] ] :=
  TempHold[ args ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ceIdentity*)
ceIdentity[ exp_ ] :=
  exp //. HoldPattern @ CompoundExpression[ e_ ] :> e;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformFromStructure*)
CanonicalTransformFromStructure[ exp_ ] :=
  Module[ { transformed },
      transformed = exp // RightComposition @@ $StructuralTransformations;
      transformed //. CanonicalCompoundExpression -> CompoundExpression
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformLevel2Flat*)
CanonicalTransformLevel2Flat[ expression_ ] :=
  expression // RightComposition @@
    flatLevel2 /@ $FlatLevel2Functions;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*flatLevel2*)
flatLevel2[ function_ ] :=
  Function[ expression,
            expression //.
              f : HoldPattern @ function[ a_ ] /; ! UFlatQ @ a :>
                RuleCondition @ HFlatten @ f //.
                  TempHold[ a___ ] :> a
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformLevel2Orderless*)
CanonicalTransformLevel2Orderless[ expression_ ] :=
  expression // RightComposition @@
    orderlessLevel2 /@ $OrderlessLevel2Functions;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*orderlessLevel2*)
orderlessLevel2[ function_ ] :=
  Function[ expression,
            expression //.
              HoldPattern @ function @ l : h_[ a___ ] /; ! UOrderedQ @ l :>
                RuleCondition @ TempHold[ h ] @ Sort @ TempHold @ a //.
                  TempHold[ f_ ] @ TempHold[ a___ ] :> function @ f @ a
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformTables*)
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*allRangeToTable*)
allRangeToTable[ exp_ ] :=
  exp //. r_Range :> RuleCondition @ rangeToTable @ r //.
    {
        CanonicalTable -> Table,
        TempHold[ a___ ] :> a
    };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rangeToTable*)
rangeToTable // Attributes = { HoldFirst };

rangeToTable[ HoldPattern @ Range[ args___? NumericQ ] ] :=
  With[ { i = NewLocalSymbol[ ] },
      fillTableIterators @ Table[ i, { i, args } ]
  ];

rangeToTable[ HoldPattern @ Range[ args___ ] ] :=
  TempHold[ Range ][ TempHold @ args ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*allArrayToTable*)
allArrayToTable[ exp_ ] :=
  exp //. a_Array :> RuleCondition @ arrayToTable @ a //.
    {
        CanonicalTable -> Table,
        TempHold[ a___ ] :> a
    };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*arrayToTable*)
arrayToTable // Attributes = { HoldFirst };

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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*distributeAllMapsOverTables*)
distributeAllMapsOverTables[ exp_ ] :=
  Inline[ $tableMapPatt,
      exp //. m : $tableMapPatt :> RuleCondition @ distributeMapOverTable @ m //.
        TempHold[ a___ ] :> a
  ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*distributeMapOverTable*)
distributeMapOverTable // Attributes = { HoldFirst };
distributeMapOverTable[ HoldPattern @ Map[ f_, Table[ exp_, iter___ ] ] ] :=
  TempHold @ Table[ f @ exp, iter ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*distributeAllListableOverTable*)
distributeAllListableOverTable[ exp_ ] :=
  iDistributeAllListableOverTable ~FixedPoint~ exp;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*iDistributeAllListableOverTable*)
iDistributeAllListableOverTable[ exp_ ] :=
  Inline[ $tableListablePatt,
      exp //. t : $tableListablePatt :>
        RuleCondition @ distributeListableOverTable @ t //.
          TempHold[ a___ ] :> a
  ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*distributeListableOverTable*)
distributeListableOverTable // Attributes = { HoldFirst };

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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*unrollAllTables*)
unrollAllTables[ exp_ ] :=
  exp //. t : HoldPattern @ Table[ _, _, __ ] :> RuleCondition @ unrollTable @ t //.
      TempHold[ a___ ] :> a;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*unrollTable*)
unrollTable // Attributes = { HoldFirst };

unrollTable[ HoldPattern @ Table[ exp_, i_ ] ] :=
  TempHold @ Table[ exp, i ];

unrollTable[ HoldPattern @ Table[ exp_, i_, ii__ ] ] :=
  With[ { inner = unrollTable @ Table[ exp, ii ] },
      TempHold @ Table[ inner, i ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fillAllTableIterators*)
fillAllTableIterators[ expression_ ] :=
  expression //. t_Table :> RuleCondition @ fillTableIterators @ t //.
    {
        CanonicalTable -> Table,
        TempHold[ a___ ] :> a
    };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*fillTableIterators*)
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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)