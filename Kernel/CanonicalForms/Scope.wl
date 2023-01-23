(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
$LocalContext;
$LocalSymbolPrefix;
$NewLocalSymbolCounter;
$ScopeTransformations;
CanonicalScopeAll;
CanonicalScopeDelayed;
CanonicalScopeFunctions;
CanonicalScopeRuleDelayed;
CanonicalScopeSetDelayed;
CanonicalScopeTables;
CanonicalScopingConstructs;
CanonicalTransformFromScope;
HideLocalSymbols;
LocalContextQ;
NewLocalSymbol;
NormalizeNames;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

(*
    In this file, SC = "Scoping Constructs".
    These are lexical renaming utilities for With, Module, and Block.
*)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Patterns*)

$delayedForms = Condition|RuleDelayed|SetDelayed|UpSetDelayed|TagSetDelayed;
$scPatt       = With|Module|Block|DynamicModule;

$extrOpts = Sequence[
    "ExcludedContexts" -> { Except[ $LocalContext ] },
    "PostProcessing"   -> Composition[ SafeTranspose, DeleteDuplicates ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$LocalContext*)
$LocalContext = Context[ $LocalContext ] <> "LocalSymbols`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$LocalSymbolPrefix*)
$LocalSymbolPrefix := $LocalContext <> "S";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$NewLocalSymbolCounter*)
$NewLocalSymbolCounter = 0;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$ScopeTransformations*)
$ScopeTransformations = {
    CanonicalScopeTables,
    CanonicalScopingConstructs,
    CanonicalScopeFunctions,
    CanonicalScopeDelayed
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopeAll*)
CanonicalScopeAll[ expr_ ] := expr // RightComposition @@ $ScopeTransformations;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopeDelayed*)
CanonicalScopeDelayed[ expression_ ] :=
  Inline[ $delayedForms,

      Module[ { listPositions, funcFolder },

          listPositions =
            Position[ expression, $delayedForms[ ___ ] ] ~SortBy~ Length;

          funcFolder = ReplacePart[ #1,
              #2 -> Extract[ #1, #2, localizeDelayed ]
          ] //. TempHold[ a___ ] :> a &;

          Fold[ funcFolder, expression, listPositions ]
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*localizeDelayed*)
localizeDelayed // Attributes = { HoldFirst };

localizeDelayed[ (f : $delayedForms )[ p__, v_ ] ] :=

Inline[ $delayedForms,

  Module[
      {
          holdPattern, holdValue, heldPattVars, symsNew,
          rulesValueReplacements, rulesPatternReplacements,
          heldPatternReplaced, heldValueReplaced
      },

      holdPattern = TempHold @ p;
      holdValue   = TempHold @ v;

      heldPattVars = Cases[ holdPattern,
                            Verbatim[ Pattern ][ s_? SafePatternNameQ, ___ ] :>
                              HoldPattern @ s,
                            Infinity,
                            Heads -> True
                     ];

      heldPattVars = Cases[ DeleteDuplicates @ heldPattVars,
                            Verbatim[ HoldPattern ][ s_ ] /;
                              Context @ Unevaluated @ s =!= $LocalContext
                     ];

      symsNew = NewLocalSymbol @@@ heldPattVars;

      rulesValueReplacements = RuleDelayed @@@ SafeTranspose[ { heldPattVars,
                                                                symsNew } ];

      rulesPatternReplacements = Function[ { hp, ls },
                                           Verbatim[ Pattern ][ hp, a___ ] :>
                                             Pattern[ ls, a ],
                                           { HoldAll }
                                 ] @@@ rulesValueReplacements;

      holdPattern =
          Replace[
            ReplaceAll[
              holdPattern,
              HoldPattern[ Verbatim[ Condition ][ patt_, test_ ] ] /;
                  ! FreeQ[ HoldComplete @ test,
                           Alternatives @@ heldPattVars
                  ] :>
                  TrEval[
                    With[
                      {
                        testReplaced =
                            TempHold @ test /.
                                rulesValueReplacements
                      },
                      TempHold @ condition[ patt, testReplaced ]
                    ]
                  ]
            ],
            TempHold[ a___ ] :> a,
            { 1, Infinity }
          ] /. condition -> Condition;

      heldPatternReplaced = holdPattern /. rulesPatternReplacements;
      heldValueReplaced   = holdValue   /. rulesValueReplacements;

      Inline[ { heldPatternReplaced, heldValueReplaced },
          TempHold[ heldPatternReplaced ~f~ heldValueReplaced ]
      ]
  ]
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopeFunctions*)
CanonicalScopeFunctions[ exp_ ] :=

  Module[ { pos, folder },

      pos = Reverse[ Position[ exp, _Function ] ~SortBy~ Length ];

      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, localizeFunction ] ] //.
                 TempHold[ a___ ] :> a &;

      Fold[ folder, exp, pos ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*localizeFunction*)
localizeFunction // Attributes = { HoldFirst };

localizeFunction[ HoldPattern @ Function[ { var__? SymbolQ }, expr__ ] ] :=

  Module[ { newLocals, replRules, newExp },

      newLocals = HoldComplete @@ {
          ReleaseHold[ NewLocalSymbol /@ HoldComplete @ var ]
      };

      replRules =
        With[ { new = newLocals },
            {
                Thread[ RuleDelayed[ HoldPattern /@ HoldComplete @ var, new ],
                        HoldComplete
                ] // ReleaseHold
            }
        ];

      newExp = TempHold @ Function[ { var }, expr ] //. replRules;

      newExp
  ];


localizeFunction[ HoldPattern @ Function[ var_? SymbolQ, expr__ ] ] :=
  localizeFunction[ Function[ { var }, expr ] ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopeRuleDelayed*)
CanonicalScopeRuleDelayed[ expression_ ] :=

  Module[ { listPositions, funcFolder },

      listPositions = Position[ expression, _RuleDelayed ] ~SortBy~ Length;

      funcFolder = ReplacePart[ #1,
          #2 -> Extract[ #1, #2, localizeDelayed ]
      ] //. TempHold[ a___ ] :> a &;

      Fold[ funcFolder, expression, listPositions ]
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopeSetDelayed*)
CanonicalScopeSetDelayed[ expression_ ] :=

  Module[ { listPositions, funcFolder },

      listPositions = Position[ expression, _SetDelayed ] ~SortBy~ Length;

      funcFolder = ReplacePart[ #1,
                                #2 -> Extract[ #1, #2, localizeDelayed ]
                   ] //. TempHold[ a___ ] :> a &;

      Fold[ funcFolder, expression, listPositions ]
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopeTables*)
CanonicalScopeTables::unroll =
"Warning: Tables should be unrolled before transforming scope.";

tableScopeFolder =
  With[ { p = First @ #2, n = Last @ #2 },
      With[ { f = Function[ t, localizeTable[ t, n ], HoldFirst ] },

          ReplacePart[ #1, p -> Extract[ #1, p, f ] ]
      ]
  ] &;

CanonicalScopeTables[ exp_ ] :=
    Inline[ tableScopeFolder,
      Module[ { pos, idx, localized, reapplication },
        pos = Reverse @ SortBy[ Position[ exp, _Table ], Length ];
        idx = Transpose @ { pos, Range @ Length @ pos };
        localized = StripTempHolds @ Fold[ tableScopeFolder, exp, idx ];
        reapplication = localized //. HoldApply[ f_, { a___ } ] :> f @ a;
        (*NormalizeNames[ reapplication, "ExcludedContexts" -> { "System`" } ]*)
        reapplication
      ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*localizeTable*)
localizeTable // Attributes = { HoldFirst };

localizeTable[ t : HoldPattern @ Table[ _, { i_? LocalContextQ , ___ } ] ] :=
  TempHold @ t;

localizeTable[ table : HoldPattern @ Table[ exp_, { i_, ii___ } ] ] :=
  Module[ { newLocal, replRule, newExp, newIter },
      newLocal = NewLocalSymbol @ i;
      replRule = Inline[ newLocal, HoldPattern @ i :> newLocal ];
      newExp   = TempHold @ exp //. replRule;
      newIter  = Inline[ newLocal, TempHold[ { newLocal, ii } ] ];
      Inline[ { newExp, newIter }, HoldApply @ Table[ newExp, newIter ] ]
  ];

localizeTable[ table : HoldPattern @ Table[ exp_, { i_, ii___ } ], n_ ] :=
  Module[ { newLocal, replRule, newExp, newIter },
      newLocal = NewLocalSymbol @ n;
      replRule = Inline[ newLocal, HoldPattern @ i :> newLocal ];
      newExp   = TempHold @ exp //. replRule;
      newIter  = Inline[ newLocal, TempHold[ { newLocal, ii } ] ];
      Inline[ { newExp, newIter }, HoldApply @ Table[ newExp, newIter ] ]
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalScopingConstructs*)
CanonicalScopingConstructs[ exp_ ] :=

  Module[ { pos, folder },

      pos = Reverse[ Position[ exp, $scPatt[ { ___ }, ___ ] ] ~SortBy~ Length ];

      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, localizeSC ] ] //.
                 TempHold[ a___ ] :> a &;

      Fold[ folder, exp, pos ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*localizeSC*)
localizeSC // Attributes = { HoldAll };

localizeSC[ expression : $scPatt[ { ___ }, ___ ] ] :=
  Inline[ $scPatt,

      Module[
          {
              scExp, locals, newLocals, replRules, heldAssignments, repl,
              setReplaced, symReplaced, heldExpression, heldSCOut
          },

          scExp  = TempHold @ expression;
          locals = DeleteDuplicates @ getSCLocalVars[ scExp, Hold ];

          newLocals = ReplaceMasked[ locals,
                                     Hold[ x_ ] :> NewLocalSymbol @ x,
                                     { 1 }
                      ];

          replRules = RuleDelayed @@@ SafeTranspose @ { HoldPattern @@@ locals,
                                                        newLocals };


          heldAssignments = Extract[ scExp, { 1, 1 }, TempHold ];

          repl = HoldPattern[ var_ = val_ ] :>
                   With[ { v = TempHold @ var /. replRules },
                       TempHold[ v = val ]
                   ];


          setReplaced = ReplaceMasked[ heldAssignments, repl, { 2 },
                                       Evaluate -> True ];

          symReplaced = ReplaceMasked[ setReplaced, replRules, { 2 },
                                       Evaluate -> True ];

          heldExpression = Extract[ scExp, { 1, 2 }, TempHold ] /. replRules;

          heldSCOut = HoldComplete @@ ReplacePart[ scExp,
                                                   {
                                                     { 1, 1 } -> symReplaced,
                                                     { 1, 2 } -> heldExpression
                                                   }
                                      ];

          TempHold @@ (heldSCOut //. TempHold[ a___ ] :> a)
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getSCLocalVars*)
getSCLocalVars // Attributes = { HoldFirst };

getSCLocalVars[
    $scPatt[ { localAssignments___ }, localExpression_, ___ ],
    Optional[ wrapper_, HoldPattern ]
] := Inline[
    $scPatt,
    Cases[
        Hold @ localAssignments,
        HoldPattern[ x_ = _ ] | (x_) :> wrapper @ x
    ]
];

getSCLocalVars[ TempHold[ a___ ], Optional[ wrapper_, HoldPattern ] ] :=
    getSCLocalVars[ a, wrapper ];

getSCLocalVars[ a_Symbol, Optional[ wrapper_, HoldPattern ] ] :=
    With[ { a$ = a }, getSCLocalVars[ a$, wrapper ] ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformFromScope*)
CanonicalTransformFromScope[ expression_ ] :=

  Inline[ $extrOpts,

      Module[ { scoped, wrap, names, symbols, regSymNames, regSyms, replRules },

          scoped = CanonicalScopeAll @ expression;

          wrap = Function[ sym,
                           { SymbolName @ Unevaluated @ sym, HoldPattern @ sym },
                           { HoldFirst }
                 ];

          { names, symbols } = ExtractSymbols[ scoped, wrap, $extrOpts ] /.
                                 { } -> { { }, { } };

          regSymNames = MapIndexed[ renameSymbol, names ];

          Unprotect @@ regSymNames;
          ClearAll  @@ regSymNames;
          Protect   @@ regSymNames;

          regSyms = Symbol /@ regSymNames;

          replRules = RuleDelayed @@@ SafeTranspose @ { symbols, regSyms };

          scoped /. replRules
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*renameSymbol*)
renameSymbol[ symName_String, { i_Integer } | i_Integer ] :=
  StringJoin @ { $LocalSymbolPrefix,
                 ToString @ i,
                 symName ~StringCases~ "$" };

renameSymbol[ { i_Integer } | i_Integer ] :=
  StringJoin @ { $LocalSymbolPrefix, ToString @ i };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*HideLocalSymbols*)
HideLocalSymbols[ TempHold[ expression___ ],
                  placeholder_: Nothing
] :=
  TempHold @@ HideLocalSymbols[ HoldComplete @ expression,
                                placeholder
              ];


HideLocalSymbols[ expression_,
                  placeholder_: Nothing
] :=
  Module[ { localized },

      localized = CanonicalScopeAll @ expression;

      If[ placeholder === Nothing,
          DeleteCases[ localized, _? LocalContextQ, Infinity, Heads -> True ],
          localized /. s_? LocalContextQ :> placeholder
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*LocalContextQ*)
LocalContextQ // Attributes = { HoldFirst };
LocalContextQ[ s_? SymbolQ ] := SafeContext @ s === $LocalContext;
LocalContextQ[ s_String    ] := ToExpression[ s, StandardForm, LocalContextQ ];
LocalContextQ[ ___         ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NewLocalSymbol*)
NewLocalSymbol // Attributes = { HoldFirst };

(* From an existing symbol *)
NewLocalSymbol[ x_? SymbolQ ] :=

  Module[ { symName, newSym, newSymName },

      symName    = SymbolName @ Unevaluated @ x;
      newSym     = Unique[ $LocalContext <> symName <> "$" ];
      newSymName = $LocalContext <> SymbolName @ newSym;
      SetAttributes[ Evaluate @ newSymName, Temporary ];

      newSym
  ];


(* From a symbol name *)
NewLocalSymbol[ x_String ] := ToExpression[ x, StandardForm, NewLocalSymbol ];


(* From an integer *)
NewLocalSymbol[ i_Integer ] :=
    Module[ { newSymName },
        $NewLocalSymbolCounter = i;
        newSymName = $LocalSymbolPrefix <> ToString @ i;
        Unprotect @@ { newSymName };
        ClearAll @@ { newSymName };
        With[ { newSymbol = Symbol @ newSymName },
            SetAttributes[ newSymbol, Temporary ];
            newSymbol
        ]
    ];


(* From nothing *)
NewLocalSymbol[ ] :=
  NewLocalSymbol @@ { ++$NewLocalSymbolCounter };


(* From something else, given that it evaluates to a new form *)
NewLocalSymbol[ x_ ] :=
  With[ { x$ = x },
      NewLocalSymbol @ x$ /;
        Hold @ x =!= Hold @ x$
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*formatLocalSymbols*)
formatLocalSymbols[ ] :=
    Map[ formatLocalSymbol,
         Select[
             Names[ $LocalContext <> "*" ],
             ToExpression[ #, InputForm, FormatValues ] === { } &
         ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*formatLocalSymbol*)
formatLocalSymbol[ name_String ] :=
    formatLocalSymbol[ name, Last @ StringSplit[ name, "`" ] ];

formatLocalSymbol[ name_String, base_String ] :=
    Module[ { i },
        If[ StringMatchQ[ base, "S" ~~ DigitCharacter.. ],
            i = ToExpression @ StringDrop[ base, 1 ];
            formatLocalSymbol[
                name,
                base,
                RawBoxes @ SubscriptBox[ "\[ScriptS]", ToBoxes @ i ],
                localSymbolColor @ i
            ],
            formatLocalSymbol[ name, base, base, localSymbolColor @ base ]
        ]
    ];

formatLocalSymbol[ name_, base_, label_, color_ ] :=
    With[ { box = MakeBoxes @ label },
        ToExpression[
            name,
            InputForm,
            Function[
                newSymbol,
                newSymbol /: MakeBoxes[ newSymbol, StandardForm ] :=
                    InterpretationBox[
                        StyleBox[
                            box,
                            FontColor -> color,
                            FontWeight -> "DemiBold"
                        ],
                        newSymbol,
                        Selectable -> False,
                        SelectWithContents -> True
                    ],
                HoldAllComplete
            ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*localSymbolColor*)
localSymbolColor[ name_String ] :=
    localSymbolColor[ name, Last @ StringSplit[ name, "`" ] ];

localSymbolColor[ name_, base_String ] := localSymbolColor @
    If[ StringMatchQ[ base, "S" ~~ DigitCharacter.. ],
        ToExpression @ StringDrop[ base, 1 ],
        Mod[ Hash @ name, 64 ]
    ];

localSymbolColor[ n_Integer ] := ColorData[ 97 ][ Mod[ n, 64 ] ];
localSymbolColor[ ___       ] := localSymbolColor[ 1 ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NormalizeNames*)
NormalizeNames // Options = {
    "ExcludedContexts"  -> Automatic,
    "PreserveNumbering" -> False,
    "RemoveTypes"       -> False,
    "ForceGlobal"       -> False
};

NormalizeNames[ expr_, opts: OptionsPattern[ ] ] /; ! $NormalizeNames := expr;

NormalizeNames[ exp_, opts: OptionsPattern[ ] ] :=
  Module[
      {
          ectx, freeSymbols, offset, index, newSymbols,
          positions, replacements, replaced
      },

      ClearAll @@ { $LocalContext <> "*" };

      $NewLocalSymbolCounter = 0;

      ectx = Replace[ OptionValue @ "ExcludedContexts",
                      Automatic -> $excludedContexts
             ];

      freeSymbols = ExtractSymbols[ exp,
                                    HoldPattern,
                                    Infinity,
                                    "ExcludedContexts" -> ectx ];

      offset =
        If[ OptionValue @ "PreserveNumbering"
            ,
            1 + Max @ Cases[ exp,
                             s_Symbol?LocalContextQ :> ToExpression @
                             StringDrop[ SymbolName @ Unevaluated @ s, 1 ],
                             Infinity ]
            ,
            1
        ];

      index = Range[ offset, offset + Length @ freeSymbols - 1 ];
      newSymbols = NewLocalSymbol /@ index;
      positions = Position[ exp, # ] & /@ freeSymbols;
      replacements = Thread[ positions -> newSymbols ];
      replaced = ReplacePart[ exp, replacements ];

      replaced = If[ OptionValue @ "RemoveTypes",
          removeTypes @ replaced,
          replaced
      ];

      If[ OptionValue @ "ForceGlobal"
          ,
          replaced /. s_Symbol?LocalContextQ :>
            TrEval @ Symbol[ "Global`" <> SymbolName @ Unevaluated @ s ]
          ,
          replaced
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$excludedContexts*)
$excludedContexts :=
    With[ { contexts = Language`$InternalContexts },
        Union[
            If[ ListQ @ contexts, contexts, $legacyExcludedContexts ],
            DeleteCases[
                Contexts[ "Wolfram`CodeEquivalenceUtilities`*" ],
                $LocalContext
            ]
        ]
    ];

$legacyExcludedContexts :=
    Cases[
        Lookup[
            Options @ Language`ExtendedFullDefinition,
            "ExcludedContexts",
            { }
        ],
        ctx_String? StringQ :>
            StringReplace[
                ctx,
                c: Except[ "`" ] ~~ EndOfString :> c <> "`"
            ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*removeTypes*)
removeTypes[ exp_ ] := exp /. TypedSymbol[ s_Symbol, _ ] :> s;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)
