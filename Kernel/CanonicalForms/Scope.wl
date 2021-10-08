Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;



BeginPackage[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`",
        "Wolfram`CodeEquivalenceUtilities`Types`"
    }
];


Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)
$LocalContext                   ::usage = "";
$LocalSymbolPrefix              ::usage = "";
$ScopeTransformations           ::usage = "";
LocalContextQ                   ::usage = "";
$NewLocalSymbolCounter;
NewLocalSymbol                  ::usage = "";
CanonicalScopingConstructs      ::usage = "";
CanonicalScopeTables            ::usage = "";
CanonicalScopeFunctions         ::usage = "";
CanonicalScopeDelayed           ::usage = "";
CanonicalScopeRuleDelayed       ::usage = "";
CanonicalScopeSetDelayed        ::usage = "";
CanonicalScopeRuleDelayed       ::usage = "";
CanonicalScopeAll               ::usage = "";
CanonicalTransformFromScope     ::usage = "";
HideLocalSymbols                ::usage = "";
NormalizeNames                  ::usage = "";


Begin[ "`Private`" ];



(******************************************************************************)



$LocalContext := Context[ $LocalContext ] <> "LocalSymbols`";

$LocalSymbolPrefix := $LocalContext <> "S";

$ScopeTransformations =
  {
      CanonicalScopeTables,
      CanonicalScopingConstructs,
      CanonicalScopeFunctions,
      CanonicalScopeDelayed
  };



(******************************************************************************)



LocalContextQ // Attributes = { HoldFirst };
LocalContextQ // Options    = { };


(*LocalContextQ[ HoldPattern[ Symbol[ s_String ] ] ] :=
  SafeContext @ s === $LocalContext;*)


LocalContextQ[ s_? SymbolQ ] :=
    SafeContext @ s === $LocalContext;


LocalContextQ[ s_String ] :=
    ToExpression[ s, StandardForm, LocalContextQ ];


LocalContextQ[ ___ ] := False;



(******************************************************************************)


$NewLocalSymbolCounter // ClearAll;
$NewLocalSymbolCounter = 0;


NewLocalSymbol // Attributes = { HoldFirst };
NewLocalSymbol // Options    = { };


(* From an existing symbol *)
NewLocalSymbol[ x_? SymbolQ ] :=

  Module[ { symName, newSym, newSymName },

      symName    = SymbolName @ Unevaluated @ x;
      newSym     = Unique[ $LocalContext <> symName <> "$" ];
      newSymName = $LocalContext <> SymbolName @ newSym;

      Inline[ newSymName, newSymName // Attributes = { Temporary } ];

      newSym
  ];


(* From a symbol name *)
NewLocalSymbol[ x_String ] := ToExpression[ x, StandardForm, NewLocalSymbol ];


(* From an integer *)
NewLocalSymbol[ i_Integer ] :=
  Module[ { newSymName, newSymbol },
      $NewLocalSymbolCounter = i;
      newSymName = $LocalSymbolPrefix <> ToString @ i;
      Unprotect @@ { newSymName };
      ClearAll @@ { newSymName };
      newSymbol  = Symbol @ newSymName;
      Inline[ newSymName, newSymName // Attributes = { Temporary } ];
      newSymbol
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



(******************************************************************************)



(*
    In this section, SC = "Scoping Constructs".
    These are lexical renaming utilities for With, Module, and Block.
*)

$scPatt = With | Module | Block | DynamicModule;



getSCLocalVars // ClearAll;
getSCLocalVars // Attributes = { HoldFirst };
getSCLocalVars // Options    = { };


getSCLocalVars[ $scPatt[ { localAssignments___ }, localExpression_, ___ ],
    wrapper_ : HoldPattern
] :=
  Inline[ $scPatt,
      Cases[ Hold @ localAssignments,
             HoldPattern[ x_ = _ ] | x_ :> wrapper @ x
      ]
  ];


getSCLocalVars[ TempHold[ a___ ],
    wrapper_: HoldPattern
] :=
  getSCLocalVars[ a, wrapper ];


getSCLocalVars[ a_Symbol,
    wrapper_: HoldPattern
] :=
  With[ { a$ = a },
      getSCLocalVars[ a$, wrapper ]
  ];



localizeSC // ClearAll;
localizeSC // Attributes = { HoldAll };
localizeSC // Options    = { };


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



CanonicalScopingConstructs // Attributes = { };
CanonicalScopingConstructs // Options    = { };


CanonicalScopingConstructs[ exp_ ] :=

  Module[ { pos, folder },

      pos = Reverse[ Position[ exp, $scPatt[ { ___ }, ___ ] ] ~SortBy~ Length ];

      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, localizeSC ] ] //.
                 TempHold[ a___ ] :> a &;

      Fold[ folder, exp, pos ]
  ];



(******************************************************************************)



localizeFunction // ClearAll;
localizeFunction // Attributes = { HoldFirst };
localizeFunction // Options    = { };


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




CanonicalScopeFunctions // Attributes = { };
CanonicalScopeFunctions // Options    = { };


CanonicalScopeFunctions[ exp_ ] :=

  Module[ { pos, folder },

      pos = Reverse[ Position[ exp, _Function ] ~SortBy~ Length ];

      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, localizeFunction ] ] //.
                 TempHold[ a___ ] :> a &;

      Fold[ folder, exp, pos ]
  ];



(******************************************************************************)



localizeTable // ClearAll;
localizeTable // Attributes = { HoldFirst };
localizeTable // Options    = { };


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



CanonicalScopeTables // Attributes = { };
CanonicalScopeTables // Options    = { };

CanonicalScopeTables::unroll =
  "Warning: Tables should be unrolled before transforming scope.";


(*CanonicalScopeTables[ exp_ ] :=

  Module[ { *)(*unrolled, *)(*pos, folder },

      *)(* TODO: move table unrolling to Structural` and reimplement *)(*
      *)(*unrolled = unrollAllTables @ exp;*)(*

      If[ ! FreeQ[ exp, HoldPattern @ Table[ _, _, __ ] ],
          Message[ CanonicalScopeTables::unroll ];
      ];

      pos = Position[ exp*)(*unrolled*)(*,
                      HoldPattern @ Table[ _, { _, ___ } ]
            ] ~SortBy~ Length;

      folder = ReplacePart[ #1, #2 -> Extract[ #1, #2, localizeTable ] ] //.
                 TempHold[ a___ ] :> a &;

      Fold[ folder, exp*)(*unrolled*)(*, pos ]
  ];*)


tableScopeFolder // ClearAll;

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



(******************************************************************************)



localizeDelayed // ClearAll;
localizeDelayed // Attributes = { HoldFirst };
localizeDelayed // Options    = { };

$delayedForms = Condition | RuleDelayed | SetDelayed | UpSetDelayed | TagSetDelayed;


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



CanonicalScopeDelayed // Attributes = { };
CanonicalScopeDelayed // Options    = { };


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



CanonicalScopeSetDelayed // Attributes = { };
CanonicalScopeSetDelayed // Options    = { };


CanonicalScopeSetDelayed[ expression_ ] :=

  Module[ { listPositions, funcFolder },

      listPositions = Position[ expression, _SetDelayed ] ~SortBy~ Length;

      funcFolder = ReplacePart[ #1,
                                #2 -> Extract[ #1, #2, localizeDelayed ]
                   ] //. TempHold[ a___ ] :> a &;

      Fold[ funcFolder, expression, listPositions ]
  ];



CanonicalScopeRuleDelayed // Attributes = { };
CanonicalScopeRuleDelayed // Options    = { };


CanonicalScopeRuleDelayed[ expression_ ] :=

  Module[ { listPositions, funcFolder },

      listPositions = Position[ expression, _RuleDelayed ] ~SortBy~ Length;

      funcFolder = ReplacePart[ #1,
          #2 -> Extract[ #1, #2, localizeDelayed ]
      ] //. TempHold[ a___ ] :> a &;

      Fold[ funcFolder, expression, listPositions ]
  ];



(******************************************************************************)



CanonicalScopeAll // Attributes = { };
CanonicalScopeAll // Options    = { };


CanonicalScopeAll[ expression_ ] :=
  expression // RightComposition @@ $ScopeTransformations;



(******************************************************************************)



renameSymbol // ClearAll;
renameSymbol // Attributes = { };
renameSymbol // Options    = { };


renameSymbol[ symName_String, { i_Integer } | i_Integer ] :=
  StringJoin @ { $LocalSymbolPrefix,
                 ToString @ i,
                 symName ~StringCases~ "$" };


renameSymbol[ { i_Integer } | i_Integer ] :=
  StringJoin @ { $LocalSymbolPrefix, ToString @ i };



$extrOpts =
  Sequence[
      "ExcludedContexts" -> { Except @ $LocalContext },
      "PostProcessing"   -> (SafeTranspose @* DeleteDuplicates)
  ];



CanonicalTransformFromScope // Attributes = { };
CanonicalTransformFromScope // Options    = { };


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



(******************************************************************************)

$excludedContexts :=
  With[ { contexts = Language`$InternalContexts },
      Union[ If[ ListQ @ contexts,
                 contexts,
                 $legacyExcludedContexts
             ],
             DeleteCases[ Contexts[ "Wolfram`CodeEquivalenceUtilities`*" ], $LocalContext ]
      ]
  ];

$legacyExcludedContexts :=
  Cases[ Lookup[ Options @ Language`ExtendedFullDefinition,
                 "ExcludedContexts",
                 { }
         ],
         ctx_String? StringQ :>
           StringReplace[
               ctx,
               c: Except["`"]~~EndOfString :> c <> "`"
           ]
  ];


NormalizeNames // Attributes = { };
NormalizeNames // Options    = {
    "ExcludedContexts" -> Automatic,
    "PreserveNumbering" -> False,
    "RemoveTypes" -> False,
    "ForceGlobal" -> False
};



removeTypes[ exp_ ] := exp /. TypedSymbol[ s_Symbol, _ ] :> s;


NormalizeNames[ exp_, opts : OptionsPattern[ ] ] :=

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


(******************************************************************************)



HideLocalSymbols // Attributes = { };
HideLocalSymbols // Options    = { };


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


(******************************************************************************)



End[];

EndPackage[];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
