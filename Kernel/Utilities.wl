Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`Utilities`" ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

StringArg;
StringsQ;
DefinitionRules;
ExtractSymbols                  ::usage = "";
HoldApply                       ::usage = "";
Inline                          ::usage = "";
PartialEvaluation               ::usage = "";
ReplaceMasked                   ::usage = "";
ReplaceMaskedRepeated           ::usage = "";
SafeTranspose                   ::usage = "";
SeqHead                         ::usage = "";
TempHold                        ::usage = "";
TrEval                          ::usage = "";
UAtomQ                          ::usage = "";
UFlatQ                          ::usage = "";
UStringQ;
UIntegerQ;
UNumericQ                       ::usage = "";
UOrderedQ                       ::usage = "";
HFlatten                        ::usage = "";
HeldHead;
DefineError                     ::usage = "";
StripHead                       ::usage = "";
StripTempHolds                  ::usage = "";
$EvaluateTempHoldReplacements   ::usage = "";
SymbolHead                      ::usage = "";
CopySymbol                      ::usage = "";
SymbolQ                         ::usage = "";
UnlockedSymbolQ;
SafeContext                     ::usage = "";
ToFullString                    ::usage = "";
FullSymbolName                  ::usage = "";
UpValueSymbols                  ::usage = "";
SafePatternNameQ                ::usage = "";
FoldModule                      ::usage = "";
CompressedExpression;
CompressExpression;
WithHolding;
UnpackDispatches;
LookupSequence;
ReplaceStrict;
AbortOnMessage;
$FailOnMessageStack;
FailOnMessage;
ShowStack;
StackTraceOnMessage;
RenameSymbol;
HomePath;
RelativePath;
ToWDX;
FromWDX;
AbsoluteTimeString;
FromAbsoluteTimeString;
MakeSelfExtractingExpression;
MakeSelfExtractingAPIFunction;
MakeSelfExtractingFunction;
CloudEvaluateBinary;
DefinedQ;
LogMessages;
URLFetchNoCookies;
WithNoCookies;
EmptyContext;
DependentNames;
DependentSymbols;
DependencyGraph;
PreserveDefinitions;
MinimalFullDefinition;
FullDefinitionData;
ToDefinitionList;
CompressWithDefinitions;
TrackNewSymbols;
$CurrentCloudVersionNumber;
CreateCloudPackage;
CreateCloudPackageString;
DeployCloudPackage;
ToSandboxedExpression;
SandboxedDefinition;
CleanSandbox;
ContextDepth;
Subcontexts;
ReplaceContext;
ClearContext;
WithContext;
CompressWithContext;
DefinitionLoader;
SymbolLoader;
EvaluateInContext;
SearchNames;
SymbolInformation;
FormatCode;
FormatCodeString;
MakeInputCell;
GetKeyValuePattern;
FindInvalidKeyValuePairs;
AssertPattern;
CheckPattern;
VerifyPattern;
$CatchFunction;
CatchMine;
SetContext;
ShowProgress;
AddToPath;
WithSystemOptions;
StrictLexicalScoping;
SymbolHash;
EvaluatedTo;
EvaluationReportData;
EvaluationReport;
OnceUnlessFailed;
HashString;
HashStringQ;
HashPath;
CloudHashPut;
SafeSerialize;
SafeDeserialize;
InertTraceQ;
CloudObjectQ;
CloudObjectExistsQ;
CloudFileType;
CloudFiles;
ViewUsagePatterns;
CompressedStringQ;
UnsetAllOnce;
UUIDStringQ;
CompressCloudObject;
CreateTaskFile;
EvaluateInFreshKernel;
SequenceDiff;



Begin[ "`Private`" ];



(******************************************************************************)



getUnboundSymbols // ClearAll;
getUnboundSymbols // Attributes = { HoldAllComplete };

getUnboundSymbols[ args___ ] :=
  Module[ { $fail },
      Quiet[ DeleteCases[ Internal`GetUnboundSymbols @ args,
                          s_ /; Check[ Context @ Unevaluated @ s,
                                       $fail,
                                       Context::notfound
                                ] === $fail
             ],
             Context::notfound
      ]
  ];



(******************************************************************************)


renameURL // Attributes = { };
renameURL // Options    = { };


renameURL[ s_String ] :=
  If[ StringQ @ $WolframUUID && StringQ @ $UserURLBase,
      StringReplace[ s, "user-" <> $WolframUUID :> $UserURLBase ],
      s
  ];


renameURL[ CloudObject[ s_String ] ] :=
  CloudObject @ renameURL @ s;



$cloudRootDirectory // ClearAll;
$cloudRootDirectory := renameURL @ $CloudRootDirectory;

$cloudRootPattern // ClearAll;
$cloudRootPattern :=
  If[ StringQ @ $WolframUUID && StringQ @ $UserURLBase,
      $CloudBase ~~ ("/" | "") ~~ "objects" ~~ ("/" | "") ~~ (("user-" ~~ $WolframUUID) | $UserURLBase) ~~ ("/" | ""),
      ""
  ];



(******************************************************************************)



stringsQ // ClearAll;
stringsQ // Attributes = { HoldAllComplete };
stringsQ // Options    = { };


stringsQ[ s_String ]           := StringQ @ Unevaluated @ s;
stringsQ[ { s___? stringsQ } ] := True;
stringsQ[ ___ ]                := False;



StringsQ // Attributes = { };
StringsQ // Options    = { };

StringsQ[ s_ ] := stringsQ @ s;



(******************************************************************************)



TempHold // Attributes = { HoldAllComplete };
TempHold // Options    = { };


TempHold[ th_TempHold ] := th;


TempHold /:
  Part[ th_TempHold, spec_ ] /; Length @ th > 1 :=
    TempHold @@ (
        HoldComplete @@ Part[ List @@ TempHold /@ th, spec ] //.
          TempHold[ a___ ] :> a
    );


TempHold /:
  Part[ th : TempHold[ _ ], { 1 } | 1 ] := th;


TempHold /:
  First[ th_TempHold ] := th[[ 1 ]];


TempHold /:
  Last[ th_TempHold ] := th[[ -1 ]];


TempHold /:
  Most[ th_TempHold ] := th[[ 1 ;; -2 ]];


TempHold /:
  Rest[ th_TempHold ] := th[[ 2 ;; -1 ]];


TempHold /: TempHold[ exp___ ] /. p_ :> e_ :=
  TempHold @@ ( HoldComplete @ exp /. p :> TrEval @ e ) /;
    TrueQ @ $EvaluateTempHoldReplacements;


TempHold /: TempHold[ exp___ ] //. p_ :> e_ :=
  TempHold @@ ( HoldComplete @ exp //. p :> TrEval @ e ) /;
    TrueQ @ $EvaluateTempHoldReplacements;



(******************************************************************************)

StripHead // Attributes = { };
StripHead // Options    = { };

StripHead[ expression_, head_ ] :=
  expression //. {
    (f_)[ a1___, head[ a2___ ], a3___ ] :> f[ a1, a2, a3 ],
      head[ a___ ] :> a
  };

(******************************************************************************)

StripTempHolds[ expression_ ] :=
  StripHead[ expression, TempHold ];

(******************************************************************************)

HoldApply // Attributes = { HoldAllComplete };
HoldApply // Options    = { };


HoldApply[ (f_)[ args___ ] ] := HoldApply[ f, { args } ];


HoldApply /: Normal @ HoldApply[ f_, { args___ } ] :=
  HoldComplete @ f @ args;



(******************************************************************************)



(* Trott-Strzebonski in-place evaluation
   http://library.wolfram.com/infocenter/Conferences/377/ *)
TrEval // Attributes = { };
TrEval // Options    = { };


TrEval /: HoldPattern[ swap_ :> TrEval @ eval_ ] :=
  swap :> With[ { eval$ = eval }, eval$ /; True ];



(******************************************************************************)



U // Attributes = { };
U // Options    = { };


U[ f_ ] := Function[ Null, f @ Unevaluated @ ##, { HoldAllComplete } ];



(******************************************************************************)


UStringQ  = U @ StringQ;
UIntegerQ = U @ IntegerQ;
UAtomQ    = U @ AtomQ;
UNumericQ = U @ NumericQ;
UOrderedQ = U @ OrderedQ;


(******************************************************************************)



ReplaceMasked // Attributes = { };
ReplaceMasked // Options    =
  {
      Heads     -> True,
      Evaluate  -> False,
      Masking   -> { }
  };


ReplaceMasked[ repl : (_Rule | _RuleDelayed | _List),
                levelSpec_,
                opts : OptionsPattern[ ]
             ][ expression_ ] :=
               ReplaceMasked[ expression, repl, levelSpec, opts ];


ReplaceMasked[ expression_,
                (Rule | RuleDelayed)[ match_, replace_ ],
                levelSpec_,
                opts : OptionsPattern[ ]
] :=
  Module[
      {
          maskHold, maskedExp, masking, maskPos, maskedExpressions, masked, pos,
          heldSubExpressions, indexedSubExpressions, rule, indexedReplacement,
          readyRules, delIdx, replacedHeld, replRules, replaced
      },

      maskHold // Attributes = { HoldAllComplete };

      masking = Alternatives @@ OptionValue @ Masking;

      maskPos = Position[ expression, masking ];

      maskedExpressions = Extract[ expression, maskPos, maskHold ];

      masked = ReplacePart[ expression, maskPos -> maskedExp ];

      pos = Position[ masked,
                      match,
                      levelSpec,
                      Heads -> OptionValue @ Heads
            ];

      heldSubExpressions = Extract[ masked, pos, Hold ];
      indexedSubExpressions = Thread[ { pos, heldSubExpressions } ];

      rule = If[ OptionValue @ Evaluate,
                 Rule,
                 RuleDelayed
             ];

      indexedReplacement = With[ { insRule = rule },
                                 Cases[ indexedSubExpressions,
                                        { p_, held : Hold @ match } :>
                                          p ~insRule~ Hold @ replace
                                 ]
                           ];

      Remove[rule];

      delIdx = { #, 2, 0 } & /@ Range @ Length @ indexedReplacement;
      readyRules = indexedReplacement ~Delete~ delIdx;

      replacedHeld = masked ~ReplacePart~ readyRules;
      replRules = Thread[ maskPos -> maskedExpressions ];

      replaced = replacedHeld ~ReplacePart~ replRules;

      DeleteCases[ replaced, maskHold, Infinity, Heads -> True ]
  ];


ReplaceMasked[ expression_,
                patterns_List,
                levelSpec_,
                opts : OptionsPattern[ ]
] :=
  Fold[ ReplaceMasked[ #1, #2, levelSpec, opts ] &,
        expression,
        patterns
  ];



(******************************************************************************)



ReplaceMaskedRepeated // Attributes = { };
ReplaceMaskedRepeated // Options    =
  {
      Heads     -> False,
      Evaluate  -> True,
      Masking   -> { }
  };


ReplaceMaskedRepeated[ expression_,
                        pattern_,
                        levelSpec_,
                        opts : OptionsPattern[ ]
] :=
  ReplaceMasked[ #, pattern, levelSpec, opts ] & ~FixedPoint~ expression;



(******************************************************************************)



DefinitionRules // ClearAll;
DefinitionRules // Attributes = {HoldAllComplete};

DefinitionRules[sym_?SymbolQ] :=
  Cases[Flatten@Replace[
      Language`ExtendedDefinition[sym],
      Language`DefinitionList[HoldForm[sym] -> defs_] :> Values@defs
  ], _Rule | _RuleDelayed];

DefinitionRules[sym_String?NameQ] :=
  ToExpression[sym, StandardForm, DefinitionRules];



(******************************************************************************)



SeqHead // ClearAll;
SeqHead // Attributes = { };
SeqHead // Options = { };


SeqHead[ _, __ ] := Sequence;


SeqHead[ expression_ ] := Head @ expression;


SeqHead[ arg___ ] := Null /; (
    Message[ SeqHead::argx, SeqHead, Length @ { arg } ];
    False
);



(******************************************************************************)



(*Inline // Attributes = { HoldAllComplete };
Inline // Options    =
  {
      Definitions     -> { OwnValues, DownValues, UpValues },
      MaxIterations   -> 1,
      "ForceSymbolic" -> False
  };


Inline[ function_,
        expression_,
        wrapper_ : Automatic,
        options : OptionsPattern[ ]
] :=
  Module[

      {
          held, definitionTypes, getDefinitions, replacementRules,
          makeSymbolic, symbolicRules, replaced, wrapperFunction
      },

      held = HoldComplete @ expression;
      definitionTypes = OptionValue @ Definitions;

      getDefinitions =
        If[ SeqHead @ function === Inactive
            ,
            With[ { f = Identity @@ function },
                With[ { def = # @ f }, Inactivate @ def ] &
            ]
            ,
            # @ function &
        ];

      replacementRules =
        Flatten @ Prepend[
            getDefinitions /@ definitionTypes,
            HoldPattern[ function /@ list_ ] :> (function @ # & /@ list)
        ];

      *)(*replacementRules = getDefinitions /@ definitionTypes // Flatten;*)(*
      makeSymbolic = # /. Verbatim[ Blank ][ ___ ] -> Blank[ ] &;

      symbolicRules = If[ OptionValue @ "ForceSymbolic",
          MapAt[ makeSymbolic, replacementRules, { All, 1 } ],
          replacementRules
      ];

      Off @ ReplaceRepeated::rrlim;

      replaced = ReplaceRepeated[ held, symbolicRules,
          MaxIterations -> OptionValue @ MaxIterations
      ];

      On @ ReplaceRepeated::rrlim;

      wrapperFunction = wrapper /. Automatic -> Identity;

      wrapperFunction @@ replaced
  ];



Inline[ f_, exp_, opts : OptionsPattern[ ] ] :=
  Inline[ f, exp, Identity, opts ];



iInline // ClearAll;
iInline // Attributes = { HoldAllComplete };
iInline // Options    = Options @ Inline;


iInline[ expression_, function_ ] :=
  With[ { currentOptions = Options @ iInline },
        Inline[ function, expression, HoldComplete, currentOptions ]
  ];



listToHold // ClearAll;
listToHold // Attributes = { HoldAllComplete };
listToHold // Options    = { };


listToHold[ { a$___ } ] := Hold @ a$;
listToHold[ list_List, All ] := Identity @@ (Hold @ list /. List -> Hold);



dropHead // ClearAll;
dropHead // Attributes = { };
dropHead // Options    = { };


dropHead = Identity @@ # &;



Inline[
    functionList : { ___ },
    expression_,
    wrapper_ : Automatic,
    options : OptionsPattern[ ]
] :=
  Module[

      {
          heldExpression, heldFunctions, functionCount,
          folded, unstacked, wrapperFunction
      },

      heldExpression = HoldComplete @ expression;
      heldFunctions = listToHold @ functionList;
      functionCount = Length @ heldFunctions;

      iInline ~SetOptions~ options;
      folded = Fold[ iInline, heldExpression, heldFunctions ];
      iInline // Options = Options @ Inline;

      unstacked = Nest[ dropHead, folded, functionCount ];
      wrapperFunction = wrapper /. Automatic -> Identity;

      wrapperFunction @@ unstacked
  ];*)


Inline // Attributes = { HoldAllComplete };
Inline // Options    = { MaxIterations -> Automatic };

Inline[ x_? SymbolQ, expr_, opts: OptionsPattern[ ] ] :=
  Inline[ { x }, expr, ## &, opts ];

Inline[ x_? SymbolQ,
        expr_,
        wrapper_,
        opts: OptionsPattern [ ]
] :=
  Inline[ { x }, expr, wrapper, opts ];

Inline[ { xs___? SymbolQ },
        expr_,
        opts: OptionsPattern[ ]
] :=
    Inline[ { xs }, expr, ## &, opts ];

Inline[ { xs___? SymbolQ },
        expr_,
        wrapper_,
        opts: OptionsPattern [ ]
] :=

  Module[ { defs, iter },

      defs = Flatten[ List @@ DefinitionRules /@ HoldComplete @ xs ];

      iter = Replace[ OptionValue @ MaxIterations,
                      Automatic -> 2 Length @ defs
             ];

      wrapper @@ ReplaceRepeated[ HoldComplete @ expr,
                                  defs,
                                  MaxIterations -> iter
                 ]
  ];


(*Inline[ f_ ][ a$___ ] := Inline[ f, a$ ];*)


Inline /:
  (patt_ := Inline[ f_, exp_, a___ ]) :=
    Inline[ f, HoldComplete[ patt := exp ], a ] // ReleaseHold;



(******************************************************************************)



PartialEvaluation // Attributes = { HoldAllComplete };
PartialEvaluation // Options    = { };
PartialEvaluation // SyntaxInformation =
  {
      "LocalVariables" -> { "Solve", { 1, 1 } }
  };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::VariableError::Module:: *)
PartialEvaluation[ { v___ }, HoldPattern @ CompoundExpression[ e__, r_ ] ] :=
  Module[ { v },
      CompoundExpression[ e ];
      Inline[ { v }, HoldApply @ r ] // StripTempHolds
  ];



(******************************************************************************)



UFlatQ // Attributes = { HoldFirst };
UFlatQ // Options    = { };

UFlatQ[ exp : h_[ ___ ] ] :=
  With[ { held = HoldComplete @ exp //. HoldPattern @ h :> TempHold },
      Flatten @ ReleaseHold @ held === ReleaseHold @ held
  ];



(******************************************************************************)



ExtractSymbols // Attributes = { };
ExtractSymbols // Options    =
  {
      "ExcludedContexts"    -> Automatic,
      "PostProcessing"      -> DeleteDuplicates,
      Heads                 -> True
  };


ExtractSymbols[ expression_, opts : OptionsPattern[ ] ] :=
  ExtractSymbols[ expression, Hold, Infinity, opts ];


ExtractSymbols[ expression_, wrapper_, opts : OptionsPattern[ ] ] :=
  ExtractSymbols[ expression, wrapper, Infinity, opts ];


ExtractSymbols[ expression_,
                wrapper_,
                levelSpec_,
                opts: OptionsPattern[ ]
] :=
  Module[ { excludedContexts, match, contextPattern, validQ, caseOpts },

      excludedContexts = OptionValue @ "ExcludedContexts" /.
                           { None -> { }, Automatic -> { "System`" } };

      match = If[ AllTrue[ StringQ /@ excludedContexts, Identity ],
                  StringMatchQ,
                  MatchQ
              ];

      contextPattern = Alternatives @@ excludedContexts;

      validQ = Inline[ { match, contextPattern },
                 Function[ symbol
                           ,
                           SymbolQ @ symbol &&
                             ! match[ SafeContext @ symbol, contextPattern ]
                           ,
                           { HoldAllComplete }
                 ]
               ];

      caseOpts = Sequence @@ FilterRules[
          Options @ ExtractSymbols ~Join~ { opts },
          Options @ Cases
      ];

      Cases[ expression, s_? validQ :> wrapper @ s, levelSpec, caseOpts ] //
        OptionValue @ "PostProcessing"
  ];



(******************************************************************************)



SafeTranspose // Attributes = { };
SafeTranspose // Options    = { };


SafeTranspose[ { } ] := { };


SafeTranspose[ list_ ] := Transpose @ list;



(******************************************************************************)



HFlatten // Attributes = { HoldFirst };
HFlatten // Options    = { };

HFlatten[ function_[ exp : head_[ args___ ] ], wrapper_: TempHold ] :=

  Module[ { heldInner, heldOuter },

      heldInner = ReplaceMasked[ HoldComplete @ exp,
                                 HoldPattern @ head -> TempHold,
                                 All,
                                 Masking -> { HoldPattern[ _function ] }
                  ];

      heldOuter = With[ { h = Flatten @@ heldInner },
                        HoldComplete @ function @ h
                  ] /. TempHold -> head;

      wrapper @@ heldOuter
  ];



(******************************************************************************)

HeldHead // Attributes = { HoldAllComplete };
HeldHead[ f_[ ___ ], w_ ] := w @ f;
HeldHead[ x_? UAtomQ, w_ ] := With[ { h = Head @ Unevaluated @ x }, w @ h ];
HeldHead[ x_ ] := HeldHead[ x, TempHold ];

(******************************************************************************)



DefineError // Attributes = { HoldAllComplete };
DefineError // Options    = { };


DefineError[ (Rule | RuleDelayed)[ pattern_, exec_ ] ] :=
  pattern /; ( exec; False ) := Null;


DefineError[ rules : (_Rule | _RuleDelayed) ... ] :=
  DefineError /@ Unevaluated @ { rules };



(******************************************************************************)



SymbolHead // Attributes = { HoldAllComplete };
SymbolHead // Options    = { };

SymbolHead[ s_ ] :=
  HoldComplete @ Head @ Unevaluated @ s /. OwnValues @ s // ReleaseHold;



(******************************************************************************)



CopySymbol // Attributes = { HoldAllComplete };
CopySymbol // Options    = { };


(* Main definition *)
CopySymbol[ f_? SymbolQ, g_? SymbolQ ] :=
  Module[ { fDef },
      ClearAll @ g;
      fDef = Language`ExtendedDefinition @ f;
      If[ FreeQ[ fDef, HoldPattern[ g ] ],
          Language`ExtendedDefinition[ g ] = fDef /. HoldPattern[ f ] :> g,
          With[ { s = Unique @ SymbolName @ Unevaluated @ g },
              Language`ExtendedDefinition[ g ] =
                fDef /. HoldPattern[ g ] :> s /. HoldPattern[ f ] :> g
          ]
      ]
  ];

(* Copy to the same name in a different context *)
CopySymbol[ f_? SymbolQ, ctx_String /; StringMatchQ[ ctx, __ ~~ "`" ] ] :=
  With[ { name = SymbolName @ Unevaluated @ f },
      ToExpression[ ctx <> name,
                    InputForm,
                    Function[ g, CopySymbol[ f, g ], { HoldAllComplete } ]
      ]
  ];


CopySymbol[ a___, f_String, b___ ] :=
  ToExpression[ f,
                InputForm,
                Function[ sym, CopySymbol[ a, sym, b ], { HoldAllComplete } ]
  ];


(******************************************************************************)



SymbolQ // Attributes = { HoldAllComplete };
SymbolQ // Options    = { };


SymbolQ[ s_Symbol ] := Depth @ HoldComplete @ s === 2;
SymbolQ[ ___ ] := False;



(******************************************************************************)



UnlockedSymbolQ // Attributes = { HoldAllComplete };
UnlockedSymbolQ // Options    = { };


UnlockedSymbolQ[ s_? SymbolQ ] := FreeQ[ Attributes @ Unevaluated @ s, Locked ];
UnlockedSymbolQ[ ___ ] := False;



(******************************************************************************)



safeContext // ClearAll;
safeContext // Attributes = { HoldAllComplete };
safeContext // Options    = { };


safeContext[ sym_? SymbolQ ] :=
    With[
      {
        s = StringTake[ ToString @ HoldComplete @ sym,
                        { 14, -2 }
            ]
      },

      Context @ s
    ];



allContexts // ClearAll;
allContexts // Attributes = { HoldAllComplete, SequenceHold };
allContexts // Options    = { };


allContexts[ expr_ ] :=
    DeleteDuplicates[
      Cases[ HoldComplete @ expr,
             HoldPattern[ s_ ? SymbolQ ] :> safeContext @ s,
             Infinity,
             Heads -> True
      ]
    ];



filterContextPath // ClearAll;
filterContextPath // Attributes = { HoldFirst, SequenceHold };
filterContextPath // Options    = { };


filterContextPath[ _     , None      ] := { };
filterContextPath[ _     , Automatic ] := $ContextPath;
filterContextPath[ expr_ , All       ] := allContexts @ expr;
filterContextPath[ expr_ , Full      ] := allContexts @ expr;
filterContextPath[ _     , c_        ] := c;



filterContext // ClearAll;
filterContext // Attributes = { HoldFirst, SequenceHold };
filterContext // Options    = { };


filterContext[ expr_, Automatic ] := $Context;

filterContext[ expr_, None ] :=
  With[ { c = allContexts @ expr },
        NestWhile[ "$" <> # &,
                   "None`",
                   ! FreeQ[ c, # ] &
        ]
  ];

filterContext[ _, c_ ] := c;



ToFullString // Attributes = { HoldAllComplete, SequenceHold };
ToFullString // Options    =
    {
      "ContextPath" :> $ContextPath,
      "Context"     :> $Context
    };


ToFullString[ expr_, OptionsPattern[ ] ] :=

    Block[
      {
        $ContextPath = filterContextPath[ expr, OptionValue @ "ContextPath" ],
        $Context     = filterContext[     expr, OptionValue @ "Context"     ]
      },

      StringTake[ ToString[ FullForm @ HoldComplete @ expr,
                            CharacterEncoding -> "ASCII" ]
                  ,
                  { If[ MemberQ[ $ContextPath, "System`" ], 14, 21 ], -2 }
      ]
    ];



(******************************************************************************)



$FailureContext = "$Failed`";



$start    = "$" | LetterCharacter;
$char     = Except[ "`", "$" | LetterCharacter | DigitCharacter ];
$context  = $start ~~ $char ... ~~ "`";
$sym      = $start ~~ $char ...;


PossibleNameQ[ s_String ] :=
    StringMatchQ[ s, ("`" | "") ~~ $context ... ~~ $sym ];



FullSymbolName // Attributes = { HoldAllComplete };
FullSymbolName // Options    = { };


FullSymbolName[ symbol_? SymbolQ ] :=

    Module[ { string },

      string = Block[ { $ContextPath = { }, $Context = "None`" },
                      StringTake[ ToString @ HoldComplete @ symbol,
                                  { 21, -2 }
                      ]
               ];

      If[ ! StringMatchQ[ string, Context @@ { string } ~~ __ ],
          "None`" <> string,
          string
      ]
  ];


FullSymbolName[ Verbatim[ HoldPattern ][ symbol_Symbol ] ] :=
    FullSymbolName @ symbol;


FullSymbolName[ string_String ? NameQ ] :=
    ToExpression[ string, StandardForm, FullSymbolName ];


FullSymbolName[ string_String ? PossibleNameQ ] :=
    string;


(*FullSymbolName[ string_String ? PossibleNameQ ] :=
    StringDrop[
      StringJoin @ Riffle[ Most @ StringSplit[ " " <> string, "`" ], "`" ],
      1
    ];*)


FullSymbolName[ string_String ? (Not @* NameQ) ] :=
    $FailureContext <> "$Failed" /; (
      Message[ FullSymbolName::notfound, string ];
      True
    );


FullSymbolName[ a : Except[ _String ? (Not @* NameQ) ] ... ] :=
    $FailureContext <> "$Failed" /; (
      Message[ FullSymbolName::ssle, HoldForm @ FullSymbolName @ a, 1 ];
      True
    );



(******************************************************************************)



SafeContext // Attributes = { HoldAllComplete };
SafeContext // Options    = { };


SafeContext[ symbol_? SymbolQ ] :=
  Block[ { $Context = "System`", $ContextPath = { "System`" } },
      With[ { name = StringTake[ ToString[ HoldComplete @ symbol,
                                           InputForm
                                 ],
                                 { 14, -2 }
                     ]
            },
            Context @ name
      ]
  ];


SafeContext[ Verbatim[ HoldPattern ][ symbol_? SymbolQ ] ] :=
    SafeContext @ symbol;


SafeContext[ Verbatim[ Unevaluated ][ symbol_? SymbolQ ] ] :=
    SafeContext @ symbol;


SafeContext[ string_String ] :=
    ToExpression[ string, StandardForm, SafeContext ];


SafeContext[ a___ ] :=
    (
      Message[ SafeContext::ssle,
               HoldForm @ SafeContext @ a,
               1
      ];

      $FailureContext
    );



(******************************************************************************)



iUpValueSymbols // ClearAll;
iUpValueSymbols // Attributes = { HoldAllComplete };
iUpValueSymbols // Options    = { };


iUpValueSymbols[ Verbatim[ HoldPattern ][ patt_ ] ] :=
  iUpValueSymbols @ patt;


iUpValueSymbols[ (f : Except[ HoldPattern, _Symbol ])[ ___ ] ] :=
  HoldComplete @ f;


iUpValueSymbols[ Verbatim[ Verbatim ][ f_ ][ ___ ] ] :=
  HoldComplete @ f;



UpValueSymbols // Attributes = { HoldAllComplete };
UpValueSymbols // Options    = { };


UpValueSymbols[ x_? SymbolQ ] :=
  Flatten[ HoldComplete @@ ( iUpValueSymbols /@ First /@ UpValues @ x ),
           1
  ];


UpValueSymbols[ ___ ] :=
  { };



(******************************************************************************)



SafePatternNameQ // Attributes = { HoldAllComplete };
SafePatternNameQ // Options    = { };


SafePatternNameQ[ x_? SymbolQ ] :=
  FreeQ[ UpValueSymbols @ x,
         Pattern,
         { 1 }
  ];



(******************************************************************************)



ClearAll[ HeldModule, HeldCompoundExpression, HeldSetDelayed ];

HeldModule              // Attributes = { HoldAllComplete };
HeldCompoundExpression  // Attributes = { HoldAllComplete };
HeldSetDelayed          // Attributes = { HoldAllComplete };


FoldModule // Attributes = { HoldAllComplete };
FoldModule // Options    = { };


FoldModule[ HeldModule[ { v_Symbol, vars___ },
                        HeldCompoundExpression[ Set[ v_, vExp_ ], expr___ ]
            ]
] :=

    With[ { newExpr = HeldCompoundExpression @ expr /.
                        HoldPattern @ v :> vExp
          },
          FoldModule @ HeldModule[ { vars }, newExpr ]
    ];


FoldModule[ HeldModule[ { }, HeldCompoundExpression @ expr_ ] ] :=
    TempHold @ expr;


FoldModule[ HoldPattern[ Module[ { v___ }, CompoundExpression[ e___ ] ] ] ] :=
    FoldModule @ HeldModule[ { v }, HeldCompoundExpression @ e ];


FoldModule /: SetDelayed[ patt_, def_FoldModule ] :=
    With[ { folded = def },
          Replace[ StripTempHolds @ HeldSetDelayed[ patt, folded ],
            HeldSetDelayed[ args___ ] :> SetDelayed[ args ]
          ]
    ];



(******************************************************************************)



CompressExpression // Attributes = { };
CompressExpression // Options    = { };


CompressExpression[ expr_, head_ ] :=

  WithContext @ Module[ { bytes, size1, size2, length, depth, leaves },

      bytes = WithContext @ BinarySerialize[ Unevaluated @ expr,
                                             PerformanceGoal -> "Size"
                            ];

      size1     = ByteCount @ Unevaluated @ expr;
      size2     = ByteCount @ bytes;
      length    = Length    @ Unevaluated @ expr;
      depth     = Depth     @ Unevaluated @ expr;
      leaves    = LeafCount @ Unevaluated @ expr;

      CompressedExpression @ <|
          "Size" ->
            <|
                "Original"      -> size1,
                "Compressed"    -> size2,
                "Length"        -> length,
                "Depth"         -> depth,
                "LeafCount"     -> leaves
            |>
          ,
          "Head"    -> head,
          "Data"    -> bytes,
          "Version" -> 2
      |>
  ];


CompressExpression[ expr : ( h_[ ___ ] ) ] :=
  CompressExpression[ Unevaluated @ expr, HoldForm @ h ];


CompressExpression[ expr_ /; AtomQ @ Unevaluated @ expr ] :=
  With[ { head = Head @ Unevaluated @ expr },
        CompressExpression[ Unevaluated @ expr, HoldForm @ head ]
  ];



CompressedExpression // Attributes = { };
CompressedExpression // Options    = { };


CompressedExpression /:
  HoldPattern[ Uncompress[ CompressedExpression[ KeyValuePattern @ { "Version" -> 2, "Data" -> b_ByteArray }  ],
                           wrapper_: ( ## & )
               ]
  ] :=
    BinaryDeserialize[ b, wrapper ];


CompressedExpression /:
  HoldPattern[ Uncompress[ CompressedExpression[ a_Association ],
                           wrapper_: ( ## & )
               ]
  ] :=

  Uncompress[ FromCharacterCode[ Developer`RawUncompress @ Normal @ a[ "Data" ],
                                 "ASCII"
              ],
              wrapper
  ];


CompressedExpression[ a_Association ][ keys__ ] := a[ keys ];



(******************************************************************************)



WithHolding // Attributes = { HoldAllComplete };
WithHolding // Options    = { };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::Arguments::With:: *)
WithHolding /:
  patt_ :> WithHolding[ { sets___Set, set_Set }, expr_ ] :=
    patt :> WithHolding[ { sets }, With[ { set }, expr /; True ] ];


WithHolding /:
  patt_ :> WithHolding[ { }, expr_ ] :=
    patt :> expr;



(******************************************************************************)



UnpackDispatches // Attributes = { };
UnpackDispatches // Options    = { };


UnpackDispatches[ expr_ ] :=
  ReplaceAll[ expr,
              d_Dispatch :> WithHolding[ { normal = Normal @ d },
                                           Dispatch @ normal
                            ]
  ];



(******************************************************************************)



(*iLookupSequence // ClearAll;
iLookupSequence // Attributes = { };
iLookupSequence // Options    = { };


iLookupSequence[ a_, { key_, seq___ }, { traversed___ } ] :=
  iLookupSequence[
      Lookup[
          a,
          key,
          Throw @ Missing[ "KeySequenceAbsent", { traversed, key } ]
      ],
      { seq },
      { traversed, key }
  ];


iLookupSequence[ a_, { }, _ ] :=
  a;



LookupSequence // Attributes = { };
LookupSequence // Options    = { };


LookupSequence[ a_, seq___ ] :=

  Catch @ Quiet[ Check[ iLookupSequence[ a, { seq }, { } ],
                        Throw @ Missing[ "KeySequenceAbsent", { seq } ],
                        Lookup::invrl
                 ],
                 Lookup::invrl
          ];*)


checkLookup // ClearAll;
checkLookup // Attributes = { HoldAllComplete };

checkLookup[ a_, args___ ] :=
  Quiet[ Check[ Lookup[ a, args ],
                Message[ LookupSequence::invrl, a ];
                Throw[ $Failed, LookupSequence ],
                Lookup::invrl
         ],
         Lookup::invrl
  ];


iLookupSequence // ClearAll;
iLookupSequence // Attributes = { HoldAllComplete };
iLookupSequence // Options    = { };

iLookupSequence[ a_, { key_, seq___ }, { traversed___ }, default_ ] :=
  iLookupSequence[ checkLookup[ a,
                                key,
                                Throw[ Replace[ default,
                                                Automatic :> Missing[ "KeySequenceAbsent",
                                                                      { traversed, key }
                                                             ]
                                       ],
                                       LookupSequence
                                ]
                   ],
                   { seq },
                   { traversed, key },
                   default
  ];

iLookupSequence[ a_, { }, _, _ ] := a;



LookupSequence // ClearAll;
LookupSequence // Attributes = { HoldAllComplete };
LookupSequence // Options    = { };


LookupSequence[ a_, { seq___ }, default_ ] :=
  With[ { result = Check[ Catch[ iLookupSequence[ a, { seq }, { }, default ],
                                 LookupSequence
                          ],
                          $LookupSequenceFailed,
                          LookupSequence::invrl
                   ] },
        result /; result =!= $LookupSequenceFailed
  ];

LookupSequence[ a_, { seq___ } ] :=
  LookupSequence[ a, { seq }, Automatic ];

LookupSequence[ keys_ ][ a_, arg___ ] :=
  LookupSequence[ a, keys, arg ];



(******************************************************************************)



ReplaceStrict // ClearAll;
ReplaceStrict // Attributes = {};
ReplaceStrict::invps =
  "Pattern Match Error\n\tExpected patterns: \n\t\t`1`\n\tActual \
expression: `2`";

holdRule // ClearAll;
holdRule[patt_ :> rhs_] := HoldComplete[patt] :> rhs;
holdRule[patt_ -> rhs_] := HoldComplete[patt] :> rhs;

ReplaceStrict[expr_, pattern : (_Rule | _RuleDelayed), lvl___] :=
  ReplaceStrict[Unevaluated@expr, {pattern}, lvl];

ReplaceStrict[expr_, {}, lvl___] :=
  Replace[Unevaluated@expr, {}, lvl];

ReplaceStrict[expr_, patterns : {(_Rule | _RuleDelayed) ..}, lvl___] :=

  Replace[HoldComplete[expr],
      Append[holdRule /@ patterns,
          HoldComplete[
              other_] :> (Message[ReplaceStrict::invps,
              Column[patterns[[All, 1]]], HoldForm @@ other]; $Failed)
      ],
      lvl
  ];



(******************************************************************************)



AbortOnMessage // ClearAll;
AbortOnMessage // Attributes = { HoldAllComplete };


msgAbort[ Hold[ msg_, True ] ] := Abort[ ];


AbortOnMessage[ evaluation_ ] :=
  Internal`HandlerBlock[ { "Message", msgAbort[ ## ] & },
                         evaluation
  ];



FailOnMessage // ClearAll;
FailOnMessage // Attributes = { HoldAllComplete };
FailOnMessage // Options    = { StackComplete -> True };


FailOnMessage::failed = "Failing after encountering: `1`";


$FailOnMessageTag    // Attributes = { Protected };
Wolfram`CodeEquivalenceUtilities`Dump`$failOnMessageStack  // Attributes = { Protected };
$FailOnMessageStack := Wolfram`CodeEquivalenceUtilities`Dump`$failOnMessageStack;
$FailOnMessageInput  // Attributes = { Protected };
$FailOnMessageFailed // Attributes = { Protected };


modifyProtected // ClearAll;
modifyProtected // Attributes = { HoldAllComplete };
modifyProtected // Options    = { };


modifyProtected[ protected_, eval_ ] :=
  Module[ { result },
      Unprotect @ protected;
      result = eval;
      Protect @ protected;
      result
  ];


MessageThrow[ t_, Hold[ msg_, True ] ] /; ! TrueQ @ $FailOnMessageFailed :=

  Module[ { inputSetQ, stackStart, stack },

      inputSetQ = MatchQ[ $FailOnMessageInput, _HoldForm ];

      stackStart = Replace[ If[ TrueQ @ inputSetQ,
          FirstPosition[ Stack[ _ ], $FailOnMessageInput, { 1 }, 1 ],
          { 1 }
      ],
          { idx_, ___ } :> idx
      ];

      stack = Most @ Stack[ _ ][[ stackStart ;; ]];

      modifyProtected[ Wolfram`CodeEquivalenceUtilities`Dump`$failOnMessageStack, Wolfram`CodeEquivalenceUtilities`Dump`$failOnMessageStack = stack ];
      modifyProtected[ $FailOnMessageInput, Unset @ $FailOnMessageInput ];

      Block[ { $FailOnMessageFailed = True },
          Message[ FailOnMessage::failed, HoldForm @ msg ]
      ];

      Throw[ $Failed, t ];
  ];


FailOnMessage[ evaluation_,
    tag : Except @ OptionsPattern[ ] : $FailOnMessageTag,
    opts : OptionsPattern[ ]
] :=
  Module[ { stackInit, inputSetQ },

      stackInit = If[ TrueQ @ OptionValue @ StackComplete, StackComplete, Identity ];
      inputSetQ = MatchQ[ $FailOnMessageInput, _HoldForm ];

      If[ ! TrueQ @ inputSetQ,
          modifyProtected[ $FailOnMessageInput, $FailOnMessageInput = HoldForm @ evaluation ]
      ];

      stackInit @
        If[ FreeQ[ Internal`Handlers[ "Message" ], HoldPattern @ MessageThrow[ tag, ___ ] ],
            Catch[ Internal`HandlerBlock[ { "Message", StackInhibit @ MessageThrow[ tag, ## ] & },
                evaluation
            ],
                $FailOnMessageTag
            ],
            evaluation
        ]
  ];



ShowStack[ KeyValuePattern[ "Stack" -> stack_ ] ] :=
  ShowStack @ stack;


ShowStack[ list : { KeyValuePattern[ "Stack" -> stack_ ], ___ } ] :=
  If[ Length @ list > 1,
      $Failed,
      ShowStack @ stack
  ];


ShowStack[ stack : { ___HoldForm } ] :=
  Module[ { hh, hcell, cellgroup },

      hh[ HoldForm[ (f_? SymbolQ)[ ___ ] ] ] :=
        ToString @ Unevaluated @ f;

      hh[ ___ ] := "";

      hcell[ lbl_, { idx_ } ] :=
        Cell[ ToString @ idx <> " " <> hh @ lbl,
              "Section",
              ShowGroupOpener -> True
        ];

      cellgroup = Cell @ CellGroupData[ { hcell @ ##, Cell[ BoxData @ ToBoxes @ #, "Input" ] }, Closed ] &;

      Internal`InheritedBlock[ { ByteArray, CloudObject, ResourceObject },

          Unprotect[ ByteArray, CloudObject, ResourceObject ];

          ByteArray /:
            MakeBoxes[ b : ByteArray[ s_String ], fmt_ ] :=
              With[ { b$ = b },
                  MakeBoxes[ Interpretation[ b$, ByteArray @ s ], fmt ]
              ];

          CloudObject /:
            MakeBoxes[ co : CloudObject[ _String ], fmt_ ] :=
              With[ { lbl = StringDelete[ First @ co, $cloudRootPattern <> "/" ] },
                  ToBoxes[ CloudObject @ Interpretation[ Hyperlink[ lbl, First @ co ],
                                                         First @ co
                                         ],
                           fmt
                  ]
              ];

          FormatValues[ ResourceObject ] = { };

          NotebookPut @ Notebook @ Flatten @ MapIndexed[ cellgroup, stack ]

      ];

  ];


ShowStack[ file_String ? FileExistsQ ] :=
  ShowStack @ Import @ file;


ShowStack[ ] :=
  ShowStack @ $FailOnMessageStack;


ShowStack[ ___ ] :=
  $Failed;



StackTraceOnMessage // Attributes = { HoldFirst };
StackTraceOnMessage // Options    = { };

StackTraceOnMessage::limit = "Message limit (`1`) reached.";


StackTraceOnMessage[ expr_, limit_: 1 ] :=

  Module[
      {
          $messagesStackTraces, handleMessage,
          successQ = True,
          result, aborted, stackData, counts
      },


      $messagesStackTraces = Internal`Bag[ ];


      handleMessage[ Hold[ msg_, True ] ] :=

        StackInhibit[

            successQ = False;

            Internal`StuffBag[ $messagesStackTraces,
                <|
                    "SessionTime" -> SessionTime[ ],
                    "Message"     -> HoldComplete @ msg,
                    "Stack"       -> Stack[ _ ]
                |>
            ];

            If[ Internal`BagLength @ $messagesStackTraces >= limit
                ,
                Throw[ $Aborted, "Abort" ]
            ];

        ];  (* /handleMessage *)


      Block[ { $Messages = { OpenWrite[ ] } },

          Internal`HandlerBlock[
              { "Message", handleMessage @ ## & },

              result = Catch[ StackBegin @ expr,
                  "Abort",
                  aborted @ #1 &
              ]
          ];

          Close /@ $Messages
      ];


      result =
        Replace[
            result,
            aborted[ r_ ] :>
              (Message[ StackTraceOnMessage::limit, limit ]; r)
        ];

      stackData = Internal`BagPart[ $messagesStackTraces, All ];

      counts = Counts[ Cases[ stackData[[ All, "Message" ]],
          HoldComplete @ Message[ msg_, ___ ] :>
            HoldForm @ msg
      ]
      ];


      <|
          "Result"        -> result,
          "Success"       -> successQ,
          "StackData"     -> stackData,
          "MessageCounts" -> counts
      |>

  ];



(******************************************************************************)



RenameSymbol // ClearAll;
RenameSymbol // Attributes = { HoldAllComplete };


RenameSymbol[ oldSymbol_? SymbolQ, newSymbol_? SymbolQ ] :=
  (
      CopySymbol[ oldSymbol, newSymbol ];
      Remove @ oldSymbol;
  );



(******************************************************************************)



HomePath // ClearAll;


HomePath[ path_String ] :=
  StringDelete[ StringDelete[ path, $HomeDirectory ],
                StartOfString ~~ $PathnameSeparator
  ];


HomePath[ obj_CloudObject ] :=
  StringDelete[ StringDelete[ First @ obj, $cloudRootPattern ],
                StartOfString ~~ $PathnameSeparator
  ];



RelativePath // ClearAll;
RelativePath[ path_String ] :=
  StringDelete[ StringDelete[ path,
                              Directory[ ]
                ],
                StartOfString ~~ $PathnameSeparator
  ];



(******************************************************************************)



exportString // ClearAll;


exportString[ expr_, fmt_ ] :=
  Internal`InheritedBlock[ { System`ConvertersDump`Utilities`ElementsQ },
      System`ConvertersDump`Utilities`ElementsQ[ x_? Developer`PackedArrayQ, _ ] =.;
      System`ConvertersDump`Utilities`ElementsQ[ x_ /; Developer`PackedArrayQ @ Unevaluated @ x, _ ] := False;
      ExportString[ Unevaluated @ expr, fmt ]
  ];



ToWDX // ClearAll;


ToWDX[ expr_, "Base64" ] :=
  WithContext @ exportString[ Unevaluated @ expr, { "Base64", "WDX" } ];


ToWDX[ expr_, "Raw" ] :=
  WithContext @ exportString[ Unevaluated @ expr, "WDX" ];


ToWDX[ expr_, _ : "Binary" ] :=
  ByteArray @ ToCharacterCode @ ToWDX[ Unevaluated @ expr, "Raw" ];



FromWDX // ClearAll;


FromWDX[ expr_String ] :=
  ImportString @ expr;


FromWDX[ expr_ByteArray ] :=
  ImportString @ FromCharacterCode @ Normal @ expr;



(******************************************************************************)



(* Mainly used for naming log files by timestamp *)
AbsoluteTimeString // Options = { "Separator" -> "-", "Digits" -> 7 };


AbsoluteTimeString[ t_DateObject, opts : OptionsPattern[ ] ] :=
  Block[ { $TimeZone = 0 },
      AbsoluteTimeString[ AbsoluteTime @ t, opts ]
  ];


AbsoluteTimeString[ opts : OptionsPattern[ ] ] :=
  Block[ { $TimeZone = 0 },
      AbsoluteTimeString[ AbsoluteTime[ ], opts ]
  ];


AbsoluteTimeString[ t_? NumericQ, opts : OptionsPattern[ ] ] :=
  Block[ { $TimeZone = 0 },
      StringInsert[ ToString @ Round[ t*10^(OptionValue[ "Digits" ]) ],
                    OptionValue[ "Separator" ],
                    -OptionValue[ "Digits" ] - 1
      ]
  ];



(******************************************************************************)



FromAbsoluteTimeString[ ts_String ] :=
  Block[ { $TimeZone = 0 },
      DateObject @
        Replace[ StringSplit[ ts, Except @ DigitCharacter ],
                 {
                     { t_ } :> ToExpression @ t,
                     { t_, d_ } :>
                       ToExpression @ t + ToExpression[ d ]*10.^-StringLength @ d
                 }
        ]
  ];



(******************************************************************************)


once // ClearAll;
once // Attributes = { HoldAllComplete };

once[ eval_, loc_ ] :=
  Quiet[ If[ TrueQ[ $VersionNumber < 11.2 ],
             Once[ loc; eval ],
             Once[ eval, loc ]
         ],
         CloudObject::invuri
  ];

once /:
  HoldPattern @ Unset[ once[ eval_, loc_ ] ] :=
    Quiet[ If[ TrueQ[ $VersionNumber < 11.2 ],
               Unset @ Once[ loc; eval ],
               Unset @ Once[ eval, loc ]
           ],
           CloudObject::invuri
    ];



(* OnceUnlessFailed:
   Same functionality as Once, except results are only cached if no messages
   were generated during evaluation. *)
OnceUnlessFailed // Attributes = { HoldAllComplete };
OnceUnlessFailed // Options    = { };

OnceUnlessFailed::fail = "Evaluating `1` resulted in the failure `2`.";


(* Use default tag if none given *)
OnceUnlessFailed[ eval_ ] :=
  OnceUnlessFailed[ eval, $DefaultTag ];


(* Use default location if none given *)
OnceUnlessFailed[ eval_, tag_ ] :=
  OnceUnlessFailed[ eval, tag, "KernelSession" ];


(* Main definition *)
OnceUnlessFailed[ eval_, $DefaultTag, location_ ] :=

  Module[ { result },

      Check[ (* use cached value if it's available *)
             result = once[ eval, location ]
             ,
             (* don't cache if a message was generated *)
             Quiet @ Unset @ once[ eval, location ];
             Throw[ result, OnceUnlessFailed ]
      ];

      (* don't cache if the result was a failure *)
      If[ FailureQ @ result,
          Quiet @ Unset @ once[ eval, location ];
          Message[ OnceUnlessFailed::fail, Short @ HoldForm @ eval, Short @ HoldForm @ # ] & @ result
      ];

      result

  ] ~Catch~ OnceUnlessFailed;


(* If given a tag, inline it to cache under a different key *)
OnceUnlessFailed[ eval_, tag_, location_ ] :=
  OnceUnlessFailed[ tag; eval, $DefaultTag, location ];


(* UpValues for Unset *)
OnceUnlessFailed /:
  HoldPattern @ Unset[ OnceUnlessFailed[ eval_ ] ] :=
    Unset @ OnceUnlessFailed[ eval, $DefaultTag ];


OnceUnlessFailed /:
  HoldPattern @ Unset[ OnceUnlessFailed[ eval_, $DefaultTag ] ] :=
    Unset @ Once @ eval;


OnceUnlessFailed /:
  HoldPattern @ Unset[ OnceUnlessFailed[ eval_, tag_ ] ] :=
    Unset @ OnceUnlessFailed[ tag; eval, $DefaultTag ];



(******************************************************************************)


MakeSelfExtractingExpression // ClearAll;
MakeSelfExtractingExpression // Attributes = { HoldAllComplete };
MakeSelfExtractingExpression // Options = {
    PerformanceGoal -> Automatic,
    Method -> "Inline",
    Permissions :> $Permissions,
    "Format" -> "MX",
    "DiscardModuleSymbols" -> True,
    "WarnOnDiscard" -> False,
    "Contexts" -> ___
};


MakeSelfExtractingExpression::discard = "Warning: `1` symbols were discarded: `2`";


makeInlineExpr[ expr_, wrapper_, Automatic, contexts_ ] :=
  makeInlineExpr[ expr, wrapper, "Size", contexts ];

makeCloudExpr[ expr_, wrapper_, Automatic, contexts_ ] :=
  makeCloudExpr[ expr, wrapper, "Speed", contexts ];


makeInlineExpr[ HoldComplete[ sym_? SymbolQ ], wrapper_, perfGoal_, contexts_ ] :=

  Module[ { def, bytes },

      SymbolQ;

      def = MinimalFullDefinition[ sym, "Contexts" -> contexts ];

      bytes = With[ { d = def, held = HoldComplete @ sym },
                    BinarySerialize[ Unevaluated[ Language`ExtendedFullDefinition[ ] = d;
                                                  ReleaseHold @ held
                                     ],
                                     PerformanceGoal -> perfGoal
                    ]
              ] // WithContext;

      With[ { b = bytes },
          wrapper @ BinaryDeserialize @ b
      ]
  ];


makeInlineExpr[ HoldComplete[ expr_ ], wrapper_, perfGoal_, contexts_ ] :=
  Block[ { $PlaceholderSymbol },
      $PlaceholderSymbol := expr;
      makeInlineExpr[ HoldComplete @ $PlaceholderSymbol, wrapper, perfGoal, contexts ]
  ];


moduleSymbolQ // ClearAll;
moduleSymbolQ // Attributes = { HoldAllComplete };

moduleSymbolQ[ sym_? SymbolQ ] :=
  GeneralUtilities`ModuleSymbolQ @ sym;

moduleSymbolQ[ name_String ] :=
  ToExpression[ name, InputForm, moduleSymbolQ ];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::PatternTest:: *)
dumpSaveToCloud[ target_CloudObject, expr_, permissions_, contexts_ ] :=

  Catch @ Module[ { allNames, names, dc, bin },

      allNames = WithContext[ ToFullString @@@ DependentSymbols[ expr, All, "Contexts" -> contexts ] ];

      names = If[ TrueQ @ $DiscardModuleSymbols,
                  Select[ allNames, Not @* moduleSymbolQ ],
                  allNames
              ];

      If[ TrueQ @ $WarnOnDiscard,
          dc = Length @ allNames - Length @ names;

          If[ dc > 0,
              Message[ MakeSelfExtractingExpression::discard, dc, Complement[ allNames, names ] ]
          ];
      ];

      bin = With[ { n = names },
          CheckPattern[ CreateCloudPackage[ target, n, "Overwrite" -> True ],
                        target ? FileExistsQ,
                        Throw @ $Failed
          ]
      ];

      SetPermissions[ bin, permissions ];

      names
  ];



$DefinedQ // ClearAll;


makeCloudExpr[ HoldComplete[ expr_ ], wrapper_, perfGoal_, permissions_, Automatic, contexts_ ] :=
  Module[ { hashPath, target },
      hashPath = Block[ { $PlaceholderSymbol = HoldComplete @ expr },
          HashPath @ MinimalFullDefinition[ $PlaceholderSymbol, "Contexts" -> contexts ]
      ];

      target = FileNameJoin @ { CloudObject[ "BinaryExpressions" ], hashPath <> ".mx" };

      makeCloudExpr[ expr, wrapper, perfGoal, permissions, target, contexts ];
    ];

makeCloudExpr[ HoldComplete[ expr_ ], wrapper_, perfGoal_, permissions_, target_, contexts0_ ] /; $CurrentCloudVersionNumber === $VersionNumber :=
  Module[ { names, contexts, bytes },

      names = dumpSaveToCloud[ target, HoldComplete @ expr, permissions, contexts0 ];

      contexts = # <> "*" & /@ Union[ SafeContext /@ names ];

      bytes = With[ { held = HoldComplete @ expr },
                    WithContext @ BinarySerialize[ Unevaluated @ ReleaseHold @ held,
                                                   PerformanceGoal -> perfGoal
                                  ]
              ];

      With[ { c = contexts, o = target, b = bytes },
          wrapper[ Quiet[ Unprotect @@ c ]; CloudGet @ o; BinaryDeserialize @ b ]
      ]
  ];


makeCloudExpr[ expr_, wrapper_, perfGoal_, permissions_, target_, contexts0_ ] /; $CurrentCloudVersionNumber =!= $VersionNumber :=

  Module[ { allNames, names, contexts, inline, obj },

      allNames = WithContext[ ToFullString @@@ DependentSymbols[ expr, All, "Contexts" -> contexts0 ] ];

      names = If[ TrueQ @ $DiscardModuleSymbols,
                  Select[ allNames, Not @* moduleSymbolQ ],
                  allNames
              ];

      contexts = # <> "*" & /@ Union[ SafeContext /@ names ];

      inline = makeInlineExpr[ expr, Hold, perfGoal, contexts ];

      obj = Replace[ inline,
                     Hold[ eval_ ] :>
                       CloudPut[ Unevaluated @ eval,
                                 target,
                                 Permissions -> permissions
                       ]
            ];

      With[ { c = contexts, o = obj },
            wrapper[ Quiet[ Unprotect @@ c ]; CloudGet @ o ]
      ]
  ];



MakeSelfExtractingExpression[ expr_, opts : OptionsPattern[ ] ] :=
  MakeSelfExtractingExpression[ expr, Hold, opts ];


MakeSelfExtractingExpression[ expr_, wrapper_, opts : OptionsPattern[ ] ] :=
  MakeSelfExtractingExpression[ expr, wrapper, Automatic, opts ];


MakeSelfExtractingExpression[ expr_, wrapper_, location_, opts : OptionsPattern[ ] ] :=
  WithContext @ Module[ { held, perf },

      held = HoldComplete @ expr;
      perf = OptionValue @ PerformanceGoal;

      Block[ {
          $WarnOnDiscard = OptionValue @ "WarnOnDiscard",
          $DiscardModuleSymbols = OptionValue @ "DiscardModuleSymbols"
      },
          Replace[ OptionValue @ Method,
               {
                   "Inline" :> makeInlineExpr[ held, wrapper, perf, OptionValue @ "Contexts" ],
                   "Cloud" :> makeCloudExpr[ held, wrapper, perf, OptionValue @ Permissions, location, OptionValue @ "Contexts" ],
                   _ :> $Failed
               }
          ]
      ]
  ];


MakeSelfExtractingFunction // ClearAll;
MakeSelfExtractingFunction // Attributes = { HoldAllComplete };
MakeSelfExtractingFunction // Options = Options @ MakeSelfExtractingExpression;


MakeSelfExtractingFunction[ f_, opts : OptionsPattern[ ] ] :=
  Module[ { held },
      held = MakeSelfExtractingExpression[ f, Hold, opts ];

      Replace[ held,
               Hold[ g_ ] :> Function[ Null, g @ ##, { HoldAllComplete } ]
      ]
  ];



(******************************************************************************)



MakeSelfExtractingAPIFunction // ClearAll;
MakeSelfExtractingAPIFunction // Attributes = { HoldAllComplete };


MakeSelfExtractingAPIFunction[ APIFunction[ args_, expr_, rest___ ] ] :=
  APIFunction[ args, MakeSelfExtractingFunction @ expr, rest ];



(******************************************************************************)



CloudEvaluateBinary // ClearAll;
CloudEvaluateBinary // Attributes = { HoldAllComplete };


CloudEvaluateBinary[ eval_, wrapper_: (## &) ] :=
  With[ { held = MakeSelfExtractingExpression @ eval },
      CloudEvaluate[ ReleaseHold @ held, wrapper ]
  ];



(******************************************************************************)



ignoringAttributes // ClearAll;
ignoringAttributes // Attributes = { };
ignoringAttributes // Options    = { };


ignoringAttributes[ Language`DefinitionList[ sym_ -> { defs___,
                                                       Attributes -> _
                                                     }
                    ]
] :=
  Language`DefinitionList[ sym -> { defs, Attributes -> { } } ];


ignoringAttributes[a___] := a;



noTemporary // ClearAll;
noTemporary // Attributes = { };
noTemporary // Options    = { };


noTemporary[
    Language`DefinitionList[
        sym_ -> { defs___, Attributes -> { a___, Temporary, b___ } }
    ]
] :=
  Language`DefinitionList[ sym -> { defs, Attributes -> { a, b } } ];


noTemporary[a___] := a;


noVersionTag[defs_] := DeleteCases[defs,
    Verbatim[HoldPattern][(version_?SymbolQ /;
      SymbolName@Unevaluated@version === "Version")[_]] -> _String,
    Infinity
];


noVersionTag[a___] := a;



DefinedQ // Attributes = { HoldAllComplete };
DefinedQ // Options    = { "Ignore" -> "Undefined" };


DefinedQ[ str_String /; StringQ @ str && NameQ @ str,
          opts : OptionsPattern[ ]
] :=
  ToExpression[ str,
                StandardForm,
                Function[ s, DefinedQ[ s, opts ], { HoldAllComplete } ]
  ];


DefinedQ[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=

  Module[ { ignore },

      ignore = Replace[ OptionValue @ "Ignore",
                             {
                                 "Temporary" -> noTemporary,
                                 "Undefined" -> ignoringAttributes,
                                 _           -> Identity
                             }
                    ];

      ! MatchQ[ ignore @ noVersionTag @ Language`ExtendedDefinition @ symbol,
          Language`DefinitionList[
              HoldForm @ symbol ->
                {
                    OwnValues -> { },
                    SubValues -> { },
                    UpValues -> { },
                    DownValues -> { },
                    NValues -> { },
                    FormatValues -> { },
                    DefaultValues -> { },
                    Messages -> { },
                    Attributes -> { }
                }
          ]
      ]
  ];


DefinedQ[ ___ ] := False;



(******************************************************************************)



handleMessage // ClearAll;
handleMessage // Attributes = { };
handleMessage // Options    = { };


handleMessage[ stream_, logDir_ ][ Hold[ msg_, True ] ] :=
  StackInhibit @
    Module[ { stack, uuid, stackDir, stackFile },

        stack     = Stack[ _ ];
        uuid      = CreateUUID[ ];
        stackDir  = FileNameJoin @ { logDir, "StackDumps" };
        stackFile = FileNameJoin @ { stackDir, uuid <> ".wdx" };

        If[ ! DirectoryQ @ stackDir, CreateDirectory @ stackDir ];
        Export[ stackFile, stack ];

        WriteString[ stream,
                     StringJoin[ "\n",
                                 uuid, "\t",
                                 DateString[ ], "\t",
                                 ToString[ Unevaluated @ msg, InputForm ],
                                 "\n"
                     ]
        ]
    ];



LogMessages // Attributes = { HoldAllComplete       };
LogMessages // Options    = { FailOnMessage -> True };


$defaultLogDir := FileNameJoin @ { $UserBaseDirectory,
                                   "ApplicationData",
                                   "CodeEquivalenceUtilities",
                                   "Logs"
                                 };


LogMessages[ evaluation_,
             logDir : Except @ OptionsPattern[ ] : $defaultLogDir,
             opts : OptionsPattern[ ]
] :=

  Module[ { logFile, stream, evaluationData },

      Hold @ $defaultLogDir;

      If[ ! DirectoryQ @ logDir, CreateDirectory @ logDir ];

      logFile =
        FileNameJoin @ {
            logDir,
            StringInsert[ ToString @ Round[ 1000000 * AbsoluteTime[ ] ],
                          "-",
                          -7
            ]
        };

      stream = OpenWrite[ logFile <> ".log", FormatType -> InputForm ];

      Internal`HandlerBlock[
          { "Message", handleMessage[ stream, logDir ] @ ## & },

          evaluationData = EvaluationData[ If[ OptionValue @ FailOnMessage,
                                               FailOnMessage,
                                               Identity
                                           ][ StackBegin @ evaluation ]
                           ];

          If[ ! evaluationData[ "Success" ],
              Export[ logFile <> ".wdx", evaluationData ]
          ];

          WriteString[ stream,
                       "LogMessages : ", DateString[ ], " : ",
                       evaluationData[ "InputString" ],
                       "\n"
          ];

          WriteString[ stream,
                       "LogMessages : ", DateString[ ], " : ",
                       "Evaluation completed.",
                       "\n"
          ];

          Close @ stream;
          evaluationData[ "Result" ]
      ]
  ];



(******************************************************************************)



(* Temporary workaround for $RequesterWolframID not being set correctly *)
(* https://jira.wolfram.com/jira/browse/CLOUD-7469                      *)

URLFetchNoCookies // Attributes = { };
URLFetchNoCookies // Options    = Options @ URLFetch;


URLFetchNoCookies[ CloudObject[ url_String, ___ ], args___ ] :=
  URLFetchNoCookies[ url, args ];


URLFetchNoCookies[ url_String, args___ ] :=
  (
      ClearCookies @ All;
      If[ TrueQ @ $CloudConnected,
          CloudObject`Private`authenticatedURLFetch,
          URLFetch
      ][ url, args ]
  );



WithNoCookies // Attributes = { HoldFirst };
WithNoCookies // Options    = { };

WithNoCookies[ eval_ ] :=
  Module[ { $inDef = False },
      Internal`InheritedBlock[ { URLFetch },
          Unprotect @ URLFetch;
          ClearAttributes[ URLFetch, ReadProtected ];

          DownValues[ URLFetch ] =
            Prepend[ DownValues @ URLFetch
                     ,
                     HoldPattern[ URLFetch[ args___ ] ] /; ! TrueQ @ $inDef :>
                       Block[ { $inDef = True }, URLFetchNoCookies @ args ]
            ];

          eval
      ]
  ];



(******************************************************************************)



$EmptyContextNumber = 0;



EmptyContext // Attributes = { };
EmptyContext // Options    = { };


EmptyContext[ name_String ] :=
  Module[ { ctx },
      While[ Names[ ( ctx = StringJoin[ StringDelete[ name, "`" ~~ EndOfString ],
                                        "$",
                                        ToString[ $EmptyContextNumber++ ],
                                        "`"
                            ] ) <> "*"
             ] =!= { }
      ];
      ctx
  ];


EmptyContext[ ] :=
  EmptyContext @ $Context;



(******************************************************************************)



definitions // ClearAll;
definitions // Attributes = { HoldAllComplete };
definitions // Options    = { };


definitions[ name_String? StringQ ] :=
  ToExpression[ name, StandardForm, definitions ];


definitions[ symbol_? SymbolQ ] :=
  DeleteCases[
      Association @ Flatten @
        Cases[ UnpackDispatches @ Language`ExtendedDefinition @ symbol,
               HoldPattern[ defType_? SymbolQ -> def_ ] :> (defType -> def),
               { 3 }
        ],
      { }
  ];



(******************************************************************************)



$excluded =
  Union[ # <> "`" & /@ Cases[ Lookup[ Options @ Language`ExtendedFullDefinition,
                                      "ExcludedContexts",
                                      { }
                              ],
                              _String,
                              Infinity
                       ],
         {
             "Wolfram`CodeEquivalenceUtilities`Dump`",
             "WolframChallenges`Dump`"
         }
  ];



iDependentNames // Attributes = { HoldAllComplete };
iDependentNames // Options    = { "ExcludedContexts" -> $excluded };


iDependentNames[ name_String? StringQ, opts : OptionsPattern[ ] ] :=
  ToExpression[ name,
                StandardForm,
                Function[ s, iDependentNames[ s, opts ], HoldAllComplete ]
  ];


iDependentNames[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=
  Module[ { defs, allNames, validCtx },

      defs = definitions @ symbol;

      allNames = Replace[ Union @ Cases[ Values @ defs,
                                         s_? SymbolQ :> HoldComplete @ s,
                                         Infinity,
                                         Heads -> True
                                  ],
                          HoldComplete[ s_ ] :> FullSymbolName @ s,
                          { 1 }
                 ];

      validCtx = Except[ Alternatives @@ OptionValue @ "ExcludedContexts" ];

      DeleteCases[ Select[ allNames,
                           MatchQ[ Context @ #, validCtx ] &
                   ],
                   FullSymbolName @ symbol
      ]
  ];



DependentNames // Attributes = { HoldAllComplete };
DependentNames // Options    = Options @ iDependentNames;


DependentNames[ name_String? StringQ, opts : OptionsPattern[ ] ] :=
  ToExpression[ name,
                StandardForm,
                Function[ s, DependentNames[ s, opts ], HoldAllComplete ]
  ];


DependentNames[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=
  Module[ { names, search },

      names = <| |>;

      search = If[ ! KeyExistsQ[ names, # ]
                   ,
                   names[ # ] = iDependentNames[ #, opts ];
                   #0 /@ names @ #
                   ,
                   { }
               ] &;

      search @ FullSymbolName @ symbol;
      names
  ];



(******************************************************************************)



(*parse1 // ClearAll;
parse1 // Attributes = { HoldAllComplete };

parse1[OwnValues | SubValues | UpValues | DownValues | NValues |
  FormatValues | DefaultValues | Messages |
  Attributes -> {expr___}] := parse2 /@ TempHold[expr];

parse1[OwnValues -> Verbatim[HoldPattern][_] :> expr_] := parse2[expr];



parse2 // ClearAll;
parse2 // Attributes = {HoldAllComplete};


parse2[Verbatim[HoldPattern][lhs_] :> rhs_] := TempHold[lhs, rhs];
parse2[Verbatim[HoldPattern][lhs_] -> rhs_] := TempHold[lhs, rhs];
parse2[expr___] := TempHold[expr];



$excludedContexts =
  Cases[ Lookup[ Options @ Language`ExtendedFullDefinition,
                 "ExcludedContexts"
         ],
         ctx_String ? StringQ :> ctx <> "`"
  ] ~Join~ {
      "GeneralUtilities`"
  };


joinInputDef[ Language`DefinitionList[ ] ] :=
  Language`DefinitionList[ ];


joinInputDef[ def : Language`DefinitionList[ _ ] ] :=
  Quiet @ Check[ Module[ { def2 },
                     def2 = ToExpression @ ToString[ def, InputForm ];
                     ReplacePart[ def,
                                  { 1, 2 } -> Merge[ { def[[ 1, 2 ]], def2[[ 1, 2 ]] },
                                                     DeleteDuplicates @* Flatten @* Join
                                              ]
                     ]
                 ],
                 def
          ];

$scPatt // ClearAll;
$scPatt = With | Module | Block | DynamicModule;

getSCLocalVars // ClearAll;
getSCLocalVars // Attributes = {};
getSCLocalVars // Options = {};

getSCLocalVars[$scPatt[{localAssignments___}, localExpression_, ___],
    wrapper_ : HoldPattern] :=
  Cases[HoldComplete@
    localAssignments, (Set | SetDelayed)[x_?SymbolQ, _] |
    x_?SymbolQ :> wrapper@x];

getSCLocalVars[HoldComplete[a___], wrapper_: HoldPattern] :=
  getSCLocalVars[Unevaluated@a, wrapper];

removeScopedSymbols // ClearAll;
removeScopedSymbols[def_] := StripTempHolds[def //. {
    (sc : $scPatt)[{a___, (set : Set | SetDelayed)[x_?SymbolQ, v_],
        b___}, expr_, opts___] :>
      RuleCondition@
        With[{newExpr =
          DeleteCases[TempHold[expr], HoldPattern[x], Infinity,
              Heads -> True]},
            TempHold@sc[{a, TempHold[set@v], b}, newExpr, opts]
        ],
    (sc : $scPatt)[{a___, x_?SymbolQ, b___}, expr_, opts___] :>
      RuleCondition@
        With[{newExpr =
          DeleteCases[TempHold[expr], HoldPattern[x], Infinity,
              Heads -> True]},
            TempHold@sc[{a, b}, newExpr, opts]
        ]
}];

removePatternSymbols // ClearAll;
removePatternSymbols[def_] :=
  StripTempHolds[
      def //. (r : RuleDelayed | SetDelayed)[lhs_, rhs_] /; !
        FreeQ[HoldComplete[lhs], Verbatim[Pattern][_?SymbolQ, ___]] :>
        RuleCondition@
          With[{syms =
            Union@Cases[HoldComplete[lhs],
                Verbatim[Pattern][s_?SymbolQ, ___] :> HoldPattern[s],
                Infinity, Heads -> True]},
              TempHold[r] @@ {DeleteCases[TempHold[lhs],
                  Verbatim[Pattern][_?SymbolQ, ___], Infinity, Heads -> True],
                  DeleteCases[TempHold[rhs], Alternatives @@ syms, Infinity,
                      Heads -> True]}
          ]];

preprocessDef // ClearAll;
preprocessDef = removePatternSymbols@*removeScopedSymbols@*joinInputDef;


DependentSymbols // Attributes = { HoldAllComplete };
DependentSymbols // Options    = {
    "Contexts"          -> ___,
    "ExcludedContexts"  -> Alternatives @@ $excludedContexts ~~ ___,
    "Self"              -> False,
    "IgnoringUndefined" -> True,
    "IgnoreModuleSymbols" -> True
};


DependentSymbols[ str_String, opts : OptionsPattern[ ] ] :=
  ToExpression[ str,
                StandardForm,
                Function[ s, DependentSymbols[ s, opts ], HoldAllComplete ]
  ];


DependentSymbols[ sym_? SymbolQ, opts : OptionsPattern[ ] ] :=

  Module[ { stage1, stage2, stage3, stage4, stage5, excl, symbols, symbols2 },

      stage1 = preprocessDef @ Language`ExtendedDefinition @ sym;

      stage2 = List @@ Replace[ stage1,
                                Language`DefinitionList[ _ -> { defs___ } ] :>
                                  parse1 /@ TempHold @ defs
                       ];

      stage3 = HoldComplete @@ (stage2 //. p_parse2 :> TrEval @ p);

      stage4 = stage3 //.
                 (f_)[ a___, TempHold[ b___ ], c___ ] :> f[ a, b, c ] //
                   DeleteDuplicates;

      stage5 =
        DeleteCases[ Cases[ TempHold @@ stage4,
                            s_? SymbolQ :> HoldComplete @ s,
                            Infinity,
                            Heads -> True
                     ],
                     HoldComplete @ TempHold
        ] // Union;

      symbols = Cases[ stage5,
                       HoldComplete[ s_? DefinedQ ] /;
                         And[
                             ! StringMatchQ[ SafeContext @ s,
                                             OptionValue @ "ExcludedContexts"
                             ],
                             StringMatchQ[ SafeContext @ s,
                                           OptionValue @ "Contexts"
                             ]
                         ] :> HoldComplete @ s
                ];

      symbols2 = If[ ! OptionValue @ "Self",
                     DeleteCases[ symbols, HoldComplete @ sym ],
                     symbols
                 ];

      If[ OptionValue @ "IgnoringUndefined",
          Cases[ symbols2, HoldComplete[ s_? DefinedQ ] ],
          symbols2
      ]
  ];


DependentSymbols[ expr_, opts : OptionsPattern[ ] ] :=
  Block[ { $TempSymbol = HoldComplete @ expr },
      DeleteCases[ DependentSymbols[ $TempSymbol, opts ],
                   HoldComplete @ $TempSymbol
      ]
  ];


getMore // ClearAll;
getMore // Attributes = { HoldFirst };

getMore[ keep_, opts___ ][ HoldComplete[ expr_ ] ] :=
  If[ ! KeyExistsQ[ keep, HoldComplete @ expr ] && DefinedQ @ expr,
      keep[ HoldComplete @ expr ] = True;
      getMore[ keep, opts ] /@ DependentSymbols[ expr, opts ]
  ];


getAll // ClearAll;
getAll // Attributes = { HoldFirst };

getAll[ expr_, opts___ ] :=
  Module[ { keep },
      keep = <| |>;
      getMore[ keep, opts ][ HoldComplete @ expr ];
      Keys @ keep
  ];



DependentSymbols[ sym_? SymbolQ, All, opts : OptionsPattern[ ] ] :=
  getAll[ sym, opts ];


DependentSymbols[ expr_, All, opts : OptionsPattern[ ] ] :=
  Block[ { $TempSymbol = HoldComplete @ expr },
      DeleteCases[ DependentSymbols[ $TempSymbol, All, opts ],
                   HoldComplete @ $TempSymbol
      ]
  ];*)




(******************************************************************************)


$RemovedRootContext // ClearAll;
(* Where to create temporary symbols to replace atomic _Removed expressions *)
$RemovedRootContext = "Removed`$";


(******************************************************************************)


$RemovedContextNumber // ClearAll;

$RemovedContextNumber := $removedContextNumber =
  If[ IntegerQ @ $removedContextNumber,
      $removedContextNumber,
      1
  ];

$RemovedContextNumber /:
  HoldPattern @ Set[ $RemovedContextNumber, n_Integer ] :=
    Set[ $removedContextNumber, n ];

$RemovedContextNumber // Protect;


$removedContextNumber // ClearAll;
$removedContextNumber = 1;


(******************************************************************************)


$RemovedContext // ClearAll;
$RemovedContext :=
  StringJoin[ $RemovedRootContext,
              ToString @ $RemovedContextNumber,
              "`"
  ];


(******************************************************************************)


SafeSymbolName // ClearAll;
SafeSymbolName // Attributes = { HoldAllComplete };

SafeSymbolName[ name_String ] := ToExpression[ name, InputForm, SafeSymbolName ];

SafeSymbolName[ symbol_Symbol? SymbolQ ] :=
  Check[ SymbolName @ Unevaluated @ symbol, $Failed ];

SafeSymbolName[ ___ ] := $Failed;


(******************************************************************************)



$excludedContexts =
  Cases[ Lookup[ Options @ Language`ExtendedFullDefinition,
                 "ExcludedContexts"
         ],
         ctx_String ? StringQ :> ctx <> "`"
  ] ~Join~ {
      "GeneralUtilities`"
  };


includedContextQ // ClearAll;
includedContextQ[ included_, excluded_ ] :=
  Function[ sym,
            SymbolQ @ sym && ! RemovedSymbolQ @ sym && FreeQ[ excluded, First[ StringSplit[ SafeContext @ sym, "`" ] ] <> "`" ] && StringMatchQ[ SafeContext @ sym, included ],
            { HoldAllComplete }
  ];


RemovedSymbolQ // ClearAll;
RemovedSymbolQ // Attributes = { HoldAllComplete };

RemovedSymbolQ[ symbol_Symbol? SymbolQ ] :=
  Internal`RemovedSymbolQ @ Unevaluated @ symbol;


$Unpacked // ClearAll;
$Unpacked // Attributes = { HoldAllComplete };

unpackQ // ClearAll;
unpackQ // Attributes = { HoldAllComplete };

(* Association *)
unpackQ[ association_Association ] :=
  AssociationQ @ Unevaluated @ association;

(* Internal`Bag *)
unpackQ[ bag_Internal`Bag ] :=
  AtomQ @ Unevaluated @ bag;

(* Dispatch *)
unpackQ[ dispatch_Dispatch ] :=
  DispatchQ @ Unevaluated @ dispatch;

(* Removed *)
unpackQ[ symbol_Symbol? RemovedSymbolQ ] :=
  True;



unpack // ClearAll;
unpack // Attributes = { HoldAllComplete };

(* Association *)
unpack[ association : Association[ rules___ ] ] :=
  $Unpacked[ Association, rules ];

(* Internal`Bag *)
unpack[ bag_Internal`Bag ] :=
  Replace[ Internal`BagPart[ bag, All, HoldComplete ],
           HoldComplete[ contents___ ] :>
             $Unpacked[ Internal`Bag, { contents } ]
  ];

(* Dispatch *)
unpack[ dispatch_Dispatch ] :=
  Replace[ ToExpression[ ToString[ HoldComplete @ dispatch, InputForm ],
                         InputForm
           ],
           HoldComplete[ Dispatch[ rules___ ] ] :>
             $Unpacked[ Dispatch, rules ]
  ];

(* Removed *)
unpack[ symbol_Symbol ] :=
  Replace[ ToExpression[ $RemovedContext <> SafeSymbolName @ symbol,
                         InputForm,
                         HoldComplete
           ],
           HoldComplete[ symbol$_ ] :> (
               $RemovedSymbolsCreated = True;
               $Unpacked[ Removed, symbol$ ]
           )
  ];


unpackExpressions // ClearAll;

unpackExpressions[ definition_ ] :=
  Internal`WithLocalSettings[
      clearRemoved[ ],
      ReplaceRepeated[ definition,
                       packed_? unpackQ :> RuleCondition @ unpack @ packed
      ],
      Null
  ];


repackExpressions // ClearAll;

repackExpressions[ definition_ ] :=
  Internal`WithLocalSettings[
      Null,
      ReplaceRepeated[ definition,
                       {
                           $Unpacked[ Removed, symbol_ ] :> symbol,
                           $Unpacked[ head_, contents___ ] :> head @ contents
                       }
      ],
      clearRemoved[ ]
  ];


clearRemoved // ClearAll;
clearRemoved[ ] /; TrueQ @ $RemovedSymbolsCreated :=
  With[ { contextPattern = $RemovedContext <> "*" },
      Quiet[
          Unprotect @ contextPattern;
          Remove @ contextPattern;
      ];
      $RemovedSymbolsCreated = False;
      $RemovedContextNumber = $RemovedContextNumber + 1;
  ];




DependentSymbols // ClearAll;
DependentSymbols // Attributes = { HoldAllComplete };
DependentSymbols // Options    = {
    "Contexts"          -> ___,
    "ExcludedContexts"  -> Alternatives @@ $excludedContexts ~~ ___,
    "Self"              -> False,
    "IgnoringUndefined" -> True,
    "IgnoreModuleSymbols" -> True
};


DependentSymbols[ string_String, opts: OptionsPattern[ ] ] :=
  ToExpression[ string,
                InputForm,
                Function[ expr,
                          DependentSymbols[ expr, All, opts ],
                          { HoldAllComplete }
                ]
  ];


DependentSymbols[ expression_, opts: OptionsPattern[ ] ] :=
  With[ { includedQ = includedContextQ[ OptionValue @ "Contexts",  OptionValue @ "ExcludedContexts" ] },
  DeleteDuplicates @
    Cases[ getUnboundSymbols[ #, HoldComplete ] & @@ {
             unpackExpressions @ HoldComplete @ expression
           },
           symbol_Symbol? includedQ :> HoldComplete @ symbol,
           Infinity,
           Heads -> True
    ]
  ];


DependentSymbols[ symbol_Symbol? SymbolQ, All, opts: OptionsPattern[ ] ] :=
  Cases[ MinimalFullDefinition @ symbol,
         HoldPattern[ HoldForm[ s_ ] -> _ ] :> HoldComplete @ s
  ];


DependentSymbols[ name_String? NameQ, All, opts: OptionsPattern[ ] ] :=
  ToExpression[ name,
                InputForm,
                Function[ symbol,
                          DependentSymbols[ symbol, All, opts ],
                          { HoldAllComplete }
                ]
  ];


DependentSymbols[ expression_, All, opts: OptionsPattern[ ] ] :=
  Block[ { $ExpressionPlaceholder },
      $ExpressionPlaceholder := expression;
      DeleteCases[ DependentSymbols[ $ExpressionPlaceholder, All, opts ],
                   HoldComplete @ $ExpressionPlaceholder
      ]
  ];



(******************************************************************************)



DependencyGraph // Attributes = { HoldFirst };
DependencyGraph // Options    = Options @ Graph;


symName // Attributes = { HoldAllComplete };
symName[ s_ ] := ToString @ Unevaluated @ s;


DependencyGraph[ name_String? NameQ, limit_Integer: 10, opts : OptionsPattern[ ] ] :=
  NestGraph[ symName @@@ DependentSymbols[ # ] &,
             name,
             limit,
             opts
  ];


DependencyGraph[ sym_? SymbolQ, limit_Integer: 10, opts : OptionsPattern[ ] ] :=
  With[ { name = symName @ sym },
      DependencyGraph[ name, limit, opts ]
  ];



(******************************************************************************)



PreserveDefinitions // Attributes = { HoldAllComplete };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::VariableError::Block:: *)
PreserveDefinitions[ eval_ ] :=

  Module[ { symbols },

      symbols = Flatten[ HoldComplete @@ DependentSymbols[ eval, All ] ];

      Replace[ symbols,
               HoldComplete[ syms___ ] :> Internal`InheritedBlock[ { syms },
                                                                   eval
                                          ]
      ]
  ];



(******************************************************************************)



(*smallDef // ClearAll;
smallDef // Attributes = {HoldAllComplete};
smallDef[sym_? SymbolQ] :=
  DeleteCases[
      Replace[Language`ExtendedDefinition[sym],
          Language`DefinitionList[
              HoldForm[_] -> {OwnValues -> {}, SubValues -> {},
                  UpValues -> {}, DownValues -> {}, NValues -> {},
                  FormatValues -> {}, DefaultValues -> {}, Messages -> {},
                  Attributes -> ({} | {Temporary})}] :> {}],
      HoldPattern[
          OwnValues | SubValues | UpValues | DownValues | NValues |
            FormatValues | DefaultValues | Messages |
            Attributes -> {}], {3}];



MinimalFullDefinition // Attributes = { HoldAllComplete };
MinimalFullDefinition // Options    = Options @ DependentSymbols;


MinimalFullDefinition[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=
  If[ StringMatchQ[ SafeContext @ symbol, OptionValue @ "ExcludedContexts" ],
      Language`DefinitionList[ ],
      DeleteCases[ Flatten[ Language`DefinitionList @@
        smallDef @@@
          DependentSymbols[ symbol, All, opts ]
      ], { } ]
  ];


MinimalFullDefinition[ name_String ? NameQ, opts : OptionsPattern[ ] ] :=
  ToExpression[ name,
                StandardForm,
                Function[ s, MinimalFullDefinition[ s, opts ], HoldAllComplete ]
  ];


MinimalFullDefinition[ expr_, opts : OptionsPattern[ ] ] :=
  Block[ { $PlaceholderSymbol },
      $PlaceholderSymbol := expr;
      MinimalFullDefinition[ $PlaceholderSymbol, opts ]
  ];*)


(******************************************************************************)


ReadProtectedQ // ClearAll;
ReadProtectedQ // Attributes = { HoldAllComplete };


ReadProtectedQ[ symbol_Symbol? SymbolQ ] :=
  ReadProtectedQ @@ { FullSymbolName @ symbol };


ReadProtectedQ[ name_String ] :=
  MemberQ[ Attributes @ name, ReadProtected ];


(******************************************************************************)

$emptyDefinitionPattern =
  Alternatives[
      OwnValues     -> { },
      SubValues     -> { },
      UpValues      -> { },
      DownValues    -> { },
      NValues       -> { },
      FormatValues  -> { },
      DefaultValues -> { },
      Messages      -> { },
      Attributes    -> { } | { Temporary }
  ];


unlockedAndReadProtectedQ // ClearAll;
unlockedAndReadProtectedQ // Attributes = { HoldAllComplete };

unlockedAndReadProtectedQ[ sym_? ReadProtectedQ ] := UnlockedSymbolQ @ sym;
unlockedAndReadProtectedQ[ ___ ] := False;


MinimalDefinition // ClearAll;
MinimalDefinition // Attributes = { HoldAllComplete };


MinimalDefinition[ HoldComplete[ symbol_Symbol? SymbolQ ] ] :=
  MinimalDefinition @ symbol;

(* need to clear read protected attribute but make sure it still shows up in the definition *)
MinimalDefinition[ symbol_Symbol? unlockedAndReadProtectedQ ] :=
  With[ { attributes = Attributes @ symbol },
      Internal`InheritedBlock[ { symbol },
          ClearAttributes[ symbol, { Protected, ReadProtected } ];
          Replace[ MinimalDefinition @ symbol,
                   HoldPattern[ Attributes -> _ ] :>
                     Attributes -> attributes,
                   { 3 }
          ]
      ]
  ];

MinimalDefinition[ sym_Symbol? SymbolQ ] :=
  Module[ { def, nonempty, undefined, simple },

      def       = Language`ExtendedDefinition @ sym;
      nonempty  = DeleteCases[ def, $emptyDefinitionPattern, 3 ];
      undefined = Language`DefinitionList[ _ -> { } ];
      simple    = Language`DefinitionList[ ];

      Replace[ nonempty, undefined -> simple ]
  ];


MinimalDefinition[ name_String ] :=
  ToExpression[ name, InputForm, MinimalDefinition ];


MinimalDefinition[ expression_ ] :=
  Block[ { $ExpressionPlaceholder },
      $ExpressionPlaceholder := expression;
      MinimalDefinition @ $ExpressionPlaceholder
  ];


MinimalDefinition[ ___ ] :=
  $Failed;


(******************************************************************************)



MinimalFullDefinition // ClearAll;
MinimalFullDefinition // Attributes = { HoldAllComplete };

MinimalFullDefinition[ symbol_Symbol? SymbolQ, opts: OptionsPattern[ DependentSymbols ] ] :=

  Module[ { $definitions, unseenQ, getNext, result },

      $definitions = <| |>;
      unseenQ[ ___ ] := True;

      getNext[ HoldComplete[ sym_ ] ? unseenQ ] :=
        With[ { definition = MinimalDefinition @ sym },
            unseenQ[ HoldComplete @ sym ] = False;
            $definitions[ HoldComplete @ sym ] = definition;

            getNext /@ DependentSymbols[ definition, opts ];
        ];

      getNext @ HoldComplete @ symbol;

      result = Flatten[ Language`DefinitionList @@ Values @ $definitions ];

      DeleteCases[ result, HoldForm[ $Unpacked ] -> _ ]

  ] ~Catch~ MinimalFullDefinition;


MinimalFullDefinition[ name_String? NameQ, opts: OptionsPattern[ DependentSymbols ] ] :=
  ToExpression[ name,
                InputForm,
                Function[ symbol, MinimalFullDefinition[ symbol, opts ], HoldAllComplete ]
  ];


MinimalFullDefinition[ expression_, opts: OptionsPattern[ DependentSymbols ] ] :=
  Block[ { $ExpressionPlaceholder },
      $ExpressionPlaceholder := expression;
      MinimalFullDefinition[ $ExpressionPlaceholder, opts ]
  ];


MinimalFullDefinition[ ___ ] :=
  $Failed;


(******************************************************************************)



FullDefinitionData // Attributes = { HoldAllComplete };
FullDefinitionData // Options    = Options @ DependentNames;


FullDefinitionData[ name_String? StringQ, opts : OptionsPattern[ ] ] :=
  ToExpression[ name,
                StandardForm,
                Function[ s, FullDefinitionData[ s, opts ], HoldAllComplete ]
  ];


FullDefinitionData[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=
  Module[ { dependencies },
      dependencies = DependentNames[ symbol, opts ];
      DeleteCases[ AssociationMap[ definitions, Keys @ dependencies ],
                   <| |>
      ]
  ];


FullDefinitionData[ expression_, opts : OptionsPattern[ ] ] :=
  Block[ { $tempSymbol = HoldComplete @ expression },
      KeyDrop[ FullDefinitionData[ $tempSymbol, opts ],
               FullSymbolName @ $tempSymbol
      ]
  ];



(******************************************************************************)



ToDefinitionList // Attributes = { };
ToDefinitionList // Options    = { };


ToDefinitionList[ defData_Association ] :=
  Language`DefinitionList @@
    KeyValueMap[ ToExpression[ #1, StandardForm, HoldForm ] -> #2 &,
                 Normal /@ defData
    ];



(******************************************************************************)



CompressWithDefinitions // Attributes = { HoldAllComplete };
CompressWithDefinitions // Options    = { };


CompressWithDefinitions[ expr_ ] :=
  With[ { def = ToDefinitionList @ FullDefinitionData @ expr },
      Compress @ Unevaluated[ Language`ExtendedFullDefinition[] = def;
                              expr
                 ]
  ];



(******************************************************************************)



TrackNewSymbols // Attributes = { };
TrackNewSymbols // Options    = { };


TrackNewSymbols[ input_ ] :=

  Module[ { $newSyms, symHandler, evaluated, newSymbolData, newSymbolNames },

      $newSyms = Internal`Bag[ ];

      symHandler = Function[ Null,
                             Internal`StuffBag[ $newSyms, HoldComplete @ ## ],
                             HoldAllComplete
                   ];

      Internal`AddHandler[ "NewSymbol", symHandler ];
      evaluated = ToExpression @ input;
      Internal`RemoveHandler[ "NewSymbol", symHandler ];
      newSymbolData = Union @ Internal`BagPart[ $newSyms, All ];

      newSymbolNames =
        Select[ Union @ Replace[ newSymbolData,
                                 HoldComplete @ { sym_String, ctx_String } :>
                                   ctx <> sym,
                                 { 1 }
                        ],
                NameQ
        ];

      <|
          "Result"     -> evaluated,
          "NewSymbols" -> newSymbolNames
      |>
  ];



(******************************************************************************)



$CurrentCloudVersionNumber :=
  $CurrentCloudVersionNumber =
    CloudEvaluate @ $VersionNumber;



CreateCloudPackage // Attributes = { };
CreateCloudPackage // Options    = {
    "Overwrite"           -> False,
    "Format"              -> "Base64",
    "EnforceVersionCheck" -> True
};


CreateCloudPackage::version = "Version mismatch (Local: `1`, Cloud: `2`)";
CreateCloudPackage::exists  = "File `1` already exists";


CreateCloudPackage[ name_String /; StringFreeQ[ name, "/"|".mx" ],
                    objects_,
                    opts : OptionsPattern[ ]
] :=
  Module[ { target },

      target = CloudEvaluate @ FileNameJoin @ { $UserBaseDirectory,
                                                "Applications",
                                                name <> ".mx"
                                              };

      CreateCloudPackage[ target, Unevaluated @ objects, opts ]
  ];



CreateCloudPackage[ obj_CloudObject,
                    objects_,
                    opts : OptionsPattern[ ]
] :=
  Module[ { target, temp },

      target = StringDelete[ First @ obj,
                             $cloudRootPattern ~~ ("/" | "")
               ];

      temp = target <> ".temp";

      CreateCloudPackage[ temp, Unevaluated @ objects, opts ];

      CloudEvaluate @
        With[
            {
                copied = CopyFile[
                    temp,
                    obj,
                    OverwriteTarget -> OptionValue[ "Overwrite" ]
                ]
            },
            DeleteFile @ temp;
            copied
        ]
  ];



CreateCloudPackage[ target_String, sym_? SymbolQ, opts : OptionsPattern[ ] ] :=
  Module[ { syms, names },
      syms = Union @ DependentSymbols[ sym, All ];
      names = FullSymbolName @@@ syms;
      CreateCloudPackage[ target, names, opts ]
  ];



CreateCloudPackage[ target_String, { }, opts: OptionsPattern[ ] ] :=
  CreateCloudPackage[ target, None, opts ];

CreateCloudPackage[ target_String,
                    objects_,
                    opts : OptionsPattern[ ]
] :=
  Module[ { file, out },

      If[ And[ $VersionNumber > $CurrentCloudVersionNumber,
               OptionValue[ "EnforceVersionCheck" ]
          ]
          ,
          Message[ CreateCloudPackage::version,
                   $VersionNumber,
                   $CurrentCloudVersionNumber
          ];
          $Failed
          ,

          file = FileNameJoin @ { $TemporaryDirectory, CreateUUID[ ] <> ".mx" };
          DumpSave[ file, objects, "SymbolAttributes" -> False ];

          out = If[ ! OptionValue @ "Overwrite" && FileExistsQ @ target
                    ,
                    Message[ CreateCloudPackage::exists, CloudEvaluate @ HomePath @ target ];
                    $Failed
                    ,
                    CopyFile[
                        file,
                        CloudObject @ target,
                        OverwriteTarget -> OptionValue[ "Overwrite" ]
                    ]
                ];

          DeleteFile @ file;

          out
      ]
  ];



(******************************************************************************)



CreateCloudPackageString // Attributes = { };
CreateCloudPackageString // Options    = { };


CreateCloudPackageString[ objects_ ] :=
  CreateCloudPackageString[ Unevaluated @ objects, Automatic ];


CreateCloudPackageString[ objects_, fmt : (Automatic | "String" | "Base64") ] :=

  Module[ { file, out },

      If[ $VersionNumber > $CurrentCloudVersionNumber
          ,
          Message[ CreateCloudPackage::version,
                   $VersionNumber,
                   $CurrentCloudVersionNumber
          ];
          $Failed
          ,

          WithContext[
              file = CreateFile[ ] <> ".mx";
              DumpSave[ file, objects ];

              out = ExportString[ Import[ file, "String" ],
                                  Replace[ fmt, Automatic -> "String" ]
                    ];

              DeleteFile @ file;
              out
          ]
      ]
  ];



(******************************************************************************)



DeployCloudPackage // Attributes = { };
DeployCloudPackage // Options    = { Permissions :> $Permissions };


DeployCloudPackage[ objects_, target_, opts : OptionsPattern[ ] ] :=

  With[
      {
          version = $VersionNumber,
          string = CreateCloudPackageString[ Unevaluated @ objects, "Base64" ]
      },

      CloudPut[ Unevaluated @
          If[ $VersionNumber < version
              ,
              Wolfram`CodeEquivalenceUtilities`CloudPackage::version =
                "Version mismatch (Local: `1`, Cloud: `2`)";
              Message[ Wolfram`CodeEquivalenceUtilities`CloudPackage::version,
                       $VersionNumber,
                       version
              ];
              $Failed
              ,
              ImportString[ string, "Base64" ]
          ]
          ,
          target
          ,
          Permissions -> OptionValue @ Permissions
      ]
  ];



(******************************************************************************)



Wolfram`CodeEquivalenceUtilities`$SandboxContext := EmptyContext[ "Sandbox" ];


Wolfram`CodeEquivalenceUtilities`$SandboxSymbols = Internal`Bag[ ];


ToSandboxedExpression // Attributes = { HoldAllComplete };
ToSandboxedExpression // Options    = {
    "Context"     :> Wolfram`CodeEquivalenceUtilities`$SandboxContext,
    "ContextPath" :> { "System`", "Developer`" }
};


ToSandboxedExpression[ input_, opts : OptionsPattern[ ] ] :=
  ToSandboxedExpression[ input, StandardForm, HoldComplete, opts ];


ToSandboxedExpression[ input_, fmt_, opts : OptionsPattern[ ] ] :=
  ToSandboxedExpression[ input, fmt, HoldComplete, opts ];


ToSandboxedExpression[ input_, fmt_, wrapper_, opts : OptionsPattern[ ] ] :=
  Block[
      {
          $Context     = OptionValue @ "Context",
          $ContextPath = OptionValue @ "ContextPath"
      },
      Module[ { expr },
          expr = ToExpression[ input, fmt, wrapper ];
          Scan[ Internal`StuffBag[ Wolfram`CodeEquivalenceUtilities`$SandboxSymbols, # ] &,
                FullSymbolName /@ Names[ $Context <> "*" ]
          ];
          expr
      ]
  ];



(******************************************************************************)



SandboxedDefinition // Attributes = { HoldAllComplete };
SandboxedDefinition // Options    = {
    "Sandbox" :> Wolfram`CodeEquivalenceUtilities`$SandboxContext,
    "AllowedContexts" -> "System`" | "Developer`"
};


SandboxedDefinition::strsym =
  "String or symbol expected at position 1 in `1`.";


SandboxedDefinition[ symbol_ ? DefinedQ, opts : OptionsPattern[ ] ] :=
  Module[ { sandbox, userDefinition, held, rule, symbols, contexts },

      sandbox = OptionValue @ "Sandbox";
      userDefinition = MinimalFullDefinition @ symbol;
      held = HoldComplete @@ userDefinition;

      rule = s_? SymbolQ /;
               ! StringMatchQ[ SafeContext @ s, Alternatives @@ OptionValue @ "AllowedContexts" ] :>
                 HoldComplete @ s;

      symbols = Union @ Cases[ held, rule, Infinity, Heads -> True ];
      contexts = Union[ SafeContext @@@ symbols ];

      Fold[ ReplaceContext[ #1, #2 -> sandbox ] &,
            userDefinition,
            contexts
      ]
  ];



SandboxedDefinition[ _String | _? SymbolQ, OptionsPattern[ ] ] :=
  Missing[ "NotFound" ];


SandboxedDefinition[ a___ ] := (
    Message[ SandboxedDefinition::strsym, HoldForm @ SandboxedDefinition @ a ];
    $Failed
);



(******************************************************************************)



CleanSandbox // Attributes = { };
CleanSandbox // Options    = { };


CleanSandbox[ ] :=
  CleanSandbox[ ___ ];


CleanSandbox[ ctx_String ] :=
  CleanSandbox[ ctx ~~ ___ ];


CleanSandbox[ pattern_ ] :=
  Module[ { names, flagged },

      names = Internal`BagPart[ Wolfram`CodeEquivalenceUtilities`$SandboxSymbols, All ];
      flagged = Select[ names, NameQ[ # ] && StringMatchQ[ #, pattern ] & ];

      Remove @@ flagged;

      Wolfram`CodeEquivalenceUtilities`$SandboxSymbols // ClearAll;
      Wolfram`CodeEquivalenceUtilities`$SandboxSymbols = Internal`Bag[ ];

      Scan[ Internal`StuffBag[ Wolfram`CodeEquivalenceUtilities`$SandboxSymbols, # ] &,
            Complement[ names, flagged ]
      ];

      flagged
  ];



(******************************************************************************)



ContextDepth // Attributes = { };
ContextDepth // Options    = { };


ContextDepth[ context_String ] :=
  Length @ StringSplit[ context, "`" ] - 1;



(******************************************************************************)



contextInLevelSpecQ // ClearAll;
contextInLevelSpecQ // Attributes = { };
contextInLevelSpecQ // Options    = { };


contextInLevelSpecQ[ subctx_, { min_, max_ } ] :=
  min <= Length @ StringSplit[ subctx, "`" ] - 1 <= max;


contextInLevelSpecQ[ { min_, max_ } ][ subctx_ ] :=
  contextInLevelSpecQ[ subctx, { min, max } ];



Subcontexts // Attributes = { HoldAllComplete };
Subcontexts // Options    = { };


Subcontexts[ ctx_String ] :=
  Subcontexts[ ctx, { 1, Infinity } ];


Subcontexts[ ctx_String, { 1, Infinity } ] :=
  DeleteCases[ Contexts[ ctx <> "*" ], ctx ];


Subcontexts[ ctx_String, { lvl_ } ] :=
  Subcontexts[ ctx, { lvl, lvl } ];


Subcontexts[ ctx_String, lvl_ ] :=
  Subcontexts[ ctx, { 1, lvl } ];


Subcontexts[ ctx_String, { min_, max_ } ] :=
  Select[ Contexts[ ctx <> "*" ],
          contextInLevelSpecQ[ { min, max } + ContextDepth @ ctx ]
  ];



(******************************************************************************)



ReplaceContext // Attributes = { };
ReplaceContext // Options =    { };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::PatternRule:: *)

(* No replacement for same context *)
ReplaceContext[ expression_, ctx_ -> ctx_ ] :=
  expression;


ReplaceContext[ expression_, oldContext_String -> newContext_String ] :=

  Module[ { heldSymbol, withHeldSymbols },

      heldSymbol // Attributes = { HoldAllComplete };

      withHeldSymbols = HoldComplete @ expression //.
        s_? SymbolQ /; SafeContext @ s === oldContext :>
          WithHolding[
              {
                  newSymString = newContext <> SymbolName @ Unevaluated @ s,
                  newSymbol = ToExpression[ newSymString,
                                            StandardForm,
                                            heldSymbol
                              ]
              },
              newSymbol
          ];

      ReleaseHold[ withHeldSymbols //. heldSymbol[ s_ ] :> s ]
  ];


ReplaceContext[ expression_, rules_List ] :=
  Identity @@ Fold[ ReplaceContext, HoldComplete @ expression, rules ];


ReplaceContext[ rules_ ][ expression_ ] :=
  ReplaceContext[ Unevaluated @ expression, rules ];



(******************************************************************************)



ClearContext // Attributes = { };
ClearContext // Options =    { };


ClearContext[ ctx_String ] :=
  Module[ { names, defined },
      names = Names[ ctx ~~ ___ ];
      defined = Select[ names, DefinedQ ];
      Quiet[ (Unprotect @ #; ClearAll @ #) & /@ names ];
      defined
  ];



(******************************************************************************)



WithContext // Attributes = { HoldAllComplete };
WithContext // Options =    { };


WithContext[ expr_ ] :=
  Block[ { $Context = "Global`", $ContextPath = { "System`", "Global`" } },
      expr
  ];



(******************************************************************************)



CompressWithContext // Attributes = { };
CompressWithContext // Options =    { };


CompressWithContext[ expr_ ] :=
  WithContext[ Compress @ Unevaluated @ expr ];



(******************************************************************************)



DefinitionLoader // Attributes = {HoldAllComplete};
DefinitionLoader[sym_] :=
  Module[{$loadedQ},
      ClearAll[$loadedQ];
      With[{def = MinimalFullDefinition[sym]},
          CompressWithContext[Unevaluated[
              If[! TrueQ@$loadedQ,
                  Language`ExtendedFullDefinition[] = def;
                  $loadedQ = True;
              ];
              sym
          ]]
      ]];


DefinitionLoader[sym_, flag_] :=
  (
      ClearAll[flag];
      flag = False;
      With[{def = MinimalFullDefinition[sym]},
          CompressWithContext[Unevaluated[
              If[! TrueQ @ flag,
                  Language`ExtendedFullDefinition[] = def;
                  flag = True;
              ];
              sym
          ]]
      ]
  );



(******************************************************************************)



SymbolLoader // Attributes = { HoldAllComplete };
SymbolLoader // Options    =
  {
      "Root"      :> CloudObject @ "Symbols",
      Permissions :> $Permissions
  };


SymbolLoader[ symbol_? SymbolQ, opts: OptionsPattern[ ] ] :=
    SymbolLoader[ symbol, ## &, opts ];


SymbolLoader[ symbol_ ? SymbolQ,
              wrapper_,
              opts: OptionsPattern[ ]
] :=

  Module[ { compressed },

      compressed =
        With[ { def = MinimalFullDefinition @ symbol },
              CompressWithContext @ Unevaluated[
                  Language`ExtendedFullDefinition[ ] = def;
                  wrapper @ symbol
            ]
        ];

      With[ { c = compressed },
          CloudPut[ Unevaluated @ Uncompress @ c,
                    FileNameJoin @ Flatten @ {
                        OptionValue @ "Root",
                        StringSplit[ FullSymbolName @ symbol, "`" ]
                    },
                    Permissions -> OptionValue @ Permissions
          ]
      ]
  ];



(******************************************************************************)



EvaluateInContext // Attributes = { HoldAllComplete };
EvaluateInContext // Options    = { };


EvaluateInContext[ expr_, context_, wrapper_: Identity ] :=

  Module[ { replaced, evaluated },

      replaced = ReplaceContext[ HoldComplete @ expr, $Context -> context ];

      evaluated = ReleaseHold @ replaced;

      With[ { e = evaluated },
          ReplaceContext[ wrapper @ e, context -> $Context ]
      ]
  ];



(******************************************************************************)



SearchNames // Attributes = { HoldAllComplete };
SearchNames // Options    = { };


SearchNames[ name_String, All ] :=
  With[ { names = Names[ "*`*" <> name <> "*" ] },
      SortBy[ names,
              EditDistance[ name, Last @ StringSplit[ #, "`" ] ] &
      ]
  ];


SearchNames[ name_String, count_Integer: 10 ] :=
  Take[ SearchNames[ name, All ],
        UpTo @ count
  ];


SearchNames[ sym_? SymbolQ, count_: 10 ] :=
  With[ { name = SymbolName @ Unevaluated @ sym },
      SearchNames[ name, count ]
  ];



(******************************************************************************)



SymbolInformation // Attributes = { HoldFirst };
SymbolInformation // Options    = { "NormalizeNames" -> True };


SymbolInformation[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=

  Block[ { $ContextPath = { } },

      Module[ { split, ctx, name, normalName, id, defTypes },

          split = StringSplit[ StringTake[ ToString @ HoldComplete @ symbol,
              { 21, -2 }
          ],
              "`"
          ];

          ctx = Replace[ StringJoin[ # <> "`" & /@ Most @ split ],
              "" -> Context @ Unevaluated @ symbol
          ];

          name = Last @ split;

          normalName =
            If[ OptionValue @ "NormalizeNames",
                StringDelete[ name, "$" ~~ DigitCharacter .. ~~ EndOfString ],
                name
            ];

          id = First @
            Replace[ StringCases[ name,
                StringExpression[ StartOfString,
                    __, "$",
                    (d : DigitCharacter ..),
                    EndOfString
                ] :> d
            ],
                { } -> { Missing @ "NotAvailable" }
            ];

          defTypes = Cases[ Language`ExtendedDefinition @ symbol,
              HoldPattern[ type_ -> Except[ { } ] ] :> type,
              { 3 }
          ];

          <|
              "Name"            -> normalName,
              "ID"              -> id,
              "Context"         -> ctx,
              "DefinedQ"        -> DefinedQ @ symbol,
              "DefinitionTypes" -> defTypes
          |>
      ]
  ];



(******************************************************************************)



FormatCode // ClearAll;
FormatCode // Attributes = { HoldAllComplete };


FormatCode[ code_ ] :=
  Block[ { GeneralUtilities`Formatting`PackagePrivate`sn },

      SetAttributes[ GeneralUtilities`Formatting`PackagePrivate`sn,
          HoldAllComplete
      ];

      GeneralUtilities`Formatting`PackagePrivate`sn[ s_? SymbolQ ] :=
        SymbolName @ Unevaluated @ s;

      GeneralUtilities`MakeFormattedBoxes @ code
  ];



FormatCodeString // Attributes = { };
FormatCodeString // Options    = { };


FormatCodeString[ string_ ] :=
  UsingFrontEnd @ First @ First @ MathLink`CallFrontEnd @
    FrontEnd`UndocumentedTestFEParserPacket[ string, True ];



MakeInputCell // Attributes = { HoldFirst };
MakeInputCell // Options    = Options @ Cell;


MakeInputCell[ e_, opts: OptionsPattern[ ] ] :=
    MakeInputCell[ e, "Input", opts ];


MakeInputCell[ e_, style_, opts : OptionsPattern[ ] ] :=
  Cell[ BoxData @ FormatCodeString @ ToString[ Unevaluated @ e, InputForm ],
        style,
        opts
  ];



(******************************************************************************)



(*at // ClearAll;
at // Attributes = { HoldAllComplete };


at[ s_? SymbolQ, n_? Positive ] :=
  ReleaseHold[ HoldComplete @ at[ s, n - 1 ] /.
    OwnValues @ Unevaluated @ s
  ];


at[ a_Association, n_? Positive ] :=
  KeyValuePattern[ { ## } ] & @@ SortBy[ Normal[ at[ #, n - 1 ] & /@ a ],
                                         First
                                 ];


at[ a_, _ ] :=
  Blank @ Head @ Unevaluated @ a;*)



getKVP // ClearAll;
getKVP // Attributes = {HoldAllComplete};

getKVP[s_?SymbolQ, n_?Positive] :=
  With[{nn = n - 1},
      ReleaseHold[
          HoldComplete@getKVP[s, nn] /.
            OwnValues@Unevaluated@s
      ]
  ];

getKVP[assoc_Association, n_Integer?Positive] :=
  Replace[
      Replace[assoc,
          {
              a : Except[_TempHold] :> TrEval@Blank[Head[Unevaluated[a]]]
          },
          {n}
      ],
      a_Association :>
        With[{kvp = KeyValuePattern[Normal[a]]}, TempHold[kvp] /; True],
      {n - 1}
  ];

getKVP[a_, _] := Blank@Head@Unevaluated@a;



KVPBoxes /: MakeBoxes[ KVPBoxes[ kvp_KeyValuePattern ], fmt_ ] :=
  FormatCode @ kvp;



GetKeyValuePattern // Attributes = { };
GetKeyValuePattern // Options    = { "MakeBoxes" -> False, "QuantityPatterns" -> True };


GetKeyValuePattern[ assoc_, Infinity, opts : OptionsPattern[ ] ] :=
  GetKeyValuePattern[ assoc, Depth @ Unevaluated @ assoc, opts ];


GetKeyValuePattern[ assoc_, n_Integer: 1, opts : OptionsPattern[ ] ] :=

  Module[ { replaced, folded, sorted, cleaned },

      replaced = assoc //.
        If[ OptionValue @ "QuantityPatterns",
            Quantity[ q_, t_ ] :> $QuantityType[ t ][ q ],
            { }
        ];

      folded = Fold[ getKVP, replaced, Range[ n, 1, -1 ] ];

      sorted = folded //.
        TempHold @ Verbatim[ KeyValuePattern ][ { a___ } ] /;
          ! OrderedQ @ HoldComplete @ a :>
            WithHolding[
                {
                    sorted = SortBy[ TempHold @ a,
                                     Function[ x,
                                               Extract[ x, 1, HoldComplete ],
                                               HoldAllComplete
                                     ]
                             ]
                },
                TempHold @ KeyValuePattern @ { sorted }
            ];

      cleaned = StripTempHolds @ sorted;

      If[ OptionValue @ "MakeBoxes",
          KVPBoxes @ cleaned,
          cleaned
      ] //. Verbatim[ Blank ][ $QuantityType[ t_ ] ] :> Quantity[ _, t ]
  ];



FindInvalidKeyValuePairs // Attributes = { };
FindInvalidKeyValuePairs // Options    = { };


findBadKeys // ClearAll;
findBadKeys // Attributes = { HoldAllComplete };

findBadKeys[ HoldComplete[ data_Association ], HoldComplete[ patt_KeyValuePattern ] ] :=
  findBadKeys[ data, patt ];

findBadKeys[ data_Association, kvp_KeyValuePattern ] :=

  Module[ { testPatterns, testKeys, missingKeys, badKeys },

      testPatterns = Association @@ kvp;
      testKeys = Keys @ testPatterns;
      missingKeys = Complement[ testKeys, Keys @ data ];

      badKeys = Select[ Complement[ testKeys, missingKeys ],
                        ! MatchQ[ Extract[ data, #, HoldComplete ],
                                  Extract[ testPatterns, #, HoldComplete ]
                        ] &
                ];

      Join[ With[ { d = Extract[ data, #, HoldComplete ], p = Extract[ testPatterns, #, HoldComplete ] },
                # -> findBadKeys[ d, p ]
            ] & /@ badKeys,
            # -> Missing[ "KeyAbsent", # ] & /@ Intersection[ testKeys, missingKeys ]
      ] // Association
  ];

findBadKeys[ HoldComplete[ expr_ ], HoldComplete[ patt_ ] ] /; ! MatchQ[ HoldComplete[ expr ], HoldComplete[ patt ] ] :=
  <|
      "Expected" -> HoldForm @ patt,
      "Actual" -> Short @ HoldForm @ expr
  |>;

findBadKeys[ ___ ] := $Failed;


FindInvalidKeyValuePairs[ data_Association, kvp_KeyValuePattern ] :=
  findBadKeys[ data, kvp ];



(******************************************************************************)



AssertPattern::ifail = "The expression `1` failed to match the expected pattern `2`.";
AssertPattern::efail = "The expression `1` evaluated to `2`, which failed to match the expected pattern `3`.";


short // ClearAll;
short // Attributes = { HoldAllComplete };

short[ expr_ ] :=
  Short[ HoldForm @ expr, 1 ];


AssertPattern[ expr_, patt_, default_: $Failed ] :=
  With[ { eval = expr },
      If[ ! MatchQ[ eval, patt ],
          If[ Hold @ eval =!= Hold @ expr,
              Message[ AssertPattern::efail, short @ expr, short @ eval, short @ patt ],
              Message[ AssertPattern::ifail, short @ expr, short @ patt ]
          ];
          default,
          eval
      ]
  ];


(******************************************************************************)



CheckPattern // Attributes = { HoldAllComplete };
CheckPattern // Options    = {
    FailOnMessage -> True,
    "RetryCount"  -> 0,
    "RetryWait"   -> 0,
    "Quiet"       :> { }
};


iCheckPattern // Attributes = { HoldFirst };
iCheckPattern // Options    = { };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
iCheckPattern[ eval_, patt_, wrapper_, attempt_, max_, wait_, Hold[ quiet_ ] ] :=
  Module[ { result },
      result =
        Check[ Quiet[ wrapper @ AssertPattern[ Unevaluated @ eval, patt ],
                      quiet
               ]
               ,
               If[ TrueQ[ attempt < max ]
                   ,
                   Pause @ wait;
                   iCheckPattern[ eval, patt, wrapper, attempt + 1, max, wait, Hold @ quiet ]
                   ,
                   $CheckPatternFailed
               ]
        ];

      If[ attempt > 0 && ! MatchQ[ result, $CheckPatternFailed ],
          Print[ "Successfully recovered ", short @ eval, " on attempt ", attempt + 1 ];
      ];

      Throw[ result, CheckPattern ]
  ];


CheckPattern[ eval_, patt_, fail_, opts : OptionsPattern[ ] ] :=

  Module[ { wrapper, attempts, wait, quiet, result },

      wrapper = If[ TrueQ @ OptionValue @ FailOnMessage,
                    FailOnMessage,
                    Identity
                ];

      attempts = Replace[ { OptionValue @ "RetryCount" },
                          {
                              { n_Integer } :> n,
                              { ___       } :> 0
                          }
                 ];

      wait = Replace[ { OptionValue @ "RetryWait" },
                      {
                          { n : _Integer | _Real } :> n,
                          { ___                  } :> 0
                      }
             ];

      quiet = OptionValue[ CheckPattern, { opts }, "Quiet", Hold ];

      result = Catch[ Quiet[ iCheckPattern[ eval,
                                            patt,
                                            wrapper,
                                            0,
                                            attempts,
                                            wait,
                                            quiet
                             ],
                             { FailOnMessage::failed }
                      ],
                      CheckPattern
               ];

      Replace[ result,
               $CheckPatternFailed :> (
                 If[ attempts > 0,
                     Print[ "Evaluating ", short @ eval, " failed on all ", attempts + 1, " attempts." ]
                 ];
                 fail
               )
      ]
  ];




(******************************************************************************)



VerifyPattern // Attributes = { HoldAllComplete };
VerifyPattern // Options    = {
    FailOnMessage -> True,
    "RetryCount"  -> 0,
    "RetryWait"   -> 0,
    "Quiet"       :> { }
};


VerifyPattern[ eval_, patt_, opts : OptionsPattern[ ] ] :=
  Module[ { $failed },
      CheckPattern[ eval, patt, $failed, opts ] =!= $failed
  ];




(******************************************************************************)



$CatchFunction = Function[ lhs, #1 &, { HoldAllComplete } ];



CatchMine // Attributes = { };
CatchMine // Options    = { };


CatchMine /:
  HoldPattern @ SetDelayed[ f_[ args___ ], CatchMine[ rhs_ ] ] :=
    With[ { heldrhs = HoldComplete @ rhs /. HoldPattern @ Throw[ e_ ] :> Throw[ e, f ] },
      Replace[ heldrhs,
               HoldComplete[ newrhs_ ] :>
                 SetDelayed @@ HoldComplete[
                     lhs : f[ args ],
                     Catch[ newrhs, f, $CatchFunction @ lhs ]
                 ]
      ]
  ];




(******************************************************************************)



SetContext // Attributes = { HoldFirst };


SetContext[ symbol_? SymbolQ,
            context_String /; StringMatchQ[ context, __ ~~ "`" ]
] :=
  ToExpression[ context <> SymbolName @ Unevaluated @ symbol,
                InputForm,
                Function[ new,
                          Language`ExtendedDefinition[ new ] =
                            Language`ExtendedDefinition[ symbol ] /.
                              HoldPattern[ symbol ] :> new;
                          Remove @ symbol;
                          context,
                          { HoldAllComplete }
                ]
  ];



(******************************************************************************)



ShowProgress // ClearAll;
ShowProgress // Attributes = { HoldFirst };


$ShowProgressLastDisplay = Null;
$TopLevelProgressDisplays = <| |>;
$ExtraMessage = "";
$ShowingTopLevelQ = True;


displayProgress // ClearAll;
displayProgress // Attributes = { HoldFirst };

displayProgress[ $progress_, label_, Indeterminate ] /; TrueQ @ $ShowingTopLevelQ :=
  Module[ { cell },
      cell = PrintTemporary @ Panel @ Column @ Flatten @ {
          label,
          ProgressIndicator[ Dynamic @ $progress, Indeterminate ],
          Dynamic @ $ExtraMessage
      };

      $ShowProgressLastDisplay = cell;
      $TopLevelProgressDisplays[ cell ] = cell;
      cell
  ];

displayProgress[ $progress_, label_, range_ : { 0, 1 } ] /; TrueQ @ $ShowingTopLevelQ :=
  Module[ { cell },
      cell = PrintTemporary @ Panel @ Column @ Flatten @ {
          label,
          Row @ { ProgressIndicator[ Dynamic @ $progress, range ],
              Spacer @ 10,
              Dynamic @ Round[ 100*$progress ]," %"
          },
          Dynamic @ $ExtraMessage
      };

      $ShowProgressLastDisplay = cell;
      $TopLevelProgressDisplays[ cell ] = cell;
      cell
  ];

displayProgress[ _, label_, _ ] :=
  $ExtraMessage = label;



clearLastProgress // ClearAll;

safeMost[ list : { __ } ] := Most @ list;
safeMost[ ___ ] := { };

clearLastProgress[ disp_CellObject ] := (
    $ExtraMessage = "";
    NotebookDelete @ disp;
);

clearLastProgress[ _ ] :=
  $ExtraMessage = "";


evalProgress // ClearAll;
evalProgress // Attributes = { HoldFirst };
evalProgress[ eval_ ] :=
  Block[ { $ShowingTopLevelQ = False },
      Catch[ eval, _, $CaughtDuringProgress ]
  ];


returnResult // ClearAll;
returnResult[ $CaughtDuringProgress[ caught___ ] ] := Throw @ caught;
returnResult[ result_ ] := result;


showProgress // ClearAll;
showProgress // Attributes = { HoldFirst };

showProgress[ (f : Do | Table)[ expr_, { iter_, list : Except[ _? NumericQ ] } ],
              label_
] :=
  Module[ { disp, i = 0.5, $progress = 0, len = Length @ list + 1, result },
      disp = displayProgress[ $progress, label ];
      result = f[ $progress = ++i/len; expr,
                  { iter, list }
               ] // evalProgress;
      clearLastProgress @ disp;
      result
  ];

showProgress[ (f : Do | Table)[ expr_, { iter_, n__? NumericQ } ],
              label_
] :=
  showProgress[ f[ expr, { iter, Range @ n } ], label ];

showProgress[ (f : Scan | Map | MapIndexed | AssociationMap | KeyValueMap)[ g_, list_ ], label_ ] :=
  Module[ { disp, i = 0.5, $progress = 0, len = Length @ list + 1, result },
      disp = displayProgress[ $progress, label ];
      result = f[ Function[ Null, $progress = ++i/len; g @ ##, { HoldAllComplete } ],
                  list
               ] // evalProgress;
      clearLastProgress @ disp;
      result
  ];

showProgress[ { evaluations___ }, label_ ] :=
  With[ { held = List @@ HoldComplete /@ HoldComplete[ evaluations ] },
      showProgress[ ReleaseHold /@ held, label ]
  ];

showProgress[ HoldPattern @ Association[ rules : (_Rule | _RuleDelayed) ... ], label_ ] :=
  Association @ showProgress[ { rules }, label ];

showProgress[ HoldPattern @ CompoundExpression[ evaluations___ ], label_ ] :=
  Last @ showProgress[ { evaluations }, label ];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::Arguments::Module:: *)
showProgress[ HoldPattern @ Module[ vars : { ___? SymbolQ }, expr_ ], label_ ] :=
  Module[ vars, showProgress[ expr, label ] ];

showProgress[ other_, label_ ] :=
  Module[ { disp, result },
      disp = displayProgress[ Clock @ Infinity, label, Indeterminate ];
      result = other // evalProgress;
      clearLastProgress @ disp;
      result
  ];


resetProgress[ ] :=
  (
      NotebookDelete @ $ShowProgressLastDisplay;
      NotebookDelete /@ $TopLevelProgressDisplays;

      $ShowProgressLastDisplay = Null;
      $TopLevelProgressDisplays = <| |>;
      $ExtraMessage = "";
      $ShowingTopLevelQ = True;
  );



(* TODO: use Internal`GetIteratorLength *)

ShowProgress[ eval_ ] /; ! $Notebooks := eval;
ShowProgress[ eval_, label_ ] /; ! $Notebooks := ( Print @ label; eval );

ShowProgress[ eval_, label_: "Please wait..."  ] /; TrueQ @ $ShowingTopLevelQ :=
  Block[ { ShowProgress },

      ShowProgress // Attributes = { HoldFirst };
      ShowProgress[ e_, msg_, ___ ] :=
        Module[ { old, r },
            old = $ExtraMessage;
            $ExtraMessage = msg;
            r = e;
            $ExtraMessage = old;
            r
        ];

      Module[ { result },
          resetProgress[ ];
          result = showProgress[ eval, label ];
          resetProgress[ ];
          returnResult @ result
      ]
  ];


ShowProgress[ eval_, label_: "Please wait..."  ] :=
  returnResult @ showProgress[ eval, label ];


(******************************************************************************)



AddToPath::nodir = "Directory `1` not found.";


AddToPath[ dir_String /; DirectoryQ @ dir ] :=
  $Path = DeleteDuplicates @ Append[ $Path, dir ];


AddToPath[ dir_String /; ! DirectoryQ @ dir ] :=
  (
      Message[ AddToPath::nodir, dir ];
      $Failed
  );


AddToPath[ File[ dir_String ] ] :=
  AddToPath @ dir;


AddToPath[ ] :=
  AddToPath @ Directory[ ];


AddToPath[ arg_ ] :=
  (
      Message[ AddToPath::strs, 1, HoldForm @ AddToPath @ arg ];
      $Failed /; False
  );


AddToPath[ args__ ] :=
  Block[ { count },
      count = Length @ HoldComplete @ args;
      Message[ AddToPath::argx, HoldForm @ AddToPath, count ];
      $Failed /; False
  ];



(******************************************************************************)



(* WithSystemOptions:
   Allows system options to be temporarily set for a given evaluation.
   If system options were stored in a symbol, it would be similar to using
   Internal`InheritedBlock on that symbol to override some part of it. *)
WithSystemOptions // Attributes = { HoldAllComplete };


$caught // ClearAll;
$caught // Attributes = { HoldAllComplete };


(* Main definition *)
WithSystemOptions[ opts : { (_String -> _) ... }, eval_ ] :=

  Module[ { keys, orig, result },

      (* save original system options so they can be restored *)
      keys = First /@ opts;
      orig = SystemOptions[ ] ~FilterRules~ keys;

      (* make the new options take effect *)
      SetSystemOptions @ opts;

      (* be sure to catch anything that's thrown from the inner evaluation
         since the system options still need to be restored *)
      result = Catch[ eval, _, $caught ];

      (* restore the old system options *)
      SetSystemOptions @ orig;

      (* if we caught something during evaluation, make sure to throw it *)
      Replace[ result,
               $caught[ e_, t_ ] :> Throw[ e, t ]
      ]
  ];


(* This lets us pretend that we have a 'HoldRestComplete' attribute *)
WithSystemOptions[ opts_, eval_ ] :=
  With[ { o = opts },
      WithSystemOptions[ o, eval ] /; HoldComplete @ o =!= HoldComplete @ opts
  ];



(******************************************************************************)



(* StrictLexicalScoping:
   This is used to avoid these kinds of issues:
   http://mathematica.stackexchange.com/q/91917/9008 *)
StrictLexicalScoping // Attributes = { HoldAllComplete };


StrictLexicalScoping[ eval_ ] :=
  WithSystemOptions[ { "StrictLexicalScoping" -> True }, eval ];



(******************************************************************************)



(* SymbolHash:
   Computes a hash on the definition of a symbol. Useful for determining if
   a symbol has been changed *)
SymbolHash // Attributes = { HoldAllComplete };
SymbolHash // Options    = { FullDefinition -> False };


SymbolHash[ f_? SymbolQ, opts : OptionsPattern[ ] ] :=

  Module[ { definitionFunction, definition, normalized },

      SymbolQ; (* inline LHS dependency *)

      (* select the appropriate definition function (if not using the
         full definition, changes to the definitions of any dependent
         symbols will not be reflected in the hash) *)
      definitionFunction = If[ OptionValue @ FullDefinition,
                               MinimalFullDefinition,
                               Language`ExtendedDefinition
                           ];

      definition = definitionFunction @ f;

      (* rename local symbols to canonical names *)
      normalized =
        (*CodeEquivalenceUtilities`CanonicalForms`Scope`CanonicalTransformFromScope @*)
          definition;

      (* normalization allows expressions like Table[ i, { i, 5 } ]
         and Table[ j, { j, 5 } ] to be treated the same *)
      Hash @ normalized
  ];



(******************************************************************************)



(* EvaluationReportData:
   Allows you to inspect how some subexpressions evaluate in single steps.
   Helper function for EvaluationReport *)
EvaluationReportData // Attributes = { HoldAllComplete };


EvaluationReportData[ eval_ ] :=

  Module[ { traceList, $bag },

      (* get a good old fashioned trace of eval *)
      traceList = Trace[ eval,
                         TraceOriginal -> True,
                         TraceAbove    -> True,
                         TraceBackward -> True,
                         TraceForward  -> True
                  ] /. HoldForm -> TempHold;

      (* we'll be finding evaluation pairs and sticking them in here *)
      $bag = Internal`Bag[ ];

      traceList //.
        {
            (* this indicates that the expression 'b' evaluated to 'c' *)
            { a___, TempHold[ b_ ], TempHold[ c_ ], d___ } :>
              TrEval[ (* save this information and then remove b *)
                      Internal`StuffBag[ $bag,
                                         EvaluatedTo[ HoldForm @ b,
                                                      HoldForm @ c
                                         ]
                      ];
                      (* keep 'c' in the list, since it may be paired
                         with the first item in 'd' *)
                      { a, TempHold @ c, d }
              ]
            ,
            (* once pairs have been saved and removed, there will be a
               single item remaining, which is now safe to remove *)
            { a___, { _TempHold }, b___ } :> { a, b }
        };

      (* dump out the bag of evaluation pairs *)
      Internal`BagPart[ $bag, All ]
  ];



(* EvaluationReport:
   Allows you to inspect how some subexpressions evaluate in single steps.
   Runs EvaluationReportData and then formats the results in a grid. *)
EvaluationReport // Attributes = { HoldAllComplete };


EvaluationReport[ eval_ ] :=
  Grid[ Replace[ DeleteDuplicates @ EvaluationReportData @ eval,
                 {
                     (* these are the idempotent subexpressions *)
                     EvaluatedTo[ a_, a_ ] :>
                       { a, Style[ "===", Orange ], a },

                     (* and these produced new results during evaluation *)
                     EvaluatedTo[ a_, b_ ] :>
                       { a, Style[ "\[DoubleLongRightArrow]", Darker @ Green ], b }
                 },
                 { 1 }
        ],
        Alignment -> Left,
        Dividers  -> All
  ];



(******************************************************************************)



HashString // Attributes = { };
HashString // Options    = { };


HashString[ expr_ ] :=
  StringPadLeft[ IntegerString[ Hash @ Unevaluated @ expr, 16 ],
                 16,
                 "0"
  ];



(******************************************************************************)



HashStringQ // Attributes = { };
HashStringQ // Options    = { };


$hashPattern = Repeated[ Alternatives @@ Join[ CharacterRange[ "a", "f" ],
                                               CharacterRange[ "0", "9" ]
                                         ],
                         { 16 }
               ];


HashStringQ[ hash_String ] :=
  Inline[ $hashPattern,
          StringQ @ hash && StringMatchQ[ hash, $hashPattern ]
  ];


HashStringQ[ ___ ] :=
  False;



(******************************************************************************)



HashPath // Attributes = { };
HashPath // Options    = { };


HashPath[ hash_String /; HashStringQ @ Unevaluated @ hash ] :=
  Module[ { dir },
      HashStringQ;
      dir = StringPartition[ StringTake[ hash, 4 ], 2 ];
      StringJoin @ Riffle[ Flatten @ { dir, hash }, "/" ]
  ];


HashPath[ expr_ ] :=
  HashPath @ HashString @ Unevaluated @ expr;



(******************************************************************************)



CloudHashPut // Attributes = { };
CloudHashPut // Options    = { };


cloudHashPut[ expr_, root_ ] :=
  CheckPattern[ With[ { c = CompressWithContext @ Unevaluated @ expr },
                      CloudPut[ Unevaluated @ Uncompress @ c,
                                FileNameJoin @ { root, HashPath @ Unevaluated @ expr }
                      ]
                ],
                _CloudObject,
                $Failed,
                "RetryCount" -> 1
  ];


CloudHashPut[ expr_, root_ ] :=
  OnceUnlessFailed @ cloudHashPut[ Unevaluated @ expr, root ];

CloudHashPut[ expr_ ] :=
  CloudHashPut[ Unevaluated @ expr, CloudObject[ "ref" ] ];



(******************************************************************************)



$symbolInitConfig = {
    <| "Head" -> Association,   "InitializedQ" -> AssociationQ          |>,
    <| "Head" -> ByteArray,     "InitializedQ" -> ByteArrayQ            |>,
    <| "Head" -> Graph,         "InitializedQ" -> GraphQ                |>,
    <| "Head" -> Image,         "InitializedQ" -> ImageQ                |>,
    <| "Head" -> RawArray,      "InitializedQ" -> Developer`RawArrayQ   |>,
    <| "Head" -> Symbol,        "InitializedQ" -> SymbolQ               |>,
    <| "Head" -> Dispatch,      "InitializedQ" -> DispatchQ             |>
};



Wolfram`CodeEquivalenceUtilities`$Initialized   // Attributes = { HoldAllComplete };
Wolfram`CodeEquivalenceUtilities`$Uninitialized // Attributes = { HoldAllComplete };



reinitializeExpression // ClearAll;

reinitializeExpression[ Wolfram`CodeEquivalenceUtilities`$Initialized[ head_, args___ ] ] :=

  Module[ { symbols },

      symbols = Flatten[
          HoldComplete @@ DeleteDuplicates @
            Cases[ HoldComplete @ args,
                   s_? UnlockedSymbolQ :> HoldComplete @ s,
                   Infinity,
                   Heads -> True
            ]
      ];

      Replace[ symbols,
               HoldComplete[ syms___ ] :> Block[ { syms }, head @ args ]
      ]
  ];



makeRules // ClearAll;

makeRules[ KeyValuePattern @ { "Head" -> head_, "InitializedQ" -> validQ_ } ] :=
  Sequence[
      expr : HoldPattern @ head[ args___ ] /; validQ @ Unevaluated @ expr :>
        RuleCondition @ Wolfram`CodeEquivalenceUtilities`$Initialized[ head, args ]
      ,
      expr : HoldPattern @ head[ args___ ] /; ! validQ @ Unevaluated @ expr :>
        RuleCondition @ Wolfram`CodeEquivalenceUtilities`$Uninitialized[ head, args ]
  ];


$extraRules // ClearAll;

$extraRules :=
  $extraRules = {
      b_ByteArray /; ByteArrayQ @ Unevaluated @ b :>
        With[ { n = Normal @ b },
              Wolfram`CodeEquivalenceUtilities`$Initialized[ ByteArray, n ] /; True
        ],
      d_Dispatch /; DispatchQ @ Unevaluated @ d :>
        With[ { n = Normal @ d },
            Wolfram`CodeEquivalenceUtilities`$Initialized[ Dispatch, n ] /; True
        ]
  };



$initializationRules // ClearAll;

$initializationRules :=
  $initializationRules =
    Join[ makeRules /@ $symbolInitConfig, $extraRules ];



markInitialization // ClearAll;

markInitialization[ expr_ ] :=
  expr //. $initializationRules;



restoreInitializedExpressions // ClearAll;

restoreInitializedExpressions[ expr_ ] :=
  CheckPattern[ expr //. {
                  e_Wolfram`CodeEquivalenceUtilities`$Initialized :> RuleCondition @ reinitializeExpression @ e,
                  Wolfram`CodeEquivalenceUtilities`$Uninitialized[ head_, data___ ] :> head @ data
                },
                _HoldComplete? (FreeQ[ Wolfram`CodeEquivalenceUtilities`$Initialized | Wolfram`CodeEquivalenceUtilities`$Uninitialized ]),
                $Failed
  ];



serialize // ClearAll;
serialize // Attributes = { };
serialize // Options    = Options @ BinarySerialize;


serialize[ expr_, "ByteArray", opts : OptionsPattern[ ] ] :=

  Module[ { marked, hash, validated },

      marked = markInitialization @ HoldComplete @ expr;
      hash = HashString @ HoldComplete @ expr;

      validated = CheckPattern[ <| "Hash" -> hash, "Expression" -> marked |>,
                                KeyValuePattern @ { "Hash" -> _? HashStringQ, "Expression" -> _HoldComplete },
                                Throw[ $Failed, SafeSerialize ]
                  ];

      Block[ { $Context = "Global`", $ContextPath = { "System`", "Global`" } },
          BinarySerialize[ validated, opts ]
      ]
  ];


serialize[ expr_, "String", opts : OptionsPattern[ ] ] :=
  Module[ { bin },
      bin = serialize[ Unevaluated @ expr, "ByteArray", opts ];
      FromCharacterCode @ Normal @ bin
  ];


serialize[ expr_, "Base64", opts : OptionsPattern[ ] ] :=
  Module[ { bin },
      bin = serialize[ Unevaluated @ expr, "ByteArray", opts ];
      Developer`EncodeBase64 @ bin
  ];



deserialize // ClearAll;
deserialize // Attributes = { };
deserialize // Options    = Options @ BinaryDeserialize;


deserialize[ bytes_ByteArray, wrapper : Except @ OptionsPattern[ ] : Identity, opts : OptionsPattern[ ] ] :=

  Module[ { deserialized, restored, expectedHash, observedHash },

      deserialized = CheckPattern[ BinaryDeserialize[ bytes, opts ],
                                   KeyValuePattern @ { "Hash" -> _? HashStringQ, "Expression" -> _HoldComplete },
                                   Throw[ $Failed, SafeDeserialize ]
                     ];

      restored = restoreInitializedExpressions @ deserialized[ "Expression" ];

      expectedHash = deserialized[ "Hash" ];
      observedHash = HashString @ restored;

      If[ expectedHash =!= observedHash,
          Message[ SafeDeserialize::hashfail, expectedHash, observedHash ];
      ];

      wrapper @@ restored
  ];


deserialize[ string_String, args___ ] :=

  Module[ { bytes },

      bytes = If[ Developer`Base64StringQ @ string,
                  Developer`DecodeBase64ToByteArray @ string,
                  ByteArray @ ToCharacterCode @ string
              ];

      deserialize[ bytes, args ]
  ];



SafeSerialize // Attributes = { SequenceHold };
SafeSerialize // Options    = Options @ serialize;


SafeSerialize[ expr_, fmt : Except @ OptionsPattern[ ] : "ByteArray", opts : OptionsPattern[ ] ] :=
  CheckPattern[ Catch[ serialize[ Unevaluated @ expr, fmt, opts ],
                       SafeSerialize
                ],
                _ByteArray? ByteArrayQ | _String? StringQ,
                $Failed
  ];



SafeDeserialize // Attributes = { };
SafeDeserialize // Options    = Options @ deserialize;
SafeDeserialize::hashfail = "Warning: expected hash `1` but found `2`.";


SafeDeserialize[ expr_, wrapper : Except @ OptionsPattern[ ] : (## &), opts : OptionsPattern[ ] ] :=
  Catch[ deserialize[ Unevaluated @ expr, wrapper, opts ],
         SafeDeserialize
  ];



(******************************************************************************)



InertTraceQ // Attributes = { HoldAllComplete, SequenceHold };
InertTraceQ // Options    = { "Debug" -> False };



allowed // ClearAll;
allowed // Attributes = { HoldAllComplete };


allowed[ head_[ args___ ] ] :=
  Alternatives[ HoldForm @ head @ args,
                HoldForm @ head,
                ReleaseHold[ HoldForm /@ HoldComplete @ args ]
  ];


allowed[ s_? SymbolQ ] :=
  Alternatives @ HoldForm @ s;


allowed[ args___ ] :=
  Throw[ False, InertTraceQ ];


$overrides = Alternatives @@ HoldForm /@ HoldComplete[
    _Quit,
    _Abort,
    _Print
];


inertTraceQ // ClearAll;
inertTraceQ // Attributes = { HoldAllComplete, SequenceHold };
inertTraceQ // Options    = Options @ InertTraceQ;


inertTraceQ[ HoldPattern @ Sequence[ args___ ], opts : OptionsPattern[ ] ] :=
  inertTraceQ[ { args }, opts ];


inertTraceQ[ expr_, opts : OptionsPattern[ ] ] :=

  Module[ { disallowedQ, stopEvaluating },

      disallowedQ =
        With[ { o = $overrides, a = allowed @ expr },
            MatchQ[ #, o ] || ! MatchQ[ #, a ] &
        ];

      stopEvaluating =
        With[ { d = disallowedQ },
            If[ d @ #,
                If[ OptionValue @ "Debug",
                    Print[ "Not allowed: ", # ]
                ];
                Throw[ False, InertTraceQ ]
            ] &
        ];

      TraceScan[ stopEvaluating, expr ];

      True
  ];



InertTraceQ[ expr_, opts : OptionsPattern[ ] ] :=
  Catch[ inertTraceQ[ expr, opts ], InertTraceQ ];



(******************************************************************************)



CloudObjectQ // Attributes = { };
CloudObjectQ // Options    = { };


CloudObjectQ[ co : CloudObject[ _String ] ] := FileExistsQ @ co;
CloudObjectQ[ ___                         ] := False;



(******************************************************************************)



CloudObjectExistsQ // Attributes = { };
CloudObjectExistsQ // Options    = { };


CloudObjectExistsQ[ co_CloudObject ] := CloudObjectQ @ co;
CloudObjectExistsQ[ s_String       ] := CloudObjectQ @ CloudObject @ s;
CloudObjectExistsQ[ ___            ] := False;



(******************************************************************************)



CloudFileType // Attributes = { };
CloudFileType // Options    = { };
CloudFileType::arg = "Invalid arguments: `1`";


CloudFileType[ file_String ] /; TrueQ @ $CloudEvaluation :=
  <|
      "ObjectType" -> If[ CloudObjectExistsQ @ file, CloudObject, None ],
      "FileType"   -> FileType @ file
  |>;


CloudFileType[ file_String ] /; ! TrueQ @ $CloudEvaluation :=
  CloudEvaluate @ CloudFileType @ file;


CloudFileType[ obj_CloudObject ] :=
  <|
      "ObjectType" -> CloudObject,
      "FileType"   -> FileType @ obj
  |>;


CloudFileType[ a___ ] :=
  (
      Message[ CloudFileType::arg, HoldForm @ a ];
      $Failed
  );



(******************************************************************************)



CloudFiles // Attributes = { };
CloudFiles // Options    = { };


CloudFiles[ args___ ] :=
  CloudEvaluate @
    GroupBy[ FileNames @ args,
             {
                 If[ CloudObjectExistsQ @ #, "CloudObjects", "Local" ] &,
                 ToString @* FileType
             }
    ];



(******************************************************************************)



ViewUsagePatterns // Attributes = { HoldAllComplete };
ViewUsagePatterns // Options    = { };


dvPatternCells // ClearAll;
dvPatternCells // Attributes = { HoldAllComplete };

dvPatternCells[ sym_ ] := MakeInputCell @@@ First /@ DownValues @ sym;


renameDVPatternCells // ClearAll;

renameDVPatternCells[ cells_ ] :=
  cells /.
    s_String :> StringReplace[ s,
                               Longest[ __ ~~ "`" ~~ name__ ~~ "_" ~~ patt___ ] :>
                                 name <> "_" <> patt
                ] /.
                  RowBox @ { s_String, ":", patt___ } :>
                    RowBox @ { StringReplace[ s, Longest[ __ ~~ "`" ~~ name__ ] :> name ], ":", patt };


toLHS // ClearAll;

toLHS[ cells_ ] :=
  Replace[ cells,
           Cell[ BoxData[ r_RowBox ], a___ ] :> Cell[ BoxData @ RowBox @ { r, ":=" }, a ],
           { 1 }
  ];


makeDVPatternNotebook // ClearAll;
makeDVPatternNotebook // Attributes = { HoldAllComplete };

makeDVPatternNotebook[ sym_ ] :=
  Module[ { cells, renamed, lhsCells, optCells },
      cells = dvPatternCells @ sym;
      renamed = renameDVPatternCells @ cells;
      lhsCells = toLHS @ renamed;

      optCells = Replace[ MakeInputCell /@ Options @ sym, {
                            { c__Cell } :> { Cell[ "Options", "Subsubsection" ], c },
                            ___ :> { }
                          }
                 ];

      Notebook[ lhsCells ~Join~ optCells,
                WindowSize -> { Automatic, All },
                WindowElements -> { "VerticalScrollBar" },
                WindowTitle -> FullSymbolName @ sym
      ] /. { "->" -> "\[Rule]", ":>" -> "\[RuleDelayed]" }
  ];



ViewUsagePatterns[ sym_? SymbolQ ] :=
  NotebookPut @ makeDVPatternNotebook @ sym;



(******************************************************************************)



CompressedStringQ[ str_String? StringQ ] :=
  StringMatchQ[ str, "1:eJ" ~~ __ ];


CompressedStringQ[ ___ ] :=
  False;



(******************************************************************************)



UnsetAllOnce[ ] :=
  Quiet[
      With[ { objects = PersistentObjects[ "Hashes/Once/*" ] },
          Length @ Map[ DeleteObject, objects ]
      ],
      { CloudObject::cloudnf, CloudObjectInformation::cloudnf }
  ];



(******************************************************************************)



UUIDStringQ[ str_String ] :=
  StringMatchQ[ str,
                RegularExpression @
                  "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"
  ];


UUIDStringQ[ ___ ] :=
  False;



(******************************************************************************)



CompressCloudObject[ obj_String ] :=
  CompressCloudObject @ CloudObject @ obj;



CompressCloudObject[ obj_CloudObject ] :=

  CatchMine @ Module[ { info, path, size, temp, data, newSize },

      If[ ! FileExistsQ @ obj, Throw @ $Failed ];

      info = First @ CloudObjectInformation @ obj;

      If[ ! StringMatchQ[ info[ "MIMEType" ],
                          "application/vnd.wolfram.expression",
                          IgnoreCase -> True
            ],
          Throw @ $Failed
      ];

      path = StringDelete[ info[ "Path" ],
                           StartOfString ~~ Longest[ Except[ "/" ] ... ] ~~ "/"
             ];

      size = info[ "FileByteCount" ];

      If[ size === 0 || StringMatchQ[ CloudEvaluate @ ReadLine @ path,
                                      "Uncompress[" ~~ __
                        ],
          Throw @
            <|
                "File" -> obj,
                "OriginalSize" -> size,
                "NewSize" -> size,
                "Ratio" -> N @ 1
            |>
      ];


      temp = CheckPattern[ CopyFile[ obj, CloudObject[ ] ],
                           _CloudObject,
                           Throw @ $Failed
             ];

      data = CheckPattern[ Replace[ ReadList @ obj, { e_ } :> e ],
                           _,
                           DeleteFile @ temp;
                           Throw @ $Failed
             ];

      With[ { compressed = CompressWithContext @ data },
          CheckPattern[ CloudPut[ Unevaluated @ Uncompress @ compressed, obj ],
                        obj,
                        CopyFile[ temp, obj ];
                        DeleteFile @ temp;
                        Throw @ $Failed
          ]
      ];

      DeleteFile @ temp;
      newSize = FileByteCount @ obj;

      <|
          "File" -> obj,
          "OriginalSize" -> size,
          "NewSize" -> newSize,
          "Ratio" -> N[ newSize / size ]
      |>
  ];



(******************************************************************************)


CreateTaskFile // ClearAll;
CreateTaskFile // Attributes = { HoldFirst };
CreateTaskFile // Options = { "AutoRemove" -> False };


CreateTaskFile[ evaluation_, opts : OptionsPattern[ ] ] :=
  With[ { obj = FileNameJoin @ { CloudObject[ "Tasks" ], AbsoluteTimeString[ ] <> "_" <> CreateUUID[ ] } },
      CreateTaskFile[ evaluation, obj, opts ]
  ];


CreateTaskFile[ evaluation_, path_String, opts : OptionsPattern[ ] ] :=
  With[ { obj = CloudObject @ path },
      CreateTaskFile[ evaluation, obj, opts ]
  ];


CreateTaskFile[ evaluation_, taskObject_CloudObject, opts : OptionsPattern[ ] ] :=

  Module[ { removeQ, task, bytes },

      removeQ = OptionValue @ "AutoRemove";

      task = With[ { r = TrueQ @ removeQ },
          MakeSelfExtractingExpression[ With[ { result = evaluation },
                                              If[ r, DeleteFile @ taskObject ];
                                              result
                                        ],
                                        HoldComplete
          ]
      ];

      bytes = Replace[ task, HoldComplete @ BinaryDeserialize[ b_ ] :> b ];

      With[ { b = bytes },
          CloudPut[ Unevaluated @ BinaryDeserialize @ b,
                    taskObject
          ]
      ]
  ];



(******************************************************************************)



MessageOnChanges // ClearAll;
MessageOnChanges // Attributes = { HoldAllComplete };
MessageOnChanges::changed = "The value for `1` has changed.";

MessageOnChanges[ s_? SymbolQ ] := (
    s /: HoldPattern[ (set : Set | SetDelayed)[ s, value___ ] ] /; ! TrueQ @ $Alerted @ HoldComplete @ s :=
      Block[ { $Alerted },
          Message[ MessageOnChanges::changed, HoldForm @ s ];
          $Alerted @ HoldComplete @ s = True;
          set[ s, value ]
      ];

    s /: HoldPattern[ (set : Set | SetDelayed)[ s[ p___ ], value___ ] ] /;
        ! TrueQ @ $Alerted @ HoldComplete @ s :=
      Block[ { $Alerted },
          Message[ MessageOnChanges::changed, HoldForm @ s ];
          $Alerted @ HoldComplete @ s = True;
          set[ s @ p, value ]
      ];
);



(******************************************************************************)


printHandler // ClearAll;

printHandler[ txt_String /; StringMatchQ[ txt, "Wolfram`CodeEquivalenceUtilities`Dump`PrintMe" ~~ __ ] ] :=
  ToExpression[ txt, InputForm, printHandler ];

printHandler[ Wolfram`CodeEquivalenceUtilities`Dump`$PrintMe[ args___ ] ] := args;
printHandler[ args___ ] := args;


EvaluateInFreshKernel // ClearAll;
EvaluateInFreshKernel // Attributes = { HoldFirst };
EvaluateInFreshKernel // Options    =
  {
      TimeConstraint        -> Infinity,
      Print                 -> Print,
      IncludeDefinitions    -> False,
      UsingFrontEnd         -> False,
      CloudConnect          -> False,
      "PassMessages"        -> True
  };

EvaluateInFreshKernel::timeout = "Timed out while evaluating `1`.";
EvaluateInFreshKernel::cloudconnect = "Failed to cloud connect.";
EvaluateInFreshKernel::wrongid = "Subkernel cloud connected as `1` but expected `2`.";


iEvaluateInFreshKernel // ClearAll;
iEvaluateInFreshKernel // Attributes = { HoldFirst };
iEvaluateInFreshKernel // Options    = Options @ EvaluateInFreshKernel;


iEvaluateInFreshKernel[ eval_, wrapper : Except[ _Rule | _RuleDelayed ], opts : OptionsPattern[ ] ] :=
  TimeConstrained[
      Module[ { link, sendExpr, result },

          link = Replace[ LinkLaunch[ First @ $CommandLine <> " -noicon" ],
                          Except[ _LinkObject ] :> Throw[ $Failed, EvaluateInFreshKernel ]
                 ];

          sendExpr = HoldComplete[

              SetOptions[ "stdout", FormatType -> InputForm ];

              Internal`InheritedBlock[ { Print },

                  Unprotect @ Print;
                  Print[ msg : Except[ _Wolfram`CodeEquivalenceUtilities`Dump`$PrintMe ], a___ ] :=
                    Print @ Wolfram`CodeEquivalenceUtilities`Dump`$PrintMe[ msg, a ];
                  Protect @ Print;

                  eval
              ]
          ];

          Replace[ sendExpr,
                   HoldComplete[ e_ ] :> LinkWrite[ link, Unevaluated[ e ] ] // WithContext
          ];

          result = Catch[
              While[ True,
                     Replace[ LinkRead[ link, HoldComplete ],
                              {
                                  HoldComplete @ TextPacket[ txt_String ] :> OptionValue[ Print ] @ printHandler @ txt,
                                  HoldComplete @ MessagePacket[ msg_Message ] :> msg,
                                  HoldComplete @ ReturnPacket[ expr_ ] :> Throw[ wrapper @ expr, iEvaluateInFreshKernel ]
                              }
                     ]
              ],
              iEvaluateInFreshKernel
          ];

          LinkClose @ link;
          result
      ]
      ,
      OptionValue @ TimeConstraint
      ,
      Message[ EvaluateInFreshKernel::timeout, HoldForm @ eval ];
      $Failed
  ];


evalWithFE // ClearAll;
evalWithFE // Attributes = { HoldAllComplete };

evalWithFE[ eval : Except[ _HoldComplete ] ] := evalWithFE @ HoldComplete @ eval;

evalWithFE[ HoldComplete[ eval_ ] ] := HoldComplete @ UsingFrontEnd @ eval;


evalWithCC // ClearAll;
evalWithCC // Attributes = { HoldAllComplete };

evalWithCC[ eval : Except[ _HoldComplete ] ] := evalWithCC @ HoldComplete @ eval;

evalWithCC[ HoldComplete[ eval_ ] ] :=

  Module[ { id },

      id = Replace[ Replace[ $WolframID, Except[ _String ] :> CloudConnect[ ] ],
                    Except[ _String ] :> (Message[ EvaluateInFreshKernel::cloudconnect ];
                                          Throw[ $Failed, EvaluateInFreshKernel ])
           ];


      With[ { id$ = id, name = FullSymbolName @ EvaluateInFreshKernel },
          HoldComplete @
            Replace[ Replace[ $WolframID, None :> CloudConnect[ ] ],
                     {
                         id$ :> eval,
                         other___ :>
                           LinkWrite[ $ParentLink,
                                      Unevaluated[
                                          ToExpression[ name,
                                                        InputForm,
                                                        Function[ s,
                                                                  Message[ s::wrongid,
                                                                           HoldForm @ other,
                                                                           HoldForm @ id$
                                                                  ],
                                                                  { HoldAllComplete }
                                                        ]
                                          ];
                                          $Failed
                                      ]
                           ]
                     }
            ]
      ]
  ];


evalWithDefs // ClearAll;
evalWithDefs // Attributes = { HoldAllComplete };

evalWithDefs[ eval : Except[ _HoldComplete ] ] := evalWithDefs @ HoldComplete @ eval;

evalWithDefs[ HoldComplete[ eval_ ] ] :=
  Replace[ MakeSelfExtractingExpression[ eval, HoldComplete ],
           HoldComplete[ e_ ] :> HoldComplete @ e
  ];



evalWithMsgs // ClearAll;
evalWithMsgs // Attributes = { HoldAllComplete };

evalWithMsgs[ eval : Except[ _HoldComplete ] ] := evalWithMsgs @ HoldComplete @ eval;

evalWithMsgs[ HoldComplete[ eval_ ] ] :=
  HoldComplete @
    Block[ { $msgHandler },
        $msgHandler[ Hold[ msg_, True ] ] := LinkWrite[ $ParentLink, Unevaluated @ MessagePacket @ msg ];
        Internal`HandlerBlock[ { "Message", $msgHandler[ ## ] & }, eval ]
    ];



EvaluateInFreshKernel[ eval_, wrapper : Except[ _Rule | _RuleDelayed ], opts : OptionsPattern[ ] ] :=

  CatchMine @ Module[ { held, getEvaluator, evaluators, prepped },

      held = HoldComplete @ eval;
      getEvaluator = If[ TrueQ @ OptionValue @ #1, #2, Identity ] &;

      evaluators = getEvaluator @@@ {
          { UsingFrontEnd     , evalWithFE   },
          { CloudConnect      , evalWithCC   },
          { IncludeDefinitions, evalWithDefs },
          { "PassMessages"    , evalWithMsgs }
      };

      prepped = Fold[ #2 @ #1 &, held, evaluators ];

      Replace[ prepped,
          HoldComplete[ p_ ] :> iEvaluateInFreshKernel[ p, wrapper, opts ]
      ]
  ];


EvaluateInFreshKernel[ eval_, opts : OptionsPattern[ ] ] :=
  EvaluateInFreshKernel[ eval, Identity, opts ];



(******************************************************************************)




SequenceDiff[seq1_, seq2_] :=
  Row@Replace[
      SequenceAlignment[seq1, seq2], {a_, b_} :>
        Style[Row[{Style[a, Red], Style[b, Darker@Green]}]], {1}];



(******************************************************************************)


End[ ]; (* `Private` *)

(* :!CodeAnalysis::EndBlock:: *)

EndPackage[ ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
