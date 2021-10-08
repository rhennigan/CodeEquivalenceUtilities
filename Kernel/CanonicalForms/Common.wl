Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


BeginPackage[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`"
    }
];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)

Canonical;
StripCanonical;
CodeTransformerObject       ::usage = "";
MakeTransformationRules     ::usage = "";
MakeCodeTransformerObject   ::usage = "";
CompactDefinition           ::usage = "";
PackDefinition              ::usage = "";
UnpackDefinition            ::usage = "";
FindDependencies            ::usage = "";
RandomValue;


Begin[ "`Private`" ];


(******************************************************************************)

Canonical // ClearAll;
Canonical // Attributes = { HoldAllComplete };

(******************************************************************************)

StripCanonical[ expression_ ] :=
  StripHead[ expression, Canonical ];

(******************************************************************************)



RandomValue /:
  HoldPattern[ Positive @ RandomValue @
    DiscreteUniformDistribution[ { _? Positive, _ } ]
  ] :=
    True;



(******************************************************************************)



trArgPattQ // ClearAll;


trArgPattQ = Function[ Null,

    Switch[ Unevaluated @ #,
      _Rule         , True,
      _RuleDelayed  , True,
      _List         , AllTrue[ Unevaluated @ #, trArgPattQ ],
      _Hold         , AllTrue[ #, trArgPattQ ],
      _HoldComplete , AllTrue[ #, trArgPattQ ],
      _TempHold     , AllTrue[ #, trArgPattQ ],

      _Symbol       , With[ { sval = HoldComplete @ # /. OwnValues @ # },
                        sval =!= HoldComplete @ # && trArgPattQ @ sval ],

      _             , False
    ],

    { HoldAllComplete }
];



(******************************************************************************)



$nohp // ClearAll;


$nohp = Except[ Verbatim[ HoldPattern ][ _ ] ];



MakeTransformationRules // Attributes = { HoldAllComplete };
MakeTransformationRules // Options    = { };


MakeTransformationRules::trform =
  "MakeTransformationRules argument `1` at position `2` does not have the
   correct form for a transformation rule.";


MakeTransformationRules[ ( Rule | RuleDelayed )[ p: $nohp, exp_ ] ] :=
  Inline[ $nohp, MakeTransformationRules[ HoldPattern @ p :> exp ] ];


MakeTransformationRules[
    RuleDelayed[ p: $nohp, With[ vars_, exp: Except[ _Condition ] ] ]
] :=
  Inline[ $nohp, HoldPattern @ p :> TrEval @ With[ vars, HoldApply @ exp ] ];


MakeTransformationRules[ p: $nohp :> eval_PartialEvaluation ] :=
  Inline[ $nohp, HoldPattern @ p :> TrEval @ eval ];


MakeTransformationRules[ ( Rule | RuleDelayed )[ pattern_, exp_ ] ] :=
  pattern :> exp;


MakeTransformationRules[ ( _[ args___ ] ) ? trArgPattQ ] :=
  MakeTransformationRules /@ Unevaluated @ { args } // Flatten;


MakeTransformationRules[ sym_Symbol ] :=
  With[ { symval = HoldComplete @ sym /. OwnValues @ Unevaluated @ sym },
      MakeTransformationRules @ symval /; symval =!= HoldComplete @ sym
  ];


DefineError[

    MakeTransformationRules[ arg_ ] :>
      With[ { pos = Position[ arg, a_ /; ! trArgPattQ @ a, { 1 },
          Heads -> False ] },
          Message[ MakeTransformationRules::trform, ## ] & @@@
            Thread @ { Extract[ arg, pos, HoldForm ], pos }
      ]
    ,
    MakeTransformationRules[ a_, args__ ] :>
      Message[ MakeTransformationRules::argx,
          MakeTransformationRules,
          Length @ Unevaluated @ { a, args }
      ]
    ,
    MakeTransformationRules[ ] :>
      Message[ MakeTransformationRules::argx,
          MakeTransformationRules,
          0
      ]

];



(******************************************************************************)



makeEdges // ClearAll;
makeEdges // Attributes = { };
makeEdges // Options    = { };

makeEdges[ HoldForm[ lhs_ :> rhs_ ] ] :=

  Module[ { patt, exp, left, right },

      patt  = HoldForm @ lhs;
      exp   = HoldForm @ rhs;
      left  = patt /. Verbatim[ Pattern ][ s_, p_ ] :> p;

      right = exp /. Cases[
          patt,
          Verbatim[ Pattern ][ s_, p_ ] :> HoldPattern @ s :> p,
          Infinity
      ];

      { patt -> left, left -> exp, exp -> right }
  ];



(******************************************************************************)



CodeTransformerObject // Attributes = { };
CodeTransformerObject // Options    = { };


CodeTransformerObject[ obj_Association ][ key_String ] := obj[ key ];


CodeTransformerObject[ obj_Association ][ expression : Except[ _String ] ] :=
  NormalizeNames[
      expression //. obj[ "Dispatch" ]
      ,
      "ExcludedContexts" -> {
          "System`",
          Except[ $LocalContext,
                  s_String /; StringMatchQ[ s, "Wolfram`CodeEquivalenceUtilities`" ~~ ___ ]
          ]
      }
      ,
      "RemoveTypes" -> False
  ];



(******************************************************************************)



MakeCodeTransformerObject // Attributes = { };
MakeCodeTransformerObject // Options    = { };


MakeCodeTransformerObject[ rules_ ] :=
  Module[
      {
          dispatch, ruleList, count, function, edgeList,
          graph, icon, equivalenceClassCount
      },

      dispatch = MakeTransformationRules @@ {
          Append[ rules, HoldApply[ f_, { v___ } ] :> f @ v ]
      } // Dispatch;

      ruleList = Sort @ { ReleaseHold[
          HoldForm /@ Flatten[ HoldComplete @@ rules ]
      ] };

      count = Length @ ruleList;

      function = Inline[ dispatch, (# //. dispatch &) ];

      graph = Graph[ Cases[ruleList,
          HoldForm[RuleDelayed[a_, b_]] :>
            NormalizeNames[HoldComplete[a] -> HoldComplete[b],
                "ExcludedContexts" -> {"System`"}]], VertexLabels -> None];

      icon = Framed[ Show[ graph, ImageSize -> { 39, 39 } ],
                     ImageSize -> { 39, 39 },
                     Background -> White,
                     FrameMargins -> 1,
                     FrameStyle -> GrayLevel @ .75
             ] /. Tooltip[ g_, _ ] :> g;

      equivalenceClassCount = Length @ WeaklyConnectedComponents @ graph;

      CodeTransformerObject @ <|
          "Count"               -> count,
          "Dispatch"            -> dispatch,
          "EquivalenceClasses"  -> equivalenceClassCount,
          "Function"            -> function,
          "Graph"               -> graph,
          "GraphIcon"           -> icon,
          "Rules"               -> ruleList,

          "Properties"          -> {
                                        "Count",
                                        "Dispatch",
                                        "EquivalenceClasses",
                                        "Function",
                                        "Graph",
                                        "GraphIcon",
                                        "Rules"
                                   }
      |>
  ];



(******************************************************************************)



rawCompress // ClearAll;
rawCompress // Attributes = { HoldAllComplete };
rawCompress // Options    = { };


rawCompress[ ] := ByteArray[ { } ];


rawCompress[ expr_ ] :=
  ByteArray @ Developer`RawCompress @
      ToCharacterCode[ ToFullString @ expr, "ASCII" ];


rawCompress[ expr___ ] :=
    rawCompress @ Sequence @ expr;



rawUncompress // ClearAll;
rawUncompress // Attributes = { };
rawUncompress // Options    = { };


rawUncompress[ b_ByteArray, wrapper_: HoldComplete ] :=
    Module[ { rawBinary, string },
      rawBinary = Developer`RawUncompress @ Normal @ b;
      string = FromCharacterCode[ rawBinary, "ASCII" ];
      ToExpression[ string, InputForm, wrapper]
    ];



symbolBaseName // ClearAll;
symbolBaseName // Attributes = { HoldAllComplete };
symbolBaseName // Options    = { };


symbolBaseName[ sym_String ] :=
    StringReplace[ sym,
                   StringExpression[
                     StartOfString,
                     first_,
                     name : Except @ ("$" | DigitCharacter) ...,
                     Longest[ ("$" | DigitCharacter) .. ],
                     EndOfString
                   ] :> first <> name
    ];


symbolBaseName[ sym_? SymbolQ ] :=
  symbolBaseName @@ { SymbolName @ Unevaluated @ sym };



$emptyDefinition =
  HoldPattern[_ -> {OwnValues -> {}, SubValues -> {}, UpValues -> {},
      DownValues -> {}, NValues -> {}, FormatValues -> {},
      DefaultValues -> {}, Messages -> {}, Attributes -> {___}}];






(*MinimalFullDefinition[ symbol_? SymbolQ ] :=

  Module[ { deps, symList, fullDefinitionList },

      deps = FindDependencies @ symbol;

      symList = DeleteDuplicates @ Prepend[ Flatten[ List @@@ Normal @ deps ],
                                            HoldForm @ symbol
                ];

      fullDefinitionList =
        Join @@ Replace[ symList,
                         HoldForm[ s_ ] :> Language`ExtendedDefinition @ s,
                         { 1 }
                ];

      DeleteCases[ fullDefinitionList, HoldPattern[ _ -> { } ], { 3 } ]
  ];


MinimalFullDefinition[ string_String ] :=
  ToExpression[ string, StandardForm, MinimalFullDefinition ];*)



CompactDefinition // Attributes = { HoldAllComplete };


CompactDefinition[ Language`DefinitionList[ expr___ ] ] :=
  With[ { b = rawCompress @ expr }, CompactDefinition @ b ];



PackDefinition // Attributes = {HoldFirst};


PackDefinition[s_Symbol] :=
  With[{min = MinimalFullDefinition[s]},
      CompactDefinition[min]
  ];


UnpackDefinition // ClearAll;
UnpackDefinition // Attributes = {};

UnpackDefinition[compact_CompactDefinition, wrapper_: Identity] :=
  With[{defList = Language`DefinitionList @@ rawUncompress @@ compact},
      wrapper[ Language`ExtendedFullDefinition[] = defList; ]
  ];


UnpackDefinition[ base64_String, wrapper_: Identity ] :=
  UnpackDefinition[ CompactDefinition @ ByteArray @ base64, wrapper ];




(******************************************************************************)



rescopedDefinitionList // ClearAll;
rescopedDefinitionList // Attributes = {HoldAllComplete};

rescopedDefinitionList[symbol_?SymbolQ] :=
    Replace[CanonicalScopeAll[Language`ExtendedDefinition[symbol]],
      Language`DefinitionList[HoldForm[symbol] -> {defs__}] :>
          List @@ Replace[
            HoldComplete[defs], {HoldPattern[_ -> {}] :> Nothing,
              HoldPattern[_ -> {def__}] :> HoldComplete[def],
              HoldPattern[_ -> def_] :> HoldComplete[def]}, {1}]];



(******************************************************************************)



$excluded =
  # <> "`" & /@ Cases[ Lookup[ Options @ Language`ExtendedFullDefinition,
                               "ExcludedContexts"
                       ],
                       _String,
                       Infinity
                ];


dependentSymbolList // ClearAll;
dependentSymbolList // Attributes = { HoldAllComplete };
dependentSymbolList // Options =
  {
      "ExcludedContexts" -> Append[ $excluded, "Wolfram`CodeEquivalenceUtilities`" ~~ ___ ]
  };


dependentSymbolList[ symbol_? SymbolQ, opts : OptionsPattern[ ] ] :=

  Module[ { excluded, rescoped, fullSymList },

      excluded = Alternatives @@ OptionValue @ "ExcludedContexts";

      rescoped = rescopedDefinitionList @ symbol;

      fullSymList =
        Cases[ rescoped,
               s_? SymbolQ /; ! StringMatchQ[ Context @ Unevaluated @ s,
                                              excluded
                                ] :> HoldComplete @ s,
               Infinity,
               Heads -> True
        ];

      DeleteCases[ DeleteDuplicates @ fullSymList,
                   HoldComplete @ symbol
      ]
  ];


dependentSymbolList[ HoldComplete[ symbol_? SymbolQ ],
                     opts : OptionsPattern[ ]
                   ] :=

    dependentSymbolList[ symbol, opts ];



(******************************************************************************)



iFindDependencies // ClearAll;
iFindDependencies // Attributes = {HoldFirst};

iFindDependencies[dependencies_, symbol_HoldComplete, depth_, limit_] /;
    depth < limit :=
    If[! KeyExistsQ[dependencies, symbol],
      With[{symList = dependentSymbolList[symbol]},
        If[symList =!= {},
          dependencies[symbol] = symList
        ];
        iFindDependencies[dependencies, #, depth + 1, limit] & /@ symList
      ]
    ];

$depthLimit = 64;

FindDependencies // ClearAll;
FindDependencies // Attributes = {HoldAllComplete};

FindDependencies[symbol_?SymbolQ] :=
    Module[{dependencies, result},
      Off[RuleDelayed::rhs];
      dependencies = <||>;
      iFindDependencies[dependencies, HoldComplete[symbol], 0, $depthLimit];
      result =
          Association[
            Normal[dependencies] /.
                HoldComplete[x_Symbol] /;
                    AtomQ[Unevaluated[x]] &&
                        FreeQ[UpValueSymbols[x], HoldForm, {1}] :> HoldForm[x]];
      On[RuleDelayed::rhs];
      result
    ];


FindDependencies[name_String] :=
  ToExpression[ name, StandardForm, FindDependencies ];



(******************************************************************************)



End[ ];

EndPackage[ ];
Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
