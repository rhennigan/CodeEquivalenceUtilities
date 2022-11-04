(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
Canonical;
CanonicalScopeAll;
CodeTransformerObject;
CompactDefinition;
FindDependencies;
MakeCodeTransformerObject;
MakeTransformationRules;
PackDefinition;
RandomValue;
StripCanonical;
TransformHold;
TransformRelease;
UnpackDefinition;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Declarations*)
$LocalContext;
NormalizeNames;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

$depthLimit = 64;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Canonical*)
Canonical // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CodeTransformerObject*)
CodeTransformerObject[ obj_Association ][ key_String ] := obj[ key ];

CodeTransformerObject[ obj_Association ][ expr: Except[ _String ] ] :=
    NormalizeNames[
        ReplaceRepeated[ expr, obj[ "Dispatch" ] ],
        "ExcludedContexts" -> {
            "System`",
            Except[
                $LocalContext,
                s_String /;
                    StringMatchQ[
                        s,
                        "Wolfram`CodeEquivalenceUtilities`" ~~ ___
                    ]
            ]
        },
        "RemoveTypes" -> False
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CompactDefinition*)
CompactDefinition // Attributes = { HoldAllComplete };

CompactDefinition[ Language`DefinitionList[ expr___ ] ] :=
    With[ { b = rawCompress @ expr }, CompactDefinition @ b ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rawCompress*)
rawCompress // Attributes = { HoldAllComplete };

rawCompress[ ] := ByteArray @ { };

rawCompress[ expr_ ] :=
    ByteArray @ Developer`RawCompress @ ToCharacterCode[
        ToFullString @ expr,
        "ASCII"
    ];

rawCompress[ expr___ ] := rawCompress @ Sequence @ expr;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FindDependencies*)
FindDependencies // Attributes = { HoldAllComplete };

FindDependencies[ symbol_? SymbolQ ] :=
    Module[ { dependencies, result },
        Off @ RuleDelayed::rhs;
        dependencies = <| |>;

        iFindDependencies[
            dependencies,
            HoldComplete @ symbol,
            0,
            $depthLimit
        ];


        result = Association @ ReplaceAll[
            Normal @ dependencies,
            HoldComplete[ x_Symbol ] /; And[
                AtomQ @ Unevaluated @ x,
                FreeQ[ UpValueSymbols @ x, HoldForm, { 1 } ]
            ] :> HoldForm @ x
        ];

        On @ RuleDelayed::rhs;
        result
    ];

FindDependencies[ name_String ] :=
    ToExpression[ name, StandardForm, FindDependencies ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*iFindDependencies*)
iFindDependencies // Attributes = { HoldFirst };

iFindDependencies[ dependencies_, symbol_HoldComplete, depth_, limit_ ] /;
    depth < limit :=
    If[ ! KeyExistsQ[ dependencies, symbol ],
        With[ { symList = dependentSymbolList @ symbol },
            If[ symList =!= { }, dependencies[ symbol ] = symList ];
            Map[ iFindDependencies[ dependencies, #1, depth + 1, limit ] &,
                 symList
            ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*dependentSymbolList*)
dependentSymbolList // Attributes = { HoldAllComplete };
dependentSymbolList // Options = { "ExcludedContexts" :> $depExcluded };

dependentSymbolList[ symbol_? SymbolQ, opts: OptionsPattern[ ] ] :=
    Module[ { excluded, rescoped, fullSymList },
        excluded = Alternatives @@ OptionValue[ "ExcludedContexts" ];
        rescoped = rescopedDefinitionList @ symbol;

        fullSymList =
            Cases[
                rescoped,
                s_? SymbolQ /;
                    ! StringStartsQ[ Context @ Unevaluated @ s, excluded ] :>
                    HoldComplete @ s,
                Infinity,
                Heads -> True
            ];

        DeleteCases[ DeleteDuplicates @ fullSymList, HoldComplete @ symbol ]
    ];

dependentSymbolList[ HoldComplete[ sym_? SymbolQ ], opts: OptionsPattern[ ] ] :=
    dependentSymbolList[ sym, opts ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$depExcluded*)
$depExcluded := $depExcluded = Append[
    StringDelete[ Language`$InternalContexts, Verbatim[ "*" ] ~~ EndOfString ],
    "Wolfram`CodeEquivalenceUtilities`"
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rescopedDefinitionList*)
rescopedDefinitionList // Attributes = { HoldAllComplete };

rescopedDefinitionList[ symbol_? SymbolQ ] :=
    Replace[
        CanonicalScopeAll @ Language`ExtendedDefinition @ symbol,
        Language`DefinitionList[ HoldForm @ symbol -> { defs__ } ] :>
            Apply[
                List,
                Replace[
                    HoldComplete @ defs,
                    {
                        HoldPattern[ _ -> { } ] :> Nothing,
                        HoldPattern[ _ -> { def__ } ] :> HoldComplete @ def,
                        HoldPattern[ _ -> def_ ] :> HoldComplete @ def
                    },
                    { 1 }
                ]
            ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*MakeCodeTransformerObject*)
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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*MakeTransformationRules*)
MakeTransformationRules // Attributes = { HoldAllComplete };

MakeTransformationRules::trform =
"MakeTransformationRules argument `1` at position `2` does not have the \
correct form for a transformation rule.";

$nohp = Except[ Verbatim[ HoldPattern ][ _ ] ];

MakeTransformationRules[ ( Rule | RuleDelayed )[ p: $nohp, exp_ ] ] :=
  Inline[ $nohp, MakeTransformationRules[ HoldPattern @ p :> exp ] ];


(* :!CodeAnalysis::Disable::Arguments::With:: *)
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*trArgPattQ*)
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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PackDefinition*)
PackDefinition // Attributes = { HoldFirst };

PackDefinition[ s_Symbol ] :=
    With[ { min = MinimalFullDefinition @ s }, CompactDefinition @ min ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*RandomValue*)
RandomValue /:
  HoldPattern[ Positive @ RandomValue @
    DiscreteUniformDistribution[ { _? Positive, _ } ]
  ] := True;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*StripCanonical*)
StripCanonical[ expression_ ] :=
    StripHead[ expression, Canonical|TransformHold|TransformRelease ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*UnpackDefinition*)
UnpackDefinition[ compact_CompactDefinition, Optional[ wrapper_, Identity ] ] :=
    With[ { defList = Language`DefinitionList @@ rawUncompress @@ compact },
        wrapper[ Language`ExtendedFullDefinition[ ] = defList; ]
    ];

UnpackDefinition[ base64_String, Optional[ wrapper_, Identity ] ] :=
    UnpackDefinition[ CompactDefinition @ ByteArray @ base64, wrapper ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rawUncompress*)
rawUncompress[ b_ByteArray, wrapper_: HoldComplete ] :=
    Module[ { rawBinary, string },
      rawBinary = Developer`RawUncompress @ Normal @ b;
      string = FromCharacterCode[ rawBinary, "ASCII" ];
      ToExpression[ string, InputForm, wrapper]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)
