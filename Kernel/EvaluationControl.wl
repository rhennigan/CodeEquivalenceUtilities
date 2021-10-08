Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;



BeginPackage[ "Wolfram`CodeEquivalenceUtilities`EvaluationControl`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Attributes`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`",
        "GeneralUtilities`",
        "Wolfram`CodeEquivalenceUtilities`Types`"
    }
];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;

ClearAll[
    $UnsafeSymbols,
    $RandomSymbols,
    Closure,
    Unsafe,
    UnsafeSymbols,
    Sandboxed,
    SandboxViolation,
    EvaluateSafely
];



(******************************************************************************)
(* Exported symbols added here with SymbolName::usage *)


$Memoize;
$UnsafeSymbols;
$UnsafeSymbolPattern;
SafeExpressionQ;
$RandomSymbols;
Closure;
Unsafe;
UnsafeSymbols;
Sandboxed;
SandboxViolation;
SandboxException;
EvaluateSafely;
EvaluatedQ;
SafeEvaluatedQ;
FixValue;


(*MakeSafe            ::usage = "";
GetSafetyWarnings   ::usage = "";
EvaluateSafely      ::usage = "";
SandboxViolation    ::usage = "";
WithHolding         ::usage = "";*)



Begin[ "`Private`" ];



(******************************************************************************)


$Memoize = True;



(******************************************************************************)


$MaxQuantityArraySize = 25;



$Optimizations := {
    <|
        "Symbol" -> "System`StructuredArray",
        "Rules" :> {
            HoldPattern[
              array: StructuredArray[
                  QuantityArray,
                  { a_, b_ } /; a > $MaxQuantityArraySize || b > $MaxQuantityArraySize,
                  StructuredArray`StructuredData[ QuantityArray, _, _String, _ ]
              ] ] :>
                QuantityArray[
                    ArrayResample[
                        QuantityMagnitude @ array,
                        { Min[ a, $MaxQuantityArraySize ], Min[ b, $MaxQuantityArraySize ] }
                    ],
                    array[ "UnitBlock" ]
                ]
        }
    |>
};

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::Arguments::Block:: *)
WithOptimizations // Attributes = { HoldAllComplete };

WithOptimizations[ eval_ ] :=

  Module[ { syms, defs },

      syms =
        Cases[ $Optimizations,
               KeyValuePattern[ "Symbol" -> sym_String ] :> Symbol @ sym
        ];

      defs =
        Cases[ $Optimizations,
               KeyValuePattern @ { "Symbol" -> name_String, "Rules" :> rules_ } :>
                 With[ { sym = Symbol @ name },
                     HoldComplete[
                         Unprotect @ sym;
                         PrependTo[ DownValues @ sym, # ] & /@ rules;
                         Protect @ sym
                     ]
                 ]
        ];

      With[ { s = syms, d = defs },
          Internal`InheritedBlock[
              s,
              ReleaseHold @ d;
              eval
          ]
      ]

  ];
(* :!CodeAnalysis::EndBlock:: *)


(******************************************************************************)



EvaluatedQ // Attributes = { HoldAllComplete };
EvaluatedQ // Options    = { };


EvaluatedQ[ x___ ] :=
  With[
      {
          result = With[ { x$ = x },
                         HoldComplete @ x$ === HoldComplete @ x
          ]
      },
      If[ $Memoize,
          EvaluatedQ[ x ] = result,
          result
      ]
  ];



SafeEvaluatedQ // Attributes = { HoldAllComplete };
SafeEvaluatedQ // Options    = { };


SafeEvaluatedQ[ x___ ] :=
  With[
      {
          result =
            With[
                {
                    x$ = EvaluateSafely[ x,
                                                 "RemoveTypes" -> False,
                                                 "Timeout" -> 0.01
                                 ]
                },
                If[ Head @ x$ === SandboxViolation,
                    SandboxViolation,
                    HoldComplete @ x$ === HoldComplete @ x
                ]
            ]
      },
      If[ $Memoize,
          SafeEvaluatedQ[ x ] = result,
          result
      ]
  ];



(******************************************************************************)



ClearAll[allSymbols];
allSymbols := allSymbols = Names["*"];

fastNames[pats0_] := With[{pats = (___ ~~ #1 ~~ ___ &) /@ pats0},
    Flatten[StringCases[allSymbols, pats]]
];

falsePositives = {
    "AnglePath",
    "AnimationRunning",
    "AnimationRunTime",
    "AppendTo",
    "ARCHProcess",
    "ARIMAProcess",
    "ARMAProcess",
    "ARProcess",
    "AssociateTo",
    "AssociationFormat",
    "BernoulliProcess",
    "BitClear",
    "BitGet",
    "BitSet",
    "BoxData",
    "CityData",
    "ClassifierInformation",
    "ClosenessCentrality",
    "CloudGet",
    "CloudObject",
    "ColorData",
    "ColorProfileData",
    "ColorSelectorSettings",
    "ColorSetter",
    "ColorSetterBox",
    "ColorSetterBoxOptions",
    "ColorToneMapping",
    "CompressedData",
    "CriticalityFailureImportance",
    "CriticalitySuccessImportance",
    "DataDistribution",
    "DataRange",
    "DataReversed",
    "Dataset",
    "DateFormat",
    "DateTicksFormat",
    "Distribute",
    "Distributed",
    "EndOfLine",
    "EndOfString",
    "EntityValue",
    "EqualTo",
    "EquatedTo",
    "EquirippleFilterKernel",
    "EstimatedProcess",
    "EvaluationData",
    "EventData",
    "ExampleData",
    "ExportForm",
    "FARIMAProcess",
    "FileNameDrop",
    "FileNameJoin",
    "FileNameSplit",
    "FileNameTake",
    "FinancialData",
    "FindAnomalies",
    "FindArgMax",
    "FindArgMin",
    "FindClique",
    "FindClusters",
    "FindCurvePath",
    "FindCycle",
    "FindDistribution",
    "FindDistributionParameters",
    "FindDivisions",
    "FindEdgeCover",
    "FindEdgeCut",
    "FindEdgeIndependentPaths",
    "FindEulerianCycle",
    "FindFaces",
    "FindFit",
    "FindFormula",
    "FindFundamentalCycles",
    "FindGeneratingFunction",
    "FindGeoLocation",
    "FindGeometricTransform",
    "FindGraphCommunities",
    "FindGraphIsomorphism",
    "FindGraphPartition",
    "FindHamiltonianCycle",
    "FindHamiltonianPath",
    "FindHiddenMarkovStates",
    "FindIndependentEdgeSet",
    "FindIndependentVertexSet",
    "FindInstance",
    "FindIntegerNullVector",
    "FindKClan",
    "FindKClique",
    "FindKClub",
    "FindKPlex",
    "FindMaximum",
    "FindMaximumFlow",
    "FindMaxValue",
    "FindMinimum",
    "FindMinimumCostFlow",
    "FindMinimumCut",
    "FindMinValue",
    "FindPath",
    "FindPeaks",
    "FindPermutation",
    "FindPostmanTour",
    "FindProcessParameters",
    "FindRoot",
    "FindSequenceFunction",
    "FindShortestPath",
    "FindShortestTour",
    "FindSpanningTree",
    "FindThreshold",
    "FindVertexCover",
    "FindVertexCut",
    "FindVertexIndependentPaths",
    "FlatTopWindow",
    "FractionalBrownianMotionProcess",
    "FractionalGaussianNoiseProcess",
    "FrenetSerretSystem",
    "FrequencySamplingFilterKernel",
    "FussellVeselyImportance",
    "GARCHProcess",
    "GenomeData",
    "GeodesicOpening",
    "GeoElevationData",
    "GeometricBrownianMotionProcess",
    "GeoPath",
    "GrammarToken",
    "GraphLinkEfficiency",
    "GroupElementToWord",
    "GroupSetwiseStabilizer",
    "HiddenMarkovProcess",
    "ImageData",
    "ImageFormattingWidth",
    "ImprovementImportance",
    "IndependentEdgeSetQ",
    "IndependentVertexSetQ",
    "Information",
    "InhomogeneousPoissonProcess",
    "ItoProcess",
    "JacobiSymbol",
    "JuliaSetBoettcher",
    "JuliaSetIterationCount",
    "JuliaSetPlot",
    "JuliaSetPoints",
    "KernelMixtureDistribution",
    "KnightTourGraph",
    "KroneckerSymbol",
    "LeastSquaresFilterKernel",
    "LiftingFilterData",
    "ListCurvePathPlot",
    "ListFormat",
    "ListStreamDensityPlot",
    "ListStreamPlot",
    "LocatorAutoCreate",
    "MandelbrotSetBoettcher",
    "MandelbrotSetDistance",
    "MandelbrotSetIterationCount",
    "MandelbrotSetMemberQ",
    "MandelbrotSetPlot",
    "MAProcess",
    "MarkovProcessProperties",
    "MaxMixtureKernels",
    "MetaInformation",
    "MissingDataMethod",
    "MissingDataRules",
    "NetEncoder",
    "NumberFormat",
    "Opener",
    "OpenerBox",
    "OpenerBoxOptions",
    "OpenerView",
    "Opening",
    "OwnValues",
    "Parallelepiped",
    "Parallelogram",
    "PathGraph",
    "PathGraphQ",
    "Permissions",
    "PoissonProcess",
    "PredictorInformation",
    "PrependTo",
    "PrintableASCIIQ",
    "ProcessEstimator",
    "ProcessParameterAssumptions",
    "ProcessParameterQ",
    "ProcessStateDomain",
    "ProcessTimeDomain",
    "Protected",
    "QueueingNetworkProcess",
    "QueueingProcess",
    "RandomWalkProcess",
    "RawBoxes",
    "RemoveAlphaChannel",
    "RemoveBackground",
    "RemoveDiacritics",
    "RenewalProcess",
    "ResourceData",
    "RuntimeAttributes",
    "RuntimeOptions",
    "SARIMAProcess",
    "SARMAProcess",
    "SaveDefinitions",
    "Set",
    "SetDelayed",
    "Setter",
    "SetterBar",
    "SetterBox",
    "SetterBoxOptions",
    "ShortestPathFunction",
    "SixJSymbol",
    "SmoothKernelDistribution",
    "SplicedDistribution",
    "SplineClosed",
    "StratonovichProcess",
    "StreamColorFunction",
    "StreamColorFunctionScaling",
    "StreamDensityPlot",
    "StreamPlot",
    "StreamPoints",
    "StreamScale",
    "StreamStyle",
    "SubstitutionSystem",
    "TelegraphProcess",
    "tempSymbol",
    "TextData",
    "ThreeJSymbol",
    "TimeFormat",
    "Today",
    "Together",
    "Tolerance",
    "ToLowerCase",
    "Tomorrow",
    "ToNumberField",
    "Tooltip",
    "Top",
    "TopHatTransform",
    "ToPolarCoordinates",
    "TopologicalSort",
    "ToRadicals",
    "ToRules",
    "ToSphericalCoordinates",
    "Total",
    "TotalHeight",
    "TotalVariationFilter",
    "TotalWidth",
    "ToTitleCase",
    "ToUpperCase",
    "TransformedProcess",
    "TypedSymbol",
    "UpTo",
    "URL",
    "URLBuild",
    "URLDecode",
    "URLParse",
    "URLQueryDecode",
    "URLQueryEncode",
    "ValidationSet",
    "VertexDataCoordinates",
    "WeatherData",
    "WhiteNoiseProcess",
    "WienerProcess",
    "WikipediaData",
    "WordCloud",
    "$SystemID"
};


$sec = DeleteCases[fastNames /@ <|
    "Evaluation" -> {"Abort"},
    "Cloud" -> {"Cloud", "Permissions"},
    "Compile" -> {"Compile"},
    "Compress" -> {"Compress", "Uncompress", "CompressedData"},
    "Context" -> {"Begin", "BeginPackage", "End", "EndPackage"},
    "Create" -> {"Create"},
    "Data" -> {"Data", "EntityValue"},
    "Device" -> {"Device"},
    "Dialog" -> {"Dialog"},
    "Dynamic" -> {"Dynamic"},
    "File" -> {"File", "Directory", "Path", "FindList"},
    "Format" -> {"Format", "MakeBoxes", "ToBoxes", "RawBoxes",
        "Typeset`ToExpression"},
    "Frontend" -> {"Notebook", "Frontend", "FrontEnd", "Clipboard",
        "CurrentImage", "CurrentValue", "AbsoluteOptions",
        "FullOptions", "SelectionEvaluate", "Paste"},
    "Introspect" -> {"ToExpression", "MakeExpression", "Attributes",
        "Stack", "Definition", "FullDefinition", "Information",
        "DefaultValues", "DownValues", "DynamicModuleValues",
        "FormatValues", "NValues", "OwnValues", "SubValues",
        "UpValues", "Stack", "Trace"},
    "IO" -> {"Print", "Put", "Get", "Needs", "Dump", "Save",
        "Import", "Export", "Splice", "Encode"},
    "Java" -> {"Java", "AddToClassPath"},
    "LibraryLink" -> {"Library"},
    "Link" -> {"Link", "Java"},
    "Mutate" -> {"Set", "Get", "To", "Protect", "Unprotect", "Clear",
        "Remove"},
    "Network" -> {"URL", "HTTP"},
    "Pause" -> {"Pause"},
    "Paclet" -> {"Paclet"},
    "Parallel" -> {"Parallel", "Kernel", "SetShared", "Distribute"},
    "Print" -> {"Print", "Echo"},
    "Process" -> {"Run", "RunThrough", "Process", "Install"},
    "Quit" -> {"Quit"},
    "Send" -> {"Send"},
    "Stream" -> {"Stream", "Open", "Close", "Read", "Write", "Find",
        "Skip"},
    "Symbol" -> {"Symbol"},
    "System" -> {"System", "SetEnvironment"},
    "Task" -> {"Task"}
|>, Alternatives @@ falsePositives, Infinity];

$additionalUnsafe = { "DeleteObject", "WordList", "WordData" };

$UnsafeSymbols = Union[ Union @@ Values[ $sec ], $additionalUnsafe ];


$UnsafeSymbolPattern =
  Replace[ Flatten[ HoldComplete @@ ToExpression[ $UnsafeSymbols,
                                                  StandardForm,
                                                  HoldComplete
                                    ]
           ],
           HoldComplete[ a___ ] :> HoldPattern @ Alternatives @ a
  ];



(******************************************************************************)



SafeExpressionQ // Attributes = { HoldAllComplete };
SafeExpressionQ // Options    = { };


SafeExpressionQ[ expr_ ] :=
  Module[ { hexpr },
      hexpr = DeleteCases[ HoldComplete @ expr,
                           _TypedSymbol,
                           Infinity,
                           Heads -> True
      ];
      FreeQ[ FullDefinitionData @ hexpr, $UnsafeSymbolPattern ]
  ];



(******************************************************************************)



$RandomSymbols =
  {
     "RandomChoice", "RandomColor", "RandomComplex", "RandomEntity",
      "RandomFunction", "RandomGraph", "RandomImage", "RandomInteger",
      "RandomPermutation", "RandomPoint", "RandomPrime", "RandomReal",
      "RandomSample", "RandomSeed", "RandomVariate", "RandomWalkProcess",
      "RandomWord",

      "Date", "DateList", "DateObject", "DateRange", "DateString", "DateValue",
      "AbsoluteTime", "LocalTime", "SessionTime", "SiderealTime", "TimeObject",
      "TimeValue", "UnixTime", "Now", "Today", "Tomorrow", "Yesterday"
  };




(******************************************************************************)


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

UnsafeSymbols // Attributes = { HoldAllComplete };
UnsafeSymbols // Options    =
  {
      "Definitions" -> Automatic,
      "SymbolList"  -> $UnsafeSymbols,
      "Seed"        :> $SessionID,
      "RemoveTypes" -> True,
      "Timeout"     -> None,
      "RetryCount"  -> None
  };

UnsafeSymbols[ f_? SymbolQ, opts : OptionsPattern[ ] ] :=

  With[
      {
          definitions =
            Replace[ OptionValue @ "Definitions",
                {
                    Automatic   -> Language`ExtendedDefinition,
                    Full        -> Language`ExtendedFullDefinition
                }
            ]
      },

      Union @ Cases[
          List @@ Replace[ definitions @ f,
                           HoldPattern[
                               HoldForm[ _ ] ->
                                 {
                                     OwnValues      -> ov_,
                                     SubValues      -> sv_,
                                     UpValues       -> uv_,
                                     DownValues     -> dv_,
                                     NValues        -> nv_,
                                     FormatValues   -> fv_,
                                     DefaultValues  -> defv_,
                                     Messages       -> mv_,
                                     Attributes     -> a_
                                 }
                           ] :>
                             Flatten @ { ov, sv, uv, dv, nv, fv, defv, mv, a }
                           ,
                           { 1 }
                  ] // Flatten
          ,
          ( s_? SymbolQ /; MemberQ[ OptionValue @ "SymbolList",
                                    SymbolName @ Unevaluated @ s
                           ]
          ) :> ToFullString[ s, "Context" -> None, "ContextPath" -> None ]
          ,
          Infinity,
          Heads -> True
      ]
  ];



(******************************************************************************)



Unsafe // Attributes = { HoldAllComplete };
Unsafe // Options    = { };


Unsafe[ s_? SymbolQ, { args___ }, holdingQ_? BooleanQ ] :=

  With[ { name = SymbolName @ Unevaluated @ s },

      If[ TrueQ @ holdingQ,
          Unsafe[ name, HoldComplete @ args ],
          Unsafe[ name, { args } ]
      ]
  ];


Unsafe[ s_? SymbolQ ] :=

  With[ { name = SymbolName @ Unevaluated @ s },

      Unsafe[ name ]
  ];


Unsafe[ s_? SymbolQ, { args___ } ] :=
  Unsafe[ s, { args }, HoldingQ @ s ];

Unsafe[ name_String ] :=
  If[ TrueQ @ HoldingQ @ name,
      Function[ Null,
                CatchUnsafe[ name, HoldComplete @ ## ],
                { HoldAllComplete }
      ],
      Function[ Null,
                CatchUnsafe[ name, { ## } ],
                { }
      ]
  ];

(******************************************************************************)



(*Sandboxed // Attributes = { };
Sandboxed // Options    = { };


Sandboxed[ { }, eval_ ] :=
  eval;


Sandboxed[ effects : { __ }, eval_ ] :=
  SandboxViolation[ eval, "Captured" -> effects ];*)



(******************************************************************************)


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::ParameterError:: *)
removeTypes // ClearAll;
removeTypes // Attributes = { HoldAllComplete };
removeTypes // Options    = { };


removeTypes[ expr_ ] :=
  Quiet[
      ReplaceAll[
          expr,
          {
              HoldPattern[
                  TypedSymbol[
                      Verbatim[ Pattern ][ symbol_, Verbatim[ Blank ][ ] ],
                      Verbatim[ Verbatim ][ patt_ ]
                  ]
              ] :> Pattern[ symbol, patt ]
              ,
              HoldPattern[ TypedSymbol[ s_? SymbolQ, _ ] ] :> TrEval @ s
          }
      ],
      RuleDelayed::rhs
  ];
(* :!CodeAnalysis::EndBlock:: *)


(*discoverBindings // ClearAll;
discoverBindings // Attributes = {HoldAllComplete};

discoverBindings[args___] :=
  Module[{inputExpr = HoldComplete[args]},
      DeleteCases[
          Flatten[Replace[
              Replace[List @@ MinimalFullDefinition[inputExpr],
                  HoldPattern[HoldForm[_] -> {defs___}] :> defs, {1}],
              HoldPattern[_?SymbolQ -> defs_] :> defs,
              {1}
          ]],
          HoldPattern[Verbatim[HoldPattern][inputExpr] :> _]
      ]];*)

discoverBindings // ClearAll;
discoverBindings // Attributes = { HoldAllComplete };
discoverBindings // Options =
  {
      "ExcludedContexts" -> { "Wolfram`CodeEquivalenceUtilities`" ~~ ___ }
  };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)
discoverBindings[ args_, opts : OptionsPattern[ ] ] :=
  Module[ { inputExpr, excluded, sanitized },

      inputExpr = HoldComplete @ args;

      excluded = Alternatives @@ OptionValue @ "ExcludedContexts";

      sanitized =
        DeleteCases[
            inputExpr,
            s_? SymbolQ /; StringMatchQ[ Context @ Unevaluated @ s,
                                         excluded
                           ],
            Infinity,
            Heads -> True
        ];

      Closure[
          DeleteCases[KeySort /@
            Association /@
              KeySort[Association[
                  Replace[Rest[List @@ MinimalFullDefinition[sanitized]],
                      HoldPattern[s_ -> d_] :> s -> Flatten[{d}], {3}]]],
              <||>
          ]]
  ];
(* :!CodeAnalysis::EndBlock:: *)



evalRestricted // ClearAll;
evalRestricted // Attributes = { HoldAllComplete };

(*evalRestricted[ expr_, timeLimit_, memoryLimit_, input_ ] :=

  MemoryConstrained[ TimeConstrained[ expr,
                                      timeLimit,
                                      TimedOut @ HoldForm @ input
                     ],
                     memoryLimit,
                     MemoryOverflow @ HoldForm @ input
  ];*)


(*
currently not using MemoryConstrained to avoid bug:
https://bugs.wolfram.com/show?number=312544
*)

evalRestricted[ expr_, timeLimit_, memoryLimit_, input_ ] :=

  TimeConstrained[ checkAbort @ expr,
                   timeLimit,
                   TimedOut @ HoldForm @ input
  ];


TimedOut /:
  HoldPattern @ FailureQ[ _TimedOut ] :=
    True;


safeStop // ClearAll;

safeStop[ symbolList : { ___String } ] :=

  Module[ { actualNames, lockedNames, pattern },

      actualNames = Select[ symbolList, NameQ ];

      lockedNames = Select[ actualNames,
                            MemberQ[ Attributes @ #, Locked] &
                    ];

      pattern = Alternatives @@ ToExpression[ lockedNames,
                                              StandardForm,
                                              HoldForm
                                ];

      Replace[ pattern,
          {
              HoldPattern[ Verbatim @ Alternatives[ ] ] :> (Null &)
              ,
              p_ :> Function[ s,
                              Replace[ HoldComplete @ s,
                                       HoldComplete[ sym : p ] :>
                                         Throw[ FullSymbolName @@ sym,
                                                $SandboxException
                                         ]
                              ],
                              { HoldAllComplete }
                    ]
          }
      ]
  ];



safeScan // ClearAll;

safeScan[ symbolList : { ___String } ] :=
  With[ { safety = safeStop @ symbolList },
      Function[ expr,
                TraceScan[ safety, expr ],
                HoldAllComplete
      ]
  ];



SandboxException // Attributes = { HoldAllComplete };


SandboxException[ eval_ ][ s_String, $SandboxException ] := (
    Message[ EvaluateSafely::unsafe, s ];
    SandboxException[ eval, "CaughtSymbol" -> Unsafe @ s ]
);

SandboxException[ eval_ ][ exc_HoldComplete, $SandboxException ] := (
    Message[ EvaluateSafely::unsafe, exc ];
    SandboxException[ eval, "CaughtEvaluation" -> exc ]
);

(******************************************************************************)



(*
EvaluateSafely[ expr___, opts : OptionsPattern[ ] ] /;
  ! FreeQ[ HoldComplete @ expr, _TypedSymbol ] :=
  Replace[ removeTypes[ HoldComplete @ expr ],
      HoldComplete[ e_ ] :> EvaluateSafely[ e, opts ]
  ];
*)

SandboxViolation // Attributes = { HoldAllComplete };



(******************************************************************************)



EvaluateSafely // Attributes = { HoldAllComplete };
EvaluateSafely // Options =
  {
      "Definitions"         -> Full,
      "SymbolList"          :> $UnsafeSymbols,
      "Seed"                :> $SessionID,
      "RemoveTypes"         -> True,
      "Timeout"             -> 30,
      "MemoryConstraint"    -> 2^24,
      "RetryCount"          -> 5
  };



iEvaluateSafely // ClearAll;
iEvaluateSafely // Attributes = Attributes @ EvaluateSafely;
iEvaluateSafely // Options    = Options    @ EvaluateSafely;



iEvaluateSafely[ expr_, opts : OptionsPattern[ ] ] :=

  Module[
      {
          tempSymbol, symbolNames, replacementRules,
          sandboxed, eval, effects
      },

      tempSymbol = If[ OptionValue @ "RemoveTypes",
                       removeTypes @ HoldComplete @ expr,
                       HoldComplete @ expr
                   ];

      SeedRandom @ OptionValue @ "Seed";

      symbolNames = UnsafeSymbols[ tempSymbol, opts ];

      replacementRules = Flatten[ makeReplacementRules /@ symbolNames ];

      $captured = Internal`Bag[ ];

      sandboxed = tempSymbol //. replacementRules;

      eval = Quiet @ Block[ { PrintTemporary },
          evalRestricted[ ReleaseHold @ sandboxed,
                          OptionValue @ "Timeout",
                          OptionValue @ "MemoryConstraint",
                          expr
          ]
      ];

      effects = Internal`BagPart[ $captured, All ];

      Quiet @
        Replace[ { eval, effects },
                 {
                     { e_ , {      } } :> e,
                     { ev_, { ef__ } } :> SandboxViolation[ ev,
                                                            "Captured" -> { ef }
                                          ]
                 }
        ] /. {
          HoldPattern @ Function[ Null, CatchUnsafe[ name_, ___ ], _ ] :>
            Unsafe[ name, Null ]
          ,
          CatchUnsafe -> Unsafe
      }
  ];



makeReplacementRules[ name_ ] :=
  ToExpression[
      name,
      StandardForm,
      With[ { w = If[ HoldingQ @ name, HoldComplete, List ] },
          Function[
              s,
              {
                  HoldPattern[ e: s[ a___ ] ] :> CatchUnsafe[ name, w @ a ],
                  HoldPattern @ s :> CatchUnsafe[ name, w @ $null$ ]
              },
              { HoldAllComplete }
          ]
      ]
  ];


CatchUnsafe[ name_, (List|HoldComplete)[ $null$ ] ] :=
  With[ { unsafe = Unsafe @ name },
      Internal`StuffBag[ $captured, unsafe ];
      unsafe
  ];

CatchUnsafe[ args___ ] :=
  With[ { unsafe = Unsafe @ args },
      Internal`StuffBag[ $captured, unsafe ];
      unsafe
  ];



whitelistDomain // ClearAll;
whitelistDomain // Attributes = { HoldAllComplete };
whitelistDomain[ domain_ ] :=
  Module[ { url },
      Condition @@ HoldComplete[ url_String, StringMatchQ[ URLParse[ url, "Domain" ], domain ] ]
  ];

whitelistPath // ClearAll;
whitelistPath // Attributes = { HoldAllComplete };
whitelistPath[ dir_ ] :=
  Alternatives[
      _String? (StringStartsQ @ dir),
      File[ _String? (StringStartsQ @ dir) ]
  ];


resourceURL :=
  Module[ { url },
      Condition @@ HoldComplete[ url_String,
          StringStartsQ[ url, "https://www.wolframcloud.com/objects/resourcesystem/" ] ||
            Quiet @ StringStartsQ[
                CloudObjectInformation[ CloudObject @ url, "Path" ],
                "resourcesystem/"
            ]
      ]
  ];


$allowedLibs // ClearAll;
$allowedLibs = _String? (
    Function[
        Null,
        StringQ @ Unevaluated @ # && StringEndsQ[ #, ".so" | ".dll" ],
        HoldAllComplete
    ]
);


contextQ // ClearAll;
contextQ // Attributes = { HoldAllComplete };

contextQ[ context_String ] :=
  Quiet @ And[
      TrueQ @ StringQ @ Unevaluated @ context,
      TrueQ @ StringEndsQ[ context, "`" ],
      ! TrueQ @ FileExistsQ @ context,
      TrueQ @ Or[
          With[ { f = FindFile @ context }, StringQ @ f && FileExistsQ @ f ],
          MemberQ[ Contexts[ ], context ]
      ]
  ];

contextQ[ ___ ] := False;


$contextPattern :=
  _String? (
      Function[ Null,
                Quiet @ And[
                    TrueQ @ StringQ @ Unevaluated @ #,
                    TrueQ @ StringEndsQ[ #, "`" ]
                ],
                { HoldAllComplete }
      ]
  );


urlBuild // ClearAll;
urlBuild[ args___ ] := URLBuild @ URLParse @ URLBuild @ args;

rebaseCloudURL[ url_ ] :=
  urlBuild @ Append[
      URLParse @ url,
      "Domain" -> URLParse[ $CloudBase, "Domain" ]
  ];

rebasedCloudURLs[ url_String ] := rebasedCloudURLs @ Alternatives @ url;
rebasedCloudURLs[ urls_ ] := Union[ rebaseCloudURL /@ urls, urls ];

safeCloudObjectQ // Attributes = { HoldAllComplete };

safeCloudObjectQ[ (URL|CloudObject)[ url_, ___ ] ] := safeCloudObjectQ @ url;

Scan[
    Function[ safeCloudObjectQ[ #1 ] = True ],
    rebasedCloudURLs @ {
        "https://www.wolframcloud.com/files/auth",
        "https://www.wolframcloud.com/files?path=auth&fields=path",
        "https://www.wolframcloud.com/OAuthVersion",
        "https://www.wolframcloud.com/app/OAuthVersion"
    }
];

safeCloudObjectQ[ url_String? UStringQ ] :=
  Wolfram`CodeEquivalenceUtilities`CachedValues`Cached[
      safeCloudObjectQ @@ URLParse[ url, { "Domain", "PathString", "Query" } ]
  ];

safeCloudObjectQ[ domain_String, path_String, query_ ] :=
  And[ MatchQ[ domain,
               URLParse[ $CloudBase, "Domain" ] | "www.wolframcloud.com"
       ],
       If[ StringStartsQ[ path, "/files"~~(EndOfString|"/") ],
           safeCloudObjectFileQ[ domain, path, query ],
           TrueQ @ Or[
               StringStartsQ[ StringTrim[ path, "/" ],
                   ("obj" | "objects") ~~ "/" ~~ $safeCloudObjectPath
               ],
               safeCloudObjectUUIDQ[ domain, path, query ]
           ]
       ]
  ];


safeCloudObjectUUIDQ[
    domain_,
    path: { "obj"|"objects", uuid_? UUIDStringQ },
    query_
] :=
  Apply[
      safeCloudObjectQ,
      {
          URLBuild @ <|
              "Scheme" -> "HTTPS",
              "Domain" -> domain,
              "Path" -> {
                  "obj",
                  CloudObjectInformation[
                      CloudObject @ URLBuild @ <|
                          "Scheme" -> "HTTPS",
                          "Domain" -> domain,
                          "Path" -> path
                      |>,
                      "Path"
                  ]
              }
          |>
      }
  ];

safeCloudObjectUUIDQ[ domain_String, path_String, query_ ] :=
  safeCloudObjectUUIDQ[ domain, StringSplit[ path, "/" ], query ];

safeCloudObjectUUIDQ[ ___ ] :=
  False;


$safeCloudObjectPath = Alternatives[
    "online-courses/DataScienceMOOC/Examples",
    "resourcesystem/ResourceVersions",
    "resourcesystem/marketplacestorage/resources",
    "user-7dfab463-2b65-4c3b-b34e-ceea4165a73d",
    "entitycache"
];

safeCloudObjectFileQ[ domain_String, path_String, { }        ] := True;
safeCloudObjectFileQ[ _, _, KeyValuePattern[ "append" -> _ ] ] := False;
safeCloudObjectFileQ[ _, "/files/auth", { } ] := True;


$safeURLs := $safeURLs = rebasedCloudURLs @ Alternatives[
    "https://www.wolframcloud.com/OAuthVersion",
    "https://www.wolframcloud.com/info",
    "https://www.wolframcloud.com/app/OAuthVersion",
    "https://www.wolframcloud.com/objects/resourcesystem/ResourceVersions",
    "https://www.wolframcloud.com/obj/resourcesystem/ResourceVersions",
    "https://www.wolframcloud.com/objects/resourcesystem/api/1.0/SearchResources",
    "https://www.wolframcloud.com/obj/resourcesystem/api/1.0/SearchResources",
    "https://www.wolframcloud.com/files?path=auth&fields=path",
    "https://www.wolframcloud.com/files?path=resourcesystem%2FVersionInformation%2F12-2%2FResourceVersions&fields=uuid"
];

$fetchElement := $fetchElement =
  Module[ { fetchElements, anyFetchElement },
      URLFetch;
      fetchElements = CURLLink`HTTP`Private`$URLFetchElements;
      anyFetchElement = Alternatives @@ fetchElements;
      anyFetchElement | { anyFetchElement... }
  ];


$WhiteListedPatterns := (Quiet[ ResourceObject; LocalObject ];
With[ {
      resourceDir = (Symbol["ResourceSystemClient`Private`resourceCacheDirectory"][ ]),
      persistenceRoot = LocalObjects`PathName @ LocalObject @ $PersistenceBase[[ 2 ]],
      cloudFilesAPI = rebasedCloudURLs @ Alternatives[ "https://www.wolframcloud.com/files" ]
  },
  With[ {
      ctx = $contextPattern,
      domains = Alternatives[
          $safeURLs,
          whitelistDomain[ "*.wikipedia.org" ],
          whitelistDomain[ "*.wolframalpha.com" ],
          whitelistDomain[ "*.wolfram.com" ]
      ],
      pathSystem = whitelistPath[ $InstallationDirectory ],
      pathApps = whitelistPath[ "/wolframcloud/userfiles/WolframApplications" ],
      pathBase = whitelistPath @ $UserBaseDirectory | whitelistPath @ $BaseDirectory,
      pathTemp = whitelistPath[ $TemporaryDirectory | "/tmp/UserTemporaryFiles/public/" ],
      pathPaclets = whitelistPath[ PacletManager`$UserBasePacletsDirectory|"/www/tomcat/webapps/app/data/Paclets/" ],
      pathPacletsTemp = whitelistPath[ FileNameJoin @ { PacletManager`$UserBasePacletsDirectory, "Temporary" } ],
      pathKB = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "Knowledgebase" },
      pathResources = Alternatives[
          "data.MX",
          "put.wl",
          ExpandFileName[ "Resources" ],
          whitelistPath[ "/www/tomcat/home/.Mathematica/LocalObjects/Persistence/NetModelIndex" ],
          whitelistPath @ resourceDir,
          whitelistPath @ ExpandFileName @ resourceDir
      ],
      pathSearchIndices = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "SearchIndices" },
      pathResourcePersistence = Alternatives[
          whitelistPath @ FileNameJoin @ { persistenceRoot, "ResourceNames" },
          whitelistPath @ FileNameJoin @ { persistenceRoot, "PersistentResourceSystemBases" }
      ],
      pathPersistence = whitelistPath @ persistenceRoot,
      pathLockFiles = whitelistPath @ ResourceSystemClient`FileLocking`$LocalLockDirectory,
      pathCache = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "ApplicationData", "CodeEquivalenceUtilities", "Index" },
      pathProcLink = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "ApplicationData", "ProcessLink" },
      pathNMIndex = whitelistPath @ FileNameJoin @ { whitelistPath @ persistenceRoot, "NetModelIndex" <> StringReplace[ToString[$VersionNumber], "." -> "-"] },
      allowedLibs = $allowedLibs,
      cloudFiles = url_String /; StringStartsQ[ url, cloudFilesAPI~~("/"|"?") ] && StringFreeQ[ url, ("&"|"?")~~"properties=" ],
      rurl = resourceURL,
      persistence = _String? (StringStartsQ[ First @ CloudObject[ "Persistence/ResourceNames" ] ]),
      urlFetch = (URLDownload|URLExecute|URLFetch|URLFetchAsynchronous|URLRead|URLSave|URLSaveAsynchronous|URLSubmit),
      shortURL = "https://wolfr.am/expand" | HoldPattern @ HTTPRequest[ KeyValuePattern @ { "Domain" -> "wolfr.am", "Path" -> "expand" }, ___ ],
      allowedFetchOptions = Except[ (Rule|RuleDelayed)[ "Method"|Method, Except[ "GET" ] ] ] ...,
      allowedFetchElements = $fetchElement
  },
      HoldComplete[
          BinaryRead[ InputStream[ pathPaclets | pathProcLink , _ ], ___ ],
          BinaryReadList[ InputStream[ String | pathPaclets | pathCache | pathResources | pathResourcePersistence | pathTemp , _ ], ___ ],
          BinaryWrite[ OutputStream[ pathTemp | pathResources, _ ], ___ ],
          CreateDirectory[ ],
          CreateDirectory[ pathCache | pathResources | pathResourcePersistence | pathLockFiles | pathSearchIndices | pathPaclets, ___ ],
          CreateFile[ pathKB | pathCache | pathResources | pathResourcePersistence | pathProcLink ],
          DeleteFile[ pathTemp | pathKB | pathPacletsTemp | pathCache | pathResources | pathResourcePersistence | pathLockFiles ],
          DeleteFile[ File[ pathTemp | pathKB | pathPacletsTemp | pathCache | pathResources | pathResourcePersistence | pathLockFiles ] ],
          DeleteFile[ "data.MX" | "put.wl" | "save.mx" ],
          DumpSave[ pathCache | pathResources, ___ ],
          DumpSave[ "save.mx", NeuralNetworks`Private`NetModel`$NetModelIndexCached ],
          Export[ pathCache, ___ ],
          Export[ "data.MX", { 1 }, "Byte" ],
          Export[ OutputStream[ pathResources, _ ], { 1 }, { "Binary", "Byte" } ],
          FileHash[ s_String /; StringStartsQ[ s, "Resources/" ], ___ ],
          FileInformation[ pathSystem ],
          FileNames[ _, pathSystem | pathResources | pathBase | pathLockFiles | pathPersistence, ___ ],
          FileNames[ "*", { }, 1 ],
          FindFile[ allowedLibs, "AccessPermission" -> "Execute" ],
          FindFile[ pathSystem | pathPaclets | pathCache | pathResources | pathResourcePersistence | pathTemp ],
          FindFile[ "put.wl" | ctx, ___ ],
          Get[ "subicon.m" | "deficon.m" | "HDF5Tools/HDF5Tools.m" | $Failed | "put.wl" | ctx, ___ ],
          Get[ pathPaclets | pathSystem | pathApps | pathBase | pathTemp | pathCache | pathResources | pathResourcePersistence | pathLockFiles | pathNMIndex, ___ ],
          Import[ InputStream[ pathPaclets, _ ], ___ ],
          Import[ InputStream[ String, _ ], { "GEOTIFF", "Data" } | { "RAWJSON" } | { "JSON" }, ___ ],
          Import[ "!cmd /c ver" | "data.MX" | pathPaclets | pathCache | pathResources | pathResourcePersistence | pathTemp, ___ ],
          Import[ domains, "Hyperlinks"|"TSV"|"String"|{"Hyperlinks"|"TSV"|"String"}, ___ ],
          LinkWrite[ _, Except @ CallPacket @ CloudSystem`Private`ServiceDispatch @ CloudSystem`CloudObject`DoCloudOperation[ Except[ "GET" ], ___ ] ],
          LinkWrite[ link_LinkObject, ___ ] /; MatchQ[ $FrontEnd, HoldPattern @ FrontEndObject @ link ],
          OpenAppend[ pathKB | pathCache, ___ ],
          OpenRead[ pathKB | pathSystem | pathTemp | pathPaclets | pathCache | pathResources | pathResourcePersistence | pathProcLink, ___ ],
          OpenWrite[ ],
          OpenWrite[ pathTemp | pathPaclets | pathPacletsTemp | pathResources | pathResourcePersistence | pathKB, ___ ],
          OpenWrite[ in_String /; StringMatchQ[ in, "in:"~~DigitCharacter.. ], ___, Method -> "RunPipe", ___ ],
          Put[ ___, pathKB | pathResources | $Failed | "put.wl" | pathResourcePersistence | pathLockFiles ],
          Put[ $Failed | CURLLink`HTTP`Private`getFile @ None ],
          Read[ InputStream[ String, _ ], ___ ],
          Read[ InputStream[ pathSystem | pathTemp | pathKB | pathPaclets | pathProcLink, _ ], ___ ],
          Read[ pathSystem | pathTemp | pathKB, ___ ],
          ReadByteArray[ pathSystem | pathTemp | pathKB, ___ ],
          ReadByteArray[ File[ pathSystem | pathTemp | pathKB ], ___ ],
          ReadList[ pathSystem | pathTemp | pathKB, ___ ],
          ReadList[ InputStream[ pathTemp|String, ___ ], ___ ],
          ReadLine[ pathSystem | pathTemp | pathKB, ___ ],
          ReadLine[ InputStream[ pathSystem | pathTemp | pathKB | String, ___ ], ___ ],
          RenameFile[ pathResources, pathResources, ___ ],
          RenameFile[ pathLockFiles, pathLockFiles, ___ ],
          urlFetch[ cloudFiles | { cloudFiles }, allowedFetchOptions ],
          urlFetch[ cloudFiles | { cloudFiles }, allowedFetchElements, allowedFetchOptions ],
          urlFetch[ (rurl|domains|shortURL) | { rurl|domains|shortURL }, ___ ],

          Write[ OutputStream[ pathPaclets, ___ ], ___ ],
          WriteString[ OutputStream[ pathKB | pathTemp, _ ] | pathKB | pathTemp, ___ ],
          CloudGet[ _? safeCloudObjectQ | (rurl|persistence) | CloudObject[ (rurl|persistence) ] | URL[ (rurl|persistence) ] ],
          Unprotect[ s_Symbol /; ! StringStartsQ[ SafeContext @ s, "WolframChallenges`"|"Wolfram`CodeEquivalenceUtilities`" ] ],
          URLFetch["https://www.wolframcloud.com/OAuthVersion"|"https://www.wolframcloud.com/app/OAuthVersion", ___],
          URLFetchAsynchronous[ "https://www.wolframcloud.com/files/7918edd1-f8a1-46d0-8b87-7755d5325634", ___ ]
      ]
  ]]);

WhiteListedPatternQ // ClearAll;
WhiteListedPatternQ // Attributes = { HoldAllComplete };

WhiteListedPatternQ := (

      WhiteListedPatternQ // ClearAll;
      WhiteListedPatternQ // Attributes = { HoldAllComplete };

      Replace[ $WhiteListedPatterns,
               patt_ :> (HoldPattern @ WhiteListedPatternQ[ patt ] := True),
               { 1 }
      ] // ReleaseHold;

      WhiteListedPatternQ[ ___ ] := False;
      WhiteListedPatternQ
  );

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
$blackList := HoldComplete[
    BinaryRead,
    BinaryReadList,
    BinaryWrite,
    ChannelListen,
    ChannelSend,
    CloudGet,
    CloudPut,
    CopyFile,
    CreateDirectory,
    CreateFile,
    DeleteFile,
    DeleteObject,
    DeviceRead,
    DeviceReadBuffer,
    DeviceReadLatest,
    DeviceReadList,
    DeviceReadTimeSeries,
    DeviceWrite,
    DeviceWriteBuffer,
    DumpSave,
    Exit,
    Export,
    FileInformation,
    FileNames,
    FilePrint,
    FileSystemMap,
    FileSystemScan,
    FindFile,
    Get,
    ImageFileApply,
    ImageFileFilter,
    ImageFileScan,
    Import,
    LinkWrite,
    NotebookWrite,
    OpenAppend,
    OpenRead,
    OpenWrite,
    Put,
    PutAppend,
    Read,
    ReadByteArray,
    ReadLine,
    ReadList,
    RenameFile,
    SendMail,
    SendMessage,
    ServiceExecute,
    SetFileDate,
    URLDownload,
    URLExecute,
    URLFetch,
    URLFetchAsynchronous,
    URLRead,
    URLSave,
    URLSaveAsynchronous,
    URLSubmit,
    Write,
    WriteLine,
    WriteString
];

$breakoutSymbols = HoldComplete[
    Package`ActivateLoad,
    PacletManager`Package`getPacletWithProgress
];

$evaluatorDebug = False;
$sandboxAbort   = True;
$printCount     = 0;

override := Function[ sym,
    With[ { sb = Symbol[ "Wolfram`CodeEquivalenceUtilities`SystemSandbox`" <> SymbolName @ Unevaluated @ sym ] },
        Unprotect @ sym;
        ClearAttributes[ sym, ReadProtected ];
        PrependTo[ DownValues @ sym,
                   HoldPattern[ sym[ args___ ] /; ! TrueQ @ WhiteListedPatternQ @ sym[ args ] ] :> (
                       Internal`StuffBag[ $Sandboxed, HoldComplete @ sym[ args ] ];
                       If[ TrueQ @ $evaluatorDebug,
                           If[ $printCount++ < 3,
                           With[ { handle = Unique[ ] },
                               handle = Stack[ _ ];
                               Print[
                                   "Sandboxed: ",
                                   HoldComplete @ sym[ args ],
                                   Iconize[ Unevaluated @ handle, "Stack Trace" ]
                               ]
                           ] ];
                       ];
                       If[ TrueQ @ $sandboxAbort,
                           Throw[ HoldComplete @ sym[ args ], $SandboxException ]
                       ];
                       sb[ args ]
                   )
        ];
        Protect @ sym;
    ],
    { HoldAllComplete }
];

breakoutOverride = Function[
    Unprotect @ #;
    #[ args___ ] /; ! TrueQ @ caughtBreakoutQ @ HoldComplete @ #[ args ] :=
      (
          caughtBreakoutQ[ HoldComplete @ #[ args ] ] = True;
          Throw[ HoldComplete @ #[ args ], $BreakoutTag ]
      )
];
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::VariableError::Block:: *)
breakoutEvaluator := Replace[
    $breakoutSymbols,
    HoldComplete[ syms___ ] :> Function[
        eval,
        Internal`InheritedBlock[ { syms }, breakoutOverride /@ { syms }; eval ],
        HoldAllComplete
    ]
];

(*revertRules :=
  List @@ Replace[ $blackList,
      s_ :> RuleCondition[
          ToExpression[
              "CodeEquivalenceUtilities`SystemSandbox`" <> SymbolName@Unevaluated@s,
              InputForm, HoldPattern] :> s], {1}];*)

revertRules := {};


evaluator := evaluator =
  With[ { o = override, r = revertRules },
      WhiteListedPatternQ;
      Replace[ $blackList, HoldComplete[syms___] :>
            Function[eval,
                Block[ { $Sandboxed, $printCount = 0 },
                $Sandboxed = Internal`Bag[ ];

                With[{result = Internal`InheritedBlock[{syms}, ReleaseHold[o /@ HoldComplete[syms]]; StackBegin @ eval]},

                    If[ Internal`BagLength[$Sandboxed] > 0,
                        Message[ EvaluateSafely::unsafe, HoldForm @ # ] & /@ Internal`BagPart[ $Sandboxed, All ];
                    ];

                    HoldComplete[ result ] /. r
                ]
                ],
                {HoldAllComplete}
            ]
      ]
  ];


GetEvaluator // ClearAll;
GetEvaluator // Attributes = { };

GetEvaluator[ ___ ] := evaluator;



preserveContexts // ClearAll;
preserveContexts // Attributes = { HoldAllComplete };

preserveContexts[ eval_ ] :=
  Module[ { original },
      Internal`WithLocalSettings[
          original = { $Context, $ContextPath },
          eval,
          { $Context, $ContextPath } = original
      ]
  ];



EvaluateSafely::unsafe = "Sandboxed the following expressions: `1`";

(*EvaluateSafely[ expr_, opts: OptionsPattern[ ] ] :=
  With[ { retryCount = OptionValue[ "RetryCount" ] },
      Catch[
          Catch[
              breakoutEvaluator @ Catch[
                  Catch @ ReleaseHold @ Check[
                      preserveContexts @ evaluator @ iEvaluateSafely[ expr, opts ],
                      HoldComplete @ SandboxException @ expr,
                      EvaluateSafely::unsafe
                  ],
                  $SandboxException,
                  SandboxException @ expr
              ],
              $Untagged,
              SandboxThrowException[ retryCount, expr, opts ]
          ],
          $BreakoutTag,
          retryWithBreakoutEval[ retryCount, expr, opts ]
      ]
  ];*)

EvaluateSafely[ expr_, opts: OptionsPattern[ ] ] :=
  Module[ { count, expanded, evaluated },
      WhiteListedPatternQ;
      count = OptionValue[ "RetryCount" ];
      expanded = expandURLs @ HoldComplete @ expr;
      evaluated = Replace[ expanded,
                           HoldComplete[ e_ ] :>
                             vetoMessages @
                               evaluateWithThrowCatchHandlers[ e, count, opts ]
                  ];

      Replace[
          evaluated,
          SandboxException[
              _,
              "CaughtEvaluation" -> HoldComplete @ Throw[ _, $Untagged ]
          ] :> evaluateWithThrowCatchHandlers[ expr, count, opts ]
      ]
  ];


vetoMessages // Attributes = { HoldAllComplete };
vetoMessages[ eval_ ] :=
  Internal`HandlerBlock[ { "Message.Veto", veto }, eval ];


veto[ Hold[ MessageName[ Erosion|Dilation, "arg1" ], _ ] ] := False;
veto[ ___ ] := True;


expandURLs[ expr_ ] :=
  expr /. url_String? UStringQ /; StringStartsQ[ url, "https://wolfr.am/" ] :>
    RuleCondition @ Wolfram`CodeEquivalenceUtilities`CanonicalForms`Rules`expandURL @ url;

(* wtf? *)
evaluateWithThrowCatchHandlers // Attributes = { HoldAllComplete };

evaluateWithThrowCatchHandlers[ expr_, retryCount_, opts: OptionsPattern[ ] ] :=
  catchThrowTagged @ Catch[
      Catch[
          Catch[
              breakoutEvaluator @ Catch[
                  Catch @ ReleaseHold @ Check[
                      preserveContexts @ evaluator @ iEvaluateSafely[ expr, opts ],
                      HoldComplete @ SandboxException @ expr,
                      EvaluateSafely::unsafe
                  ],
                  $SandboxException,
                  SandboxException @ expr
              ],
              $Untagged,
              SandboxThrowException[ retryCount, expr, opts ]
          ],
          $BreakoutTag,
          retryWithBreakoutEval[ retryCount, expr, opts ]
      ],
      _,
      SandboxThrowException[ retryCount, expr, opts ]
  ];

catchThrowTagged // Attributes = { HoldAllComplete };
catchThrowTagged[ eval_ ] :=
  Catch @ PreemptProtect @ Internal`InheritedBlock[ { Throw, Catch },
      Unprotect @ { Throw, Catch };
      PrependTo[ DownValues @ Throw, HoldPattern @ Throw[ x_ ] :> Throw[ x, $Untagged ] ];
      PrependTo[ DownValues @ Catch, HoldPattern @ Catch[ x_ ] :> Catch[ x, $Untagged ] ];
      Protect @ { Throw, Catch };
      Block[ { catchThrowTagged = ## & },
          Catch[ eval, $Untagged, SandboxThrowException @ eval ]
      ]
  ];


checkAbort // Attributes = { HoldAllComplete };
checkAbort[ eval_ ] :=
  Block[ { checkAbort = ## & },
      CheckAbort[
          eval,
          Throw[ HoldComplete @ Abort[ ], $SandboxException ]
      ]
  ];

SandboxThrowException // Attributes = { HoldAllComplete };
SandboxThrowException[ expr_ ] :=
  Function[ Null,
            SandboxException[ expr,
                              "CaughtEvaluation" -> HoldComplete @ Throw @ ##
            ],
            HoldAllComplete
  ];

SandboxThrowException[ retries_, expr_, opts___ ] :=
  Function[
      Null,
      Module[ { newOpts },
          If[ TrueQ @ Positive @ retries
              ,
              newOpts = DeleteDuplicatesBy[
                  { "RetryCount" -> retries-1, opts },
                  ToString @* First
              ];
              With[ { no = newOpts }, EvaluateSafely[ expr, no ] ]
              ,
              SandboxException[ expr,
                  "CaughtEvaluation" -> HoldComplete @ Throw @ ##
              ]
          ]
      ],
      { HoldAllComplete }
  ];

retryWithBreakoutEval // Attributes = { HoldAllComplete };
retryWithBreakoutEval[ retries_, expr_, opts___ ][ breakout_, $BreakoutTag ] :=
  Module[ { newOpts },
      If[ TrueQ @ Positive @ retries
          ,
          ReleaseHold @ breakout;
          newOpts = DeleteDuplicatesBy[
              { "RetryCount" -> retries-1, opts },
              ToString @* First
          ];
          EvaluateSafely[ expr, ## ] & @@ newOpts
          ,
          HoldComplete @ SandboxException @ expr
      ]
  ];

(******************************************************************************)



(* Forces referential transparency during evaluation *)

FixValue // Attributes = { HoldAllComplete };
FixValue // Options    = { };


FixValue[ sym_Symbol, eval_ ] :=

    Module[ { def },

      Internal`InheritedBlock[ { sym },

        Unprotect @ sym;

        ClearAttributes[ sym, { Protected, ReadProtected } ];

        def = Language`ExtendedDefinition[ sym, "ExcludedContexts" -> { } ];

        Language`ExtendedDefinition[ sym ] =
            Replace[ def,
                     HoldPattern[ lhs_ :> rhs_ ] :> lhs :> Once @ rhs,
                     { 5 }
            ];

        eval
      ]

    ];


FixValue[ { sym_Symbol, syms___ }, eval_ ] :=
  FixValue[ { syms }, FixValue[ sym, eval ] ];


FixValue[ { }, eval_ ] :=
  eval;



(******************************************************************************)



(*$sec = GeneralUtilities`Code`PackagePrivate`$SideEffectCategories;
$sec["Abort"] = {"Abort", "AbortKernels", "AbortProtect",
    "AbortScheduledTask", "AbortWithStack", "CheckAbort", "$Aborted"};
$sec["Kernel"] = {"AbortKernels", "AllowKernelInitialization",
    "CloseKernels", "KernelExecute", "Kernels", "KernelSharedQ",
    "LaunchKernels", "ShareKernel", "UnshareKernel",
    "$ConfiguredKernels", "$KernelCount", "$KernelID"};
AppendTo[$sec["IO"], "Echo"];
AppendTo[$sec["Print"], "Echo"];

$ses = Union @@ Values[$sec];*)

(*getSideEffectWarnings // ClearAll;
getSideEffectWarnings // Attributes = {HoldAllComplete};

getSideEffectWarnings[exp___] :=
  Inline[$sec,
      Module[{symNames},
          symNames =
            Union[Cases[HoldComplete[exp],
                s_Symbol :> SymbolName[Unevaluated[s]], Infinity,
                Heads -> True]];
          DeleteCases[Intersection[symNames, #] & /@ $sec, {}]
      ]];



Inline[$ses,
    unsafeQ // ClearAll;
    unsafeQ // Attributes = {HoldAllComplete};

    unsafeQ[sym_Symbol] := MemberQ[$ses, SymbolName[Unevaluated[sym]]];
    unsafeQ[sym_String] := MemberQ[$ses, sym];

    safeQ // ClearAll;
    safeQ // Attributes = {HoldAllComplete};

    safeQ[exp_] := FreeQ[Unevaluated[{exp}], s_Symbol?unsafeQ];
    unsafeQ[exp_] := ! safeQ[exp];

    safeQ[exp_, lvl_] :=
      FreeQ[Unevaluated[{exp}], s_Symbol?unsafeQ, lvl];
    unsafeQ[exp_, lvl_] := ! safeQ[exp, lvl];
];

safeEvaluate // ClearAll;
safeEvaluate // Attributes = {HoldAllComplete};

safeEvaluate[expr_] := ReleaseHold[HoldComplete[expr] /.
  s_Symbol?unsafeQ :> HoldForm[s]
];



$emptyDefinition =
  HoldPattern[_ -> {OwnValues -> {}, SubValues -> {}, UpValues -> {},
      DownValues -> {}, NValues -> {}, FormatValues -> {},
      DefaultValues -> {}, Messages -> {}, Attributes -> {___}}];



checkDefinition // ClearAll;
checkDefinition // Attributes = {HoldAllComplete};

checkDefinition[HoldForm[symbol_] -> {definitions__}] :=
  Module[{definitionValues, symbolString},
      definitionValues = HoldComplete[definitions][[All, 2]];
      symbolString = ToString[Unevaluated[symbol], InputForm];
      symbolString -> getSideEffectWarnings @@ definitionValues
  ];*)


(*
GetSafetyWarnings // ClearAll;
GetSafetyWarnings // Attributes = {HoldAllComplete};

GetSafetyWarnings[expr___] :=
  Module[{tempSym, fullDef, rescoped, newSymbolReplacements,
      oldSymbolReplacements, heldCleanup, $Remove, newFullDefinition,
      newTempSym, newReducedFullDefinition, restoredSymbols, warnings,
      tempSymString},
      tempSym = HoldComplete[expr];

      fullDef = Language`ExtendedFullDefinition[tempSym];

      rescoped = CanonicalScopeRuleDelayed[fullDef];

      newSymbolReplacements =
        Cases[rescoped[[All, 1]],
            HoldForm[s_Symbol] :>
              With[{ns = NewLocalSymbol[s]}, HoldPattern[s] :> ns]];
      oldSymbolReplacements =
        Replace[newSymbolReplacements,
            HoldPattern[RuleDelayed[Verbatim[HoldPattern][old_], new_]] :>
              HoldPattern[new] :> old, {1}];
      Set[Attributes[#], {}] & /@ newSymbolReplacements[[All, 2]];

      heldCleanup =
        With[{removal = $Remove @@ newSymbolReplacements[[All, 2]]},
            HoldComplete[removal]] /. $Remove -> Remove;

      newFullDefinition = rescoped /. newSymbolReplacements;

      Language`ExtendedFullDefinition[] = newFullDefinition;

      newTempSym = HoldComplete[tempSym] /. newSymbolReplacements;
      newReducedFullDefinition =
        DeleteCases[
            Language`ExtendedFullDefinition @@ newTempSym, $emptyDefinition];
      restoredSymbols = newReducedFullDefinition /. oldSymbolReplacements;

      warnings = Association @@ (checkDefinition /@ restoredSymbols);

      ReleaseHold[heldCleanup];

      tempSymString = ToString[Unevaluated[tempSym], InputForm];

      <|
          "TopLevel" -> warnings[tempSymString],
          "Dependencies" -> DeleteCases[KeyDrop[warnings, tempSymString], <||>]
      |>
  ];*)



(* TODO: change arguments of an unsafe holding function to use second argument
         of Unsafe instead of converting everything to a string.
         *)

(*Unsafe // ClearAll;
Unsafe // Attributes = {HoldAllComplete, SequenceHold};
Unsafe[expr : Except[_String]] :=
  With[{s = ToFullString[expr]}, Unsafe[s]];



iMakeSafe // ClearAll;
iMakeSafe // Attributes = {HoldAllComplete, SequenceHold};

iMakeSafe[expr_, wrapper_: HoldComplete] :=
  Module[{hexp, warnings, unsafeSymbols, safeHeld, allowedAssignments},
      hexp = HoldComplete[expr];
      warnings = GetSafetyWarnings[expr];
      unsafeSymbols = Keys[warnings["Dependencies"]];
      safeHeld = hexp /. {
          e : (s_Symbol[___]) /;
            HoldingQ[s] &&
              MemberQ[$ses, ToString[Unevaluated[s], InputForm]] :>
            TrEval@Unsafe[e],
          s_Symbol /;
            MemberQ[$ses, ToString[Unevaluated[s], InputForm]] :>
            TrEval@Unsafe[s],
          e : (s_Symbol[___]) /;
            HoldingQ[s] &&
              MemberQ[unsafeSymbols,
                  ToString[Unevaluated[s], InputForm]] :> TrEval@Unsafe[e],
          s_Symbol /;
            MemberQ[unsafeSymbols, ToString[Unevaluated[s], InputForm]] :>
            TrEval@Unsafe[s]
      };
      allowedAssignments = StripTempHolds[
          safeHeld /.
            HoldPattern[Unsafe[str_String]] /;
              StringMatchQ[str, ("Set[" | "SetDelayed[") ~~ __ ~~ "]"] :>
              TrEval@
                Replace[ToExpression[str, StandardForm, HoldComplete],
                    HoldComplete[(s : Set | SetDelayed)[a_, b_]] :>
                      With[{sa = iMakeSafe[a, TempHold],
                          sb = iMakeSafe[b, TempHold]}, TempHold[s[sa, sb]]]]
      ];
      wrapper @@ allowedAssignments
  ];


MakeSafe // Attributes = {HoldAllComplete, SequenceHold};

MakeSafe[expr___] :=
  Module[{safe, renamed},
      safe = iMakeSafe[Sequence[expr], HoldComplete];
      renamed =
        safe /. Replace[
            DeleteCases[ExtractSymbols[{safe}, HoldComplete],
                HoldComplete[Unsafe | TempHold]],
            HoldComplete[s_] :> HoldPattern[s] -> NewLocalSymbol[s], {1}];
      renamed
  ];


EvaluateSafely // Attributes = {HoldAllComplete};
EvaluateSafely[expr___] :=
  If[ Values[GetSafetyWarnings[expr]] =!= {<||>, <||>}
      ,
      SandboxViolation[ ## & @@ MakeSafe[Sequence[expr]] ]
      ,
      ## & @@ MakeSafe[Sequence[expr]]
];



WithHolding // Attributes = { HoldAllComplete };
WithHolding // Options    = { };


WithHolding /: patt_ :> WithHolding[ { sets___Set, set_Set }, expr_ ] :=
  patt :> WithHolding[ { sets }, With[ { set }, expr /; True ] ];


WithHolding /: patt_ :> WithHolding[ { }, expr_ ] := patt :> expr;*)

(* :!CodeAnalysis::EndBlock:: *)

End[ ]; (* `Private` *)

EndPackage[ ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
