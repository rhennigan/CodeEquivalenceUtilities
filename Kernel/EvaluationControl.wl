(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
$AllowedEvaluationPatterns;
$Memoize;
$RandomSymbols;
$UnsafeSymbolPattern;
$UnsafeSymbols;
EvaluatedQ;
EvaluateSafely;
FixValue;
SafeEvaluatedQ;
SafeExpressionQ;
SandboxException;
Unsafe;
UnsafeSymbols;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$Memoize              = True;
$MaxQuantityArraySize = 25;
$evaluatorDebug       = False;
$sandboxAbort         = True;
$printCount           = 0;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$AllowedEvaluationPatterns*)
$AllowedEvaluationPatterns = Automatic;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*allowEvaluations*)
allowEvaluations // Attributes = { HoldRest };
allowEvaluations[ allow_, eval_ ] :=
    Internal`InheritedBlock[
        { $allowedEvaluationPatterns },
        Block[
            {
                $evalPatternBlock          = True,
                $AllowedEvaluationPatterns = toAllowedEvaluationPattern @ allow
            },
            eval
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$allowedEvaluationPatterns*)
$allowedEvaluationPatterns :=
    With[ { p = toAllowedEvaluationPattern @ $AllowedEvaluationPatterns },
        If[ TrueQ @ $evalPatternBlock,
            $allowedEvaluationPatterns = p,
            p
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toAllowedEvaluationPattern*)
toAllowedEvaluationPattern[ Automatic ] := Alternatives[ ];
toAllowedEvaluationPattern[ { p___ }  ] := HoldPattern @ Alternatives @ p;
toAllowedEvaluationPattern[ p___      ] := HoldPattern @ Alternatives @ p;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$Optimizations*)
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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*EvaluatedQ*)
EvaluatedQ // Attributes = { HoldAllComplete };

EvaluatedQ[ x___ ] :=
    With[ { res = With[ { x$ = x }, HoldComplete @ x$ === HoldComplete @ x ] },
        If[ $Memoize, EvaluatedQ[ x ] = res, res ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SafeEvaluatedQ*)
SafeEvaluatedQ // Attributes = { HoldAllComplete };

SafeEvaluatedQ[ x___ ] :=
    With[
        {
            res = With[
                {
                    x$ = EvaluateSafely[
                        x,
                        "RemoveTypes" -> False,
                        "Timeout"    -> 0.01
                    ]
                },
                If[ Head @ x$ === SandboxViolation,
                    SandboxViolation,
                    HoldComplete @ x$ === HoldComplete @ x
                ]
            ]
        },
        If[ $Memoize, SafeEvaluatedQ[ x ] = res, res ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SandboxViolation*)
SandboxViolation // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$UnsafeSymbols*)
$UnsafeSymbols := $UnsafeSymbols = unsafeSymbols[ ];

unsafeSymbols[ ] := Union[ Union @@ Values @ $sec, $additionalUnsafe ];

allSymbols := allSymbols = Names[ "*" ];

$sec := DeleteCases[
    fastNames /@ $unsafeNameData,
    Alternatives @@ falsePositives,
    Infinity
];

fastNames[ pats0_ ] :=
    With[ { pats = (___ ~~ #1 ~~ ___ &) /@ pats0 },
        Flatten @ StringCases[ allSymbols, pats ]
    ];

$unsafeNameData = <|
    "Evaluation"  -> { "Abort" },
    "Cloud"       -> { "Cloud", "Permissions" },
    "Compile"     -> { "Compile" },
    "Compress"    -> { "Compress", "Uncompress", "CompressedData" },
    "Context"     -> { "Begin", "BeginPackage", "End", "EndPackage" },
    "Create"      -> { "Create" },
    "Data"        -> { "Data", "EntityValue" },
    "Device"      -> { "Device" },
    "Dialog"      -> { "Dialog" },
    "Dynamic"     -> { "Dynamic" },
    "File"        -> { "File", "Directory", "Path", "FindList" },
    "Format"      -> { "Format", "MakeBoxes", "ToBoxes", "RawBoxes",
                       "Typeset`ToExpression"
                     },
    "Frontend"    -> { "Notebook", "Frontend", "FrontEnd", "Clipboard",
                       "CurrentImage", "CurrentValue", "AbsoluteOptions",
                       "FullOptions", "SelectionEvaluate", "Paste"
                     },
    "Introspect"  -> { "ToExpression", "MakeExpression", "Attributes", "Stack",
                       "Definition", "FullDefinition", "Information",
                       "DefaultValues", "DownValues", "DynamicModuleValues",
                       "FormatValues", "NValues", "OwnValues", "SubValues",
                       "UpValues", "Stack", "Trace"
                     },
    "IO"          -> { "Print", "Put", "Get", "Needs", "Dump", "Save", "Import",
                       "Export", "Splice", "Encode"
                     },
    "Java"        -> { "Java", "AddToClassPath" },
    "LibraryLink" -> { "Library" },
    "Link"        -> { "Link", "Java" },
    "Mutate"      -> { "Set", "Get", "To", "Protect", "Unprotect", "Clear",
                       "Remove"
                     },
    "Network"     -> { "URL", "HTTP" },
    "Pause"       -> { "Pause" },
    "Paclet"      -> { "Paclet" },
    "Parallel"    -> { "Parallel", "Kernel", "SetShared", "Distribute" },
    "Print"       -> { "Print", "Echo" },
    "Process"     -> { "Run", "RunThrough", "Process", "Install" },
    "Quit"        -> { "Quit" },
    "Send"        -> { "Send" },
    "Stream"      -> { "Stream", "Open", "Close", "Read", "Write", "Find",
                       "Skip"
                     },
    "Symbol"      -> { "Symbol" },
    "System"      -> { "System", "SetEnvironment" },
    "Task"        -> { "Task" }
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*False positives*)
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*False negatives*)
$additionalUnsafe = { "DeleteObject", "WordList", "WordData" };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$UnsafeSymbolPattern*)
$UnsafeSymbolPattern :=
  Replace[ Flatten[ HoldComplete @@ ToExpression[ $UnsafeSymbols,
                                                  StandardForm,
                                                  HoldComplete
                                    ]
           ],
           HoldComplete[ a___ ] :> HoldPattern @ Alternatives @ a
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SafeExpressionQ*)
SafeExpressionQ // Attributes = { HoldAllComplete };

SafeExpressionQ[ expr_ ] :=
    Module[ { hexpr },

        hexpr = DeleteCases[
            HoldComplete @ expr,
            _TypedSymbol,
            Infinity,
            Heads -> True
        ];

        FreeQ[ FullDefinitionData @ hexpr, $UnsafeSymbolPattern ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$RandomSymbols*)
$RandomSymbols = {
    "AbsoluteTime",
    "Date",
    "DateList",
    "DateObject",
    "DateRange",
    "DateString",
    "DateValue",
    "LocalTime",
    "Now",
    "RandomChoice",
    "RandomColor",
    "RandomComplex",
    "RandomEntity",
    "RandomFunction",
    "RandomGraph",
    "RandomImage",
    "RandomInteger",
    "RandomPermutation",
    "RandomPoint",
    "RandomPrime",
    "RandomReal",
    "RandomSample",
    "RandomSeed",
    "RandomVariate",
    "RandomWalkProcess",
    "RandomWord",
    "SessionTime",
    "SiderealTime",
    "TimeObject",
    "TimeValue",
    "Today",
    "Tomorrow",
    "UnixTime",
    "Yesterday",
    "Wolfram`CodeEquivalenceUtilities`RandomValue"
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*UnsafeSymbols*)
UnsafeSymbols // Attributes = { HoldAllComplete };
UnsafeSymbols // Options    =
  {
      "Definitions" -> Automatic,
      "SymbolList"  :> $UnsafeSymbols,
      "Seed"        :> $SessionID,
      "RemoveTypes" -> True,
      "Timeout"     -> None,
      "RetryCount"  -> None
  };

UnsafeSymbols[
    f_? SymbolQ,
    opts: OptionsPattern @ { UnsafeSymbols, EvaluateSafely }
] :=

  With[
      {
          symbols = OptionValue @ "SymbolList",
          definitions =
            Replace[ OptionValue @ "Definitions",
                {
                    Automatic   -> Language`ExtendedDefinition,
                    Full        -> Language`ExtendedFullDefinition
                }
            ]
      },

      Union @ Cases[
          Replace[ deleteWhiteListed @ definitions @ f,
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
                             TempHold[ ov, sv, uv, dv, nv, fv, defv, mv, a ]
                           ,
                           { 1 }
                  ]
          ,
          ( s_? SymbolQ /; MemberQ[ symbols, SymbolName @ Unevaluated @ s ]
          ) :> ToFullString[ s, "Context" -> None, "ContextPath" -> None ]
          ,
          Infinity,
          Heads -> True
      ]
  ];

deleteWhiteListed[ expr_ ] :=
    DeleteCases[
        DeleteCases[
            expr,
            HoldPattern @ Verbatim[ HoldPattern ][
                MakeBoxes[ _? LocalContextQ, StandardForm ]
            ] :> _,
            { 5 }
        ],
        _? WhiteListedPatternQ,
        { 2, Infinity },
        Heads -> True
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Unsafe*)
Unsafe // Attributes = { HoldAllComplete };

Unsafe[ s_? SymbolQ, { args___ }, holdingQ_? BooleanQ ] :=
    With[ { name = SymbolName @ Unevaluated @ s },
        If[ TrueQ @ holdingQ,
            Unsafe[ name, HoldComplete @ args ],
            Unsafe[ name, { args } ]
        ]
    ];

Unsafe[ s_? SymbolQ ] :=
    With[ { name = SymbolName @ Unevaluated @ s }, Unsafe @ name ];

Unsafe[ s_? SymbolQ, { args___ } ] :=
    Unsafe[ s, { args }, HoldingQ @ s ];

Unsafe[ name_String ] :=
    If[ TrueQ @ HoldingQ @ name,
        Function[
            Null,
            CatchUnsafe[ name, HoldComplete @ ##1 ],
            { HoldAllComplete }
        ],
        Function[ Null, CatchUnsafe[ name, { ##1 } ], { } ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CatchUnsafe*)
CatchUnsafe[ name_, Alternatives[ List, HoldComplete ][ $null$ ] ] :=
    With[ { unsafe = Unsafe @ name },
        Internal`StuffBag[ $captured, unsafe ];
        unsafe
    ];

CatchUnsafe[ args___ ] :=
    With[ { unsafe = Unsafe @ args },
        Internal`StuffBag[ $captured, unsafe ];
        unsafe
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*EvaluateSafely*)
EvaluateSafely // Attributes = { HoldAllComplete };

EvaluateSafely // Options = {
    "Definitions"               -> Full,
    "SymbolList"                :> $UnsafeSymbols,
    "Seed"                      :> $SessionID,
    "RemoveTypes"               -> True,
    "Timeout"                   -> 30,
    "MemoryConstraint"          -> 2^24,
    "RetryCount"                -> 5,
    "AllowedEvaluationPatterns" :> $AllowedEvaluationPatterns
};

EvaluateSafely::unsafe = "Sandboxed the following expressions: `1`";

EvaluateSafely[ expr_, opts: OptionsPattern[ ] ] := allowEvaluations[
    OptionValue[ "AllowedEvaluationPatterns" ],
    Module[ { count, expanded, evaluated },
        WhiteListedPatternQ;
        count    = OptionValue[ "RetryCount" ];
        expanded = expandURLs @ HoldComplete @ expr;

        evaluated = Replace[
            expanded,
            HoldComplete[ e_ ] :>
                vetoMessages @ evaluateWithThrowCatchHandlers[
                    e,
                    count,
                    opts
                ]
        ];

        Replace[
            evaluated,
            SandboxException[
                _,
                "CaughtEvaluation" -> HoldComplete @ Throw[ _, $Untagged ]
            ] :> evaluateWithThrowCatchHandlers[ expr, count, opts ]
        ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*expandURLs*)
expandURLs[ expr_ ] :=
    ReplaceAll[
        expr,
        url_String? UStringQ /; StringStartsQ[ url, "https://wolfr.am/" ] :>
            RuleCondition @ expandURL @ url
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*vetoMessages*)
vetoMessages // Attributes = { HoldAllComplete };
vetoMessages[ e_ ] := Internal`HandlerBlock[ { "Message.Veto", veto }, e ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*veto*)
veto[ Hold[ MessageName[ Erosion|Dilation, "arg1" ], _ ] ] := False;
veto[ ___ ] := True;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*evaluateWithThrowCatchHandlers*)
evaluateWithThrowCatchHandlers // Attributes = { HoldAllComplete };

evaluateWithThrowCatchHandlers[ expr_, retryCount_, opts: OptionsPattern[ ] ] :=
    catchThrowTagged @ Catch[
        Catch[
            Catch[
                breakoutEvaluator @ Catch[
                    Catch @ ReleaseHold @ Check[
                        preserveContexts @ evaluator @
                            iEvaluateSafely[ expr, opts ]
                        ,
                        HoldComplete @ SandboxException @ expr
                        ,
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchThrowTagged*)
catchThrowTagged // Attributes = { HoldAllComplete };
catchThrowTagged[ eval_ ] :=
    Catch @ PreemptProtect @ Internal`InheritedBlock[ { Throw, Catch },

        Unprotect @ { Throw, Catch };

        PrependTo[
            DownValues @ Throw,
            HoldPattern @ Throw[ x_ ] :> Throw[ x, $Untagged ]
        ];

        PrependTo[
            DownValues @ Catch,
            HoldPattern @ Catch[ x_ ] :> Catch[ x, $Untagged ]
        ];

        Protect @ { Throw, Catch };

        Block[ { catchThrowTagged = ##1 & },
            Catch[ eval, $Untagged, SandboxThrowException @ eval ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*breakoutEvaluator*)
(* :!CodeAnalysis::Disable::VariableError::Block:: *)
breakoutEvaluator := Replace[
    $breakoutSymbols,
    HoldComplete[ syms___ ] :> Function[
        eval,
        Internal`InheritedBlock[ { syms }, breakoutOverride /@ { syms }; eval ],
        HoldAllComplete
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$breakoutSymbols*)
$breakoutSymbols = HoldComplete[
    Package`ActivateLoad,
    PacletManager`Package`getPacletWithProgress
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*breakoutOverride*)
breakoutOverride = Function[
    Unprotect @ #;
    #[ args___ ] /; ! TrueQ @ caughtBreakoutQ @ HoldComplete @ #[ args ] :=
      (
          caughtBreakoutQ[ HoldComplete @ #[ args ] ] = True;
          Throw[ HoldComplete @ #[ args ], $BreakoutTag ]
      )
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*preserveContexts*)
preserveContexts // Attributes = { HoldAllComplete };

preserveContexts[ eval_ ] :=
  Module[ { original },
      Internal`WithLocalSettings[
          original = { $Context, $ContextPath },
          eval,
          { $Context, $ContextPath } = original
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*iEvaluateSafely*)
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

      eval = PreemptProtect @ Quiet @ Block[ { PrintTemporary },
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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*removeTypes*)
(* :!CodeAnalysis::Disable::ParameterError:: *)
removeTypes // Attributes = { HoldAllComplete };

removeTypes[ expr_ ] :=
    Quiet[
        ReplaceAll[
            expr,
            {
                HoldPattern @ TypedSymbol[
                    Verbatim[ Pattern ][ symbol_, Verbatim[ Blank ][ ] ],
                    Verbatim[ Verbatim ][ patt_ ]
                ] :> symbol: patt,
                HoldPattern @ TypedSymbol[ s_? SymbolQ, _ ] :> TrEval @ s
            }
        ],
        RuleDelayed::rhs
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*makeReplacementRules*)
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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*evalRestricted*)
evalRestricted // Attributes = { HoldAllComplete };
evalRestricted[ expr_, timeLimit_, memoryLimit_, input_ ] :=
    checkAbort @ TimeConstrained[
        expr,
        timeLimit,
        TimedOut @ HoldForm @ input
    ];

TimedOut /: HoldPattern @ FailureQ[ _TimedOut ] := True;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*checkAbort*)
checkAbort // Attributes = { HoldAllComplete };
checkAbort[ eval_ ] :=
  Block[ { checkAbort = ## & },
      CheckAbort[
          eval,
          Throw[ HoldComplete @ Abort[ ], $SandboxException ]
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*SandboxThrowException*)
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*retryWithBreakoutEval*)
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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*GetEvaluator*)
GetEvaluator[ ___ ] := evaluator;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*evaluator*)
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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*override*)
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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*revertRules*)
revertRules := { };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$blackList*)
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
    WriteString,
    Developer`ReadExpressionJSONFile,
    Developer`ReadRawJSONFile,
    Developer`ReadUBJSONFile,
    Developer`ReadWXFFile,
    Developer`WriteExpressionJSONFile,
    Developer`WriteRawJSONFile,
    Developer`WriteUBJSONFile,
    Developer`WriteWXFFile
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SandboxException*)
SandboxException // Attributes = { HoldAllComplete };

SandboxException[ eval_ ][ s_String, $SandboxException ] := (
    Message[ EvaluateSafely::unsafe, s ];
    SandboxException[ eval, "CaughtSymbol" -> Unsafe @ s ]
);

SandboxException[ eval_ ][ exc_HoldComplete, $SandboxException ] := (
    Message[ EvaluateSafely::unsafe, exc ];
    SandboxException[ eval, "CaughtEvaluation" -> exc ]
);

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WhiteListedPatternQ*)
WhiteListedPatternQ // Attributes = { HoldAllComplete };

WhiteListedPatternQ := (
    WhiteListedPatternQ // ClearAll;
    WhiteListedPatternQ // Attributes = { HoldAllComplete };

    Replace[ $WhiteListedPatterns,
             patt_ :> (HoldPattern @ WhiteListedPatternQ[ patt ] := True),
             { 1 }
    ] // ReleaseHold;

    WhiteListedPatternQ[ p___ ] := MatchQ[
        HoldComplete @ p,
        _[ $allowedEvaluationPatterns ]
    ];

    WhiteListedPatternQ
);

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$WhiteListedPatterns*)
$WhiteListedPatterns := (Quiet[ ResourceObject; LocalObject ];
With[ {
      resourceDir = (Symbol["ResourceSystemClient`Private`resourceCacheDirectory"][ ]),
      persistenceRoot = LocalObjects`PathName @ LocalObject @ $PersistenceBase[[ 2 ]],
      cloudFilesAPI = rebasedCloudURLs @ Alternatives[ "https://www.wolframcloud.com/files" ],
      exampleDir = $exampleDirectory
  },
  With[ {
      ctx = $contextPattern,
      domains = Alternatives[
          $safeURLs,
          whitelistDomain[ "*.wikipedia.org" ],
          whitelistDomain[ "*.wolframalpha.com" ],
          whitelistDomain[ "*.wolfram.com" ]
      ],
      pathExamples = whitelistPath[ "ExampleData" | exampleDir ],
      pathSystem = whitelistPath[ $InstallationDirectory ],
      pathApps = whitelistPath[ "/wolframcloud/userfiles/WolframApplications" ],
      pathBase = whitelistPath @ $UserBaseDirectory | whitelistPath @ $BaseDirectory,
      pathTemp = whitelistPath[ $TemporaryDirectory | "/tmp/UserTemporaryFiles/public/" | $CacheBaseDirectory ],
      pathPaclets = whitelistPath[ PacletManager`$UserBasePacletsDirectory|"/www/tomcat/webapps/app/data/Paclets/" ],
      pathPacletsTemp = whitelistPath[ FileNameJoin @ { PacletManager`$UserBasePacletsDirectory, "Temporary" } ],
      pathKB = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "Knowledgebase" },
      pathResources = Alternatives[
          "data.MX",
          "put.wl",
          ExpandFileName[ "Resources" ],
          whitelistPath[ "/www/tomcat/home/.Mathematica/LocalObjects/Persistence/NetModelIndex" ],
          whitelistPath @ resourceDir,
          whitelistPath @ ExpandFileName @ resourceDir,
          ".#put-" <> ToString @ $ProcessID <> ".wl"
      ],
      pathSearchIndices = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "SearchIndices" },
      pathResourcePersistence = Alternatives[
          whitelistPath @ FileNameJoin @ { persistenceRoot, "ResourceNames" },
          whitelistPath @ FileNameJoin @ { persistenceRoot, "PersistentResourceSystemBases" }
      ],
      pathPersistence = whitelistPath @ persistenceRoot,
      pathLockFiles = whitelistPath @ ResourceSystemClient`FileLocking`$LocalLockDirectory,
      pathCache = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "ApplicationData", "Wolfram", "Index" },
      pathProcLink = whitelistPath @ FileNameJoin @ { $UserBaseDirectory, "ApplicationData", "ProcessLink" },
      pathNMIndex = whitelistPath @ FileNameJoin @ { persistenceRoot, "NetModelIndex" <> StringReplace[ToString[$VersionNumber], "." -> "-"] },
      allowedLibs = $allowedLibs,
      cloudFiles = url_String /; StringStartsQ[ url, cloudFilesAPI~~("/"|"?") ] && StringFreeQ[ url, ("&"|"?")~~"properties=" ],
      rurl = resourceURL,
      persistence = _String? (StringStartsQ[ First @ CloudObject[ "Persistence/ResourceNames" ] ]),
      urlFetch = (URLDownload|URLExecute|URLFetch|URLFetchAsynchronous|URLRead|URLSave|URLSaveAsynchronous|URLSubmit),
      shortURL = "https://wolfr.am/expand" | HoldPattern @ HTTPRequest[ KeyValuePattern @ { "Domain" -> "wolfr.am", "Path" -> "expand" }, ___ ],
      allowedFetchOptions = Except[ (Rule|RuleDelayed)[ "Method"|Method, Except[ "GET" ] ] ] ...,
      allowedFetchElements = $fetchElement
  },
  {
      $pathReadable = HoldPattern @ Alternatives[
            pathApps,
            pathBase,
            pathCache,
            pathExamples,
            pathKB,
            pathLockFiles,
            pathNMIndex,
            pathPaclets,
            pathPersistence,
            pathProcLink,
            pathResourcePersistence,
            pathResources,
            pathSystem,
            pathTemp
        ],

      $pathWritable = HoldPattern @ Alternatives[
            pathCache,
            pathKB,
            pathLockFiles,
            pathPaclets,
            pathPacletsTemp,
            pathProcLink,
            pathResourcePersistence,
            pathResources,
            pathSearchIndices,
            pathTemp
        ],

      reader1 = Alternatives[
        BinaryRead,
        BinaryReadList,
        FileHash,
        FileInformation,
        FindFile,
        Get,
        Import,
        OpenRead,
        Read,
        ReadByteArray,
        ReadLine,
        ReadList,
        Developer`ReadExpressionJSONFile,
        Developer`ReadRawJSONFile,
        Developer`ReadUBJSONFile,
        Developer`ReadWXFFile
      ],

      writerNull = Alternatives[
        CreateDirectory,
        CreateFile,
        OpenAppend,
        OpenWrite
      ],

      writer1 = Alternatives[
        BinaryWrite,
        CreateDirectory,
        CreateFile,
        DeleteFile,
        DumpSave,
        Export,
        OpenAppend,
        OpenWrite,
        Write,
        WriteString,
        Developer`WriteExpressionJSONFile,
        Developer`WriteRawJSONFile,
        Developer`WriteUBJSONFile,
        Developer`WriteWXFFile
      ],

      writerLast = Alternatives[
        Put,
        PutAppend
      ]
  },
  {
    $allowedRead  = HoldPattern @ reader1[ $pathReadable | File[ $pathReadable ] | InputStream[ String | $pathReadable, _ ], ___ ],
    $allowedWrite = HoldPattern @ Alternatives[
        writerNull[ ],
        writer1[ $pathWritable | File[ $pathWritable ] | OutputStream[ $pathWritable, _ ], ___ ],
        writerLast[ ___, $pathWritable | File[ $pathWritable ] | OutputStream[ $pathWritable, _ ] ]
    ]
  },
      HoldComplete[
          $allowedRead,
          $allowedWrite,
          DeleteFile[ "data.MX" | "put.wl" | "save.mx" ],
          DeleteFile[ sym_Symbol? SymbolQ ] /; ! FileExistsQ @ sym,
          DumpSave[ "save.mx", NeuralNetworks`Private`NetModel`$NetModelIndexCached ],
          Export[ "data.MX", { 1 }, "Byte" ],
          FileNames[ _, $pathReadable, ___ ],
          FileNames[ "*", { }, 1 ],
          FindFile[ allowedLibs, "AccessPermission" -> "Execute" ],
          FindFile[ "put.wl" | ctx, ___ ],
          Get[ "subicon.m" | "deficon.m" | "HDF5Tools/HDF5Tools.m" | $Failed | "put.wl" | ctx, ___ ],
          Import[ "!cmd /c ver" | "data.MX", ___ ],
          Import[ domains, "Hyperlinks"|"TSV"|"String"|{"Hyperlinks"|"TSV"|"String"}, ___ ],
          LinkWrite[ _, Except[ CallPacket[ _CloudSystem`Private`ServiceDispatch ] ] ],
          LinkWrite[ _, CallPacket @ CloudSystem`Private`ServiceDispatch[ _CloudSystem`CloudObject`DoCloudOperation? safeCloudOperationQ ] ],
          LinkWrite[ link_LinkObject, ___ ] /; MatchQ[ $FrontEnd, HoldPattern @ FrontEndObject @ link ],
          OpenWrite[ in_String /; StringMatchQ[ in, "in:"~~DigitCharacter.. ], ___, Method -> "RunPipe", ___ ],
          Put[ ___, "put.wl" | $Failed | CURLLink`HTTP`Private`getFile @ None ],
          RenameFile[ $pathWritable, $pathWritable, ___ ],
          urlFetch[ cloudFiles | { cloudFiles }, allowedFetchOptions ],
          urlFetch[ cloudFiles | { cloudFiles }, allowedFetchElements, allowedFetchOptions ],
          urlFetch[ (rurl|domains|shortURL) | { rurl|domains|shortURL }, ___ ],
          WriteString[ "stdout", ___ ],
          CloudGet[ _? safeCloudObjectQ | (rurl|persistence) | CloudObject[ (rurl|persistence) ] | URL[ (rurl|persistence) ] ],
          Unprotect[ s_Symbol /; ! StringStartsQ[ SafeContext @ s, "WolframChallenges`"|"Wolfram`CodeEquivalenceUtilities`" ] ],
          URLFetch["https://www.wolframcloud.com/OAuthVersion"|"https://www.wolframcloud.com/app/OAuthVersion", ___],
          URLFetchAsynchronous[ "https://www.wolframcloud.com/files/7918edd1-f8a1-46d0-8b87-7755d5325634", ___ ],
          URLSave[ "https://resources.wolframcloud.com/PacletRepository/pacletsite/PacletSite.mz", ___ ],
          URLRead[ _? authRequestQ, ___ ],
          URLRead[ HTTPRequest[ domains, _ ] | { HTTPRequest[ domains, _ ] }, ___ ],
          (URLSave|URLSaveAsynchronous)[ _? safeCloudObjectQ, $pathWritable, ___ ]
      ]
  ]]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$exampleDirectory*)
$exampleDirectory :=
    With[ { f = FindFile[ "ExampleData/rose.gif" ] },
        If[ FileExistsQ @ f,
            DirectoryName @ f,
            FileNameJoin @ {
                $InstallationDirectory,
                "Documentation/English/System/ExampleData"
            }
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*authRequestQ*)
authRequestQ // Attributes = { HoldFirst };

authRequestQ[
    HTTPRequest[
        Association @ OrderlessPatternSequence[
            "AbsolutePath" -> "https://www.wolframcloud.com/files/auth"|"https://account.wolfram.com/auth/request-token",
            "Query" -> { },
            ___
        ],
        _
    ]
] := True;

authRequestQ[ { _? authRequestQ } ] := True;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*whitelistDomain*)
whitelistDomain // Attributes = { HoldAllComplete };
whitelistDomain[ domain_ ] :=
  Module[ { url },
      Condition @@ HoldComplete[ url_String, StringMatchQ[ URLParse[ url, "Domain" ], domain ] ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*whitelistPath*)
whitelistPath // Attributes = { HoldAllComplete };
whitelistPath[ dir_ ] :=
  Alternatives[
      _String? (StringStartsQ @ dir),
      File[ _String? (StringStartsQ @ dir) ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*resourceURL*)
resourceURL :=
  Module[ { url },
      Condition @@ HoldComplete[ url_String,
          StringStartsQ[ url, "https://www.wolframcloud.com/"~~("obj"|"objects")~~"/resourcesystem/" ] ||
            Quiet @ StringStartsQ[
                CloudObjectInformation[ CloudObject @ url, "Path" ],
                "resourcesystem/"
            ]
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$allowedLibs*)
$allowedLibs = _String? (
    Function[
        Null,
        StringQ @ Unevaluated @ # && StringEndsQ[ #, ".so" | ".dll" ],
        HoldAllComplete
    ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*contextQ*)
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$contextPattern*)
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*urlBuild*)
urlBuild[ args___ ] := URLBuild @ URLParse @ URLBuild @ args;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rebaseCloudURL*)
rebaseCloudURL[ url_ ] :=
  urlBuild @ Append[
      URLParse @ url,
      "Domain" -> URLParse[ $CloudBase, "Domain" ]
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rebasedCloudURLs*)
rebasedCloudURLs[ url_String ] := rebasedCloudURLs @ Alternatives @ url;
rebasedCloudURLs[ urls_ ] := Union[ rebaseCloudURL /@ urls, urls ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*safeCloudObjectQ*)
safeCloudObjectQ // Attributes = { HoldAllComplete };

safeCloudObjectQ[ (URL|CloudObject)[ url_, ___ ] ] := safeCloudObjectQ @ url;

Scan[
    Function[ safeCloudObjectQ[ #1 ] = True ],
    rebasedCloudURLs @ {
        "https://www.wolframcloud.com/info",
        "https://www.wolframcloud.com/files/auth",
        "https://www.wolframcloud.com/files?path=auth&fields=path",
        "https://www.wolframcloud.com/OAuthVersion",
        "https://www.wolframcloud.com/app/OAuthVersion"
    }
];

safeCloudObjectQ[ url_String? UStringQ ] :=
  Cached[
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*safeCloudObjectUUIDQ*)
safeCloudObjectUUIDQ[ domain_, path: { "obj" | "objects", uuid_? UUIDStringQ }, query_ ] :=
    Module[ { pathString },

        pathString = Quiet[
            CloudObjectInformation[
                CloudObject @ URLBuild @ <|
                    "Scheme" -> "HTTPS",
                    "Domain" -> domain,
                    "Path"   -> path
                |>,
                "Path"
            ],
            CloudObjectInformation::cloudnf
        ];

        TrueQ @ And[
            StringQ @ pathString,
            Apply[
                safeCloudObjectQ,
                { URLBuild @ <|
                    "Scheme" -> "HTTPS",
                    "Domain" -> domain,
                    "Path"   -> { "obj", pathString }
                |> }
            ]
        ]
    ];

safeCloudObjectUUIDQ[ domain_String, path_String, query_ ] :=
  safeCloudObjectUUIDQ[ domain, StringSplit[ path, "/" ], query ];

safeCloudObjectUUIDQ[ uuid_? UUIDStringQ ] :=
    safeCloudObjectUUIDQ[ URLParse[ $CloudBase, "Domain" ], { "obj", uuid }, { } ];

safeCloudObjectUUIDQ[ ___ ] :=
  False;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*safeCloudOperationQ*)
safeCloudOperationQ // Attributes = { HoldAllComplete };
safeCloudOperationQ[ CloudSystem`CloudObject`DoCloudOperation[ args___ ] ] := safeCloudOperationQ @ args;
safeCloudOperationQ[ "GET", { "files" }, { ___ }, __ ] := True;
safeCloudOperationQ[ "GET", { "files", uuid_? UUIDStringQ, { } }, { ___ }, ___ ] := safeCloudObjectUUIDQ @ uuid;
safeCloudOperationQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$safeCloudObjectPath*)
$safeCloudObjectPath = Alternatives[
    "online-courses/DataScienceMOOC/Examples",
    "resourcesystem/ResourceVersions",
    "resourcesystem/marketplacestorage/resources",
    "user-7dfab463-2b65-4c3b-b34e-ceea4165a73d",
    "entitycache"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*safeCloudObjectFileQ*)
safeCloudObjectFileQ[ domain_String, path_String, { }        ] := True;
safeCloudObjectFileQ[ _, _, KeyValuePattern[ "append" -> _ ] ] := False;
safeCloudObjectFileQ[ _, "/files/auth", { } ] := True;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$safeURLs*)
$safeURLs := $safeURLs = rebasedCloudURLs @ Alternatives[
    "https://www.wolframcloud.com/app/OAuthVersion",
    "https://www.wolframcloud.com/files?path=auth&fields=path",
    "https://www.wolframcloud.com/files?path=resourcesystem%2FVersionInformation%2F12-2%2FResourceVersions&fields=uuid",
    "https://www.wolframcloud.com/files/auth",
    "https://www.wolframcloud.com/info",
    "https://www.wolframcloud.com/OAuthVersion",
    "https://www.wolframcloud.com/obj/resourcesystem/api/1.0/SearchResources",
    "https://www.wolframcloud.com/obj/resourcesystem/api/1.0/AcquireResource",
    "https://www.wolframcloud.com/obj/resourcesystem/ResourceVersions",
    "https://www.wolframcloud.com/objects/resourcesystem/api/1.0/SearchResources",
    "https://www.wolframcloud.com/objects/resourcesystem/api/1.0/AcquireResource",
    "https://www.wolframcloud.com/objects/resourcesystem/ResourceVersions"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$fetchElement*)
$fetchElement := $fetchElement =
  Module[ { fetchElements, anyFetchElement },
      URLFetch;
      fetchElements = CURLLink`HTTP`Private`$URLFetchElements;
      anyFetchElement = Alternatives @@ fetchElements;
      anyFetchElement | { anyFetchElement... }
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FixValue*)
(* Forces referential transparency during evaluation *)

FixValue // Attributes = { HoldAllComplete };

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

FixValue[ { }, eval_ ] := eval;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)
