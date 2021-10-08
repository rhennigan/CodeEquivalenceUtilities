Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;



BeginPackage[ "Wolfram`CodeEquivalenceUtilities`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Attributes`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Graphics`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Structural`",
        "Wolfram`CodeEquivalenceUtilities`Types`",
        "Wolfram`CodeEquivalenceUtilities`EvaluationControl`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Rules`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`",
        "Wolfram`CodeEquivalenceUtilities`Formatting`",
        "Wolfram`CodeEquivalenceUtilities`CachedValues`"(*,
        "CodeEquivalenceUtilities`Debugging`"*)
    }
];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)

$CanonicalTransformationFunctions   ::usage = "";
$TransformationLimit                ::usage = "";
$CanonicalFormFunction              ::usage = "";
$RasterPatterns                     ::usage = "";
$RasterErrorThreshold               ::usage = "";
$RasterDistanceFunction             ::usage = "";
ToCanonicalForm                     ::usage = "";
MakeCanonicalForm;
FromCanonicalForm;
rasterHeld                          ::usage = "";
$TimeLimit                          ::usage = "";
$MemoryLimit                        ::usage = "";
$RasterFailure                      ::usage = "";
EquivalenceTestData                 ::usage = "";
CodeEquivalentQ                     ::usage = "";
CanonicalTrace                      ::usage = "";
BuildDispatch;
$Dispatch;


$MemoryLimit                = 256 * 1048576;
$RasterBoxErrorThreshold    = 0.1;
$RasterErrorThreshold       = 0.01;
$RasterizeTimeLimit         = 5;
$RasterPerimeter            = 0;
$RasterSize                 = { 100, 100 };
$TimeLimit                  = 5;
$TransformationLimit        = 10;



Begin[ "`Private`" ];



(******************************************************************************)



$CanonicalTransformationFunctions =
  {
      CanonicalTransformFromAttributes,
      CanonicalTransformFromScope,
      CanonicalTransformFromStructure,
      CanonicalTransformFromGraphics
  };




$CanonicalFormFunction = RightComposition @@ $CanonicalTransformationFunctions;


$RasterPatterns = Alternatives @@
  {
      ArrayPlot,
      BarChart,
      DateListPlot,
      GeoGraphics,
      GeoListPlot,
      Graph,
      Graphics,
      Graphics3D,
      Histogram,
      Image,
      ListLinePlot,
      ListPlot,
      ListPlot3D,
      Manipulate,
      NumberLinePlot,
      PieChart,
      Plot,
      ReliefPlot,
      Rotate,
      WordCloud
  };



$RasterFailure /:
  HoldPattern @ FailureQ @ $RasterFailure := True;


$RasterDistanceFunction =
  If[ MatchQ[ #1, $RasterFailure ] || MatchQ[ #2, $RasterFailure ]
      ,
      Infinity
      ,
      averagePixelError @ ##
  ] &;



(******************************************************************************)



BuildDispatch // Attributes = { };
BuildDispatch // Options    = { };


BuildDispatch::bfail = "";

builtQ = False;


reportTemplate =
  StringTemplate @ StringJoin @ Riffle[ {
      "Rebuilt transformation rules",
      "----------------------------",
      "Count:   ``",
      "Added:   ``",
      "Removed: ``",
      "----------------------------"
  }, "\n" ];


BuildDispatch[ ] := BuildDispatch @ $TransformationRules;


BuildDispatch[ rules_ ] :=

  Module[ { oldRules, appended, newRules, added, removed },

      oldRules = With[ { n = Normal @ $Dispatch }, If[ ListQ @ n, n, { } ] ];
      appended = Append[ rules, HoldApply[ f_, { v___ } ] :> f @ v ];
      newRules = MakeTransformationRules @@ { appended };

      Catch[
          Check[ $Dispatch = Dispatch @ newRules,
                 Throw[ $Dispatch = $Failed ]
          ];

          If[ builtQ
              ,
              added   = Length @ Complement[ newRules, oldRules ];
              removed = Length @ Complement[ oldRules, newRules ];

              Print @ Style[ reportTemplate[ Length @ newRules,
                                             added,
                                             removed
                             ],
                             "Program"
                      ]
          ];

          builtQ = True;
      ];

  ];



BuildDispatch[ ];



(******************************************************************************)



cst1 // ClearAll;
cst1 // Attributes = {HoldAllComplete};
cst1[expr_] := TempHold[expr] /. $Dispatch;


cst2 // ClearAll;
cst2 // Attributes = {HoldAllComplete};
cst2[expr_] := Cached@cst1@expr;



canonicalStepTransform // ClearAll;
canonicalStepTransform // Attributes = { };
canonicalStepTransform // Options    = { };


(*canonicalStepTransform[ expr_ ] :=
  Once @ Quiet @ NormalizeNames[
      StripTempHolds[ expr /. $Dispatch ],
      "ExcludedContexts" ->
        {
            "System`",
            Except[ $LocalContext,
                s_String /; StringMatchQ[ s, "CodeEquivalenceUtilities`" ~~ ___ ]
            ]
        }
  ];*)

canonicalStepTransform[ expr0_ ] :=
  Quiet @ With[ { expr = canonicalStepTransformTop @ expr0 },
      Fold[
          Sow[
              Cached @ NormalizeNames @ StripTempHolds @ Replace[
                  #1,
                  e_ :> RuleCondition @ cst2 @ e,
                  { #2 }
              ],
              $CanonicalTrace
          ] &,
          expr,
          Reverse @ Range @ Depth @ expr
      ]
  ];

canonicalStepTransform[ expr_ ] :=
  $LastTransformation = canonicalStepTransformTop @ expr;

canonicalStepTransformTop[ expr_ ] :=
  Sow[ NormalizeNames @ StripTempHolds[ expr /. $Dispatch ],
       $CanonicalTrace
  ];


iToCanonicalForm // ClearAll;
iToCanonicalForm // Attributes = { };
iToCanonicalForm // Options    = { };


iToCanonicalForm[ expr_, wrapper_, True, limit_, timeout_ ] :=
  Module[ { reap, transformations },
      reap = Reap[ iToCanonicalForm[ expr, wrapper, False, limit, timeout ],
                   $CanonicalTrace
             ];

      transformations = Replace[ reap,
                                 {
                                     { _, { list_, ___ } } :> list,
                                     ___                   :> { $Failed }
                                 }
                        ];

      wrapper @@@ Prepend[ transformations, HoldComplete @ expr ]
  ];

iToCanonicalForm[ expr_, wrapper_, False, limit_, timeout_ ] :=
  Module[ { },
      TimeConstrained[
          FixedPoint[
              canonicalStepTransform,
              $LastTransformation = HoldComplete @ expr,
              limit
          ],
          timeout
      ];
      Replace[ $LastTransformation, HoldComplete[ e_ ] :> wrapper @ e ]
  ];



ToCanonicalForm // Attributes = { };
ToCanonicalForm // Options    = {
    "Trace"          -> False,
    "MaxIterations"  -> 512,
    "TimeConstraint" -> Infinity
};

ToCanonicalForm[ expr_, opts: OptionsPattern[ ] ] :=
    ToCanonicalForm[ expr, ## &, opts ];

ToCanonicalForm[ expr_, wrapper_, OptionsPattern[ ] ] :=
    With[
        {
            trace   = OptionValue[ "Trace" ],
            iter    = OptionValue[ "MaxIterations" ],
            timeout = OptionValue[ "TimeConstraint" ]
        },
        Cached @ StripCanonical @ iToCanonicalForm[
            expr,
            wrapper,
            trace,
            iter,
            timeout
        ]
    ];


(*ToCanonicalForm[ expr_, opts : OptionsPattern[ ] ] :=

  Module[ { fpFunction, post, normalize },

      fpFunction = If[ OptionValue @ "Trace", FixedPointList  , FixedPoint ];
      post       = If[ OptionValue @ "Trace", DeleteDuplicates, Identity   ];

      normalize = Function[ e, Prepend[ Map[ Quiet @ NormalizeNames[ #,
          "ExcludedContexts" ->
            {
                "System`",
                Except[ $LocalContext,
                    s_String /; StringMatchQ[ s, "CodeEquivalenceUtilities`" ~~ ___ ]
                ]
            }
      ] &, e ], expr ] ];

      With[ { f = fpFunction },
          iToCanonicalForm[ expr, f, OptionValue @ "MaxIterations" ]
      ] // If[ OptionValue @ "Trace", normalize, Identity ] // post
  ];*)


MakeCanonicalForm // Attributes = { HoldAllComplete };
MakeCanonicalForm // Options    = {
    "Trace"          -> False,
    "MaxIterations"  -> 512,
    "TimeConstraint" -> Infinity
};

MakeCanonicalForm[ expr_, opts: OptionsPattern[ ] ] :=
  MakeCanonicalForm[ expr, HoldForm, opts ];

MakeCanonicalForm[ expr_, wrapper_, opts: OptionsPattern[ ] ] :=
  Module[ { canonical, trace, wrapped, unwrapped },
      canonical = ToCanonicalForm[ $holdWrapper @ expr, opts ];
      trace     = TrueQ @ OptionValue[ "Trace" ];
      wrapped   = If[ trace, $holdWrapper @@@ canonical, canonical ];
      unwrapped = wrapped /. $holdWrapper -> wrapper;
      unwrapped
  ];


$holdWrapper // Attributes = { HoldAllComplete };


FromCanonicalForm // Attributes = { };
FromCanonicalForm // Options    = { };


FromCanonicalForm[ expr_ ] :=
  expr //. {
      TypedSymbol[ s_, _ ] :> s,

      s_Symbol? LocalContextQ :> TrEval @
        With[ { name = $Context <> SymbolName @ Unevaluated @ s },
            Quiet @ Unprotect @ name;
            ClearAll @ name;
            Symbol @ name
        ],

      RandomValue -> RandomVariate,

      StringArg[ x_? SymbolQ ] :> TrEval @ SymbolName @ Unevaluated @ x
  };



(******************************************************************************)



holdingQ // ClearAll;
holdingQ // Attributes = { };
holdingQ // Options    = { };


holdingQ = ! FreeQ[ Attributes @ #, HoldFirst | HoldAll | HoldAllComplete ] &;



(******************************************************************************)



rasterBoxErrorRatio // ClearAll;
rasterBoxErrorRatio // Attributes = { };
rasterBoxErrorRatio // Options    = { };


rasterBoxErrorRatio[ raster_ ] :=
  With[ { byteColors = Floor @ Flatten[ 255 * ImageData @ raster, 1 ] },
      If[ N @ Count[ byteColors, { 255, 230, 230 } ] / Length @ byteColors >
            $RasterBoxErrorThreshold
          ,
          Message[ CodeEquivalentQ::raster, raster ];
      ];
      raster
  ];



(******************************************************************************)


measurableContentArea[ img1_, img2_ ] :=
  ImageMeasurements[
      Binarize @ ImageAdjust @ ImageAdd[
          EntropyFilter[ img1, 5 ],
          EntropyFilter[ img2, 5 ]
      ],
      "Total"
  ];



rasterize[ expr_ ] :=
  Module[ { boxes, cell, packet, exec, raster, max, img, min, resized },
      boxes = ToBoxes @ Style[ expr, Antialiasing -> False ];
      cell = Cell @ BoxData @ boxes;
      packet = FrontEnd`ExportPacket[ cell, "GIF" ];
      exec = UsingFrontEnd @ First @ FrontEndExecute @ packet;
      raster = ImportString[ exec, "GIF" ];
      max = Max @ $RasterSize;
      img = ImageResize[ raster, max ];
      min = Min[ $RasterSize/ImageDimensions[ img ] ];
      resized = RemoveAlphaChannel @ ImageResize[ img, Scaled @ min ];
      ImageCrop[ resized, $RasterSize, Padding -> GrayLevel @ .5 ]
  ];



averagePixelError[ img1_, img2_ ] :=
  Module[ { i1, i2, diffs, trim },
      { i1, i2 } = ConformImages @ { img1, img2 };
      diffs = Flatten[ ImageData @ i1 - ImageData @ i2, 1 ];
      trim = $RasterPerimeter*(2*Plus @@ $RasterSize)/(Times @@ $RasterSize);
      TrimmedMean[ Norm /@ diffs, trim ]
  ];


spectralError[ img1_, img2_ ] :=
  Module[ { p1, p2 },
      p1 = ImageAdjust @ ImagePad[ ImagePeriodogram @ img1, -1 ];
      p2 = ImageAdjust @ ImagePad[ ImagePeriodogram @ img2, -1 ];
      averagePixelError[ p1, p2 ]
  ];



$hold := (Hold | HoldForm | HoldComplete);

flattenHolds // ClearAll;
flattenHolds[$hold[h : $hold[___]]] := Inline[ $hold, h ];
flattenHolds[ e___ ] := e;



rasterHeld // ClearAll;
rasterHeld // Attributes = { HoldFirst };
rasterHeld // Options    = { };


rasterHeld[ (f_? holdingQ)[ g : ($RasterPatterns[ ___ ]) ] ] :=
  Inline[ { holdingQ, $RasterPatterns },
          rasterHeld @ g
  ];


rasterHeld[ g : ($RasterPatterns[ ___ ]) ] :=
  Inline[ $RasterPatterns,
          (*SeedRandom @ $SessionID;*)
          TimeConstrained[ Cached @ Check[ rasterize @ g // rasterBoxErrorRatio,
                                           $RasterFailure
                                    ],
              $RasterizeTimeLimit,
              $RasterFailure
          ]
  ];

rasterHeld[ g_ ] := (
    (*SeedRandom @ $SessionID;*)
    TimeConstrained[ Cached @ Check[ rasterize @ g // rasterBoxErrorRatio,
                                     $RasterFailure
                              ],
                     $RasterizeTimeLimit,
                     $RasterFailure
    ]
);


(******************************************************************************)



iEquivalenceTestData // ClearAll;
iEquivalenceTestData // Attributes = { HoldAllComplete };
iEquivalenceTestData // Options    = { "AllTests" -> False };


iEquivalenceTestData[ a___, sym_ ? SymbolQ, b___ ] :=
  With[ { eval = HoldComplete[ a, sym, b ] /. OwnValues @ Unevaluated @ sym },
      iEquivalenceTestData @@ eval /; eval =!= HoldComplete[ a, sym, b ]
  ];


iEquivalenceTestData[ expr1_, expr2_, opts : OptionsPattern[ ] ] :=

  Module[
      {
          t0, timing, throwQ, testData, equivalentQ,
          hexpr1, hexpr2, cexpr1, cexpr2, eval1, eval2, randomPatt,
          hasRandomSymbolsQ, reval1, reval2, rpatt, needsRasterizationQ,
          raster1, raster2
      },

      t0 = AbsoluteTime[ ];
      timing = (testData[ "Timing", # ] = AbsoluteTime[ ] - t0 ) &;

      throwQ        = ! OptionValue @ "AllTests";
      testData      = <| "Timing" -> <| |> |>;
      equivalentQ   = False;

      hexpr1 = flattenHolds @ HoldComplete @ expr1;
      hexpr2 = flattenHolds @ HoldComplete @ expr2;

      testData[ "SameQ" ] = hexpr1 === hexpr2;
      timing[ "SameQ" ];

      If[ testData[ "SameQ" ]
          ,
          equivalentQ = True;
          testData["EquivalentQ"] = True;
          If[ throwQ, Throw @ testData ]
      ];

      cexpr1 = ToCanonicalForm @ hexpr1;
      timing[ "ToCanonicalForm1" ];

      cexpr2 = ToCanonicalForm @ hexpr2;
      timing[ "ToCanonicalForm2" ];

      testData[ "CanonicalForms" ] = <| 1 -> cexpr1, 2 -> cexpr2 |>;
      testData[ "CanonicalEquivalentQ" ] = cexpr1 === cexpr2;

      If[ testData[ "CanonicalEquivalentQ" ]
          ,
          equivalentQ = True;
          testData[ "EquivalentQ" ] = True;
          If[ throwQ, Throw @ testData ]
      ];

      eval1 = EvaluateSafely @@ FromCanonicalForm[ cexpr1 ];
      timing[ "Evaluate1" ];

      eval2 = EvaluateSafely @@ FromCanonicalForm[ cexpr2 ];
      timing[ "Evaluate2" ];

      testData[ "SandboxForms" ] = <| 1 -> eval1, 2 -> eval2 |>;

      testData[ "SandboxEquivalentQ" ] =
        NormalizeNames @ eval1 === NormalizeNames @ eval2;
      timing[ "SandboxEquivalentQ" ];

      If[ testData[ "SandboxEquivalentQ" ]
          ,
          equivalentQ = True;
          testData[ "EquivalentQ" ] = True;
          If[ throwQ, Throw @ testData ]
      ];

      randomPatt =
        Alternatives @@ Map[ ToExpression[ #, StandardForm, HoldPattern ] &,
                             $RandomSymbols
                        ];

      hasRandomSymbolsQ = ! FreeQ[ cexpr1, randomPatt ] &&
                          ! FreeQ[ cexpr2, randomPatt ];

      If[ hasRandomSymbolsQ
          ,
          reval1 =
            Replace[ cexpr1,
                     HoldComplete[ e_ ] :>
                       EvaluateSafely[ e,
                           "SymbolList" -> $UnsafeSymbols ~Join~ $RandomSymbols
                       ]
            ];
          timing[ "RandomSandboxForms1" ];

          reval2 =
            Replace[ cexpr2,
                     HoldComplete[ e_ ] :>
                       EvaluateSafely[ e,
                           "SymbolList" -> $UnsafeSymbols ~Join~ $RandomSymbols
                       ]
            ];
          timing[ "RandomSandboxForms2" ];

          testData[ "RandomSandboxForms" ] = <| 1 -> reval1, 2 -> reval2 |>;

          testData[ "RandomSandboxEquivalentQ" ] =
            NormalizeNames @ reval1 === NormalizeNames @ reval2;
          timing[ "RandomSandboxEquivalentQ" ];

          If[ testData[ "RandomSandboxEquivalentQ" ]
              ,
              equivalentQ = True;
              testData[ "EquivalentQ" ] = True;
              If[ throwQ, Throw @ testData ]
          ];

      ]; (* /hasRandomSymbolsQ *)


      rpatt = $RasterPatterns | $RasterPatterns[ ___ ];

      needsRasterizationQ = ! FreeQ[ hexpr1, rpatt ] &&
                            ! FreeQ[ hexpr2, rpatt ];

      If[ needsRasterizationQ
          ,
          raster1 = With[ { in = eval1 }, rasterHeld @ in ];
          timing[ "Raster1" ];

          raster2 = With[ { in = eval2 }, rasterHeld @ in ];
          timing[ "Raster2" ];

          testData[ "RasterForms" ] = <| 1 -> raster1, 2 -> raster2 |>;

          testData[ "RasterError" ] = $RasterDistanceFunction[ raster1, raster2 ];
          timing[ "RasterError" ];

          testData[ "RasterEquivalentQ" ] =
            testData[ "RasterError" ] < $RasterErrorThreshold;

          If[ testData[ "RasterEquivalentQ" ]
              ,
              equivalentQ = True;
              testData[ "EquivalentQ" ] = True;
              If[ throwQ, Throw @ testData ]
          ];

      ]; (* /needsRasterizationQ *)


      testData[ "EquivalentQ" ] = equivalentQ;
      testData

  ];



(******************************************************************************)



EquivalenceTestData // Attributes = { HoldAllComplete };
EquivalenceTestData // Options    = { "AllTests" -> False };


EquivalenceTestData[ expr1_, expr2_, opts : OptionsPattern[ ] ] :=
  Cached @ Catch @ iEquivalenceTestData[ expr1, expr2, opts ];



(******************************************************************************)



CodeEquivalentQ // Attributes = { HoldAllComplete };
CodeEquivalentQ // Options    = { };
CodeEquivalentQ::raster = "Errors detected in rasterization: `1`";

CodeEquivalentQ[   ] := True;
CodeEquivalentQ[ _ ] := True;

CodeEquivalentQ[ expr1_, expr2_ ] :=
  EquivalenceTestData[ expr1, expr2 ][ "EquivalentQ" ];

CodeEquivalentQ[ a_, b_, rest__ ] :=
    TrueQ @ CodeEquivalentQ[ a, b ] && TrueQ @ CodeEquivalentQ[ b, rest ]

(******************************************************************************)



CanonicalTrace[ expression_ ] :=
  With[ { rules = Normal[Wolfram`CodeEquivalenceUtilities`Private`$transformer["Dispatch"]]},
      DeleteDuplicates[
          DeleteCases[
              FixedPointList[
                  NormalizeNames[# /. rules,
                      "ExcludedContexts" -> {"System`",
                          Except[$LocalContext,
                              s_String /; StringMatchQ[s, "Wolfram`CodeEquivalenceUtilities`" ~~ ___]]}
                  ] &, expression, 100], e_ /; ! FreeQ[e, TempHold]]] /.
        s_Symbol?LocalContextQ :>
          TrEval[Symbol[$Context <> SymbolName[Unevaluated[s]]]]
  ];



(******************************************************************************)

AllEquivalentBy[ exprs_, f_ ] :=
    AllEquivalentBy[ Unevaluated @ exprs, f, { 1 } ];

AllEquivalentBy[ exprs_List, f_, level_ ] :=
    Module[ { mapped },
        mapped = Map[ f, Unevaluated @ exprs, level ];
        CodeEquivalentQ @@ mapped
    ];

AllEquivalentBy[ _[ args___ ], f_, level_ ] :=
    AllEquivalentBy[ Unevaluated @ { args }, f, level ];

(******************************************************************************)

End[ ]; (* `Private` *)



(******************************************************************************)



Wolfram`CodeEquivalenceUtilities`Package`$Loaded = True;

Wolfram`CodeEquivalenceUtilities`Package`$Version =
  Lookup[ PacletManager`PacletInformation @ "CodeEquivalenceUtilities", "Version" ];



(******************************************************************************)



EndPackage[ ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
