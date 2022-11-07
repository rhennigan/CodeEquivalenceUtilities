(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
$Dispatch;
$LastTransformation;
$RasterDistanceFunction;
$RasterFailure;
BuildDispatch;
CodeEquivalentQ;
EquivalenceTestData;
FromCanonicalForm;
MakeCanonicalForm;
ToCanonicalForm;
TransformHold;
TransformRelease;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Declarations*)
NormalizeNames;
TypedSymbol;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$Dispatch*)
$Dispatch = None;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$LastTransformation*)
$LastTransformation = None;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CodeEquivalentQ*)
CodeEquivalentQ // Attributes = { HoldAllComplete };
CodeEquivalentQ::raster = "Errors detected in rasterization: `1`";

CodeEquivalentQ[   ] := True;
CodeEquivalentQ[ _ ] := True;

CodeEquivalentQ[ expr1_, expr2_ ] :=
  EquivalenceTestData[ expr1, expr2 ][ "EquivalentQ" ];

CodeEquivalentQ[ a_, b_, rest__ ] :=
    TrueQ @ CodeEquivalentQ[ a, b ] && TrueQ @ CodeEquivalentQ[ b, rest ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*EquivalenceTestData*)
EquivalenceTestData // Attributes = { HoldAllComplete     };
EquivalenceTestData // Options    = { "AllTests" -> False };

EquivalenceTestData[ expr1_, expr2_, opts : OptionsPattern[ ] ] :=
    WithCleanup[
        Cached @ Catch @ iEquivalenceTestData[ expr1, expr2, opts ],
        formatLocalSymbols[ ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*iEquivalenceTestData*)
iEquivalenceTestData // Attributes = { HoldAllComplete     };
iEquivalenceTestData // Options    = { "AllTests" -> False };

iEquivalenceTestData[ a___, sym_? SymbolQ, b___ ] :=
    With[ { eval = HoldComplete[ a, sym, b ] /. OwnValues @ Unevaluated @ sym },
        iEquivalenceTestData @@ eval /; eval =!= HoldComplete[ a, sym, b ]
    ];

iEquivalenceTestData[ expr1_, expr2_, opts: OptionsPattern[ ] ] :=
    withEvaluatorHandler @ Module[
        {
            t0, timing, throwQ, testData, equivalentQ,
            hexpr1, hexpr2, cexpr1, cexpr2, eval1, eval2, randomPatt,
            hasRandomSymbolsQ, reval1, reval2, rpatt, needsRasterizationQ,
            raster1, raster2
        },

        t0 = AbsoluteTime[ ];
        timing = (testData[ "Timing", # ] = AbsoluteTime[ ] - t0 ) &;

        throwQ      = ! OptionValue @ "AllTests";
        testData    = <| "Timing" -> <| |> |>;
        equivalentQ = False;

        hexpr1 = flattenHolds @ HoldComplete @ expr1;
        hexpr2 = flattenHolds @ HoldComplete @ expr2;

        testData[ "SameQ" ] = hexpr1 === hexpr2;
        timing[ "SameQ" ];

        If[ testData[ "SameQ" ]
            ,
            equivalentQ = True;
            testData[ "EquivalentQ" ] = True;
            If[ throwQ, Throw @ testData ]
        ];

        testData[ "EqualQ" ] = hexpr1 ~equalQ~ hexpr2;
        timing[ "EqualQ" ];

        If[ testData[ "EqualQ" ]
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
        testData[ "CanonicalEquivalentQ" ] = cexpr1 ~equalQ~ cexpr2;

        If[ testData[ "CanonicalEquivalentQ" ]
            ,
            equivalentQ = True;
            testData[ "EquivalentQ" ] = True;
            If[ throwQ, Throw @ testData ]
        ];

        eval1 = EvaluateSafely @@ hexpr1;
        timing[ "Evaluate1" ];

        eval2 = EvaluateSafely @@ hexpr2;
        timing[ "Evaluate2" ];

        testData[ "SandboxForms" ] = <| 1 -> eval1, 2 -> eval2 |>;

        testData[ "SandboxEquivalentQ" ] =
            NormalizeNames @ eval1 ~equalQ~ NormalizeNames @ eval2;
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
                NormalizeNames @ reval1 ~equalQ~ NormalizeNames @ reval2;
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

            testData[ "RasterError" ] = $RasterDistanceFunction[
                raster1,
                raster2
            ];
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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*withEvaluatorHandler*)
withEvaluatorHandler // Attributes = { HoldAllComplete };
withEvaluatorHandler[ eval_ ] := Block[ { $UnsafeSymbols = { } }, eval ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*flattenHolds*)
flattenHolds[ $$hold[ h: $$hold[ ___ ] ] ] := h;
flattenHolds[ e___ ] := e;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*equalQ*)
equalQ[ a___ ] := TrueQ @ Or[ SameQ @ a, equal0 @ a ];

equal0[ a___ ] :=
    Replace[ Equal @ a,
             eq_Equal :> TimeConstrained[ Simplify @ eq, 1, False ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToCanonicalForm*)
ToCanonicalForm // Options = {
    "Trace"          -> False,
    "MaxIterations"  -> 512,
    "TimeConstraint" -> Infinity,
    "PostProcessing" -> Simplify
};

ToCanonicalForm[ expr_, opts: OptionsPattern[ ] ] :=
    ToCanonicalForm[ expr, ##1 &, opts ];

ToCanonicalForm[ expr_, wrapper_, OptionsPattern[ ] ] :=
    With[
        {
            trace   = OptionValue[ "Trace"          ],
            iter    = OptionValue[ "MaxIterations"  ],
            timeout = OptionValue[ "TimeConstraint" ],
            post    = OptionValue[ "PostProcessing" ]
        },
        WithCleanup[
            Cached @ StripCanonical @ iToCanonicalForm[
                expr,
                wrapper,
                trace,
                iter,
                timeout,
                post
            ],
            formatLocalSymbols[ ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*AllEquivalentBy*)
AllEquivalentBy[ exprs_, f_ ] :=
    AllEquivalentBy[ Unevaluated @ exprs, f, { 1 } ];

AllEquivalentBy[ exprs_List, f_, level_ ] :=
    CodeEquivalentQ @@ Cases[ Unevaluated @ exprs, e_ :> f @ e, level ];

AllEquivalentBy[ _[ args___ ], f_, level_ ] :=
    AllEquivalentBy[ Unevaluated @ { args }, f, level ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*iToCanonicalForm*)
iToCanonicalForm[ expr_, wrapper_, True, limit_, timeout_, post_ ] :=
    Module[ { reap, transformations },
        reap = Reap[
            iToCanonicalForm[ expr, wrapper, False, limit, timeout, post ],
            $CanonicalTrace
        ];

        transformations = Replace[ reap,
                                   {
                                       { _, { list_, ___ } } :> list,
                                       ___                   :> { $Failed }
                                   }
                          ];

        Apply[
            wrapper,
            deleteAdjacentDuplicates @ Flatten @ {
                { HoldComplete @ expr },
                transformations,
                { $LastTransformation }
            },
            { 1 }
        ]
    ];

iToCanonicalForm[ expr_, wrapper_, False, limit_, timeout_, post_ ] :=
    Module[ { res },

        TimeConstrained[
            cycleDetectTransform[ preprocessCanonical @ expr, limit, post ],
            timeout
        ];

        res = TimeConstrained[
            postApply[ post, $LastTransformation ],
            timeout,
            $LastTransformation
        ];

        Replace[ res, HoldComplete[ e_ ] :> wrapper @ e ]
    ];

ToCanonicalForm::cycle = "Infinite loop detected in transformations; returning early.";

cycleDetectTransform[ expr_, limit_, post_ ] :=
    Internal`InheritedBlock[ { seenQ },
        Module[ { next, previous, step },

            markSeen @ expr;
            next = previous = HoldComplete @ expr;

            step = Function[
                next = postApply[ post, canonicalStepTransform @ # ];
                If[ next =!= previous && TrueQ @ seenQ @ next,
                    Message[ ToCanonicalForm::cycle ];
                    Throw[
                        Sow[ $LastTransformation = previous, $CanonicalTrace ],
                        $tag
                    ],
                    markSeen @ next;
                    previous = next
                ]
            ];

            Catch[
                FixedPoint[
                    step,
                    $LastTransformation = HoldComplete @ expr,
                    limit
                ],
                $tag
            ]
        ]
    ];


seenQ[ ___ ] := False;

markSeen[ HoldComplete[ held_HoldComplete ] ] := markSeen @ held;
markSeen[ held_HoldComplete ] := (seenQ[ Verbatim[ held ] ] = True);
markSeen[ other_ ] := markSeen @ HoldComplete @ other;


deleteAdjacentDuplicates[ list_ ] := First /@ Split @ list;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*preprocessCanonical*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SymbolVersionTooNew:: *)
preprocessCanonical[ expr_ ] := Sow[
    $LastTransformation = ReplaceAll[
        expr,
        {
            Verbatim[ PacletSymbol ][
                "Wolfram/CodeEquivalenceUtilities",
                "Wolfram`CodeEquivalenceUtilities`TransformHold"
            ] :> TransformHold,
            Verbatim[ PacletSymbol ][
                "Wolfram/CodeEquivalenceUtilities",
                "Wolfram`CodeEquivalenceUtilities`TransformRelease"
            ] :> TransformRelease
        }
    ],
    $CanonicalTrace
];
(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*postApply*)
postApply[ post_, (h1: $$hold|TempHold)[ (h2: $$hold|TempHold)[ expr_ ] ] ] :=
    With[ { p = postApply[ post, h2[ expr ] ] }, h1 @ p ];

postApply[ post_, (h: $$hold|TempHold)[ expr_ ] ] :=
    With[
        {
            pre = Apply[
                inactivate,
                TempHold @ expr /. Inactive -> Inactive @ Inactive
            ]
        },
        { new = post @ pre },
        h @@ ReplaceAll[
            Activate[ TempHold @ new /. Inactive @ Inactive -> $inactive ],
            $inactive -> Inactive
        ]
    ];

postApply[ post_, expr_ ] :=
    With[
        {
            pre = Apply[
                inactivate,
                TempHold @ expr /. Inactive -> Inactive @ Inactive
            ]
        },
        { new = post @ pre },
        ReplaceAll[
            Activate[ TempHold @ new /. Inactive @ Inactive -> $inactive ],
            $inactive -> Inactive
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*inactivate*)
inactivate // Attributes = { HoldAllComplete };
inactivate[ expr_ ] :=
    ReplaceRepeated[
        Unevaluated @ expr,
        {
            th_TransformHold :> th,
            i_Inactive :> i,
            s: Except[ $$simplifyInert, _Symbol? SymbolQ ] :> Inactive @ s
        }
    ];

$inactive // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$simplifySymbolNames*)
$simplifySymbolNames := $simplifySymbolNames = Developer`ReadWXFFile @
    PacletObject[ "Wolfram/CodeEquivalenceUtilities" ][
        "AssetLocation",
        "SimplifySymbols"
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$simplifySymbols*)
$simplifySymbols := $simplifySymbols = Enclose[
    Module[ { as, compatible, flat, held },
        as         = ConfirmBy[ $simplifySymbolNames, AssociationQ ];
        compatible = KeySelect[ as, LessEqualThan @ $VersionNumber ];
        flat       = Flatten @ Values @ compatible;
        held       = ToExpression[ flat, InputForm, HoldComplete ];
        Replace[
            Flatten @ Apply[ HoldComplete, held ],
            HoldComplete[ s___ ] :> HoldPattern @ Alternatives @ s
        ]
    ],
    Alternatives[ ] &
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$$simplifyInert*)
$$simplifyInert := $$simplifyInert = Alternatives @@ {
    $simplifySymbols,
    _Symbol? inertQ
};

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*inertQ*)
inertQ // Attributes = { HoldAllComplete };

inertQ[ sym_Symbol? SymbolQ ] :=
    And[ ! System`Private`HasAnyEvaluationsQ @ sym,
         ! System`Private`HasAnyCodesQ @ sym
    ];

inertQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*canonicalStepTransform*)
canonicalStepTransform[ expr0_ ] :=
    $LastTransformation = Quiet @ With[
        { expr = canonicalStepTransformTop @ expr0 },
        Fold[
            Function[
                Sow[
                    Cached @ NormalizeNames @ StripTempHolds @ Replace[
                        #1,
                        e_ :> RuleCondition @ cst2 @ e,
                        { #2 }
                    ],
                    $CanonicalTrace
                ]
            ],
            expr,
            Reverse @ Range @ Depth @ expr
        ]
    ];

canonicalStepTransform[ expr_ ] :=
    $LastTransformation = canonicalStepTransformTop @ expr;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*canonicalStepTransformTop*)
canonicalStepTransformTop[ expr_ ] :=
    Sow[ canonicalStepTransformTop0 @ releaseTransforms @ expr,
         $CanonicalTrace
    ];

canonicalStepTransformTop0[ expr_ ] :=
    NormalizeNames @ StripTempHolds[ expr /. $Dispatch ];

releaseTransforms[ expr_ ] := ReplaceAll[
    expr,
    TransformRelease[ a___ ] :> RuleCondition[
        TransformRelease @ a /. $Dispatch
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*cst1*)
cst1 // Attributes = { HoldAllComplete };
cst1[ expr_ ] := TempHold @ expr /. $Dispatch;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*cst2*)
cst2 // Attributes = { HoldAllComplete };
cst2[ expr_ ] := Cached @ cst1 @ expr;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*MakeCanonicalForm*)
MakeCanonicalForm // Attributes = { HoldAllComplete };

MakeCanonicalForm // Options    = {
    "Trace"          -> False,
    "MaxIterations"  -> 512,
    "TimeConstraint" -> Infinity,
    "PostProcessing" -> Simplify
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$holdWrapper*)
$holdWrapper // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FromCanonicalForm*)
FromCanonicalForm[ expr_ ] := ReplaceRepeated[ expr, $fromCanonicalFormRules ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$fromCanonicalFormRules*)
$fromCanonicalFormRules = Dispatch @ {
    TypedSymbol[ s_, _ ] :> s,
    s_Symbol? LocalContextQ :> RuleCondition @ fromLocalContextRHS @ s,
    RandomValue :> RandomVariate,
    StringArg[ x_? SymbolQ ] :> RuleCondition @ SymbolName @ Unevaluated @ x
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fromLocalContextRHS*)
fromLocalContextRHS // Attributes = { HoldAllComplete };
fromLocalContextRHS[ s_Symbol ] :=
    With[ { name = $Context <> SymbolName @ Unevaluated @ s },
        ToExpression[ name, InputForm, $ConditionHold ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BuildDispatch*)
BuildDispatch[ ] := BuildDispatch @ $TransformationRules;

BuildDispatch[ rules_ ] :=
    Module[ { oldRules, appended, newRules, added, removed },

        oldRules = With[ { n = Normal @ $Dispatch }, If[ ListQ @ n, n, { } ] ];
        appended = DeleteDuplicates @ Append[ rules, HoldApply[ f_, { v___ } ] :> f @ v ];
        newRules = MakeTransformationRules @@ { appended };
        $dispatchHash = Hash @ newRules;

        Catch[
            Check[ $Dispatch = Dispatch @ newRules,
                   Throw[ $Dispatch = $Failed, $tag ]
            ];

            If[ $builtDispatch,
                added   = Length @ Complement[ newRules, oldRules ];
                removed = Length @ Complement[ oldRules, newRules ];
                DebugPrint @ Style[
                    TemplateApply[
                        $buildDispatchReportTemplate,
                        { Length @ newRules, added, removed }
                    ],
                    "Program"
                ]
            ];

            $builtDispatch = True
            ,
            $tag
        ];

        $Dispatch
    ];


$builtDispatch = False;

$buildDispatchReportTemplate = "\
Rebuilt transformation rules
----------------------------
Count:   ``
Added:   ``
Removed: ``
----------------------------";

BuildDispatch[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Rasterization*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$RasterFailure*)
$RasterFailure /: HoldPattern @ FailureQ @ $RasterFailure := True;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$RasterDistanceFunction*)
$RasterDistanceFunction = Function[
    If[ Or[ MatchQ[ #1, $RasterFailure ], MatchQ[ #2, $RasterFailure ] ],
        Infinity,
        averagePixelError @ ##1
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rasterHeld*)
rasterHeld // Attributes = { HoldFirst };

rasterHeld[ (f_? holdingQ)[ g: $RasterPatterns[ ___ ] ] ] :=
    Inline[ $RasterPatterns, rasterHeld @ g ];

rasterHeld[ g: $RasterPatterns[ ___ ] ] := Inline[ $RasterPatterns,
    TimeConstrained[
        Cached @ Check[ rasterBoxErrorRatio @ rasterize @ HoldForm @ g,
                        $RasterFailure
                 ],
        $RasterizeTimeLimit,
        $RasterFailure
    ]
];

rasterHeld[ g_ ] :=
    TimeConstrained[
        Cached @ Check[ rasterBoxErrorRatio @ rasterize @ HoldForm @ g,
                        $RasterFailure
                 ],
        $RasterizeTimeLimit,
        $RasterFailure
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*holdingQ*)
holdingQ // Attributes = { HoldAllComplete };
holdingQ[ s_Symbol? SymbolQ ] :=
    ! FreeQ[ Attributes @ s, HoldFirst | HoldAll | HoldAllComplete ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rasterize*)
rasterize[ expr_ ] :=
    Module[ { boxes, cell, packet, exec, raster, max, img, min, resized },
        boxes   = ToBoxes @ Style[ expr, Antialiasing -> False ];
        cell    = Cell @ BoxData @ boxes;
        packet  = FrontEnd`ExportPacket[ cell, "GIF" ];
        exec    = UsingFrontEnd @ First @ FrontEndExecute @ packet;
        raster  = ImportString[ exec, "GIF" ];
        max     = Max @ $RasterSize;
        img     = ImageResize[ raster, max ];
        min     = Min[ $RasterSize/ImageDimensions[ img ] ];
        resized = RemoveAlphaChannel @ ImageResize[ img, Scaled @ min ];
        ImageCrop[ resized, $RasterSize, Padding -> GrayLevel @ .5 ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rasterBoxErrorRatio*)
rasterBoxErrorRatio[ raster_ ] :=
    With[ { byteColors = Floor @ Flatten[ 255 * ImageData @ raster, 1 ] },
        If[ N @ Count[ byteColors, { 255, 230, 230 } ] / Length @ byteColors >
                $RasterBoxErrorThreshold
            ,
            Message[ CodeEquivalentQ::raster, raster ];
        ];
        raster
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*averagePixelError*)
averagePixelError[ img1_, img2_ ] :=
    Module[ { i1, i2, diffs, trim },
        { i1, i2 } = ConformImages @ { img1, img2 };
        diffs = Flatten[ ImageData @ i1 - ImageData @ i2, 1 ];
        trim = $RasterPerimeter*(2*Plus @@ $RasterSize)/(Times @@ $RasterSize);
        TrimmedMean[ Norm /@ diffs, trim ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)