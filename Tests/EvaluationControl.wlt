(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest[
    ClearAll[ f, g ];
    f[ x_ ] := (Put[ x, CreateFile[ ] ]; Abort[ ]; Quit[ ] );
    g[ x_ ] := Echo @ f @ x;
    UnsafeSymbols @ g
    ,
    { "System`Echo" }
    ,
    TestID -> "UnsafeSymbols001@@Tests/EvaluationControl.wlt:12,1-21,2"
];


VerificationTest[
    ClearAll[ f, g ];
    f[ x_ ] := (Put[ x, CreateFile[ ] ]; Abort[ ]; Quit[ ]);
    g[ x_ ] := Echo @ f @ x;
    UnsafeSymbols[ g, "Definitions" -> Full ]
    ,
    {
        "System`Abort",
        "System`CreateFile",
        "System`Echo",
        "System`Put",
        "System`Quit"
    }
    ,
    TestID -> "UnsafeSymbols002@@Tests/EvaluationControl.wlt:24,1-39,2"
];


VerificationTest[
    CompoundExpression[ Unsafe[ HoldComplete, { 1 + 2, 3 + 4 } ] ]
    ,
    Unsafe[ "HoldComplete", HoldComplete[ 1 + 2, 3 + 4 ] ]
    ,
    TestID -> "Unsafe001@@Tests/EvaluationControl.wlt:42,1-48,2"
];


VerificationTest[
    CompoundExpression[ Unsafe[ List, { 1 + 2, 3 + 4 } ] ]
    ,
    Unsafe[ "List", { 1 + 2, 3 + 4 } ]
    ,
    TestID -> "Unsafe002@@Tests/EvaluationControl.wlt:51,1-57,2"
];


VerificationTest[
    f // ClearAll;
    EvaluateSafely[
    f[ x_ ] := (Echo[ x^2 ]; x^2);
    Table[ f @ i, { i, 3 } ] ]
    ,
    SandboxViolation[
        { 1, 4, 9 },
        "Captured" -> {
            Unsafe[ "System`Echo", { 1 } ],
            Unsafe[ "System`Echo", { 4 } ],
            Unsafe[ "System`Echo", { 9 } ]
        }
    ]
    ,
    TestID -> "EvaluateSafely001@@Tests/EvaluationControl.wlt:60,1-76,2"
];

VerificationTest[
    EvaluateSafely @ Echo[ 1 + 1 ]
    ,
    SandboxViolation[
        Unsafe[ "System`Echo", { 2 } ],
        "Captured" -> { Unsafe[ "System`Echo", { 2 } ] }
    ]
    ,
    TestID -> "EvaluateSafely002@@Tests/EvaluationControl.wlt:78,1-87,2"
];


VerificationTest[
    CompoundExpression[
        Apply[
            EvaluateSafely,
            ToCanonicalForm @ HoldComplete @ Table[ Echo[ i^2 ];
            i, { i, 3 } ]
        ]
    ]
    ,
    SandboxViolation[
        { 1, 2, 3 },
        "Captured" -> {
            Unsafe[ "System`Echo", { 1 } ],
            Unsafe[ "System`Echo", { 4 } ],
            Unsafe[ "System`Echo", { 9 } ]
        }
    ]
    ,
    TestID -> "EvaluateSafelyTypes001@@Tests/EvaluationControl.wlt:90,1-109,2"
];


VerificationTest[
  EvaluateSafely[Array[Echo, 2]],
  SandboxViolation[{Unsafe["System`Echo", {1}], Unsafe["System`Echo", {2}]}, "Captured" -> {Unsafe["System`Echo", Null], Unsafe["System`Echo", {1}], Unsafe["System`Echo", {2}]}],
  TestID -> "Untitled-4@@Tests/EvaluationControl.wlt:112,1-116,2"
];


VerificationTest[
  Block[{x}, With[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID[]}]}, Quiet[EvaluateSafely[x[[0]]["Put"]["Hello", file]], EvaluateSafely::unsafe];  !FileExistsQ[file]]],
  TestID -> "Untitled-5@@Tests/EvaluationControl.wlt:119,1-122,2"
];


VerificationTest[
  EvaluateSafely[GeoListPlot[TakeLargestBy[Normal[ResourceData["Fireballs and Bolides"]], #Altitude & , 10][[All,"NearestCity"]], GeoLabels -> True]],
  GeoGraphics[_Graphics, OptionsPattern[]],
  SameTest -> MatchQ,
  TestID -> "Untitled-6@@Tests/EvaluationControl.wlt:125,1-130,2"
];

(* :!CodeAnalysis::EndBlock:: *)