(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";

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
    TestID -> "UnsafeSymbols001@@Tests/EvaluationControl.wlt:15,1-24,2"
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
    TestID -> "UnsafeSymbols002@@Tests/EvaluationControl.wlt:27,1-42,2"
];


VerificationTest[
    CompoundExpression[ Unsafe[ HoldComplete, { 1 + 2, 3 + 4 } ] ]
    ,
    Unsafe[ "HoldComplete", HoldComplete[ 1 + 2, 3 + 4 ] ]
    ,
    TestID -> "Unsafe001@@Tests/EvaluationControl.wlt:45,1-51,2"
];


VerificationTest[
    CompoundExpression[ Unsafe[ List, { 1 + 2, 3 + 4 } ] ]
    ,
    Unsafe[ "List", { 1 + 2, 3 + 4 } ]
    ,
    TestID -> "Unsafe002@@Tests/EvaluationControl.wlt:54,1-60,2"
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
    TestID -> "EvaluateSafely001@@Tests/EvaluationControl.wlt:63,1-79,2"
];

VerificationTest[
    EvaluateSafely @ Echo[ 1 + 1 ]
    ,
    SandboxViolation[
        Unsafe[ "System`Echo", { 2 } ],
        "Captured" -> { Unsafe[ "System`Echo", { 2 } ] }
    ]
    ,
    TestID -> "EvaluateSafely002@@Tests/EvaluationControl.wlt:81,1-90,2"
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
    TestID -> "EvaluateSafelyTypes001@@Tests/EvaluationControl.wlt:93,1-112,2"
];


VerificationTest[
  EvaluateSafely[Array[Echo, 2]],
  SandboxViolation[{Unsafe["System`Echo", {1}], Unsafe["System`Echo", {2}]}, "Captured" -> {Unsafe["System`Echo", Null], Unsafe["System`Echo", {1}], Unsafe["System`Echo", {2}]}],
  TestID -> "Untitled-4@@Tests/EvaluationControl.wlt:115,1-119,2"
];


VerificationTest[
  Block[{x}, With[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID[]}]}, Quiet[EvaluateSafely[x[[0]]["Put"]["Hello", file]], EvaluateSafely::unsafe];  !FileExistsQ[file]]],
  TestID -> "Untitled-5@@Tests/EvaluationControl.wlt:122,1-125,2"
];


VerificationTest[
    If[ StringQ @ Environment[ "GITHUB_ACTIONS" ],
        $skipped,
        EvaluateSafely @ GeoListPlot[
            Part[
                TakeLargestBy[
                    Normal @ ResourceData[ "Fireballs and Bolides" ],
                    #Altitude &,
                    10
                ],
                All,
                "NearestCity"
            ],
            GeoLabels -> True
        ]
    ],
    $skipped | GeoGraphics[ _Graphics, OptionsPattern[ ] ],
    SameTest -> MatchQ,
    TestID   -> "Untitled-6@@Tests/EvaluationControl.wlt:128,1-147,2"
];


VerificationTest[
    CodeEquivalentQ[ RandomInteger /@ Range[ 5 ], Array[ RandomInteger, 5 ] ],
    True,
    TestID -> "CodeEquivalentQ-MessageTest-1@@Tests/EvaluationControl.wlt:150,1-154,2"
];

VerificationTest[
    CodeEquivalentQ[ RandomInteger /@ Range[ 5 ], Array[ RandomInteger, 6 ] ],
    True,
    TestID -> "CodeEquivalentQ-MessageTest-2@@Tests/EvaluationControl.wlt:156,1-160,2"
];

(* :!CodeAnalysis::EndBlock:: *)