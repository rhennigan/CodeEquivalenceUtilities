(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";
On[ EvaluateSafely::unsafe ];

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
    TestID -> "UnsafeSymbols001@@Tests/EvaluationControl.wlt:16,1-25,2"
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
    TestID -> "UnsafeSymbols002@@Tests/EvaluationControl.wlt:28,1-43,2"
];


VerificationTest[
    CompoundExpression[ Unsafe[ HoldComplete, { 1 + 2, 3 + 4 } ] ]
    ,
    Unsafe[ "HoldComplete", HoldComplete[ 1 + 2, 3 + 4 ] ]
    ,
    TestID -> "Unsafe001@@Tests/EvaluationControl.wlt:46,1-52,2"
];


VerificationTest[
    CompoundExpression[ Unsafe[ List, { 1 + 2, 3 + 4 } ] ]
    ,
    Unsafe[ "List", { 1 + 2, 3 + 4 } ]
    ,
    TestID -> "Unsafe002@@Tests/EvaluationControl.wlt:55,1-61,2"
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
    TestID -> "EvaluateSafely001@@Tests/EvaluationControl.wlt:64,1-80,2"
];

VerificationTest[
    EvaluateSafely @ Echo[ 1 + 1 ]
    ,
    SandboxViolation[
        Unsafe[ "System`Echo", { 2 } ],
        "Captured" -> { Unsafe[ "System`Echo", { 2 } ] }
    ]
    ,
    TestID -> "EvaluateSafely002@@Tests/EvaluationControl.wlt:82,1-91,2"
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
    TestID -> "EvaluateSafelyTypes001@@Tests/EvaluationControl.wlt:94,1-113,2"
];


VerificationTest[
  EvaluateSafely[Array[Echo, 2]],
  SandboxViolation[{Unsafe["System`Echo", {1}], Unsafe["System`Echo", {2}]}, "Captured" -> {Unsafe["System`Echo", Null], Unsafe["System`Echo", {1}], Unsafe["System`Echo", {2}]}],
  TestID -> "Untitled-4@@Tests/EvaluationControl.wlt:116,1-120,2"
];


VerificationTest[
  Block[{x}, With[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID[]}]}, Quiet[EvaluateSafely[x[[0]]["Put"]["Hello", file]], EvaluateSafely::unsafe];  !FileExistsQ[file]]],
  TestID -> "Untitled-5@@Tests/EvaluationControl.wlt:123,1-126,2"
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
    TestID   -> "Untitled-6@@Tests/EvaluationControl.wlt:129,1-148,2"
];


VerificationTest[
    CodeEquivalentQ[ RandomInteger /@ Range[ 5 ], Array[ RandomInteger, 5 ] ],
    True,
    TestID -> "CodeEquivalentQ-MessageTest-1@@Tests/EvaluationControl.wlt:151,1-155,2"
];

VerificationTest[
    CodeEquivalentQ[ RandomInteger /@ Range[ 5 ], Array[ RandomInteger, 6 ] ],
    False,
    TestID -> "CodeEquivalentQ-MessageTest-2@@Tests/EvaluationControl.wlt:157,1-161,2"
];

(* :!CodeAnalysis::EndBlock:: *)