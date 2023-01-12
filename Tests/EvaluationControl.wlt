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
    f[ x_ ] := (Put[ x, CreateFile[ "test.txt" ] ]; Abort[ ]; Quit[ ]);
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
    Block[ { x },
        With[ { file = FileNameJoin @ { $HomeDirectory, CreateUUID[ ] } },
            Quiet[ EvaluateSafely @ Part[ x, 0 ][ "Put" ][ "Hello", file ], EvaluateSafely::unsafe ];
            WithCleanup[ ! FileExistsQ @ file, Quiet @ DeleteFile @ file ]
        ]
    ],
    TestID -> "Untitled-5@@Tests/EvaluationControl.wlt:123,1-131,2"
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
    TestID   -> "Untitled-6@@Tests/EvaluationControl.wlt:134,1-153,2"
];


VerificationTest[
    CodeEquivalentQ[ RandomInteger /@ Range[ 5 ], Array[ RandomInteger, 5 ] ],
    True,
    TestID -> "CodeEquivalentQ-MessageTest-1@@Tests/EvaluationControl.wlt:156,1-160,2"
];

VerificationTest[
    CodeEquivalentQ[ RandomInteger /@ Range[ 5 ], Array[ RandomInteger, 6 ] ],
    False,
    TestID -> "CodeEquivalentQ-MessageTest-2@@Tests/EvaluationControl.wlt:162,1-166,2"
];

VerificationTest[
    UsingFrontEnd @ Block[ { LinkWrite = HoldComplete @ ##1 & },
        With[ { link = RandomChoice @ Links[ ] },
            EvaluateSafely @ LinkWrite[
                link,
                CallPacket @ CloudSystem`Private`ServiceDispatch @ CloudSystem`CloudObject`DoCloudOperation[
                    "GET",
                    { "files" },
                    { "test-user/path/to/file", "fields" -> "uuid" },
                    "text/plain",
                    { }
                ]
            ]
        ]
    ],
    HoldComplete[ _LinkObject, _CallPacket ],
    SameTest -> MatchQ,
    TestID   -> "ServiceDispatch-1@@Tests/EvaluationControl.wlt:168,1-186,2"
];

VerificationTest[
    UsingFrontEnd @ Block[ { LinkWrite = HoldComplete @ ##1 & },
        With[ { link = RandomChoice @ Links[ ] },
            EvaluateSafely @ LinkWrite[
                link,
                CallPacket @ CloudSystem`Private`ServiceDispatch @ CloudSystem`CloudObject`DoCloudOperation[
                    "DELETE",
                    { "files", "00000000-0000-0000-0000-000000000000" },
                    { "recursive" -> "true", "filter" -> "file" },
                    "text/plain",
                    { }
                ]
            ]
        ]
    ],
    _SandboxViolation,
    SameTest -> MatchQ,
    TestID   -> "ServiceDispatch-2@@Tests/EvaluationControl.wlt:188,1-206,2"
];

VerificationTest[
    UsingFrontEnd @ Block[ { LinkWrite = HoldComplete @ ##1 & },
        With[ { link = RandomChoice @ Links[ ] },
            EvaluateSafely @ LinkWrite[
                link,
                CallPacket @ CloudSystem`Private`ServiceDispatch @ CloudSystem`CloudObject`DoCloudOperation[
                    "POST",
                    { "files", { } },
                    { "append" -> "false", "icons" -> "FileBrowser,IOS", "path" -> "test-user/path/to/file" },
                    "application/vnd.wolfram.expression",
                    { 50, 10 }
                ]
            ]
        ]
    ],
    _SandboxViolation,
    SameTest -> MatchQ,
    TestID   -> "ServiceDispatch-3@@Tests/EvaluationControl.wlt:208,1-226,2"
];

VerificationTest[
    UsingFrontEnd @ Block[ { LinkWrite = HoldComplete @ ##1 & },
        With[ { link = RandomChoice @ Links[ ] },
            EvaluateSafely @ LinkWrite[
                link,
                CallPacket @ CloudSystem`Private`ServiceDispatch @ CloudSystem`CloudObject`DoCloudOperation[
                    "GET",
                    { "files", "00000000-0000-0000-0000-000000000000", { } },
                    { },
                    "text/plain",
                    { }
                ]
            ]
        ]
    ],
    _SandboxViolation,
    SameTest -> MatchQ,
    TestID   -> "ServiceDispatch-4@@Tests/EvaluationControl.wlt:228,1-246,2"
];


(* :!CodeAnalysis::EndBlock:: *)