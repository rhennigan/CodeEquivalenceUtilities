(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";
On[ EvaluateSafely::unsafe ];
ClearEvalCache[ ];

VerificationTest[
    $tmp = FileNameJoin @ { DirectoryName @ $TestFileName, "Data", "Temporary" },
    _String,
    SameTest -> MatchQ,
    TestID -> "TemporaryDirectory@@Tests/AllowedEvaluationPatterns.wlt:10,1-15,2"
]

VerificationTest[
    If[ DirectoryQ @ $tmp, DeleteDirectory[ $tmp, DeleteContents -> True ] ],
    Null,
    SameTest -> MatchQ,
    TestID -> "ClearTemporaryDirectory@@Tests/AllowedEvaluationPatterns.wlt:17,1-22,2"
]

VerificationTest[
    CreateDirectory[ $tmp, CreateIntermediateDirectories -> True ],
    _? DirectoryQ,
    SameTest -> MatchQ,
    TestID -> "CreateTemporaryDirectory@@Tests/AllowedEvaluationPatterns.wlt:24,1-29,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$AllowedEvaluationPatterns*)

VerificationTest[
    $dataFile = FileNameJoin @ { $tmp, "data.wl" },
    _String,
    SameTest -> MatchQ,
    TestID   -> "DataFile@@Tests/AllowedEvaluationPatterns.wlt:35,1-40,2"
]

VerificationTest[
    Put[ Range[ 10 ], $dataFile ],
    Null,
    TestID -> "PutData@@Tests/AllowedEvaluationPatterns.wlt:42,1-46,2"
]

VerificationTest[
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    False,
    { EvaluateSafely::unsafe },
    TestID -> "DefaultAllowedPatterns@@Tests/AllowedEvaluationPatterns.wlt:48,1-53,2"
]

VerificationTest[
    $AllowedEvaluationPatterns = HoldPattern[ Get ][ $dataFile ];
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    True,
    TestID -> "CustomAllowedPatterns@@Tests/AllowedEvaluationPatterns.wlt:55,1-60,2"
]

VerificationTest[
    $AllowedEvaluationPatterns =.,
    Null,
    TestID -> "ResetAllowedEvaluationPatterns@@Tests/AllowedEvaluationPatterns.wlt:62,1-66,2"
]

VerificationTest[
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    False,
    { EvaluateSafely::unsafe },
    TestID -> "DefaultAllowedPatterns-2@@Tests/AllowedEvaluationPatterns.wlt:68,1-73,2"
]

VerificationTest[
    $AllowedEvaluationPatterns = { _Get, _Put };
    With[ { f = $dataFile }, CodeEquivalentQ[
        Put[ Range[ 5 ], f ];
        Get @ f,
        Range[ 5 ]
    ] ],
    True,
    TestID -> "CustomAllowedPatterns-2@@Tests/AllowedEvaluationPatterns.wlt:75,1-84,2"
]

VerificationTest[
    Put[ Unevaluated @ DeleteFile @ $dataFile, $dataFile ],
    Null,
    TestID -> "PutEvilData@@Tests/AllowedEvaluationPatterns.wlt:86,1-90,2"
]

VerificationTest[
    $AllowedEvaluationPatterns = _Get;
    With[ { f = $dataFile }, EvaluateSafely @ Get @ f ],
    SandboxException[
        Get[ file_ ],
        "CaughtEvaluation" -> HoldComplete @ DeleteFile[ file_ ]
    ] /; file === $dataFile,
    { EvaluateSafely::unsafe },
    SameTest -> MatchQ,
    TestID   -> "NestedSandboxRestriction@@Tests/AllowedEvaluationPatterns.wlt:92,1-102,2"
]

VerificationTest[
    StringTrim @ ReadString @ $dataFile,
    "DeleteFile[$dataFile]",
    TestID -> "DeleteBlocked@@Tests/AllowedEvaluationPatterns.wlt:104,1-108,2"
]

VerificationTest[
    $AllowedEvaluationPatterns =.,
    Null,
    TestID -> "ResetAllowedEvaluationPatterns-2@@Tests/AllowedEvaluationPatterns.wlt:110,1-114,2"
]

VerificationTest[
    DeleteDirectory[ $tmp, DeleteContents -> True ],
    Null,
    TestID -> "Cleanup@@Tests/AllowedEvaluationPatterns.wlt:116,1-120,2"
]