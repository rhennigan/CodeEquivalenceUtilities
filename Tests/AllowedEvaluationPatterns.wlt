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
    TestID -> "TemporaryDirectory@@Tests/AllowedEvaluationPatterns.wlt:11,1-16,2"
]

VerificationTest[
    If[ DirectoryQ @ $tmp, DeleteDirectory[ $tmp, DeleteContents -> True ] ],
    Null,
    SameTest -> MatchQ,
    TestID -> "ClearTemporaryDirectory@@Tests/AllowedEvaluationPatterns.wlt:18,1-23,2"
]

VerificationTest[
    CreateDirectory[ $tmp, CreateIntermediateDirectories -> True ],
    _? DirectoryQ,
    SameTest -> MatchQ,
    TestID -> "CreateTemporaryDirectory@@Tests/AllowedEvaluationPatterns.wlt:25,1-30,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$AllowedEvaluationPatterns*)

VerificationTest[
    $dataFile = FileNameJoin @ { $tmp, "data.wl" },
    _String,
    SameTest -> MatchQ,
    TestID   -> "DataFile@@Tests/AllowedEvaluationPatterns.wlt:36,1-41,2"
]

VerificationTest[
    Put[ Range[ 10 ], $dataFile ],
    Null,
    TestID -> "PutData@@Tests/AllowedEvaluationPatterns.wlt:43,1-47,2"
]

VerificationTest[
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    False,
    { EvaluateSafely::unsafe },
    TestID -> "DefaultAllowedPatterns@@Tests/AllowedEvaluationPatterns.wlt:49,1-54,2"
]

VerificationTest[
    $AllowedEvaluationPatterns = HoldPattern[ Get ][ $dataFile ];
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    True,
    TestID -> "CustomAllowedPatterns@@Tests/AllowedEvaluationPatterns.wlt:56,1-61,2"
]

VerificationTest[
    $AllowedEvaluationPatterns =.,
    Null,
    TestID -> "ResetAllowedEvaluationPatterns@@Tests/AllowedEvaluationPatterns.wlt:63,1-67,2"
]

VerificationTest[
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    False,
    { EvaluateSafely::unsafe },
    TestID -> "DefaultAllowedPatterns-2@@Tests/AllowedEvaluationPatterns.wlt:69,1-74,2"
]

VerificationTest[
    $AllowedEvaluationPatterns = { _Get, _Put };
    With[ { f = $dataFile }, CodeEquivalentQ[
        Put[ Range[ 5 ], f ];
        Get @ f,
        Range[ 5 ]
    ] ],
    True,
    TestID -> "CustomAllowedPatterns-2@@Tests/AllowedEvaluationPatterns.wlt:76,1-85,2"
]

VerificationTest[
    Put[ Unevaluated @ DeleteFile @ $dataFile, $dataFile ],
    Null,
    TestID -> "PutEvilData@@Tests/AllowedEvaluationPatterns.wlt:87,1-91,2"
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
    TestID   -> "NestedSandboxRestriction@@Tests/AllowedEvaluationPatterns.wlt:93,1-103,2"
]

VerificationTest[
    StringTrim @ ReadString @ $dataFile,
    "DeleteFile[$dataFile]",
    TestID -> "DeleteBlocked@@Tests/AllowedEvaluationPatterns.wlt:105,1-109,2"
]

VerificationTest[
    $AllowedEvaluationPatterns =.,
    Null,
    TestID -> "ResetAllowedEvaluationPatterns-2@@Tests/AllowedEvaluationPatterns.wlt:111,1-115,2"
]

VerificationTest[
    DeleteDirectory[ $tmp, DeleteContents -> True ],
    Null,
    TestID -> "Cleanup@@Tests/AllowedEvaluationPatterns.wlt:117,1-121,2"
]