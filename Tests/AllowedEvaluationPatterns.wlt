(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";
ClearEvalCache[ ];

VerificationTest[
    $tmp = FileNameJoin @ { DirectoryName @ $TestFileName, "Data", "Temporary" },
    _String,
    SameTest -> MatchQ,
    TestID -> "TemporaryDirectory"
]

VerificationTest[
    If[ DirectoryQ @ $tmp, DeleteDirectory[ $tmp, DeleteContents -> True ] ],
    Null,
    SameTest -> MatchQ,
    TestID -> "ClearTemporaryDirectory"
]

VerificationTest[
    CreateDirectory[ $tmp, CreateIntermediateDirectories -> True ],
    _? DirectoryQ,
    SameTest -> MatchQ,
    TestID -> "CreateTemporaryDirectory"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$AllowedEvaluationPatterns*)

$dataFile = FileNameJoin @ { $tmp, "data.wl" };

VerificationTest[
    Put[ Range[ 10 ], $dataFile ],
    Null,
    TestID -> "PutData"
]

VerificationTest[
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    False,
    { EvaluateSafely::unsafe },
    TestID -> "DefaultAllowedPatterns"
]

VerificationTest[
    $AllowedEvaluationPatterns = HoldPattern[ Get ][ $dataFile ];
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    True,
    TestID -> "CustomAllowedPatterns"
]

VerificationTest[
    $AllowedEvaluationPatterns =.,
    Null,
    TestID -> "ResetAllowedEvaluationPatterns"
]

VerificationTest[
    With[ { f = $dataFile }, CodeEquivalentQ[ Get @ f, Range[ 10 ] ] ],
    False,
    { EvaluateSafely::unsafe },
    TestID -> "DefaultAllowedPatterns-2"
]

VerificationTest[
    $AllowedEvaluationPatterns = { _Get, _Put };
    With[ { f = $dataFile }, CodeEquivalentQ[
        Put[ Range[ 5 ], f ];
        Get @ f,
        Range[ 5 ]
    ] ],
    True,
    TestID -> "CustomAllowedPatterns-2"
]

VerificationTest[
    Put[ Unevaluated @ DeleteFile @ $dataFile, $dataFile ],
    Null,
    TestID -> "PutEvilData"
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
    TestID   -> "NestedSandboxRestriction"
]

VerificationTest[
    StringTrim @ ReadString @ $dataFile,
    "DeleteFile[$dataFile]",
    TestID -> "DeleteBlocked"
]

VerificationTest[
    $AllowedEvaluationPatterns =.,
    Null,
    TestID -> "ResetAllowedEvaluationPatterns-2"
]

VerificationTest[
    DeleteDirectory[ $tmp, DeleteContents -> True ],
    Null,
    TestID -> "Cleanup"
]