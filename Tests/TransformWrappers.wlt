(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet @ ResourceObject;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";
On[ EvaluateSafely::unsafe ];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest[
    MakeCanonicalForm @ TransformHold[ 3 + 1 + 2 ],
    HoldForm[ Verbatim[ Plus ][ 3, 1, 2 ] ],
    SameTest -> MatchQ,
    TestID   -> "TransformHold-1@@Tests/TransformWrappers.wlt:16,1-21,2"
]

VerificationTest[
    MakeCanonicalForm @ Array[ TransformHold @ Range, 10 ],
    HoldForm @ Table[ _Range, _ ],
    SameTest -> MatchQ,
    TestID   -> "TransformHold-2@@Tests/TransformWrappers.wlt:23,1-28,2"
]

VerificationTest[
    MakeCanonicalForm @ Array[ Range, TransformHold[ 3 + 1 + 2 ] ],
    HoldForm @ Table[ _Table, { _, 1, Verbatim[ Plus ][ 3, 1, 2 ], 1 } ],
    SameTest -> MatchQ,
    TestID   -> "TransformHold-3@@Tests/TransformWrappers.wlt:30,1-35,2"
]

VerificationTest[
    MakeCanonicalForm @ TransformHold[ 2 + 1 + TransformRelease[ 4 + 3 ] ],
    HoldForm[ Verbatim[ Plus ][ 2, 1, 7 ] ],
    SameTest -> MatchQ,
    TestID   -> "TransformRelease-1@@Tests/TransformWrappers.wlt:37,1-42,2"
]

VerificationTest[
    MakeCanonicalForm @ Range @ System`PacletSymbol[ "Wolfram/CodeEquivalenceUtilities", "Wolfram`CodeEquivalenceUtilities`TransformHold" ][ 3 + 1 + 2 ],
    MakeCanonicalForm @ Range @ TransformHold[ 3 + 1 + 2 ],
    TestID -> "PacletSymbol-TransformHold@@Tests/TransformWrappers.wlt:44,1-48,2"
]

VerificationTest[
    MakeCanonicalForm[
        Range @ System`PacletSymbol[
            "Wolfram/CodeEquivalenceUtilities",
            "Wolfram`CodeEquivalenceUtilities`TransformHold"
        ][ 3 + 1 + 2 ],
        Trace -> True
    ],
    { ___ },
    SameTest -> MatchQ,
    TestID -> "MakeCanonicalForm-Trace-EvaluationLeak@@Tests/TransformWrappers.wlt:50,1-61,2"
]

(* :!CodeAnalysis::EndBlock:: *)