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
    TestID   -> "TransformHold-1"
]

VerificationTest[
    MakeCanonicalForm @ Array[ TransformHold @ Range, 10 ],
    HoldForm @ Table[ _Range, _ ],
    SameTest -> MatchQ,
    TestID   -> "TransformHold-2"
]

VerificationTest[
    MakeCanonicalForm @ Array[ Range, TransformHold[ 3 + 1 + 2 ] ],
    HoldForm @ Table[ _Table, { _, 1, Verbatim[ Plus ][ 3, 1, 2 ], 1 } ],
    SameTest -> MatchQ,
    TestID   -> "TransformHold-3"
]

VerificationTest[
    MakeCanonicalForm @ TransformHold[ 2 + 1 + TransformRelease[ 4 + 3 ] ],
    HoldForm[ Verbatim[ Plus ][ 2, 1, 7 ] ],
    SameTest -> MatchQ,
    TestID   -> "TransformRelease-1"
]

(* :!CodeAnalysis::EndBlock:: *)