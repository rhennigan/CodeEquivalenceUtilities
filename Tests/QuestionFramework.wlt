(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*AssessmentFunction*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Default rules*)
VerificationTest[
    af = AssessmentFunction[
        { HoldPattern[ RandomInteger /@ Range[ 5 ] ] -> 1 },
        "CodeEquivalence"
    ],
    _AssessmentFunction,
    SameTest -> MatchQ,
    TestID   -> "AssessmentFunction"
]

VerificationTest[
    res = af[ HoldPattern @ Array[ RandomInteger, 5 ] ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    True,
    TestID   -> "AssessmentResult-Correct"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Custom rules*)
VerificationTest[
    af = AssessmentFunction[
        { HoldPattern @ CharacterRange[ "A", "Z" ] -> 1 },
        "CodeEquivalence",
        Method -> <|
            "TransformationRules" -> {
                c_String? LowerCaseQ :> RuleCondition @ ToUpperCase @ c
            }
        |>
    ],
    _AssessmentFunction,
    SameTest -> MatchQ,
    TestID   -> "AssessmentFunction-Custom"
]

VerificationTest[
    res = af @ HoldPattern @ Alphabet[ ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject-Custom"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    True,
    TestID -> "AssessmentResult-Correct-Custom"
]

VerificationTest[
    res = af @ HoldPattern @ ToLowerCase @ Alphabet[ ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject-Custom-Incorrect"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    False,
    TestID -> "AssessmentResult-Correct-Custom-Incorrect"
]
