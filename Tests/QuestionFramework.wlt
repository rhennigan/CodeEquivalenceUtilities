(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";
On[ EvaluateSafely::unsafe ];

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
    TestID   -> "AssessmentFunction@@Tests/QuestionFramework.wlt:17,1-25,2"
]

VerificationTest[
    res = af[ HoldPattern @ Array[ RandomInteger, 5 ] ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject@@Tests/QuestionFramework.wlt:27,1-32,2"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    True,
    TestID   -> "AssessmentResult-Correct@@Tests/QuestionFramework.wlt:34,1-38,2"
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
                HoldPattern @ FromCharacterCode[ 96 + i_ ] :>
                    FromCharacterCode[ 64 + i ]
            }
        |>
    ],
    _AssessmentFunction,
    SameTest -> MatchQ,
    TestID   -> "AssessmentFunction-Custom@@Tests/QuestionFramework.wlt:43,1-57,2"
]

VerificationTest[
    res = af @ HoldPattern @ Alphabet[ ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject-Custom@@Tests/QuestionFramework.wlt:59,1-64,2"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    True,
    TestID -> "AssessmentResult-Correct-Custom@@Tests/QuestionFramework.wlt:66,1-70,2"
]

VerificationTest[
    res = af @ HoldPattern @ ToLowerCase @ Alphabet[ ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject-Custom-Incorrect@@Tests/QuestionFramework.wlt:72,1-77,2"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    False,
    TestID -> "AssessmentResult-Correct-Custom-Incorrect@@Tests/QuestionFramework.wlt:79,1-83,2"
]
