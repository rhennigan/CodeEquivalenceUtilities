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
    TestID   -> "AssessmentFunction@@Tests/QuestionFramework.wlt:16,1-24,2"
]

VerificationTest[
    res = af[ HoldPattern @ Array[ RandomInteger, 5 ] ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject@@Tests/QuestionFramework.wlt:26,1-31,2"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    True,
    TestID   -> "AssessmentResult-Correct@@Tests/QuestionFramework.wlt:33,1-37,2"
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
    TestID   -> "AssessmentFunction-Custom@@Tests/QuestionFramework.wlt:42,1-55,2"
]

VerificationTest[
    res = af @ HoldPattern @ Alphabet[ ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject-Custom@@Tests/QuestionFramework.wlt:57,1-62,2"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    True,
    TestID -> "AssessmentResult-Correct-Custom@@Tests/QuestionFramework.wlt:64,1-68,2"
]

VerificationTest[
    res = af @ HoldPattern @ ToLowerCase @ Alphabet[ ],
    _AssessmentResultObject,
    SameTest -> MatchQ,
    TestID   -> "AssessmentResultObject-Custom-Incorrect@@Tests/QuestionFramework.wlt:70,1-75,2"
]

VerificationTest[
    res[ "AnswerCorrect" ],
    False,
    TestID -> "AssessmentResult-Correct-Custom-Incorrect@@Tests/QuestionFramework.wlt:77,1-81,2"
]
