(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*GetAttributes*)
VerificationTest[
    GetAttributes[
        Function[ Null, IntegerQ[ Unevaluated[ #1 ] ], HoldAllComplete ]
    ],
    { HoldAllComplete },
    TestID -> "GetAttributes1@@Tests/Attributes.wlt:18,1-24,2"
];

VerificationTest[
    GetAttributes[
        Function[ { x, y },
            MakeBoxes[ { x, y }, StandardForm ],
            HoldAllComplete
        ]
    ],
    { HoldAllComplete },
    TestID -> "GetAttributes2@@Tests/Attributes.wlt:26,1-35,2"
];

VerificationTest[
    GetAttributes[
        Function[ {x, y},
            MakeBoxes[ { x, y }, StandardForm ],
            { HoldFirst, SequenceHold }
        ]
    ],
    { HoldFirst, SequenceHold },
    TestID -> "GetAttributes3@@Tests/Attributes.wlt:37,1-46,2"
];

VerificationTest[
    GetAttributes[
        Function[ x, MakeBoxes[ x, StandardForm ], { HoldFirst, SequenceHold } ]
    ],
    { HoldFirst, SequenceHold },
    TestID -> "GetAttributes4@@Tests/Attributes.wlt:48,1-54,2"
];

VerificationTest[
    GetAttributes[ Function[ Null, IntegerQ[ Unevaluated[ #1 ] ] ] ],
    { },
    TestID -> "GetAttributes5@@Tests/Attributes.wlt:56,1-60,2"
];

VerificationTest[
    GetAttributes[ Function[ { x, y }, MakeBoxes[ { x, y }, StandardForm ] ] ],
    { },
    TestID -> "GetAttributes6@@Tests/Attributes.wlt:62,1-66,2"
];

VerificationTest[
    GetAttributes[ Function[ x, MakeBoxes[ x, StandardForm ] ] ],
    { },
    TestID -> "GetAttributes7@@Tests/Attributes.wlt:68,1-72,2"
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*List*)
VerificationTest[
    With[ { names = RandomSample[ Names[ "*`*" ], 100 ] },
        Attributes /@ names === GetAttributes /@ names
    ],
    TestID -> "GetAttributesList1@@Tests/Attributes.wlt:77,1-82,2"
];


VerificationTest[
    With[ { names = RandomSample[ Names[ "*`*" ], 100 ] },
        Attributes @ names === GetAttributes @ names
    ],
    TestID -> "GetAttributesList2@@Tests/Attributes.wlt:85,1-90,2"
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*EvalLeaks*)
(* Check for evaluation leaks *)

VerificationTest[
    Module[ { x = "constant" },
        {
            Cases[
                Function[ Null, x = #1, HoldFirst ],
                e_ :> GetAttributes @ e,
                Infinity,
                Heads -> True
            ],
            x
        }
    ],
    {
        {
            { HoldAll, Protected },                 (* Function *)
            { Protected },                          (* Null *)
            { HoldFirst, Protected, SequenceHold }, (* Set *)
            { Temporary },                          (* x *)
            { NHoldAll, Protected },                (* Slot *)
            { },                                    (* 1 *)
            { },                                    (* #1 *)
            { },                                    (* x = #1 *)
            { Protected }                           (* HoldFirst *)
        },
        "constant"
    },
    TestID -> "GetAttributesEvalLeaks1@@Tests/Attributes.wlt:97,1-124,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ListableQ*)
VerificationTest[
    ListableQ /@
      {
          List,
          "List",
          Plus,
          "Plus",
          Function[ x, x ],
          Function[ x, x, { Orderless } ],
          Function[ x, x, Listable ],
          Function[ x, x, { Orderless, Listable } ]
      },
    { False, False, True, True, False, False, True, True },
    TestID -> "ListableQ1@@Tests/Attributes.wlt:129,1-143,2"
];

VerificationTest[
    Block[ { x },
        x // Attributes = { Listable };
        x := List;
        {
            ListableQ @@ { x },
            ListableQ @ x,
            ListableQ @ "x"
        }
    ],
    { False, True, True },
    TestID -> "ListableQ2@@Tests/Attributes.wlt:145,1-157,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*HoldingQ*)
VerificationTest[
    HoldingQ /@
      {
          List,
          "List",
          Hold,
          "Hold",
          Function[ x, x ],
          Function[ x, x, { Orderless } ],
          Function[ x, x, HoldFirst ],
          Function[ x, x, { Orderless, HoldAllComplete } ]
      },
    { False, False, True, True, False, False, True, True },
    TestID -> "HoldingQ1@@Tests/Attributes.wlt:162,1-176,2"
];

VerificationTest[
    Block[ { x },
        x // Attributes = { HoldFirst };
        x := List;
        {
            HoldingQ @@ { x },
            HoldingQ @ x,
            HoldingQ @ "x"
        }
    ],
    { False, True, True },
    TestID -> "HoldingQ2@@Tests/Attributes.wlt:178,1-190,2"
];

(* :!CodeAnalysis::EndBlock:: *)