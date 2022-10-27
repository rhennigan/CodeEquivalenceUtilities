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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*GetAttributes*)
VerificationTest[
    GetAttributes[
        Function[ Null, IntegerQ[ Unevaluated[ #1 ] ], HoldAllComplete ]
    ],
    { HoldAllComplete },
    TestID -> "GetAttributes1@@Tests/Attributes.wlt:20,1-26,2"
];

VerificationTest[
    GetAttributes[
        Function[ { x, y },
            MakeBoxes[ { x, y }, StandardForm ],
            HoldAllComplete
        ]
    ],
    { HoldAllComplete },
    TestID -> "GetAttributes2@@Tests/Attributes.wlt:28,1-37,2"
];

VerificationTest[
    GetAttributes[
        Function[ {x, y},
            MakeBoxes[ { x, y }, StandardForm ],
            { HoldFirst, SequenceHold }
        ]
    ],
    { HoldFirst, SequenceHold },
    TestID -> "GetAttributes3@@Tests/Attributes.wlt:39,1-48,2"
];

VerificationTest[
    GetAttributes[
        Function[ x, MakeBoxes[ x, StandardForm ], { HoldFirst, SequenceHold } ]
    ],
    { HoldFirst, SequenceHold },
    TestID -> "GetAttributes4@@Tests/Attributes.wlt:50,1-56,2"
];

VerificationTest[
    GetAttributes[ Function[ Null, IntegerQ[ Unevaluated[ #1 ] ] ] ],
    { },
    TestID -> "GetAttributes5@@Tests/Attributes.wlt:58,1-62,2"
];

VerificationTest[
    GetAttributes[ Function[ { x, y }, MakeBoxes[ { x, y }, StandardForm ] ] ],
    { },
    TestID -> "GetAttributes6@@Tests/Attributes.wlt:64,1-68,2"
];

VerificationTest[
    GetAttributes[ Function[ x, MakeBoxes[ x, StandardForm ] ] ],
    { },
    TestID -> "GetAttributes7@@Tests/Attributes.wlt:70,1-74,2"
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*List*)
VerificationTest[
    With[ { names = RandomSample[ Names[ "*`*" ], 100 ] },
        Attributes /@ names === GetAttributes /@ names
    ],
    TestID -> "GetAttributesList1@@Tests/Attributes.wlt:79,1-84,2"
];


VerificationTest[
    With[ { names = RandomSample[ Names[ "*`*" ], 100 ] },
        Attributes @ names === GetAttributes @ names
    ],
    TestID -> "GetAttributesList2@@Tests/Attributes.wlt:87,1-92,2"
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
    TestID -> "GetAttributesEvalLeaks1@@Tests/Attributes.wlt:99,1-126,2"
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
    TestID -> "ListableQ1@@Tests/Attributes.wlt:131,1-145,2"
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
    TestID -> "ListableQ2@@Tests/Attributes.wlt:147,1-159,2"
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
    TestID -> "HoldingQ1@@Tests/Attributes.wlt:164,1-178,2"
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
    TestID -> "HoldingQ2@@Tests/Attributes.wlt:180,1-192,2"
];

(* :!CodeAnalysis::EndBlock:: *)