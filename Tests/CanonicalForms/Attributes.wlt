(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

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
    TestID -> "GetAttributes1"
];

VerificationTest[
    GetAttributes[
        Function[ { x, y },
            MakeBoxes[ { x, y }, StandardForm ],
            HoldAllComplete
        ]
    ],
    { HoldAllComplete },
    TestID -> "GetAttributes2"
];

VerificationTest[
    GetAttributes[
        Function[ {x, y},
            MakeBoxes[ { x, y }, StandardForm ],
            { HoldFirst, SequenceHold }
        ]
    ],
    { HoldFirst, SequenceHold },
    TestID -> "GetAttributes3"
];

VerificationTest[
    GetAttributes[
        Function[ x, MakeBoxes[ x, StandardForm ], { HoldFirst, SequenceHold } ]
    ],
    { HoldFirst, SequenceHold },
    TestID -> "GetAttributes4"
];

VerificationTest[
    GetAttributes[ Function[ Null, IntegerQ[ Unevaluated[ #1 ] ] ] ],
    { },
    TestID -> "GetAttributes5"
];

VerificationTest[
    GetAttributes[ Function[ { x, y }, MakeBoxes[ { x, y }, StandardForm ] ] ],
    { },
    TestID -> "GetAttributes6"
];

VerificationTest[
    GetAttributes[ Function[ x, MakeBoxes[ x, StandardForm ] ] ],
    { },
    TestID -> "GetAttributes7"
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*List*)
VerificationTest[
    With[ { names = RandomSample[ Names[ "*`*" ], 100 ] },
        Attributes /@ names === GetAttributes /@ names
    ],
    TestID -> "GetAttributesList1"
];


VerificationTest[
    With[ { names = RandomSample[ Names[ "*`*" ], 100 ] },
        Attributes @ names === GetAttributes @ names
    ],
    TestID -> "GetAttributesList2"
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
    TestID -> "GetAttributesEvalLeaks1"
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
    TestID -> "ListableQ1"
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
    TestID -> "ListableQ2"
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
    TestID -> "HoldingQ1"
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
    TestID -> "HoldingQ2"
];
