(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet @ ResourceObject;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";
On[ EvaluateSafely::unsafe ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest[
  Replace[ToCanonicalForm[HoldComplete[Reverse[RandomColor[100]]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]] === Replace[ToCanonicalForm[HoldComplete[Table[RandomColor[], 100]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]],
  TestID -> "Untitled-55@@Tests/Rules.wlt:13,1-16,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]],
  TestID -> "Untitled-56@@Tests/Rules.wlt:18,1-22,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]] === Table[n + Range[n], {n, 10}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]] === Table[(n + 1) + Range[n + 1], {n, 0, 9}],
  TestID -> "Untitled-57@@Tests/Rules.wlt:24,1-27,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-58@@Tests/Rules.wlt:29,1-33,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]] === Table[Range[2^3 + 2]^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-59@@Tests/Rules.wlt:35,1-38,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-60@@Tests/Rules.wlt:40,1-44,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]] === Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-61@@Tests/Rules.wlt:46,1-49,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[RandomInteger /@ Reverse[Range[5]]]]],
  ToCanonicalForm[HoldComplete[RandomInteger /@ Range[5]]],
  TestID -> "Untitled-62@@Tests/Rules.wlt:51,1-55,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[IntegerDigits[4294967296]][[2]]]],
  ToCanonicalForm[HoldComplete[IntegerDigits[4294967296][[-2]]]],
  TestID -> "Untitled-63@@Tests/Rules.wlt:57,1-61,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[i^3 - i^2, {i, 10}]]],
  TestID -> "Untitled-64@@Tests/Rules.wlt:63,1-67,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Style["A", 100]]],
  ToCanonicalForm[HoldForm[Style["A", FontSize -> 100]]],
  TestID -> "Untitled-65@@Tests/Rules.wlt:69,1-73,2"
];

VerificationTest[
  CodeEquivalentQ[#1 + 2 & , Function[n, n + 2]],
  TestID -> "Untitled-66@@Tests/Rules.wlt:75,1-78,2"
];

VerificationTest[
  CodeEquivalentQ[2*(x + 1), 2*x + 2],
  TestID -> "Untitled-67@@Tests/Rules.wlt:80,1-83,2"
];

VerificationTest[
  CodeEquivalentQ[2*(#1 + 1) & , 2*#1 + 2 & ],
  TestID -> "Untitled-68@@Tests/Rules.wlt:85,1-88,2"
];

VerificationTest[
  CodeEquivalentQ[Grid[RandomColor[{10, 10}]], Grid[Array[RandomColor[] & , {10, 10}]]],
  TestID -> "Untitled-69@@Tests/Rules.wlt:90,1-93,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[4]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-70@@Tests/Rules.wlt:95,1-99,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Reverse[Range[4]]]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-71@@Tests/Rules.wlt:101,1-105,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListPlot[Range[10, 14]]]],
  ToCanonicalForm[HoldComplete[ListPlot[{10, 11, 12, 13, 14}]]],
  TestID -> "Untitled-72@@Tests/Rules.wlt:107,1-111,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Column[{1, 2, 3, 4, 5}]]],
  ToCanonicalForm[HoldComplete[Column[Range[5]]]],
  TestID -> "Untitled-73@@Tests/Rules.wlt:113,1-117,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[Range[5]^2]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[{1, 4, 9, 16, 25}]]],
  TestID -> "Untitled-74@@Tests/Rules.wlt:119,1-123,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-75@@Tests/Rules.wlt:125,1-129,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[0, 10] + 10]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-76@@Tests/Rules.wlt:131,1-135,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]*3]],
  ToCanonicalForm[HoldComplete[3*Range[10]]],
  TestID -> "Untitled-77@@Tests/Rules.wlt:137,1-141,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 2, 20, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 20, 2]]],
  TestID -> "Untitled-78@@Tests/Rules.wlt:143,1-147,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[BarChart[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[BarChart[Table[n^2, {n, 10}]]]],
  TestID -> "Untitled-79@@Tests/Rules.wlt:149,1-153,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[IntegerDigits[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[Table[IntegerDigits[n^2], {n, 10}]]],
  TestID -> "Untitled-80@@Tests/Rules.wlt:155,1-159,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[n^3 - n^2, {n, 10}]]],
  TestID -> "Untitled-81@@Tests/Rules.wlt:161,1-165,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 1, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[1, 100, 2]]],
  TestID -> "Untitled-82@@Tests/Rules.wlt:167,1-171,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n^2, {n, 2, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 100, 2]^2]],
  TestID -> "Untitled-83@@Tests/Rules.wlt:173,1-177,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[1/Range[20]]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[Table[1/n, {n, 20}]]]],
  TestID -> "Untitled-84@@Tests/Rules.wlt:179,1-183,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]]]],
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[n], {n, 100}]]]],
  TestID -> "Untitled-85@@Tests/Rules.wlt:185,1-189,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{Red, Yellow, Green}[[RandomInteger[2] + 1]], 100]]],
  ToCanonicalForm[HoldComplete[Table[{Red, Green, Yellow}[[RandomInteger[2] + 1]], 100]]],
  TestID -> "Untitled-86@@Tests/Rules.wlt:191,1-195,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Array[Range, 5, 2]]]],
  HoldComplete[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 7 - TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-87@@Tests/Rules.wlt:197,1-201,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  HoldComplete[Table[9 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 11, 1}]],
  TestID -> "Untitled-88@@Tests/Rules.wlt:203,1-207,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[Floor[3*i^2 - 2*i], {i, 5}]]]],
  With[{n = -34}, HoldForm[Table[96 + n*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer] + 3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer]^2, {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 5, 1}]]],
  TestID -> "Untitled-89@@Tests/Rules.wlt:209,1-213,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[3*i^2 - 4*i + 3, {i, -4, 13, 1}]]]],
  ToCanonicalForm[HoldForm[{458, 387, 322, 263, 210, 163, 122, 87, 58, 35, 18, 7, 2, 3, 10, 23, 42, 67}]],
  TestID -> "Untitled-90@@Tests/Rules.wlt:215,1-219,2"
];

VerificationTest[
  ReleaseHold[FromCanonicalForm[ToCanonicalForm[HoldForm[Table[i, {i, 1, -3, -1}]]]]],
  Table[i, {i, 1, -3, -1}],
  TestID -> "Untitled-91@@Tests/Rules.wlt:221,1-225,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[i^2, {i, 3, 9, 2}]]]],
  ToCanonicalForm[HoldForm[Table[i^2, {i, 9, 3, -2}]]],
  TestID -> "Untitled-92@@Tests/Rules.wlt:227,1-231,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Array[Range @* RandomInteger, 10, 3]]],
  HoldForm[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, RandomValue[DiscreteUniformDistribution[{0, 2 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer]}]], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1, 10, 1}]],
  TestID -> "Untitled-93@@Tests/Rules.wlt:233,1-237,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - 1 & )[Range[5]]]],
  HoldForm[Table[-1 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-94@@Tests/Rules.wlt:239,1-243,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - RandomInteger[#1] & )[Range[3, 7, 3]]]],
  HoldForm[(#1 - RandomInteger[#1] & )[Table[3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 2, 1}]]],
  TestID -> "Untitled-95@@Tests/Rules.wlt:245,1-249,2"
];

(* VerificationTest[
  CodeEquivalentQ[Take[Sort[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[]], 10], Sort[TakeSmallestBy[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[], First, 10]]],
  True,
  TestID -> "Untitled-96@@Tests/Rules.wlt:248,1-252,2"
]; *)

VerificationTest[
  StringQ @ Environment[ "GITHUB_ACTIONS" ] || CodeEquivalentQ[Graphics3D[{Orange, Sphere[RandomReal[1, {100, 3}], 0.05]}], Graphics3D[{Orange, Table[Sphere[RandomReal[1, {3}], 0.05], {100}]}]],
  True,
  TestID -> "Untitled-97@@Tests/Rules.wlt:257,1-261,2"
];

VerificationTest[
  CodeEquivalentQ[(Framed[Style[#1, RandomColor[]], Background -> RandomColor[]] & ) /@ Alphabet[], (Framed[#1, Background -> RandomColor[]] & ) /@ (Style[#1, RandomColor[]] & ) /@ Alphabet[]],
  TestID -> "Untitled-98@@Tests/Rules.wlt:263,1-266,2"
];

VerificationTest[
  CodeEquivalentQ[NestList[#1 + First[RandomReal[{-1, 1}, {1, 3}]] & , {0, 0, 0}, 1000], NestList[#1 + RandomReal[{-1, 1}, 3] & , {0, 0, 0}, 1000]],
  TestID -> "Untitled-99@@Tests/Rules.wlt:268,1-271,2"
];

VerificationTest[
  CodeEquivalentQ[TakeLargestBy[WordList[], StringLength, 10], Take[SortBy[WordList[], StringLength[#1] & ], -10]],
  TestID -> "Untitled-100@@Tests/Rules.wlt:273,1-276,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[Dilation[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-101@@Tests/Rules.wlt:278,1-282,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[img, 1]],
  TestID -> "Untitled-102@@Tests/Rules.wlt:284,1-288,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[Erosion[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-103@@Tests/Rules.wlt:290,1-294,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[img, 1]],
  TestID -> "Untitled-104@@Tests/Rules.wlt:296,1-300,2"
];

VerificationTest[
  MakeCanonicalForm[StringTake["test string", 4]],
  MakeCanonicalForm[StringJoin[Take[Characters["test string"], 4]]],
  TestID -> "Untitled-105@@Tests/Rules.wlt:302,1-306,2"
]

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[StringJoin[Table[FromLetterNumber[RandomInteger[25] + 1], 5]], 100]]],
  ToCanonicalForm[HoldComplete[Table[StringJoin[FromLetterNumber[Table[RandomInteger[25] + 1, 5]]], 100]]],
  TestID -> "Untitled-106@@Tests/Rules.wlt:308,1-312,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Column[{Style[n, Plain], Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[Column[{n, Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  TestID -> "Untitled-107@@Tests/Rules.wlt:314,1-318,2"
];

VerificationTest[
  ToCanonicalForm[StringJoin[#1, StringReverse[#1]] & ],
  ToCanonicalForm[StringJoin[#1, StringJoin[Reverse[Characters[#1]]]] & ],
  TestID -> "Untitled-108@@Tests/Rules.wlt:320,1-324,2"
];

VerificationTest[
    MakeCanonicalForm @ Replace[ x, y_Integer :> y + 1, 2 ],
    MakeCanonicalForm @ Replace[ x, y_Integer :> y + 1, { 1, 2 }, Heads -> False ],
    TestID -> "Replace@@Tests/Rules.wlt:326,1-330,2"
]

VerificationTest[
    MakeCanonicalForm @ Replace[ x, y_ :> y + 1, All, Heads -> True ],
    MakeCanonicalForm[ x /. y_ :> y + 1 ],
    TestID -> "ReplaceAll@@Tests/Rules.wlt:332,1-336,2"
]

VerificationTest[
    MakeCanonicalForm @ Replace[ x, y_ :> y + 1, { 0, Infinity }, Heads -> True ],
    MakeCanonicalForm[ x /. y_ :> y + 1 ],
    TestID -> "ReplaceAll@@Tests/Rules.wlt:338,1-342,2"
]

VerificationTest[
    MakeCanonicalForm @ If[ TrueQ @ a, b, c ],
    MakeCanonicalForm @ If[ a, b, c, c ],
    TestID -> "If@@Tests/Rules.wlt:344,1-348,2"
]

VerificationTest[
    MakeCanonicalForm @ If[ TrueQ @ True, a, b ],
    MakeCanonicalForm @ If[ TrueQ @ False, b, a ],
    TestID -> "If-TrueQ@@Tests/Rules.wlt:350,1-354,2"
]

VerificationTest[
    MakeCanonicalForm @ TrueQ @ StringQ @ x,
    MakeCanonicalForm @ StringQ @ x,
    TestID -> "TrueQ-StringQ@@Tests/Rules.wlt:356,1-360,2"
]

VerificationTest[
    MakeCanonicalForm @ Which[ a, b, True, c ],
    MakeCanonicalForm @ If[ a, b, c ],
    TestID -> "Which-If@@Tests/Rules.wlt:362,1-366,2"
]

VerificationTest[
    MakeCanonicalForm @ Which[ a, b, c, d, True, e ],
    MakeCanonicalForm @ If[ a, b, If[ c, d, e ] ],
    TestID -> "Which-If@@Tests/Rules.wlt:368,1-372,2"
]

VerificationTest[
    MakeCanonicalForm @ Switch[ x, _Integer, 1, _String, 2, _, 3 ],
    MakeCanonicalForm @ Replace[ x, { _Integer :> 1, _String :> 2, _ :> 3 } ],
    TestID -> "Switch-Replace@@Tests/Rules.wlt:374,1-378,2"
]


VerificationTest[
    MakeCanonicalForm[ 1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)) ],
    MakeCanonicalForm[ 1/(1 + x^3) ],
    TestID -> "Simplify-Canonical-Forms@@Tests/Rules.wlt:381,1-385,2"
]

VerificationTest[
    ToCanonicalForm @ HoldComplete[ 1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)) ],
    MakeCanonicalForm[ 1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)), HoldComplete],
    TestID -> "ToCanonicalForm-MakeCanonicalForm-Comparison@@Tests/Rules.wlt:387,1-391,2"
]

VerificationTest[
    CodeEquivalentQ[
        1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)),
        1.0/(1 + x^3)
    ],
    TestID -> "Math-Equality-Testing@@Tests/Rules.wlt:393,1-399,2"
]

VerificationTest[
    MakeCanonicalForm[ Table[ If[ IntegerQ @ x, x + 1, $Failed ], { x, 10 } ] ],
    MakeCanonicalForm[ Range[ 10 ] + 1 ],
    TestID -> "If-Type-Optimization-1"
]

VerificationTest[
    MakeCanonicalForm[ Table[ If[ IntegerQ @ x, x + 1, $Failed ], { x, 10 } ] ],
    MakeCanonicalForm[ Table[ If[ AtomQ @ x, x + 1, $Failed ], { x, 10 } ] ],
    TestID -> "If-Type-Optimization-2"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <| |>,
    HoldForm @ True,
    TestID -> "AssociationQ-1"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <| { "a" -> 1, <| "b" -> 2 |> } |>,
    HoldForm @ True,
    TestID -> "AssociationQ-2"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <|  "a" -> 1, <| "b" -> 2 |> |>,
    HoldForm @ True,
    TestID -> "AssociationQ-3"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <| 123 |>,
    HoldForm @ AssociationQ @ <| 123 |>,
    TestID -> "AssociationQ-4"
]

VerificationTest[
    MakeCanonicalForm @ Table[
        IntegerQ @ n,
        { n, Table[ StringLength @ x, { x, { "a", "bb", "ccc" } } ] }
    ],
    HoldForm @ Table[ True, _ ],
    SameTest -> MatchQ,
    TestID   -> "StringLength-To-Integer-Type"
]

VerificationTest[
    MakeCanonicalForm @ { 1.1, 2.2, 3.3, 4.4, 5.5 },
    MakeCanonicalForm @ Table[ x, { x, 1.1, 5.5, 1.1 } ],
    TestID -> "Roll-Real-Tables"
]

VerificationTest[
    MakeCanonicalForm @ { 1, 3/2, 2, 5/2, 3, 7/2, 4, 9/2, 5 },
    MakeCanonicalForm @ Table[ i, { i, 1, 5, 1/2 } ],
    TestID -> "Roll-Mixed-Rational-Tables"
]

VerificationTest[
    MakeCanonicalForm[ Plus @@ { } ],
    HoldForm[ 0 ],
    TestID -> "Plus-Zero-Arguments"
]

VerificationTest[
    MakeCanonicalForm[ StringLength /@ { "a", "bb", "ccc" } ],
    MakeCanonicalForm[ Table[ Length @ Characters @ x, { x, { "a", "bb", "ccc" } } ] ],
    TestID -> "Map-Strings-To-Table"
]