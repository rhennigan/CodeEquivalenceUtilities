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
  Replace[ToCanonicalForm[HoldComplete[Reverse[RandomColor[100]]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]] === Replace[ToCanonicalForm[HoldComplete[Table[RandomColor[], 100]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]],
  TestID -> "Untitled-55@@Tests/Rules.wlt:16,1-19,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]],
  TestID -> "Untitled-56@@Tests/Rules.wlt:21,1-25,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]] === Table[n + Range[n], {n, 10}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]] === Table[(n + 1) + Range[n + 1], {n, 0, 9}],
  TestID -> "Untitled-57@@Tests/Rules.wlt:27,1-30,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-58@@Tests/Rules.wlt:32,1-36,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]] === Table[Range[2^3 + 2]^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-59@@Tests/Rules.wlt:38,1-41,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-60@@Tests/Rules.wlt:43,1-47,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]] === Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-61@@Tests/Rules.wlt:49,1-52,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[RandomInteger /@ Reverse[Range[5]]]]],
  ToCanonicalForm[HoldComplete[RandomInteger /@ Range[5]]],
  TestID -> "Untitled-62@@Tests/Rules.wlt:54,1-58,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[IntegerDigits[4294967296]][[2]]]],
  ToCanonicalForm[HoldComplete[IntegerDigits[4294967296][[-2]]]],
  TestID -> "Untitled-63@@Tests/Rules.wlt:60,1-64,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[i^3 - i^2, {i, 10}]]],
  TestID -> "Untitled-64@@Tests/Rules.wlt:66,1-70,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Style["A", 100]]],
  ToCanonicalForm[HoldForm[Style["A", FontSize -> 100]]],
  TestID -> "Untitled-65@@Tests/Rules.wlt:72,1-76,2"
];

VerificationTest[
  CodeEquivalentQ[#1 + 2 & , Function[n, n + 2]],
  TestID -> "Untitled-66@@Tests/Rules.wlt:78,1-81,2"
];

VerificationTest[
  CodeEquivalentQ[2*(x + 1), 2*x + 2],
  TestID -> "Untitled-67@@Tests/Rules.wlt:83,1-86,2"
];

VerificationTest[
  CodeEquivalentQ[2*(#1 + 1) & , 2*#1 + 2 & ],
  TestID -> "Untitled-68@@Tests/Rules.wlt:88,1-91,2"
];

VerificationTest[
  CodeEquivalentQ[Grid[RandomColor[{10, 10}]], Grid[Array[RandomColor[] & , {10, 10}]]],
  TestID -> "Untitled-69@@Tests/Rules.wlt:93,1-96,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[4]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-70@@Tests/Rules.wlt:98,1-102,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Reverse[Range[4]]]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-71@@Tests/Rules.wlt:104,1-108,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListPlot[Range[10, 14]]]],
  ToCanonicalForm[HoldComplete[ListPlot[{10, 11, 12, 13, 14}]]],
  TestID -> "Untitled-72@@Tests/Rules.wlt:110,1-114,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Column[{1, 2, 3, 4, 5}]]],
  ToCanonicalForm[HoldComplete[Column[Range[5]]]],
  TestID -> "Untitled-73@@Tests/Rules.wlt:116,1-120,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[Range[5]^2]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[{1, 4, 9, 16, 25}]]],
  TestID -> "Untitled-74@@Tests/Rules.wlt:122,1-126,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-75@@Tests/Rules.wlt:128,1-132,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[0, 10] + 10]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-76@@Tests/Rules.wlt:134,1-138,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]*3]],
  ToCanonicalForm[HoldComplete[3*Range[10]]],
  TestID -> "Untitled-77@@Tests/Rules.wlt:140,1-144,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 2, 20, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 20, 2]]],
  TestID -> "Untitled-78@@Tests/Rules.wlt:146,1-150,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[BarChart[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[BarChart[Table[n^2, {n, 10}]]]],
  TestID -> "Untitled-79@@Tests/Rules.wlt:152,1-156,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[IntegerDigits[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[Table[IntegerDigits[n^2], {n, 10}]]],
  TestID -> "Untitled-80@@Tests/Rules.wlt:158,1-162,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[n^3 - n^2, {n, 10}]]],
  TestID -> "Untitled-81@@Tests/Rules.wlt:164,1-168,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 1, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[1, 100, 2]]],
  TestID -> "Untitled-82@@Tests/Rules.wlt:170,1-174,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n^2, {n, 2, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 100, 2]^2]],
  TestID -> "Untitled-83@@Tests/Rules.wlt:176,1-180,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[1/Range[20]]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[Table[1/n, {n, 20}]]]],
  TestID -> "Untitled-84@@Tests/Rules.wlt:182,1-186,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]]]],
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[n], {n, 100}]]]],
  TestID -> "Untitled-85@@Tests/Rules.wlt:188,1-192,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{Red, Yellow, Green}[[RandomInteger[2] + 1]], 100]]],
  ToCanonicalForm[HoldComplete[Table[{Red, Green, Yellow}[[RandomInteger[2] + 1]], 100]]],
  TestID -> "Untitled-86@@Tests/Rules.wlt:194,1-198,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Array[Range, 5, 2]]]],
  HoldComplete[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 7 - TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-87@@Tests/Rules.wlt:200,1-204,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  HoldComplete[Table[9 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 11, 1}]],
  TestID -> "Untitled-88@@Tests/Rules.wlt:206,1-210,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[Floor[3*i^2 - 2*i], {i, 5}]]]],
  With[{n = -34}, HoldForm[Table[96 + n*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer] + 3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer]^2, {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 5, 1}]]],
  TestID -> "Untitled-89@@Tests/Rules.wlt:212,1-216,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[3*i^2 - 4*i + 3, {i, -4, 13, 1}]]]],
  ToCanonicalForm[HoldForm[{458, 387, 322, 263, 210, 163, 122, 87, 58, 35, 18, 7, 2, 3, 10, 23, 42, 67}]],
  TestID -> "Untitled-90@@Tests/Rules.wlt:218,1-222,2"
];

VerificationTest[
  ReleaseHold[FromCanonicalForm[ToCanonicalForm[HoldForm[Table[i, {i, 1, -3, -1}]]]]],
  Table[i, {i, 1, -3, -1}],
  TestID -> "Untitled-91@@Tests/Rules.wlt:224,1-228,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[i^2, {i, 3, 9, 2}]]]],
  ToCanonicalForm[HoldForm[Table[i^2, {i, 9, 3, -2}]]],
  TestID -> "Untitled-92@@Tests/Rules.wlt:230,1-234,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Array[Range @* RandomInteger, 10, 3]]],
  HoldForm[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, RandomValue[DiscreteUniformDistribution[{0, 2 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer]}]], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1, 10, 1}]],
  TestID -> "Untitled-93@@Tests/Rules.wlt:236,1-240,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - 1 & )[Range[5]]]],
  HoldForm[Table[-1 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-94@@Tests/Rules.wlt:242,1-246,2"
];

VerificationTest[
    MakeCanonicalForm[ # - RandomInteger[ # ] &[ Range[ 3, 7, 3 ] ] ],
    HoldForm @ {
        3 - RandomValue @ DiscreteUniformDistribution @ { 3, 6 },
        6 - RandomValue @ DiscreteUniformDistribution @ { 3, 6 }
    },
    TestID -> "Untitled-95@@Tests/Rules.wlt:248,1-255,2"
];

(* VerificationTest[
  CodeEquivalentQ[Take[Sort[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[]], 10], Sort[TakeSmallestBy[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[], First, 10]]],
  True,
  TestID -> "Untitled-96@@Tests/Rules.wlt:248,1-252,2"
]; *)

VerificationTest[
  StringQ @ Environment[ "GITHUB_ACTIONS" ] || CodeEquivalentQ[Graphics3D[{Orange, Sphere[RandomReal[1, {100, 3}], 0.05]}], Graphics3D[{Orange, Table[Sphere[RandomReal[1, {3}], 0.05], {100}]}]],
  True,
  TestID -> "Untitled-97@@Tests/Rules.wlt:263,1-267,2"
];

VerificationTest[
  CodeEquivalentQ[(Framed[Style[#1, RandomColor[]], Background -> RandomColor[]] & ) /@ Alphabet[], (Framed[#1, Background -> RandomColor[]] & ) /@ (Style[#1, RandomColor[]] & ) /@ Alphabet[]],
  TestID -> "Untitled-98@@Tests/Rules.wlt:269,1-272,2"
];

VerificationTest[
  CodeEquivalentQ[NestList[#1 + First[RandomReal[{-1, 1}, {1, 3}]] & , {0, 0, 0}, 1000], NestList[#1 + RandomReal[{-1, 1}, 3] & , {0, 0, 0}, 1000]],
  TestID -> "Untitled-99@@Tests/Rules.wlt:274,1-277,2"
];

VerificationTest[
  CodeEquivalentQ[TakeLargestBy[WordList[], StringLength, 10], Take[SortBy[WordList[], StringLength[#1] & ], -10]],
  TestID -> "Untitled-100@@Tests/Rules.wlt:279,1-282,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[Dilation[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-101@@Tests/Rules.wlt:284,1-288,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[img, 1]],
  TestID -> "Untitled-102@@Tests/Rules.wlt:290,1-294,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[Erosion[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-103@@Tests/Rules.wlt:296,1-300,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[img, 1]],
  TestID -> "Untitled-104@@Tests/Rules.wlt:302,1-306,2"
];

VerificationTest[
  MakeCanonicalForm[StringTake["test string", 4]],
  MakeCanonicalForm[StringJoin[Take[Characters["test string"], 4]]],
  TestID -> "Untitled-105@@Tests/Rules.wlt:308,1-312,2"
]

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[StringJoin[Table[FromLetterNumber[RandomInteger[25] + 1], 5]], 100]]],
  ToCanonicalForm[HoldComplete[Table[StringJoin[FromLetterNumber[Table[RandomInteger[25] + 1, 5]]], 100]]],
  TestID -> "Untitled-106@@Tests/Rules.wlt:314,1-318,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Column[{Style[n, Plain], Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[Column[{n, Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  TestID -> "Untitled-107@@Tests/Rules.wlt:320,1-324,2"
];

VerificationTest[
  ToCanonicalForm[StringJoin[#1, StringReverse[#1]] & ],
  ToCanonicalForm[StringJoin[#1, StringJoin[Reverse[Characters[#1]]]] & ],
  TestID -> "Untitled-108@@Tests/Rules.wlt:326,1-330,2"
];

VerificationTest[
    MakeCanonicalForm @ Replace[ x, y_Integer :> y + 1, 2 ],
    MakeCanonicalForm @ Replace[ x, y_Integer :> y + 1, { 1, 2 }, Heads -> False ],
    TestID -> "Replace@@Tests/Rules.wlt:332,1-336,2"
]

VerificationTest[
    MakeCanonicalForm @ Replace[ x, y_ :> y + 1, All, Heads -> True ],
    MakeCanonicalForm[ x /. y_ :> y + 1 ],
    TestID -> "ReplaceAll@@Tests/Rules.wlt:338,1-342,2"
]

VerificationTest[
    MakeCanonicalForm @ Replace[ x, y_ :> y + 1, { 0, Infinity }, Heads -> True ],
    MakeCanonicalForm[ x /. y_ :> y + 1 ],
    TestID -> "ReplaceAll@@Tests/Rules.wlt:344,1-348,2"
]

VerificationTest[
    MakeCanonicalForm @ If[ TrueQ @ a, b, c ],
    MakeCanonicalForm @ If[ a, b, c, c ],
    TestID -> "If@@Tests/Rules.wlt:350,1-354,2"
]

VerificationTest[
    MakeCanonicalForm @ If[ TrueQ @ True, a, b ],
    MakeCanonicalForm @ If[ TrueQ @ False, b, a ],
    TestID -> "If-TrueQ@@Tests/Rules.wlt:356,1-360,2"
]

VerificationTest[
    MakeCanonicalForm @ TrueQ @ StringQ @ x,
    MakeCanonicalForm @ StringQ @ x,
    TestID -> "TrueQ-StringQ@@Tests/Rules.wlt:362,1-366,2"
]

VerificationTest[
    MakeCanonicalForm @ Which[ a, b, True, c ],
    MakeCanonicalForm @ If[ a, b, c ],
    TestID -> "Which-If@@Tests/Rules.wlt:368,1-372,2"
]

VerificationTest[
    MakeCanonicalForm @ Which[ a, b, c, d, True, e ],
    MakeCanonicalForm @ If[ a, b, If[ c, d, e ] ],
    TestID -> "Which-If@@Tests/Rules.wlt:374,1-378,2"
]

VerificationTest[
    MakeCanonicalForm @ Switch[ x, _Integer, 1, _String, 2, _, 3 ],
    MakeCanonicalForm @ Replace[ x, { _Integer :> 1, _String :> 2, _ :> 3 } ],
    TestID -> "Switch-Replace@@Tests/Rules.wlt:380,1-384,2"
]


VerificationTest[
    MakeCanonicalForm[ 1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)) ],
    MakeCanonicalForm[ 1/(1 + x^3) ],
    TestID -> "Simplify-Canonical-Forms@@Tests/Rules.wlt:387,1-391,2"
]

VerificationTest[
    ToCanonicalForm @ HoldComplete[ 1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)) ],
    MakeCanonicalForm[ 1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)), HoldComplete],
    TestID -> "ToCanonicalForm-MakeCanonicalForm-Comparison@@Tests/Rules.wlt:393,1-397,2"
]

VerificationTest[
    CodeEquivalentQ[
        1/(3 (1 + x)) - (-1 + 2 x)/(6 (1 - x + x^2)) + 2/(3 (1 + 1/3 (-1 + 2 x)^2)),
        1.0/(1 + x^3)
    ],
    TestID -> "Math-Equality-Testing@@Tests/Rules.wlt:399,1-405,2"
]

VerificationTest[
    MakeCanonicalForm[ Table[ If[ IntegerQ @ x, x + 1, $Failed ], { x, 10 } ] ],
    MakeCanonicalForm[ Range[ 10 ] + 1 ],
    TestID -> "If-Type-Optimization-1@@Tests/Rules.wlt:407,1-411,2"
]

VerificationTest[
    MakeCanonicalForm[ Table[ If[ IntegerQ @ x, x + 1, $Failed ], { x, 10 } ] ],
    MakeCanonicalForm[ Table[ If[ AtomQ @ x, x + 1, $Failed ], { x, 10 } ] ],
    TestID -> "If-Type-Optimization-2@@Tests/Rules.wlt:413,1-417,2"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <| |>,
    HoldForm @ True,
    TestID -> "AssociationQ-1@@Tests/Rules.wlt:419,1-423,2"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <| { "a" -> 1, <| "b" -> 2 |> } |>,
    HoldForm @ True,
    TestID -> "AssociationQ-2@@Tests/Rules.wlt:425,1-429,2"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <|  "a" -> 1, <| "b" -> 2 |> |>,
    HoldForm @ True,
    TestID -> "AssociationQ-3@@Tests/Rules.wlt:431,1-435,2"
]

VerificationTest[
    MakeCanonicalForm @ AssociationQ @ <| 123 |>,
    HoldForm @ AssociationQ @ <| 123 |>,
    TestID -> "AssociationQ-4@@Tests/Rules.wlt:437,1-441,2"
]

VerificationTest[
    MakeCanonicalForm @ Table[
        IntegerQ @ n,
        { n, Table[ StringLength @ x, { x, { "a", "bb", "ccc", "dddd" } } ] }
    ],
    HoldForm @ Table[ True, _ ],
    SameTest -> MatchQ,
    TestID   -> "StringLength-To-Integer-Type@@Tests/Rules.wlt:443,1-451,2"
]

VerificationTest[
    MakeCanonicalForm @ { 1.1, 2.2, 3.3, 4.4, 5.5 },
    MakeCanonicalForm @ Table[ x, { x, 1.1, 5.5, 1.1 } ],
    TestID -> "Roll-Real-Tables@@Tests/Rules.wlt:453,1-457,2"
]

VerificationTest[
    MakeCanonicalForm @ { 1, 3/2, 2, 5/2, 3, 7/2, 4, 9/2, 5 },
    MakeCanonicalForm @ Table[ i, { i, 1, 5, 1/2 } ],
    TestID -> "Roll-Mixed-Rational-Tables@@Tests/Rules.wlt:459,1-463,2"
]

VerificationTest[
    MakeCanonicalForm[ Plus @@ { } ],
    HoldForm[ 0 ],
    TestID -> "Plus-Zero-Arguments@@Tests/Rules.wlt:465,1-469,2"
]

VerificationTest[
    MakeCanonicalForm[ StringLength /@ { "a", "bb", "ccc" } ],
    MakeCanonicalForm[ Table[ Length @ Characters @ x, { x, { "a", "bb", "ccc" } } ] ],
    TestID -> "Map-Strings-To-Table@@Tests/Rules.wlt:471,1-475,2"
]

VerificationTest[
    MakeCanonicalForm[ IntegerQ /@ StringLength /@ { "a", "bb", "ccc" } ],
    MakeCanonicalForm[ Array[ True &, 3 ] ],
    TestID -> "Simplify-Literals@@Tests/Rules.wlt:477,1-481,2"
]

VerificationTest[
    MakeCanonicalForm[ IntegerQ /@ StringLength /@ {  } ],
    MakeCanonicalForm[ { } ],
    TestID -> "Empty-Tables-1@@Tests/Rules.wlt:483,1-487,2"
]

VerificationTest[
    MakeCanonicalForm[ Table[ i + 1, { i, 0 } ] ],
    MakeCanonicalForm[ { } ],
    TestID -> "Empty-Tables-2@@Tests/Rules.wlt:489,1-493,2"
]

VerificationTest[
    MakeCanonicalForm[ 2 * Range[ 3 ] + 1 ],
    HoldForm @ { 3, 5, 7 },
    TestID -> "Small-Tables@@Tests/Rules.wlt:495,1-499,2"
]

VerificationTest[
    MakeCanonicalForm[ 2 * Range[ 4 ] + 1 ],
    HoldForm[ _Table ],
    SameTest -> MatchQ,
    TestID   -> "Not-Small-Tables-1@@Tests/Rules.wlt:501,1-506,2"
]

VerificationTest[
    MakeCanonicalForm[ 2 * Range[ 4 ] + 1 ],
    MakeCanonicalForm[ { 3, 5, 7, 9 } ],
    TestID -> "Not-Small-Tables-2@@Tests/Rules.wlt:508,1-512,2"
]

VerificationTest[
    MakeCanonicalForm @ BarChart @ EntityValue[ EntityClass[ "Planet", All ], "Mass" ],
    MakeCanonicalForm @ BarChart @ EntityClass[ "Planet", All ][ "Mass" ],
    TestID -> "EntityClass-Property@@Tests/Rules.wlt:514,1-518,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, { { 1 } -> "a", 3 -> "b" } ],
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, { 1 -> "a", { 3 } -> "b" } ],
    TestID -> "ReplacePart-1@@Tests/Rules.wlt:520,1-524,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, 1 -> "a" ],
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, { 1 -> "a" } ],
    TestID -> "ReplacePart-2@@Tests/Rules.wlt:526,1-530,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, { 1 -> "a" } ],
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, { 1 } -> "a" ],
    TestID -> "ReplacePart-3@@Tests/Rules.wlt:532,1-536,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, 1 -> "a", Heads -> Automatic ],
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, 1 -> "a" ],
    TestID -> "ReplacePart-4@@Tests/Rules.wlt:538,1-542,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ Hold[ 1, 2, 3 ], { { 1 } :> Echo[ "a" ], 3 -> "b" } ],
    MakeCanonicalForm @ ReplacePart[ Hold[ 1, 2, 3 ], { 1 :> Echo[ "a" ], { 3 } -> "b" } ],
    TestID -> "ReplacePart-5@@Tests/Rules.wlt:544,1-548,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ { 1 -> "a", 3 -> "b" } ][ { 1, 2, 3 } ],
    MakeCanonicalForm @ ReplacePart[ { 1, 2, 3 }, { 1 -> "a", 3 -> "b" } ],
    TestID -> "ReplacePart-6@@Tests/Rules.wlt:550,1-554,2"
]

VerificationTest[
    MakeCanonicalForm @ ReplacePart[ 1 -> "a" ],
    MakeCanonicalForm @ ReplacePart[ { { 1 } -> "a" } ],
    TestID -> "ReplacePart-7@@Tests/Rules.wlt:556,1-560,2"
]

VerificationTest[
    MakeCanonicalForm[ Map[ Sqrt ][ { 1, 2, 3 } ] ],
    MakeCanonicalForm[ Map[ Sqrt, { 1, 2, 3 } ] ],
    TestID -> "OperatorForms-1@@Tests/Rules.wlt:562,1-566,2"
]

VerificationTest[
    MakeCanonicalForm[ Replace[ 1 -> "x" ][ 1 ] ],
    MakeCanonicalForm[ Replace[ 1, 1 -> "x" ] ],
    TestID -> "OperatorForms-2@@Tests/Rules.wlt:568,1-572,2"
]

VerificationTest[
    MakeCanonicalForm[ Insert[ "x", 2 ][ { 1, 2, 3 } ] ],
    MakeCanonicalForm[ Insert[ { 1, 2, 3 }, "x", 2 ] ],
    TestID -> "OperatorForms-3@@Tests/Rules.wlt:574,1-578,2"
]

VerificationTest[
    MakeCanonicalForm[ Map[ Sqrt, # ] & ],
    MakeCanonicalForm[ Map[ Sqrt ] ],
    TestID -> "OperatorForms-4@@Tests/Rules.wlt:580,1-584,2"
]

VerificationTest[
    MakeCanonicalForm[ Replace[ 2 -> "x" ] ],
    MakeCanonicalForm[ Replace[ #, 2 -> "x" ] & ],
    TestID -> "OperatorForms-5@@Tests/Rules.wlt:586,1-590,2"
]

VerificationTest[
    MakeCanonicalForm[ Insert[ "x", 3 ] ],
    MakeCanonicalForm[ Insert[ #, "x", 3 ] & ],
    TestID -> "OperatorForms-6@@Tests/Rules.wlt:592,1-596,2"
]

VerificationTest[
    MakeCanonicalForm[ Equal[ a, b ] && Equal[ c, b ] && Equal[ d, b ] ],
    MakeCanonicalForm[ Equal[ a, b, c, b, d, b ] ],
    TestID -> "Equal-1@@Tests/Rules.wlt:598,1-602,2"
]

VerificationTest[
    MakeCanonicalForm[ Equal[ a, b ] && Equal[ c, b ] && Equal[ d, b ] ],
    MakeCanonicalForm[ Equal[ a, b, c, d ] ],
    TestID -> "Equal-2@@Tests/Rules.wlt:604,1-608,2"
]

VerificationTest[
    MakeCanonicalForm[ Equal[ c, d, a, b ] ],
    MakeCanonicalForm[ Equal[ a, b, c, d ] ],
    TestID -> "Equal-3@@Tests/Rules.wlt:610,1-614,2"
]

VerificationTest[
    MakeCanonicalForm @ Select[ Range[ 1000 ], Equal[ Mod[ #1, 7 ], Mod[ #1, 8 ], 1 ] & ],
    MakeCanonicalForm @ Select[ Range[ 1000 ], Equal[ Mod[ #1, 7 ], 1 ] && Equal[ Mod[ #1, 8 ], 1 ] & ],
    TestID -> "Nested-Select-Equal@@Tests/Rules.wlt:616,1-620,2"
]

VerificationTest[
    MakeCanonicalForm[ a === b && c === b && d === a ],
    MakeCanonicalForm[ c === d === a === b ],
    TestID -> "SameQ-And@@Tests/Rules.wlt:622,1-626,2"
]

VerificationTest[
    MakeCanonicalForm[ Echo /@ { 2, 8, 18, 32, 50, 72, 98, 128, 162, 200 } ],
    MakeCanonicalForm[ Table[ Echo[ 2 * x^2 ], { x, 10 } ] ],
    TestID -> "Eliminate-Iterator-Table@@Tests/Rules.wlt:628,1-632,2"
]

VerificationTest[
    MakeCanonicalForm[ 1 == 2 ],
    HoldForm[ False ],
    TestID -> "InertEqual-1@@Tests/Rules.wlt:634,1-638,2"
]

VerificationTest[
    MakeCanonicalForm[ "a" == 2.3 ],
    HoldForm[ False ],
    TestID -> "InertEqual-2@@Tests/Rules.wlt:640,1-644,2"
]

VerificationTest[
    MakeCanonicalForm[ "a" == a ],
    HoldForm[ _Equal ],
    SameTest -> MatchQ,
    TestID   -> "InertEqual-3@@Tests/Rules.wlt:646,1-651,2"
]

VerificationTest[
    Length @ MakeCanonicalForm[
        Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ 1 ],
        Trace -> True,
        "MaxIterations" -> 50
    ] < 50,
    True,
    { ToCanonicalForm::cycle },
    TestID -> "Cycle-Detection-1@@Tests/Rules.wlt:653,1-662,2"
]

VerificationTest[
    MakeCanonicalForm @ Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ 1 ],
    HoldForm @ Wolfram`CodeEquivalenceUtilities`Internal`LoopTest[ 1, 1 ],
    { ToCanonicalForm::cycle },
    TestID -> "Cycle-Detection-2@@Tests/Rules.wlt:664,1-669,2"
]

VerificationTest[
    MakeCanonicalForm @ IntegerQ @ Length @ { 1, 2, 3 },
    HoldForm @ True,
    TestID -> "IntTypeQ-Length-1@@Tests/Rules.wlt:671,1-675,2"
]

VerificationTest[
    MakeCanonicalForm @ Range @ Length @ Range[ 5 ],
    MakeCanonicalForm @ Range[ 5 ],
    TestID -> "IntType-Length-2@@Tests/Rules.wlt:677,1-681,2"
]

VerificationTest[
    MakeCanonicalForm @ Table[ i, { i, Count[ Range[ 5 ], _Integer ] } ],
    HoldForm @ Table[ _TypedSymbol, _ ],
    SameTest -> MatchQ,
    TestID   -> "IntTypeQ-Count-1@@Tests/Rules.wlt:683,1-688,2"
]

VerificationTest[
    MakeCanonicalForm @ Count[ Range[ 10 ], x_ ],
    HoldForm[ 10 ],
    TestID -> "IntTypeQ-Count-2@@Tests/Rules.wlt:690,1-694,2"
]

VerificationTest[
    MakeCanonicalForm @ Table[ i, { i, Count @ Range[ 5 ] } ],
    HoldForm @ Table[ _Symbol, _ ],
    SameTest -> MatchQ,
    TestID   -> "Not-IntTypeQ-Count@@Tests/Rules.wlt:696,1-701,2"
]

VerificationTest[
    CodeEquivalentQ[ Range[ 4 ], Range @ { 1, 2, 3, 4 } ],
    False,
    SameTest -> MatchQ,
    TestID   -> "Validate-Range-Arguments@@Tests/Rules.wlt:703,1-708,2"
]

VerificationTest[
    CodeEquivalentQ[ Max @ IntegerDigits[ 2^20 ], IntegerDigits[ 2^20 ] ],
    False,
    SameTest -> MatchQ,
    TestID   -> "Preserve-Max-In-Simplify@@Tests/Rules.wlt:710,1-715,2"
]

(* :!CodeAnalysis::EndBlock:: *)