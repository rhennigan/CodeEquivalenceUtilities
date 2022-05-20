(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet @ ResourceObject;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest[
  Replace[ToCanonicalForm[HoldComplete[Reverse[RandomColor[100]]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]] === Replace[ToCanonicalForm[HoldComplete[Table[RandomColor[], 100]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]],
  TestID -> "Untitled-55@@Tests/Rules.wlt:12,1-15,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]],
  TestID -> "Untitled-56@@Tests/Rules.wlt:17,1-21,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]] === Table[n + Range[n], {n, 10}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]] === Table[(n + 1) + Range[n + 1], {n, 0, 9}],
  TestID -> "Untitled-57@@Tests/Rules.wlt:23,1-26,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-58@@Tests/Rules.wlt:28,1-32,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]] === Table[Range[2^3 + 2]^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-59@@Tests/Rules.wlt:34,1-37,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-60@@Tests/Rules.wlt:39,1-43,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]] === Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-61@@Tests/Rules.wlt:45,1-48,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[RandomInteger /@ Reverse[Range[5]]]]],
  ToCanonicalForm[HoldComplete[RandomInteger /@ Range[5]]],
  TestID -> "Untitled-62@@Tests/Rules.wlt:50,1-54,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[IntegerDigits[4294967296]][[2]]]],
  ToCanonicalForm[HoldComplete[IntegerDigits[4294967296][[-2]]]],
  TestID -> "Untitled-63@@Tests/Rules.wlt:56,1-60,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[i^3 - i^2, {i, 10}]]],
  TestID -> "Untitled-64@@Tests/Rules.wlt:62,1-66,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Style["A", 100]]],
  ToCanonicalForm[HoldForm[Style["A", FontSize -> 100]]],
  TestID -> "Untitled-65@@Tests/Rules.wlt:68,1-72,2"
];

VerificationTest[
  CodeEquivalentQ[#1 + 2 & , Function[n, n + 2]],
  TestID -> "Untitled-66@@Tests/Rules.wlt:74,1-77,2"
];

VerificationTest[
  CodeEquivalentQ[2*(x + 1), 2*x + 2],
  TestID -> "Untitled-67@@Tests/Rules.wlt:79,1-82,2"
];

VerificationTest[
  CodeEquivalentQ[2*(#1 + 1) & , 2*#1 + 2 & ],
  TestID -> "Untitled-68@@Tests/Rules.wlt:84,1-87,2"
];

VerificationTest[
  CodeEquivalentQ[Grid[RandomColor[{10, 10}]], Grid[Array[RandomColor[] & , {10, 10}]]],
  TestID -> "Untitled-69@@Tests/Rules.wlt:89,1-92,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[4]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-70@@Tests/Rules.wlt:94,1-98,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Reverse[Range[4]]]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-71@@Tests/Rules.wlt:100,1-104,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListPlot[Range[10, 14]]]],
  ToCanonicalForm[HoldComplete[ListPlot[{10, 11, 12, 13, 14}]]],
  TestID -> "Untitled-72@@Tests/Rules.wlt:106,1-110,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Column[{1, 2, 3, 4, 5}]]],
  ToCanonicalForm[HoldComplete[Column[Range[5]]]],
  TestID -> "Untitled-73@@Tests/Rules.wlt:112,1-116,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[Range[5]^2]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[{1, 4, 9, 16, 25}]]],
  TestID -> "Untitled-74@@Tests/Rules.wlt:118,1-122,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-75@@Tests/Rules.wlt:124,1-128,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[0, 10] + 10]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-76@@Tests/Rules.wlt:130,1-134,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]*3]],
  ToCanonicalForm[HoldComplete[3*Range[10]]],
  TestID -> "Untitled-77@@Tests/Rules.wlt:136,1-140,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 2, 20, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 20, 2]]],
  TestID -> "Untitled-78@@Tests/Rules.wlt:142,1-146,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[BarChart[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[BarChart[Table[n^2, {n, 10}]]]],
  TestID -> "Untitled-79@@Tests/Rules.wlt:148,1-152,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[IntegerDigits[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[Table[IntegerDigits[n^2], {n, 10}]]],
  TestID -> "Untitled-80@@Tests/Rules.wlt:154,1-158,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[n^3 - n^2, {n, 10}]]],
  TestID -> "Untitled-81@@Tests/Rules.wlt:160,1-164,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 1, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[1, 100, 2]]],
  TestID -> "Untitled-82@@Tests/Rules.wlt:166,1-170,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n^2, {n, 2, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 100, 2]^2]],
  TestID -> "Untitled-83@@Tests/Rules.wlt:172,1-176,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[1/Range[20]]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[Table[1/n, {n, 20}]]]],
  TestID -> "Untitled-84@@Tests/Rules.wlt:178,1-182,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]]]],
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[n], {n, 100}]]]],
  TestID -> "Untitled-85@@Tests/Rules.wlt:184,1-188,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{Red, Yellow, Green}[[RandomInteger[2] + 1]], 100]]],
  ToCanonicalForm[HoldComplete[Table[{Red, Green, Yellow}[[RandomInteger[2] + 1]], 100]]],
  TestID -> "Untitled-86@@Tests/Rules.wlt:190,1-194,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Array[Range, 5, 2]]]],
  HoldComplete[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 7 - TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-87@@Tests/Rules.wlt:196,1-200,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  HoldComplete[Table[9 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 11, 1}]],
  TestID -> "Untitled-88@@Tests/Rules.wlt:202,1-206,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[Floor[3*i^2 - 2*i], {i, 5}]]]],
  With[{n = -34}, HoldForm[Table[96 + n*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer] + 3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer]^2, {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 5, 1}]]],
  TestID -> "Untitled-89@@Tests/Rules.wlt:208,1-212,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[3*i^2 - 4*i + 3, {i, -4, 13, 1}]]]],
  ToCanonicalForm[HoldForm[{458, 387, 322, 263, 210, 163, 122, 87, 58, 35, 18, 7, 2, 3, 10, 23, 42, 67}]],
  TestID -> "Untitled-90@@Tests/Rules.wlt:214,1-218,2"
];

VerificationTest[
  ReleaseHold[FromCanonicalForm[ToCanonicalForm[HoldForm[Table[i, {i, 1, -3, -1}]]]]],
  Table[i, {i, 1, -3, -1}],
  TestID -> "Untitled-91@@Tests/Rules.wlt:220,1-224,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[i^2, {i, 3, 9, 2}]]]],
  ToCanonicalForm[HoldForm[Table[i^2, {i, 9, 3, -2}]]],
  TestID -> "Untitled-92@@Tests/Rules.wlt:226,1-230,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Array[Range @* RandomInteger, 10, 3]]],
  HoldForm[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, RandomValue[DiscreteUniformDistribution[{0, 2 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer]}]], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S2, _Integer], 1, 10, 1}]],
  TestID -> "Untitled-93@@Tests/Rules.wlt:232,1-236,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - 1 & )[Range[5]]]],
  HoldForm[Table[-1 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-94@@Tests/Rules.wlt:238,1-242,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - RandomInteger[#1] & )[Range[3, 7, 3]]]],
  HoldForm[(-RandomInteger[#1] + #1 & )[Table[3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`LocalSymbols`S1, _Integer], 1, 2, 1}]]],
  TestID -> "Untitled-95@@Tests/Rules.wlt:244,1-248,2"
];

(* VerificationTest[
  CodeEquivalentQ[Take[Sort[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[]], 10], Sort[TakeSmallestBy[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[], First, 10]]],
  True,
  TestID -> "Untitled-96@@Tests/Rules.wlt:248,1-252,2"
]; *)

VerificationTest[
  StringQ @ Environment[ "GITHUB_ACTIONS" ] || CodeEquivalentQ[Graphics3D[{Orange, Sphere[RandomReal[1, {100, 3}], 0.05]}], Graphics3D[{Orange, Table[Sphere[RandomReal[1, {3}], 0.05], {100}]}]],
  True,
  TestID -> "Untitled-97@@Tests/Rules.wlt:256,1-260,2"
];

VerificationTest[
  CodeEquivalentQ[(Framed[Style[#1, RandomColor[]], Background -> RandomColor[]] & ) /@ Alphabet[], (Framed[#1, Background -> RandomColor[]] & ) /@ (Style[#1, RandomColor[]] & ) /@ Alphabet[]],
  TestID -> "Untitled-98@@Tests/Rules.wlt:262,1-265,2"
];

VerificationTest[
  CodeEquivalentQ[NestList[#1 + First[RandomReal[{-1, 1}, {1, 3}]] & , {0, 0, 0}, 1000], NestList[#1 + RandomReal[{-1, 1}, 3] & , {0, 0, 0}, 1000]],
  TestID -> "Untitled-99@@Tests/Rules.wlt:267,1-270,2"
];

VerificationTest[
  CodeEquivalentQ[TakeLargestBy[WordList[], StringLength, 10], Take[SortBy[WordList[], StringLength[#1] & ], -10]],
  TestID -> "Untitled-100@@Tests/Rules.wlt:272,1-275,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[Dilation[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-101@@Tests/Rules.wlt:277,1-281,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[img, 1]],
  TestID -> "Untitled-102@@Tests/Rules.wlt:283,1-287,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[Erosion[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-103@@Tests/Rules.wlt:289,1-293,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[img, 1]],
  TestID -> "Untitled-104@@Tests/Rules.wlt:295,1-299,2"
];

VerificationTest[
  MakeCanonicalForm[StringTake["test string", 4]],
  MakeCanonicalForm[StringJoin[Take[Characters["test string"], 4]]],
  TestID -> "Untitled-105@@Tests/Rules.wlt:301,1-305,2"
]

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[StringJoin[Table[FromLetterNumber[RandomInteger[25] + 1], 5]], 100]]],
  ToCanonicalForm[HoldComplete[Table[StringJoin[FromLetterNumber[Table[RandomInteger[25] + 1, 5]]], 100]]],
  TestID -> "Untitled-106@@Tests/Rules.wlt:307,1-311,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Column[{Style[n, Plain], Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[Column[{n, Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  TestID -> "Untitled-107@@Tests/Rules.wlt:313,1-317,2"
];

VerificationTest[
  ToCanonicalForm[StringJoin[#1, StringReverse[#1]] & ],
  ToCanonicalForm[StringJoin[#1, StringJoin[Reverse[Characters[#1]]]] & ],
  TestID -> "Untitled-108@@Tests/Rules.wlt:319,1-323,2"
];