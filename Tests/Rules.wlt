(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet @ ResourceObject;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest[
  Replace[ToCanonicalForm[HoldComplete[Reverse[RandomColor[100]]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]] === Replace[ToCanonicalForm[HoldComplete[Table[RandomColor[], 100]]], HoldComplete[e_] :> EvaluateSafely[e, "SymbolList" -> Join[$UnsafeSymbols, $RandomSymbols]]],
  TestID -> "Untitled-55@@Tests/Rules.wlt:10,1-13,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]],
  TestID -> "Untitled-56@@Tests/Rules.wlt:15,1-19,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n + Range[n], {n, 10}]]] === Table[n + Range[n], {n, 10}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[(n + 1) + Range[n + 1], {n, 0, 9}]]] === Table[(n + 1) + Range[n + 1], {n, 0, 9}],
  TestID -> "Untitled-57@@Tests/Rules.wlt:21,1-24,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-58@@Tests/Rules.wlt:26,1-30,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[Range[2^3 + 2]^(n + 1), {n, 3}]]] === Table[Range[2^3 + 2]^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-59@@Tests/Rules.wlt:32,1-35,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]],
  ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]],
  TestID -> "Untitled-60@@Tests/Rules.wlt:37,1-41,2"
];

VerificationTest[
  EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}]]] === Table[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), {n, 3}] === EvaluateSafely @@ ToCanonicalForm[HoldComplete[Table[n^p, {p, 2, 4}, {n, 10}]]] === Table[n^p, {p, 2, 4}, {n, 10}],
  TestID -> "Untitled-61@@Tests/Rules.wlt:43,1-46,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[RandomInteger /@ Reverse[Range[5]]]]],
  ToCanonicalForm[HoldComplete[RandomInteger /@ Range[5]]],
  TestID -> "Untitled-62@@Tests/Rules.wlt:48,1-52,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[IntegerDigits[4294967296]][[2]]]],
  ToCanonicalForm[HoldComplete[IntegerDigits[4294967296][[-2]]]],
  TestID -> "Untitled-63@@Tests/Rules.wlt:54,1-58,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[i^3 - i^2, {i, 10}]]],
  TestID -> "Untitled-64@@Tests/Rules.wlt:60,1-64,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Style["A", 100]]],
  ToCanonicalForm[HoldForm[Style["A", FontSize -> 100]]],
  TestID -> "Untitled-65@@Tests/Rules.wlt:66,1-70,2"
];

VerificationTest[
  CodeEquivalentQ[#1 + 2 & , Function[n, n + 2]],
  TestID -> "Untitled-66@@Tests/Rules.wlt:72,1-75,2"
];

VerificationTest[
  CodeEquivalentQ[2*(x + 1), 2*x + 2],
  TestID -> "Untitled-67@@Tests/Rules.wlt:77,1-80,2"
];

VerificationTest[
  CodeEquivalentQ[2*(#1 + 1) & , 2*#1 + 2 & ],
  TestID -> "Untitled-68@@Tests/Rules.wlt:82,1-85,2"
];

VerificationTest[
  CodeEquivalentQ[Grid[RandomColor[{10, 10}]], Grid[Array[RandomColor[] & , {10, 10}]]],
  TestID -> "Untitled-69@@Tests/Rules.wlt:87,1-90,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[4]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-70@@Tests/Rules.wlt:92,1-96,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Reverse[Range[4]]]]],
  ToCanonicalForm[HoldComplete[Reverse[Reverse[{1, 2, 3, 4}]]]],
  TestID -> "Untitled-71@@Tests/Rules.wlt:98,1-102,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListPlot[Range[10, 14]]]],
  ToCanonicalForm[HoldComplete[ListPlot[{10, 11, 12, 13, 14}]]],
  TestID -> "Untitled-72@@Tests/Rules.wlt:104,1-108,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Column[{1, 2, 3, 4, 5}]]],
  ToCanonicalForm[HoldComplete[Column[Range[5]]]],
  TestID -> "Untitled-73@@Tests/Rules.wlt:110,1-114,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[Range[5]^2]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[{1, 4, 9, 16, 25}]]],
  TestID -> "Untitled-74@@Tests/Rules.wlt:116,1-120,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-75@@Tests/Rules.wlt:122,1-126,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[0, 10] + 10]],
  ToCanonicalForm[HoldComplete[9 + Range[11]]],
  TestID -> "Untitled-76@@Tests/Rules.wlt:128,1-132,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]*3]],
  ToCanonicalForm[HoldComplete[3*Range[10]]],
  TestID -> "Untitled-77@@Tests/Rules.wlt:134,1-138,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 2, 20, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 20, 2]]],
  TestID -> "Untitled-78@@Tests/Rules.wlt:140,1-144,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[BarChart[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[BarChart[Table[n^2, {n, 10}]]]],
  TestID -> "Untitled-79@@Tests/Rules.wlt:146,1-150,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[IntegerDigits[Range[10]^2]]],
  ToCanonicalForm[HoldComplete[Table[IntegerDigits[n^2], {n, 10}]]],
  TestID -> "Untitled-80@@Tests/Rules.wlt:152,1-156,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[10]^3 - Range[10]^2]],
  ToCanonicalForm[HoldComplete[Table[n^3 - n^2, {n, 10}]]],
  TestID -> "Untitled-81@@Tests/Rules.wlt:158,1-162,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n, {n, 1, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[1, 100, 2]]],
  TestID -> "Untitled-82@@Tests/Rules.wlt:164,1-168,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[n^2, {n, 2, 100, 2}]]],
  ToCanonicalForm[HoldComplete[Range[2, 100, 2]^2]],
  TestID -> "Untitled-83@@Tests/Rules.wlt:170,1-174,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[NumberLinePlot[1/Range[20]]]],
  ToCanonicalForm[HoldComplete[NumberLinePlot[Table[1/n, {n, 20}]]]],
  TestID -> "Untitled-84@@Tests/Rules.wlt:176,1-180,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]]]],
  ToCanonicalForm[HoldComplete[ListLinePlot[Table[RandomInteger[n], {n, 100}]]]],
  TestID -> "Untitled-85@@Tests/Rules.wlt:182,1-186,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[{Red, Yellow, Green}[[RandomInteger[2] + 1]], 100]]],
  ToCanonicalForm[HoldComplete[Table[{Red, Green, Yellow}[[RandomInteger[2] + 1]], 100]]],
  TestID -> "Untitled-86@@Tests/Rules.wlt:188,1-192,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Reverse[Array[Range, 5, 2]]]],
  HoldComplete[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], 1, 7 - TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S2, _Integer], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S2, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-87@@Tests/Rules.wlt:194,1-198,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Range[11] + 9]],
  HoldComplete[Table[9 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], 1, 11, 1}]],
  TestID -> "Untitled-88@@Tests/Rules.wlt:200,1-204,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[Floor[3*i^2 - 2*i], {i, 5}]]]],
  HoldForm[Table[96 - 34*TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer] + 3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer]^2, {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-89@@Tests/Rules.wlt:206,1-210,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[3*i^2 - 4*i + 3, {i, -4, 13, 1}]]]],
  ToCanonicalForm[HoldForm[{458, 387, 322, 263, 210, 163, 122, 87, 58, 35, 18, 7, 2, 3, 10, 23, 42, 67}]],
  TestID -> "Untitled-90@@Tests/Rules.wlt:212,1-216,2"
];

VerificationTest[
  ReleaseHold[FromCanonicalForm[ToCanonicalForm[HoldForm[Table[i, {i, 1, -3, -1}]]]]],
  Table[i, {i, 1, -3, -1}],
  TestID -> "Untitled-91@@Tests/Rules.wlt:218,1-222,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Reverse[Table[i^2, {i, 3, 9, 2}]]]],
  ToCanonicalForm[HoldForm[Table[i^2, {i, 9, 3, -2}]]],
  TestID -> "Untitled-92@@Tests/Rules.wlt:224,1-228,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[Array[Range @* RandomInteger, 10, 3]]],
  HoldForm[Table[Table[TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], 1, RandomValue[DiscreteUniformDistribution[{0, 2 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S2, _Integer]}]], 1}], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S2, _Integer], 1, 10, 1}]],
  TestID -> "Untitled-93@@Tests/Rules.wlt:230,1-234,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - 1 & )[Range[5]]]],
  HoldForm[Table[-1 + TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], 1, 5, 1}]],
  TestID -> "Untitled-94@@Tests/Rules.wlt:236,1-240,2"
];

VerificationTest[
  ToCanonicalForm[HoldForm[(#1 - RandomInteger[#1] & )[Range[3, 7, 3]]]],
  HoldForm[(-RandomInteger[#1] + #1 & )[Table[3*TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], {TypedSymbol[Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`LocalSymbols`S1, _Integer], 1, 2, 1}]]],
  TestID -> "Untitled-95@@Tests/Rules.wlt:242,1-246,2"
];

VerificationTest[
  CodeEquivalentQ[Take[Sort[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[]], 10], Sort[TakeSmallestBy[({GenomeData[#1, "SequenceLength"], #1} & ) /@ GenomeData[], First, 10]]],
  True,
  TestID -> "Untitled-96@@Tests/Rules.wlt:248,1-252,2"
];

VerificationTest[
  CodeEquivalentQ[Graphics3D[{Orange, Sphere[RandomReal[1, {100, 3}], 0.05]}], Graphics3D[{Orange, Table[Sphere[RandomReal[1, {3}], 0.05], {100}]}]],
  True,
  TestID -> "Untitled-97@@Tests/Rules.wlt:254,1-258,2"
];

VerificationTest[
  CodeEquivalentQ[(Framed[Style[#1, RandomColor[]], Background -> RandomColor[]] & ) /@ Alphabet[], (Framed[#1, Background -> RandomColor[]] & ) /@ (Style[#1, RandomColor[]] & ) /@ Alphabet[]],
  TestID -> "Untitled-98@@Tests/Rules.wlt:260,1-263,2"
];

VerificationTest[
  CodeEquivalentQ[NestList[#1 + First[RandomReal[{-1, 1}, {1, 3}]] & , {0, 0, 0}, 1000], NestList[#1 + RandomReal[{-1, 1}, 3] & , {0, 0, 0}, 1000]],
  TestID -> "Untitled-99@@Tests/Rules.wlt:265,1-268,2"
];

VerificationTest[
  CodeEquivalentQ[TakeLargestBy[WordList[], StringLength, 10], Take[SortBy[WordList[], StringLength[#1] & ], -10]],
  TestID -> "Untitled-100@@Tests/Rules.wlt:270,1-273,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[Dilation[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-101@@Tests/Rules.wlt:275,1-279,2"
];

VerificationTest[
  MakeCanonicalForm[Dilation[Dilation[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Dilation[img, 1]],
  TestID -> "Untitled-102@@Tests/Rules.wlt:281,1-285,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[Erosion[img, {{0, 0, 0}, {1, 1, 1}, {0, 0, 0}}], {{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}]],
  TestID -> "Untitled-103@@Tests/Rules.wlt:287,1-291,2"
];

VerificationTest[
  MakeCanonicalForm[Erosion[Erosion[img, {{1, 1, 1}}], {{1}, {1}, {1}}]],
  MakeCanonicalForm[Erosion[img, 1]],
  TestID -> "Untitled-104@@Tests/Rules.wlt:293,1-297,2"
];

VerificationTest[
  MakeCanonicalForm[StringTake["test string", 4]],
  MakeCanonicalForm[StringJoin[Take[Characters["test string"], 4]]],
  TestID -> "Untitled-105@@Tests/Rules.wlt:299,1-303,2"
]

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[StringJoin[Table[FromLetterNumber[RandomInteger[25] + 1], 5]], 100]]],
  ToCanonicalForm[HoldComplete[Table[StringJoin[FromLetterNumber[Table[RandomInteger[25] + 1, 5]]], 100]]],
  TestID -> "Untitled-106@@Tests/Rules.wlt:305,1-309,2"
];

VerificationTest[
  ToCanonicalForm[HoldComplete[Table[Column[{Style[n, Plain], Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  ToCanonicalForm[HoldComplete[Table[Column[{n, Style[n, Bold], Style[n, Italic]}], {n, 10}]]],
  TestID -> "Untitled-107@@Tests/Rules.wlt:311,1-315,2"
];

VerificationTest[
  ToCanonicalForm[StringJoin[#1, StringReverse[#1]] & ],
  ToCanonicalForm[StringJoin[#1, StringJoin[Reverse[Characters[#1]]]] & ],
  TestID -> "Untitled-108@@Tests/Rules.wlt:317,1-321,2"
];