(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence  = "Session";
On[ EvaluateSafely::unsafe ];
$testDataDirectory = FileNameJoin @ { DirectoryName @ $TestFileName, "Data" };
$answerKey         = Get @ FileNameJoin @ { $testDataDirectory, "EIWLData.wl" };


ValidateCorrect // Attributes = { HoldFirst };

ValidateCorrect[ expr_, ex_ ] :=
    With[ { ans = Lookup[ $answerKey, ex ] },
        CodeEquivalentQ[ HoldComplete @ expr, ans ]
    ];


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
ValidateOptional // Attributes = { HoldFirst };

ValidateOptional[ expr_, ex_ ] :=
    If[ ! StringQ @ Environment[ "GITHUB_ACTIONS" ],
        ValidateCorrect[ expr, ex ],
        With[ { ans = Lookup[ $answerKey, ex ] },
            If[ ! TrueQ @ Quiet @ CodeEquivalentQ[ HoldComplete @ expr, ans ],
                Print[ "::warning::An optional validation has failed: ", ex ];
            ];
            True
        ]
    ];
(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 3*)
VerificationTest[
  ValidateCorrect[Range[4], "3.1"],
  TestID -> "Untitled-110@@Tests/EIWL.wlt:43,1-46,2"
];

VerificationTest[
  ValidateCorrect[Range[100], "3.2"],
  TestID -> "Untitled-111@@Tests/EIWL.wlt:48,1-51,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[4]], "3.3"],
  TestID -> "Untitled-112@@Tests/EIWL.wlt:53,1-56,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[50]], "3.4"],
  TestID -> "Untitled-113@@Tests/EIWL.wlt:58,1-61,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[4]]], "3.5"],
  TestID -> "Untitled-114@@Tests/EIWL.wlt:63,1-66,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Join[Range[100], Reverse[Range[99]]]], "3.6"],
  TestID -> "Untitled-115@@Tests/EIWL.wlt:68,1-71,2"
];

VerificationTest[
  ValidateCorrect[Range[RandomInteger[10]], "3.7"],
  TestID -> "Untitled-116@@Tests/EIWL.wlt:73,1-76,2"
];

VerificationTest[
  ValidateCorrect[Range[10], "3.8"],
  TestID -> "Untitled-117@@Tests/EIWL.wlt:78,1-81,2"
];

VerificationTest[
  ValidateCorrect[Range[5], "3.9"],
  TestID -> "Untitled-118@@Tests/EIWL.wlt:83,1-86,2"
];

VerificationTest[
  ValidateCorrect[Join[{1, 2}, {3, 4}, {5}], "3.9"],
  TestID -> "Untitled-119@@Tests/EIWL.wlt:88,1-91,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Range[10], Range[5]], "3.10"],
  TestID -> "Untitled-120@@Tests/EIWL.wlt:93,1-96,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[20], Reverse[Range[20]]], "3.11"],
  TestID -> "Untitled-121@@Tests/EIWL.wlt:98,1-101,2"
];

VerificationTest[
  ValidateCorrect[Range[4], "x3.1"],
  TestID -> "Untitled-122@@Tests/EIWL.wlt:103,1-106,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Reverse[Range[4]]], "x3.1"],
  TestID -> "Untitled-123@@Tests/EIWL.wlt:108,1-111,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[5], Reverse[Range[4]]], "x3.2"],
  TestID -> "Untitled-124@@Tests/EIWL.wlt:113,1-116,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[5]]], "x3.2"],
  TestID -> "Untitled-125@@Tests/EIWL.wlt:118,1-121,2"
];

VerificationTest[
  ValidateCorrect[Join[Reverse[Range[3]], Reverse[Range[4]], Reverse[Range[5]]], "x3.3"],
  TestID -> "Untitled-126@@Tests/EIWL.wlt:123,1-126,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10, 14]], "x3.4"],
  TestID -> "Untitled-127@@Tests/EIWL.wlt:128,1-131,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[{10, 11, 12, 13, 14}], "x3.4"],
  TestID -> "Untitled-128@@Tests/EIWL.wlt:133,1-136,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Reverse[Range[10]], Range[10]], "x3.5"],
  TestID -> "Untitled-129@@Tests/EIWL.wlt:138,1-141,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 4*)
VerificationTest[
  ValidateCorrect[BarChart[{1, 1, 2, 3, 5}], "4.1"],
  TestID -> "Untitled-130@@Tests/EIWL.wlt:146,1-149,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Fibonacci[Range[5]]], "4.1"],
  TestID -> "Untitled-131@@Tests/EIWL.wlt:151,1-154,2"
];

VerificationTest[
  ValidateCorrect[PieChart[Range[10]], "4.2"],
  TestID -> "Untitled-132@@Tests/EIWL.wlt:156,1-159,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Reverse[Range[20]]], "4.3"],
  TestID -> "Untitled-133@@Tests/EIWL.wlt:161,1-164,2"
];

VerificationTest[
  ValidateCorrect[Column[{1, 2, 3, 4, 5}], "4.4"],
  TestID -> "Untitled-134@@Tests/EIWL.wlt:166,1-169,2"
];

VerificationTest[
  ValidateCorrect[Column[Range[5]], "4.4"],
  TestID -> "Untitled-135@@Tests/EIWL.wlt:171,1-174,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[{1, 4, 9, 16, 25}], "4.5"],
  TestID -> "Untitled-136@@Tests/EIWL.wlt:176,1-179,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Range[5]^2], "4.5"],
  TestID -> "Untitled-137@@Tests/EIWL.wlt:181,1-184,2"
];

VerificationTest[
  ValidateCorrect[Column[{PieChart[{1}], PieChart[{1, 1}], PieChart[{1, 1, 1}]}], "4.7"],
  TestID -> "Untitled-138@@Tests/EIWL.wlt:186,1-189,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 5*)
VerificationTest[
  ValidateCorrect[Reverse[Range[10]^2], "5.1"],
  TestID -> "Untitled-139@@Tests/EIWL.wlt:194,1-197,2"
];

VerificationTest[
  ValidateCorrect[Total[Range[10]^2], "5.2"],
  TestID -> "Untitled-140@@Tests/EIWL.wlt:199,1-202,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10]^2], "5.3"],
  TestID -> "Untitled-141@@Tests/EIWL.wlt:204,1-207,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[4], Range[4]]], "5.4"],
  TestID -> "Untitled-142@@Tests/EIWL.wlt:209,1-212,2"
];

VerificationTest[
  ValidateCorrect[Range[11] + 9, "5.5"],
  TestID -> "Untitled-143@@Tests/EIWL.wlt:214,1-217,2"
];

VerificationTest[
  ValidateCorrect[Range[0, 10] + 10, "5.5"],
  TestID -> "Untitled-144@@Tests/EIWL.wlt:219,1-222,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[5]^2, Range[5]^3]], "5.6"],
  TestID -> "Untitled-145@@Tests/EIWL.wlt:224,1-227,2"
];

VerificationTest[
  ValidateCorrect[Length[IntegerDigits[2^128]], "5.7"],
  TestID -> "Untitled-146@@Tests/EIWL.wlt:229,1-232,2"
];

VerificationTest[
  ValidateCorrect[First[IntegerDigits[2^32]], "5.8"],
  TestID -> "Untitled-147@@Tests/EIWL.wlt:234,1-237,2"
];

VerificationTest[
  ValidateCorrect[Take[IntegerDigits[2^100], 10], "5.9"],
  TestID -> "Untitled-148@@Tests/EIWL.wlt:239,1-242,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-149@@Tests/EIWL.wlt:244,1-247,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[1 ;; 10]], "5.9"],
  TestID -> "Untitled-150@@Tests/EIWL.wlt:249,1-252,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-151@@Tests/EIWL.wlt:254,1-257,2"
];

VerificationTest[
  ValidateCorrect[Max[IntegerDigits[2^20]], "5.10"],
  TestID -> "Untitled-152@@Tests/EIWL.wlt:259,1-262,2"
];

VerificationTest[
  ValidateCorrect[Count[IntegerDigits[2^1000], 0], "5.11"],
  TestID -> "Untitled-153@@Tests/EIWL.wlt:264,1-267,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-154@@Tests/EIWL.wlt:269,1-272,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-155@@Tests/EIWL.wlt:274,1-277,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[IntegerDigits[2^128]], "5.13"],
  TestID -> "Untitled-156@@Tests/EIWL.wlt:279,1-282,2"
];

VerificationTest[
  ValidateCorrect[Drop[Take[Range[100], 20], 10], "5.14"],
  TestID -> "Untitled-157@@Tests/EIWL.wlt:284,1-287,2"
];

VerificationTest[
  ValidateCorrect[Take[Drop[Range[100], 10], 10], "5.14"],
  TestID -> "Untitled-158@@Tests/EIWL.wlt:289,1-292,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*3, "x5.1"],
  TestID -> "Untitled-159@@Tests/EIWL.wlt:294,1-297,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*Range[10], "x5.2"],
  TestID -> "Untitled-160@@Tests/EIWL.wlt:299,1-302,2"
];

VerificationTest[
  ValidateCorrect[Last[IntegerDigits[2^37]], "x5.3"],
  TestID -> "Untitled-161@@Tests/EIWL.wlt:304,1-307,2"
];

VerificationTest[
  ValidateCorrect[First[Drop[IntegerDigits[2^32], 2]], "x5.4"],
  TestID -> "Untitled-162@@Tests/EIWL.wlt:309,1-312,2"
];

VerificationTest[
  ValidateCorrect[Reverse[IntegerDigits[2^32]][[2]], "x5.4"],
  TestID -> "Untitled-163@@Tests/EIWL.wlt:314,1-317,2"
];

VerificationTest[
  ValidateCorrect[Total[IntegerDigits[3^126]], "x5.5"],
  TestID -> "Untitled-164@@Tests/EIWL.wlt:319,1-322,2"
];

VerificationTest[
  ValidateCorrect[PieChart[IntegerDigits[2^32]], "x5.6"],
  TestID -> "Untitled-165@@Tests/EIWL.wlt:324,1-327,2"
];

VerificationTest[
  ValidateCorrect[{PieChart[IntegerDigits[2^20]], PieChart[IntegerDigits[2^40]], PieChart[IntegerDigits[2^60]]}, "x5.7"],
  TestID -> "Untitled-166@@Tests/EIWL.wlt:329,1-332,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 6*)
VerificationTest[
  ValidateCorrect[Table[1000, 5], "6.1"],
  TestID -> "Untitled-167@@Tests/EIWL.wlt:337,1-340,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3, {n, 10, 20}], "6.2"],
  TestID -> "Untitled-168@@Tests/EIWL.wlt:342,1-345,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[n^2, {n, 20}]], "6.3"],
  TestID -> "Untitled-169@@Tests/EIWL.wlt:347,1-350,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 20, 2], "6.4"],
  TestID -> "Untitled-170@@Tests/EIWL.wlt:352,1-355,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n, {n, 10}], "6.4"],
  TestID -> "Untitled-171@@Tests/EIWL.wlt:357,1-360,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 2, 20, 2}], "6.4"],
  TestID -> "Untitled-172@@Tests/EIWL.wlt:362,1-365,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 10}], "6.5"],
  TestID -> "Untitled-173@@Tests/EIWL.wlt:367,1-370,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Table[n^2, {n, 10}]], "6.6"],
  TestID -> "Untitled-174@@Tests/EIWL.wlt:372,1-375,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Range[10]^2], "6.6"],
  TestID -> "Untitled-175@@Tests/EIWL.wlt:377,1-380,2"
];

VerificationTest[
  ValidateCorrect[Table[IntegerDigits[n^2], {n, 10}], "6.7"],
  TestID -> "Untitled-176@@Tests/EIWL.wlt:382,1-385,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[Range[10]^2], "6.7"],
  TestID -> "Untitled-177@@Tests/EIWL.wlt:387,1-390,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Length[IntegerDigits[n^2]], {n, 100}]], "6.8"],
  TestID -> "Untitled-178@@Tests/EIWL.wlt:392,1-395,2"
];

VerificationTest[
  ValidateCorrect[Table[First[IntegerDigits[n^2]], {n, 20}], "6.9"],
  TestID -> "Untitled-179@@Tests/EIWL.wlt:397,1-400,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[n^2]], {n, 100}]], "6.10"],
  TestID -> "Untitled-180@@Tests/EIWL.wlt:402,1-405,2"
];

VerificationTest[
  ValidateCorrect[Range[10]^3 - Range[10]^2, "x6.1"],
  TestID -> "Untitled-181@@Tests/EIWL.wlt:407,1-410,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3 - n^2, {n, 10}], "x6.1"],
  TestID -> "Untitled-182@@Tests/EIWL.wlt:412,1-415,2"
];

VerificationTest[
  ValidateCorrect[Range[1, 100, 2], "x6.2"],
  TestID -> "Untitled-183@@Tests/EIWL.wlt:417,1-420,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n - 1, {n, 50}], "x6.2"],
  TestID -> "Untitled-184@@Tests/EIWL.wlt:422,1-425,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 1, 100, 2}], "x6.2"],
  TestID -> "Untitled-185@@Tests/EIWL.wlt:427,1-430,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 100, 2]^2, "x6.3"],
  TestID -> "Untitled-186@@Tests/EIWL.wlt:432,1-435,2"
];

VerificationTest[
  ValidateCorrect[Table[n^2, {n, 2, 100, 2}], "x6.3"],
  TestID -> "Untitled-187@@Tests/EIWL.wlt:437,1-440,2"
];

VerificationTest[
  ValidateCorrect[Table[(2*n)^2, {n, 50}], "x6.3"],
  TestID -> "Untitled-188@@Tests/EIWL.wlt:442,1-445,2"
];

VerificationTest[
  ValidateCorrect[Range[-3, 2], "x6.4"],
  TestID -> "Untitled-189@@Tests/EIWL.wlt:447,1-450,2"
];

VerificationTest[
  ValidateCorrect[Table[Column[{n, n^2, n^3}], {n, 20}], "x6.5"],
  TestID -> "Untitled-190@@Tests/EIWL.wlt:452,1-455,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Last[IntegerDigits[n^2]], {n, 100}]], "x6.6"],
  TestID -> "Untitled-191@@Tests/EIWL.wlt:457,1-460,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[3*n]], {n, 100}]], "x6.7"],
  TestID -> "Untitled-192@@Tests/EIWL.wlt:462,1-465,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n]], {n, 200}]], "x6.8"],
  TestID -> "Untitled-193@@Tests/EIWL.wlt:467,1-470,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n^2]], {n, 100}]], "x6.9"],
  TestID -> "Untitled-194@@Tests/EIWL.wlt:472,1-475,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[1/Range[20]], "x6.10"],
  TestID -> "Untitled-195@@Tests/EIWL.wlt:477,1-480,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[1/n, {n, 20}]], "x6.10"],
  TestID -> "Untitled-196@@Tests/EIWL.wlt:482,1-485,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]], "x6.11"],
  TestID -> "Untitled-197@@Tests/EIWL.wlt:487,1-490,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 7*)
VerificationTest[
  ValidateCorrect[Table[{Green, Yellow, Red}[[RandomInteger[{1, 3}]]], 100], "7.13"],
  TestID -> "Untitled-198@@Tests/EIWL.wlt:495,1-498,2"
];

VerificationTest[
  ValidateCorrect[Table[RGBColor[0, 1 - Abs[x], 0], {x, -1, 1, 0.1}], "x7.3"],
  TestID -> "Untitled-199@@Tests/EIWL.wlt:500,1-503,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 11*)
VerificationTest[
  ValidateCorrect[Length[TextWords[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-200@@Tests/EIWL.wlt:508,1-511,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["Computer"]]], "11.9"],
  TestID -> "Untitled-201@@Tests/EIWL.wlt:513,1-516,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-202@@Tests/EIWL.wlt:518,1-521,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 14*)
VerificationTest[
  ValidateCorrect[Graphics[Table[Circle[{x, y}, 1], {x, 1, 10}, {y, 1, 10}]], "14.3"],
  TestID -> "Untitled-203@@Tests/EIWL.wlt:526,1-529,2"
];

VerificationTest[
  ValidateCorrect[Graphics[Table[Style[Circle[RandomInteger[10, 2], 1], RandomColor[]], 30]], "x14.2"],
  TestID -> "Untitled-204@@Tests/EIWL.wlt:531,1-534,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 17*)
VerificationTest[
  ValidateCorrect[UnitConvert[Quantity[2500, "Yen"], Quantity["USDollars"]], "17.6"],
  TestID -> "Untitled-205@@Tests/EIWL.wlt:539,1-542,2"
];

VerificationTest[
  ValidateCorrect[Rotate[Entity["HistoricalSite", "GreatPyramidOfGiza::kbgx6"]["Image"], 180*Degree], "x17.4"],
  TestID -> "Untitled-206@@Tests/EIWL.wlt:544,1-547,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 22*)
VerificationTest[
  ValidateCorrect[Dendrogram[Table[Rasterize[FromLetterNumber[i]], {i, 10}]], "22.15"],
  TestID -> "Untitled-207@@Tests/EIWL.wlt:552,1-555,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 23*)
VerificationTest[
  ValidateCorrect[Graphics[Table[{RandomColor[], Circle[RandomReal[{0, 10}, 2], RandomReal[{0, 2}]]}, 50]], "23.9"],
  TestID -> "Untitled-208@@Tests/EIWL.wlt:560,1-563,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 25*)
VerificationTest[
  ValidateCorrect[a[b[c[d[x]]]], "25.3"],
  TestID -> "Untitled-209@@Tests/EIWL.wlt:568,1-571,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 30*)
VerificationTest[
  WordList;
  ValidateCorrect[SortBy[Take[WordList[], 50], StringTake[StringReverse[#1], 1] & ], "30.10"],
  TestID -> "Untitled-210@@Tests/EIWL.wlt:576,1-580,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[SortBy[Range[200], StringLength[IntegerName[#1]] & ]], "x30.6"],
  TestID -> "Untitled-211@@Tests/EIWL.wlt:582,1-585,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 35*)
VerificationTest[
  ValidateOptional[Cases[(Interpreter["University"][StringJoin["U of ", #1]] & ) /@ ToUpperCase[Alphabet[]], _Entity], "35.5"],
  TestID -> "Untitled-212@@Tests/EIWL.wlt:590,1-593,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 44*)
VerificationTest[
  ValidateOptional[Import["https://www.google.com/", {"HTML", "Images"}], "44.1"],
  TestID -> "Untitled-213@@Tests/EIWL.wlt:598,1-601,2"
];

VerificationTest[
  ImageCollage;
  ValidateOptional[ImageCollage[Import["http://nps.gov", "Images"]], "44.4"],
  TestID -> "Untitled-214@@Tests/EIWL.wlt:603,1-607,2"
];

VerificationTest[
  ValidateOptional[Length[Import["http://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-215@@Tests/EIWL.wlt:609,1-612,2"
];

VerificationTest[
  ValidateOptional[Length[Import["https://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-216@@Tests/EIWL.wlt:614,1-617,2"
];

VerificationTest[
  ValidateOptional[Length[Import["http://en.wikipedia.org/", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-217@@Tests/EIWL.wlt:619,1-622,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 45*)
VerificationTest[
    ValidateOptional[
        GeoListPlot[
            Part[
                TakeLargestBy[
                    Normal @ ResourceData[ "Fireballs and Bolides" ],
                    #Altitude &,
                    10
                ],
                All,
                "NearestCity"
            ],
            GeoLabels -> True
        ],
        "45.13"
    ],
    TestID -> "Untitled-218@@Tests/EIWL.wlt:627,1-644,2"
];
