(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";

$testDataDirectory =
    PacletObject[ "Wolfram/CodeEquivalenceUtilities" ][
        "AssetLocation",
        "TestData"
    ];

$answerKey = Get @ FileNameJoin @ { $testDataDirectory, "EIWLData.wl" };


ValidateCorrect // Attributes = { HoldFirst };

ValidateCorrect[ expr_, ex_ ] :=
    With[ { ans = Lookup[ $answerKey, ex ] },
        CodeEquivalentQ[ HoldComplete @ expr, ans ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 3*)
VerificationTest[
  ValidateCorrect[Range[4], "3.1"],
  TestID -> "Untitled-110@@Tests/EIWL.wlt:32,1-35,2"
];

VerificationTest[
  ValidateCorrect[Range[100], "3.2"],
  TestID -> "Untitled-111@@Tests/EIWL.wlt:37,1-40,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[4]], "3.3"],
  TestID -> "Untitled-112@@Tests/EIWL.wlt:42,1-45,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[50]], "3.4"],
  TestID -> "Untitled-113@@Tests/EIWL.wlt:47,1-50,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[4]]], "3.5"],
  TestID -> "Untitled-114@@Tests/EIWL.wlt:52,1-55,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Join[Range[100], Reverse[Range[99]]]], "3.6"],
  TestID -> "Untitled-115@@Tests/EIWL.wlt:57,1-60,2"
];

VerificationTest[
  ValidateCorrect[Range[RandomInteger[10]], "3.7"],
  TestID -> "Untitled-116@@Tests/EIWL.wlt:62,1-65,2"
];

VerificationTest[
  ValidateCorrect[Range[10], "3.8"],
  TestID -> "Untitled-117@@Tests/EIWL.wlt:67,1-70,2"
];

VerificationTest[
  ValidateCorrect[Range[5], "3.9"],
  TestID -> "Untitled-118@@Tests/EIWL.wlt:72,1-75,2"
];

VerificationTest[
  ValidateCorrect[Join[{1, 2}, {3, 4}, {5}], "3.9"],
  TestID -> "Untitled-119@@Tests/EIWL.wlt:77,1-80,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Range[10], Range[5]], "3.10"],
  TestID -> "Untitled-120@@Tests/EIWL.wlt:82,1-85,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[20], Reverse[Range[20]]], "3.11"],
  TestID -> "Untitled-121@@Tests/EIWL.wlt:87,1-90,2"
];

VerificationTest[
  ValidateCorrect[Range[4], "x3.1"],
  TestID -> "Untitled-122@@Tests/EIWL.wlt:92,1-95,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Reverse[Range[4]]], "x3.1"],
  TestID -> "Untitled-123@@Tests/EIWL.wlt:97,1-100,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[5], Reverse[Range[4]]], "x3.2"],
  TestID -> "Untitled-124@@Tests/EIWL.wlt:102,1-105,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[5]]], "x3.2"],
  TestID -> "Untitled-125@@Tests/EIWL.wlt:107,1-110,2"
];

VerificationTest[
  ValidateCorrect[Join[Reverse[Range[3]], Reverse[Range[4]], Reverse[Range[5]]], "x3.3"],
  TestID -> "Untitled-126@@Tests/EIWL.wlt:112,1-115,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10, 14]], "x3.4"],
  TestID -> "Untitled-127@@Tests/EIWL.wlt:117,1-120,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[{10, 11, 12, 13, 14}], "x3.4"],
  TestID -> "Untitled-128@@Tests/EIWL.wlt:122,1-125,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Reverse[Range[10]], Range[10]], "x3.5"],
  TestID -> "Untitled-129@@Tests/EIWL.wlt:127,1-130,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 4*)
VerificationTest[
  ValidateCorrect[BarChart[{1, 1, 2, 3, 5}], "4.1"],
  TestID -> "Untitled-130@@Tests/EIWL.wlt:135,1-138,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Fibonacci[Range[5]]], "4.1"],
  TestID -> "Untitled-131@@Tests/EIWL.wlt:140,1-143,2"
];

VerificationTest[
  ValidateCorrect[PieChart[Range[10]], "4.2"],
  TestID -> "Untitled-132@@Tests/EIWL.wlt:145,1-148,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Reverse[Range[20]]], "4.3"],
  TestID -> "Untitled-133@@Tests/EIWL.wlt:150,1-153,2"
];

VerificationTest[
  ValidateCorrect[Column[{1, 2, 3, 4, 5}], "4.4"],
  TestID -> "Untitled-134@@Tests/EIWL.wlt:155,1-158,2"
];

VerificationTest[
  ValidateCorrect[Column[Range[5]], "4.4"],
  TestID -> "Untitled-135@@Tests/EIWL.wlt:160,1-163,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[{1, 4, 9, 16, 25}], "4.5"],
  TestID -> "Untitled-136@@Tests/EIWL.wlt:165,1-168,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Range[5]^2], "4.5"],
  TestID -> "Untitled-137@@Tests/EIWL.wlt:170,1-173,2"
];

VerificationTest[
  ValidateCorrect[Column[{PieChart[{1}], PieChart[{1, 1}], PieChart[{1, 1, 1}]}], "4.7"],
  TestID -> "Untitled-138@@Tests/EIWL.wlt:175,1-178,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 5*)
VerificationTest[
  ValidateCorrect[Reverse[Range[10]^2], "5.1"],
  TestID -> "Untitled-139@@Tests/EIWL.wlt:183,1-186,2"
];

VerificationTest[
  ValidateCorrect[Total[Range[10]^2], "5.2"],
  TestID -> "Untitled-140@@Tests/EIWL.wlt:188,1-191,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10]^2], "5.3"],
  TestID -> "Untitled-141@@Tests/EIWL.wlt:193,1-196,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[4], Range[4]]], "5.4"],
  TestID -> "Untitled-142@@Tests/EIWL.wlt:198,1-201,2"
];

VerificationTest[
  ValidateCorrect[Range[11] + 9, "5.5"],
  TestID -> "Untitled-143@@Tests/EIWL.wlt:203,1-206,2"
];

VerificationTest[
  ValidateCorrect[Range[0, 10] + 10, "5.5"],
  TestID -> "Untitled-144@@Tests/EIWL.wlt:208,1-211,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[5]^2, Range[5]^3]], "5.6"],
  TestID -> "Untitled-145@@Tests/EIWL.wlt:213,1-216,2"
];

VerificationTest[
  ValidateCorrect[Length[IntegerDigits[2^128]], "5.7"],
  TestID -> "Untitled-146@@Tests/EIWL.wlt:218,1-221,2"
];

VerificationTest[
  ValidateCorrect[First[IntegerDigits[2^32]], "5.8"],
  TestID -> "Untitled-147@@Tests/EIWL.wlt:223,1-226,2"
];

VerificationTest[
  ValidateCorrect[Take[IntegerDigits[2^100], 10], "5.9"],
  TestID -> "Untitled-148@@Tests/EIWL.wlt:228,1-231,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-149@@Tests/EIWL.wlt:233,1-236,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[1 ;; 10]], "5.9"],
  TestID -> "Untitled-150@@Tests/EIWL.wlt:238,1-241,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-151@@Tests/EIWL.wlt:243,1-246,2"
];

VerificationTest[
  ValidateCorrect[Max[IntegerDigits[2^20]], "5.10"],
  TestID -> "Untitled-152@@Tests/EIWL.wlt:248,1-251,2"
];

VerificationTest[
  ValidateCorrect[Count[IntegerDigits[2^1000], 0], "5.11"],
  TestID -> "Untitled-153@@Tests/EIWL.wlt:253,1-256,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-154@@Tests/EIWL.wlt:258,1-261,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-155@@Tests/EIWL.wlt:263,1-266,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[IntegerDigits[2^128]], "5.13"],
  TestID -> "Untitled-156@@Tests/EIWL.wlt:268,1-271,2"
];

VerificationTest[
  ValidateCorrect[Drop[Take[Range[100], 20], 10], "5.14"],
  TestID -> "Untitled-157@@Tests/EIWL.wlt:273,1-276,2"
];

VerificationTest[
  ValidateCorrect[Take[Drop[Range[100], 10], 10], "5.14"],
  TestID -> "Untitled-158@@Tests/EIWL.wlt:278,1-281,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*3, "x5.1"],
  TestID -> "Untitled-159@@Tests/EIWL.wlt:283,1-286,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*Range[10], "x5.2"],
  TestID -> "Untitled-160@@Tests/EIWL.wlt:288,1-291,2"
];

VerificationTest[
  ValidateCorrect[Last[IntegerDigits[2^37]], "x5.3"],
  TestID -> "Untitled-161@@Tests/EIWL.wlt:293,1-296,2"
];

VerificationTest[
  ValidateCorrect[First[Drop[IntegerDigits[2^32], 2]], "x5.4"],
  TestID -> "Untitled-162@@Tests/EIWL.wlt:298,1-301,2"
];

VerificationTest[
  ValidateCorrect[Reverse[IntegerDigits[2^32]][[2]], "x5.4"],
  TestID -> "Untitled-163@@Tests/EIWL.wlt:303,1-306,2"
];

VerificationTest[
  ValidateCorrect[Total[IntegerDigits[3^126]], "x5.5"],
  TestID -> "Untitled-164@@Tests/EIWL.wlt:308,1-311,2"
];

VerificationTest[
  ValidateCorrect[PieChart[IntegerDigits[2^32]], "x5.6"],
  TestID -> "Untitled-165@@Tests/EIWL.wlt:313,1-316,2"
];

VerificationTest[
  ValidateCorrect[{PieChart[IntegerDigits[2^20]], PieChart[IntegerDigits[2^40]], PieChart[IntegerDigits[2^60]]}, "x5.7"],
  TestID -> "Untitled-166@@Tests/EIWL.wlt:318,1-321,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 6*)
VerificationTest[
  ValidateCorrect[Table[1000, 5], "6.1"],
  TestID -> "Untitled-167@@Tests/EIWL.wlt:326,1-329,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3, {n, 10, 20}], "6.2"],
  TestID -> "Untitled-168@@Tests/EIWL.wlt:331,1-334,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[n^2, {n, 20}]], "6.3"],
  TestID -> "Untitled-169@@Tests/EIWL.wlt:336,1-339,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 20, 2], "6.4"],
  TestID -> "Untitled-170@@Tests/EIWL.wlt:341,1-344,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n, {n, 10}], "6.4"],
  TestID -> "Untitled-171@@Tests/EIWL.wlt:346,1-349,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 2, 20, 2}], "6.4"],
  TestID -> "Untitled-172@@Tests/EIWL.wlt:351,1-354,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 10}], "6.5"],
  TestID -> "Untitled-173@@Tests/EIWL.wlt:356,1-359,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Table[n^2, {n, 10}]], "6.6"],
  TestID -> "Untitled-174@@Tests/EIWL.wlt:361,1-364,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Range[10]^2], "6.6"],
  TestID -> "Untitled-175@@Tests/EIWL.wlt:366,1-369,2"
];

VerificationTest[
  ValidateCorrect[Table[IntegerDigits[n^2], {n, 10}], "6.7"],
  TestID -> "Untitled-176@@Tests/EIWL.wlt:371,1-374,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[Range[10]^2], "6.7"],
  TestID -> "Untitled-177@@Tests/EIWL.wlt:376,1-379,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Length[IntegerDigits[n^2]], {n, 100}]], "6.8"],
  TestID -> "Untitled-178@@Tests/EIWL.wlt:381,1-384,2"
];

VerificationTest[
  ValidateCorrect[Table[First[IntegerDigits[n^2]], {n, 20}], "6.9"],
  TestID -> "Untitled-179@@Tests/EIWL.wlt:386,1-389,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[n^2]], {n, 100}]], "6.10"],
  TestID -> "Untitled-180@@Tests/EIWL.wlt:391,1-394,2"
];

VerificationTest[
  ValidateCorrect[Range[10]^3 - Range[10]^2, "x6.1"],
  TestID -> "Untitled-181@@Tests/EIWL.wlt:396,1-399,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3 - n^2, {n, 10}], "x6.1"],
  TestID -> "Untitled-182@@Tests/EIWL.wlt:401,1-404,2"
];

VerificationTest[
  ValidateCorrect[Range[1, 100, 2], "x6.2"],
  TestID -> "Untitled-183@@Tests/EIWL.wlt:406,1-409,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n - 1, {n, 50}], "x6.2"],
  TestID -> "Untitled-184@@Tests/EIWL.wlt:411,1-414,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 1, 100, 2}], "x6.2"],
  TestID -> "Untitled-185@@Tests/EIWL.wlt:416,1-419,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 100, 2]^2, "x6.3"],
  TestID -> "Untitled-186@@Tests/EIWL.wlt:421,1-424,2"
];

VerificationTest[
  ValidateCorrect[Table[n^2, {n, 2, 100, 2}], "x6.3"],
  TestID -> "Untitled-187@@Tests/EIWL.wlt:426,1-429,2"
];

VerificationTest[
  ValidateCorrect[Table[(2*n)^2, {n, 50}], "x6.3"],
  TestID -> "Untitled-188@@Tests/EIWL.wlt:431,1-434,2"
];

VerificationTest[
  ValidateCorrect[Range[-3, 2], "x6.4"],
  TestID -> "Untitled-189@@Tests/EIWL.wlt:436,1-439,2"
];

VerificationTest[
  ValidateCorrect[Table[Column[{n, n^2, n^3}], {n, 20}], "x6.5"],
  TestID -> "Untitled-190@@Tests/EIWL.wlt:441,1-444,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Last[IntegerDigits[n^2]], {n, 100}]], "x6.6"],
  TestID -> "Untitled-191@@Tests/EIWL.wlt:446,1-449,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[3*n]], {n, 100}]], "x6.7"],
  TestID -> "Untitled-192@@Tests/EIWL.wlt:451,1-454,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n]], {n, 200}]], "x6.8"],
  TestID -> "Untitled-193@@Tests/EIWL.wlt:456,1-459,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n^2]], {n, 100}]], "x6.9"],
  TestID -> "Untitled-194@@Tests/EIWL.wlt:461,1-464,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[1/Range[20]], "x6.10"],
  TestID -> "Untitled-195@@Tests/EIWL.wlt:466,1-469,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[1/n, {n, 20}]], "x6.10"],
  TestID -> "Untitled-196@@Tests/EIWL.wlt:471,1-474,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]], "x6.11"],
  TestID -> "Untitled-197@@Tests/EIWL.wlt:476,1-479,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 7*)
VerificationTest[
  ValidateCorrect[Table[{Green, Yellow, Red}[[RandomInteger[{1, 3}]]], 100], "7.13"],
  TestID -> "Untitled-198@@Tests/EIWL.wlt:484,1-487,2"
];

VerificationTest[
  ValidateCorrect[Table[RGBColor[0, 1 - Abs[x], 0], {x, -1, 1, 0.1}], "x7.3"],
  TestID -> "Untitled-199@@Tests/EIWL.wlt:489,1-492,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 11*)
VerificationTest[
  ValidateCorrect[Length[TextWords[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-200@@Tests/EIWL.wlt:497,1-500,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["Computer"]]], "11.9"],
  TestID -> "Untitled-201@@Tests/EIWL.wlt:502,1-505,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-202@@Tests/EIWL.wlt:507,1-510,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 14*)
VerificationTest[
  ValidateCorrect[Graphics[Table[Circle[{x, y}, 1], {x, 1, 10}, {y, 1, 10}]], "14.3"],
  TestID -> "Untitled-203@@Tests/EIWL.wlt:515,1-518,2"
];

VerificationTest[
  ValidateCorrect[Graphics[Table[Style[Circle[RandomInteger[10, 2], 1], RandomColor[]], 30]], "x14.2"],
  TestID -> "Untitled-204@@Tests/EIWL.wlt:520,1-523,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 17*)
VerificationTest[
  ValidateCorrect[UnitConvert[Quantity[2500, "Yen"], Quantity["USDollars"]], "17.6"],
  TestID -> "Untitled-205@@Tests/EIWL.wlt:528,1-531,2"
];

VerificationTest[
  ValidateCorrect[Rotate[Entity["HistoricalSite", "GreatPyramidOfGiza::kbgx6"]["Image"], 180*Degree], "x17.4"],
  TestID -> "Untitled-206@@Tests/EIWL.wlt:533,1-536,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 22*)
VerificationTest[
  ValidateCorrect[Dendrogram[Table[Rasterize[FromLetterNumber[i]], {i, 10}]], "22.15"],
  TestID -> "Untitled-207@@Tests/EIWL.wlt:541,1-544,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 23*)
VerificationTest[
  ValidateCorrect[Graphics[Table[{RandomColor[], Circle[RandomReal[{0, 10}, 2], RandomReal[{0, 2}]]}, 50]], "23.9"],
  TestID -> "Untitled-208@@Tests/EIWL.wlt:549,1-552,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 25*)
VerificationTest[
  ValidateCorrect[a[b[c[d[x]]]], "25.3"],
  TestID -> "Untitled-209@@Tests/EIWL.wlt:557,1-560,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 30*)
VerificationTest[
  WordList;
  ValidateCorrect[SortBy[Take[WordList[], 50], StringTake[StringReverse[#1], 1] & ], "30.10"],
  TestID -> "Untitled-210@@Tests/EIWL.wlt:565,1-569,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[SortBy[Range[200], StringLength[IntegerName[#1]] & ]], "x30.6"],
  TestID -> "Untitled-211@@Tests/EIWL.wlt:571,1-574,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 35*)
VerificationTest[
  StringQ @ Environment[ "GITHUB_ACTIONS" ] || ValidateCorrect[Cases[(Interpreter["University"][StringJoin["U of ", #1]] & ) /@ ToUpperCase[Alphabet[]], _Entity], "35.5"],
  TestID -> "Untitled-212@@Tests/EIWL.wlt:579,1-582,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 44*)
VerificationTest[
  ValidateCorrect[Import["https://www.google.com/", {"HTML", "Images"}], "44.1"],
  TestID -> "Untitled-213@@Tests/EIWL.wlt:587,1-590,2"
];

VerificationTest[
  ImageCollage;
  ValidateCorrect[ImageCollage[Import["http://nps.gov", "Images"]], "44.4"],
  TestID -> "Untitled-214@@Tests/EIWL.wlt:592,1-596,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["http://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-215@@Tests/EIWL.wlt:598,1-601,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["https://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-216@@Tests/EIWL.wlt:603,1-606,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["http://en.wikipedia.org/", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-217@@Tests/EIWL.wlt:608,1-611,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 45*)
VerificationTest[
    Or[ StringQ @ Environment[ "GITHUB_ACTIONS" ],
        ValidateCorrect[
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
        ]
    ],
    TestID -> "Untitled-218@@Tests/EIWL.wlt:616,1-635,2"
];
