(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
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
  TestID -> "Untitled-110@@Tests/EIWL.wlt:31,1-34,2"
];

VerificationTest[
  ValidateCorrect[Range[100], "3.2"],
  TestID -> "Untitled-111@@Tests/EIWL.wlt:36,1-39,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[4]], "3.3"],
  TestID -> "Untitled-112@@Tests/EIWL.wlt:41,1-44,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[50]], "3.4"],
  TestID -> "Untitled-113@@Tests/EIWL.wlt:46,1-49,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[4]]], "3.5"],
  TestID -> "Untitled-114@@Tests/EIWL.wlt:51,1-54,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Join[Range[100], Reverse[Range[99]]]], "3.6"],
  TestID -> "Untitled-115@@Tests/EIWL.wlt:56,1-59,2"
];

VerificationTest[
  ValidateCorrect[Range[RandomInteger[10]], "3.7"],
  TestID -> "Untitled-116@@Tests/EIWL.wlt:61,1-64,2"
];

VerificationTest[
  ValidateCorrect[Range[10], "3.8"],
  TestID -> "Untitled-117@@Tests/EIWL.wlt:66,1-69,2"
];

VerificationTest[
  ValidateCorrect[Range[5], "3.9"],
  TestID -> "Untitled-118@@Tests/EIWL.wlt:71,1-74,2"
];

VerificationTest[
  ValidateCorrect[Join[{1, 2}, {3, 4}, {5}], "3.9"],
  TestID -> "Untitled-119@@Tests/EIWL.wlt:76,1-79,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Range[10], Range[5]], "3.10"],
  TestID -> "Untitled-120@@Tests/EIWL.wlt:81,1-84,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[20], Reverse[Range[20]]], "3.11"],
  TestID -> "Untitled-121@@Tests/EIWL.wlt:86,1-89,2"
];

VerificationTest[
  ValidateCorrect[Range[4], "x3.1"],
  TestID -> "Untitled-122@@Tests/EIWL.wlt:91,1-94,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Reverse[Range[4]]], "x3.1"],
  TestID -> "Untitled-123@@Tests/EIWL.wlt:96,1-99,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[5], Reverse[Range[4]]], "x3.2"],
  TestID -> "Untitled-124@@Tests/EIWL.wlt:101,1-104,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[5]]], "x3.2"],
  TestID -> "Untitled-125@@Tests/EIWL.wlt:106,1-109,2"
];

VerificationTest[
  ValidateCorrect[Join[Reverse[Range[3]], Reverse[Range[4]], Reverse[Range[5]]], "x3.3"],
  TestID -> "Untitled-126@@Tests/EIWL.wlt:111,1-114,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10, 14]], "x3.4"],
  TestID -> "Untitled-127@@Tests/EIWL.wlt:116,1-119,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[{10, 11, 12, 13, 14}], "x3.4"],
  TestID -> "Untitled-128@@Tests/EIWL.wlt:121,1-124,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Reverse[Range[10]], Range[10]], "x3.5"],
  TestID -> "Untitled-129@@Tests/EIWL.wlt:126,1-129,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 4*)
VerificationTest[
  ValidateCorrect[BarChart[{1, 1, 2, 3, 5}], "4.1"],
  TestID -> "Untitled-130@@Tests/EIWL.wlt:134,1-137,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Fibonacci[Range[5]]], "4.1"],
  TestID -> "Untitled-131@@Tests/EIWL.wlt:139,1-142,2"
];

VerificationTest[
  ValidateCorrect[PieChart[Range[10]], "4.2"],
  TestID -> "Untitled-132@@Tests/EIWL.wlt:144,1-147,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Reverse[Range[20]]], "4.3"],
  TestID -> "Untitled-133@@Tests/EIWL.wlt:149,1-152,2"
];

VerificationTest[
  ValidateCorrect[Column[{1, 2, 3, 4, 5}], "4.4"],
  TestID -> "Untitled-134@@Tests/EIWL.wlt:154,1-157,2"
];

VerificationTest[
  ValidateCorrect[Column[Range[5]], "4.4"],
  TestID -> "Untitled-135@@Tests/EIWL.wlt:159,1-162,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[{1, 4, 9, 16, 25}], "4.5"],
  TestID -> "Untitled-136@@Tests/EIWL.wlt:164,1-167,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Range[5]^2], "4.5"],
  TestID -> "Untitled-137@@Tests/EIWL.wlt:169,1-172,2"
];

VerificationTest[
  ValidateCorrect[Column[{PieChart[{1}], PieChart[{1, 1}], PieChart[{1, 1, 1}]}], "4.7"],
  TestID -> "Untitled-138@@Tests/EIWL.wlt:174,1-177,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 5*)
VerificationTest[
  ValidateCorrect[Reverse[Range[10]^2], "5.1"],
  TestID -> "Untitled-139@@Tests/EIWL.wlt:182,1-185,2"
];

VerificationTest[
  ValidateCorrect[Total[Range[10]^2], "5.2"],
  TestID -> "Untitled-140@@Tests/EIWL.wlt:187,1-190,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10]^2], "5.3"],
  TestID -> "Untitled-141@@Tests/EIWL.wlt:192,1-195,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[4], Range[4]]], "5.4"],
  TestID -> "Untitled-142@@Tests/EIWL.wlt:197,1-200,2"
];

VerificationTest[
  ValidateCorrect[Range[11] + 9, "5.5"],
  TestID -> "Untitled-143@@Tests/EIWL.wlt:202,1-205,2"
];

VerificationTest[
  ValidateCorrect[Range[0, 10] + 10, "5.5"],
  TestID -> "Untitled-144@@Tests/EIWL.wlt:207,1-210,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[5]^2, Range[5]^3]], "5.6"],
  TestID -> "Untitled-145@@Tests/EIWL.wlt:212,1-215,2"
];

VerificationTest[
  ValidateCorrect[Length[IntegerDigits[2^128]], "5.7"],
  TestID -> "Untitled-146@@Tests/EIWL.wlt:217,1-220,2"
];

VerificationTest[
  ValidateCorrect[First[IntegerDigits[2^32]], "5.8"],
  TestID -> "Untitled-147@@Tests/EIWL.wlt:222,1-225,2"
];

VerificationTest[
  ValidateCorrect[Take[IntegerDigits[2^100], 10], "5.9"],
  TestID -> "Untitled-148@@Tests/EIWL.wlt:227,1-230,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-149@@Tests/EIWL.wlt:232,1-235,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[1 ;; 10]], "5.9"],
  TestID -> "Untitled-150@@Tests/EIWL.wlt:237,1-240,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-151@@Tests/EIWL.wlt:242,1-245,2"
];

VerificationTest[
  ValidateCorrect[Max[IntegerDigits[2^20]], "5.10"],
  TestID -> "Untitled-152@@Tests/EIWL.wlt:247,1-250,2"
];

VerificationTest[
  ValidateCorrect[Count[IntegerDigits[2^1000], 0], "5.11"],
  TestID -> "Untitled-153@@Tests/EIWL.wlt:252,1-255,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-154@@Tests/EIWL.wlt:257,1-260,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-155@@Tests/EIWL.wlt:262,1-265,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[IntegerDigits[2^128]], "5.13"],
  TestID -> "Untitled-156@@Tests/EIWL.wlt:267,1-270,2"
];

VerificationTest[
  ValidateCorrect[Drop[Take[Range[100], 20], 10], "5.14"],
  TestID -> "Untitled-157@@Tests/EIWL.wlt:272,1-275,2"
];

VerificationTest[
  ValidateCorrect[Take[Drop[Range[100], 10], 10], "5.14"],
  TestID -> "Untitled-158@@Tests/EIWL.wlt:277,1-280,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*3, "x5.1"],
  TestID -> "Untitled-159@@Tests/EIWL.wlt:282,1-285,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*Range[10], "x5.2"],
  TestID -> "Untitled-160@@Tests/EIWL.wlt:287,1-290,2"
];

VerificationTest[
  ValidateCorrect[Last[IntegerDigits[2^37]], "x5.3"],
  TestID -> "Untitled-161@@Tests/EIWL.wlt:292,1-295,2"
];

VerificationTest[
  ValidateCorrect[First[Drop[IntegerDigits[2^32], 2]], "x5.4"],
  TestID -> "Untitled-162@@Tests/EIWL.wlt:297,1-300,2"
];

VerificationTest[
  ValidateCorrect[Reverse[IntegerDigits[2^32]][[2]], "x5.4"],
  TestID -> "Untitled-163@@Tests/EIWL.wlt:302,1-305,2"
];

VerificationTest[
  ValidateCorrect[Total[IntegerDigits[3^126]], "x5.5"],
  TestID -> "Untitled-164@@Tests/EIWL.wlt:307,1-310,2"
];

VerificationTest[
  ValidateCorrect[PieChart[IntegerDigits[2^32]], "x5.6"],
  TestID -> "Untitled-165@@Tests/EIWL.wlt:312,1-315,2"
];

VerificationTest[
  ValidateCorrect[{PieChart[IntegerDigits[2^20]], PieChart[IntegerDigits[2^40]], PieChart[IntegerDigits[2^60]]}, "x5.7"],
  TestID -> "Untitled-166@@Tests/EIWL.wlt:317,1-320,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 6*)
VerificationTest[
  ValidateCorrect[Table[1000, 5], "6.1"],
  TestID -> "Untitled-167@@Tests/EIWL.wlt:325,1-328,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3, {n, 10, 20}], "6.2"],
  TestID -> "Untitled-168@@Tests/EIWL.wlt:330,1-333,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[n^2, {n, 20}]], "6.3"],
  TestID -> "Untitled-169@@Tests/EIWL.wlt:335,1-338,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 20, 2], "6.4"],
  TestID -> "Untitled-170@@Tests/EIWL.wlt:340,1-343,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n, {n, 10}], "6.4"],
  TestID -> "Untitled-171@@Tests/EIWL.wlt:345,1-348,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 2, 20, 2}], "6.4"],
  TestID -> "Untitled-172@@Tests/EIWL.wlt:350,1-353,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 10}], "6.5"],
  TestID -> "Untitled-173@@Tests/EIWL.wlt:355,1-358,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Table[n^2, {n, 10}]], "6.6"],
  TestID -> "Untitled-174@@Tests/EIWL.wlt:360,1-363,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Range[10]^2], "6.6"],
  TestID -> "Untitled-175@@Tests/EIWL.wlt:365,1-368,2"
];

VerificationTest[
  ValidateCorrect[Table[IntegerDigits[n^2], {n, 10}], "6.7"],
  TestID -> "Untitled-176@@Tests/EIWL.wlt:370,1-373,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[Range[10]^2], "6.7"],
  TestID -> "Untitled-177@@Tests/EIWL.wlt:375,1-378,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Length[IntegerDigits[n^2]], {n, 100}]], "6.8"],
  TestID -> "Untitled-178@@Tests/EIWL.wlt:380,1-383,2"
];

VerificationTest[
  ValidateCorrect[Table[First[IntegerDigits[n^2]], {n, 20}], "6.9"],
  TestID -> "Untitled-179@@Tests/EIWL.wlt:385,1-388,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[n^2]], {n, 100}]], "6.10"],
  TestID -> "Untitled-180@@Tests/EIWL.wlt:390,1-393,2"
];

VerificationTest[
  ValidateCorrect[Range[10]^3 - Range[10]^2, "x6.1"],
  TestID -> "Untitled-181@@Tests/EIWL.wlt:395,1-398,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3 - n^2, {n, 10}], "x6.1"],
  TestID -> "Untitled-182@@Tests/EIWL.wlt:400,1-403,2"
];

VerificationTest[
  ValidateCorrect[Range[1, 100, 2], "x6.2"],
  TestID -> "Untitled-183@@Tests/EIWL.wlt:405,1-408,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n - 1, {n, 50}], "x6.2"],
  TestID -> "Untitled-184@@Tests/EIWL.wlt:410,1-413,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 1, 100, 2}], "x6.2"],
  TestID -> "Untitled-185@@Tests/EIWL.wlt:415,1-418,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 100, 2]^2, "x6.3"],
  TestID -> "Untitled-186@@Tests/EIWL.wlt:420,1-423,2"
];

VerificationTest[
  ValidateCorrect[Table[n^2, {n, 2, 100, 2}], "x6.3"],
  TestID -> "Untitled-187@@Tests/EIWL.wlt:425,1-428,2"
];

VerificationTest[
  ValidateCorrect[Table[(2*n)^2, {n, 50}], "x6.3"],
  TestID -> "Untitled-188@@Tests/EIWL.wlt:430,1-433,2"
];

VerificationTest[
  ValidateCorrect[Range[-3, 2], "x6.4"],
  TestID -> "Untitled-189@@Tests/EIWL.wlt:435,1-438,2"
];

VerificationTest[
  ValidateCorrect[Table[Column[{n, n^2, n^3}], {n, 20}], "x6.5"],
  TestID -> "Untitled-190@@Tests/EIWL.wlt:440,1-443,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Last[IntegerDigits[n^2]], {n, 100}]], "x6.6"],
  TestID -> "Untitled-191@@Tests/EIWL.wlt:445,1-448,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[3*n]], {n, 100}]], "x6.7"],
  TestID -> "Untitled-192@@Tests/EIWL.wlt:450,1-453,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n]], {n, 200}]], "x6.8"],
  TestID -> "Untitled-193@@Tests/EIWL.wlt:455,1-458,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n^2]], {n, 100}]], "x6.9"],
  TestID -> "Untitled-194@@Tests/EIWL.wlt:460,1-463,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[1/Range[20]], "x6.10"],
  TestID -> "Untitled-195@@Tests/EIWL.wlt:465,1-468,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[1/n, {n, 20}]], "x6.10"],
  TestID -> "Untitled-196@@Tests/EIWL.wlt:470,1-473,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]], "x6.11"],
  TestID -> "Untitled-197@@Tests/EIWL.wlt:475,1-478,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 7*)
VerificationTest[
  ValidateCorrect[Table[{Green, Yellow, Red}[[RandomInteger[{1, 3}]]], 100], "7.13"],
  TestID -> "Untitled-198@@Tests/EIWL.wlt:483,1-486,2"
];

VerificationTest[
  ValidateCorrect[Table[RGBColor[0, 1 - Abs[x], 0], {x, -1, 1, 0.1}], "x7.3"],
  TestID -> "Untitled-199@@Tests/EIWL.wlt:488,1-491,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 11*)
VerificationTest[
  ValidateCorrect[Length[TextWords[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-200@@Tests/EIWL.wlt:496,1-499,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["Computer"]]], "11.9"],
  TestID -> "Untitled-201@@Tests/EIWL.wlt:501,1-504,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-202@@Tests/EIWL.wlt:506,1-509,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 14*)
VerificationTest[
  ValidateCorrect[Graphics[Table[Circle[{x, y}, 1], {x, 1, 10}, {y, 1, 10}]], "14.3"],
  TestID -> "Untitled-203@@Tests/EIWL.wlt:514,1-517,2"
];

VerificationTest[
  ValidateCorrect[Graphics[Table[Style[Circle[RandomInteger[10, 2], 1], RandomColor[]], 30]], "x14.2"],
  TestID -> "Untitled-204@@Tests/EIWL.wlt:519,1-522,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 17*)
VerificationTest[
  ValidateCorrect[UnitConvert[Quantity[2500, "Yen"], Quantity["USDollars"]], "17.6"],
  TestID -> "Untitled-205@@Tests/EIWL.wlt:527,1-530,2"
];

VerificationTest[
  ValidateCorrect[Rotate[Entity["HistoricalSite", "GreatPyramidOfGiza::kbgx6"]["Image"], 180*Degree], "x17.4"],
  TestID -> "Untitled-206@@Tests/EIWL.wlt:532,1-535,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 22*)
VerificationTest[
  ValidateCorrect[Dendrogram[Table[Rasterize[FromLetterNumber[i]], {i, 10}]], "22.15"],
  TestID -> "Untitled-207@@Tests/EIWL.wlt:540,1-543,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 23*)
VerificationTest[
  ValidateCorrect[Graphics[Table[{RandomColor[], Circle[RandomReal[{0, 10}, 2], RandomReal[{0, 2}]]}, 50]], "23.9"],
  TestID -> "Untitled-208@@Tests/EIWL.wlt:548,1-551,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 25*)
VerificationTest[
  ValidateCorrect[a[b[c[d[x]]]], "25.3"],
  TestID -> "Untitled-209@@Tests/EIWL.wlt:556,1-559,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 30*)
VerificationTest[
  ValidateCorrect[SortBy[Take[WordList[], 50], StringTake[StringReverse[#1], 1] & ], "30.10"],
  TestID -> "Untitled-210@@Tests/EIWL.wlt:564,1-567,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[SortBy[Range[200], StringLength[IntegerName[#1]] & ]], "x30.6"],
  TestID -> "Untitled-211@@Tests/EIWL.wlt:569,1-572,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 35*)
VerificationTest[
  ValidateCorrect[Cases[(Interpreter["University"][StringJoin["U of ", #1]] & ) /@ ToUpperCase[Alphabet[]], _Entity], "35.5"],
  TestID -> "Untitled-212@@Tests/EIWL.wlt:577,1-580,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 44*)
VerificationTest[
  ValidateCorrect[Import["https://www.google.com/", {"HTML", "Images"}], "44.1"],
  TestID -> "Untitled-213@@Tests/EIWL.wlt:585,1-588,2"
];

VerificationTest[
  ValidateCorrect[ImageCollage[Import["http://nps.gov", "Images"]], "44.4"],
  TestID -> "Untitled-214@@Tests/EIWL.wlt:590,1-593,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["http://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-215@@Tests/EIWL.wlt:595,1-598,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["https://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-216@@Tests/EIWL.wlt:600,1-603,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["http://en.wikipedia.org/", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-217@@Tests/EIWL.wlt:605,1-608,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 45*)
VerificationTest[
  ValidateCorrect[GeoListPlot[TakeLargestBy[Normal[ResourceData["Fireballs and Bolides"]], #Altitude & , 10][[All,"NearestCity"]], GeoLabels -> True], "45.13"],
  TestID -> "Untitled-218@@Tests/EIWL.wlt:613,1-616,2"
];
