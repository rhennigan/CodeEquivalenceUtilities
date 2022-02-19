(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

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
  TestID -> "Untitled-110@@Tests/EIWL.wlt:29,1-32,2"
];

VerificationTest[
  ValidateCorrect[Range[100], "3.2"],
  TestID -> "Untitled-111@@Tests/EIWL.wlt:34,1-37,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[4]], "3.3"],
  TestID -> "Untitled-112@@Tests/EIWL.wlt:39,1-42,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Range[50]], "3.4"],
  TestID -> "Untitled-113@@Tests/EIWL.wlt:44,1-47,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[4]]], "3.5"],
  TestID -> "Untitled-114@@Tests/EIWL.wlt:49,1-52,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Join[Range[100], Reverse[Range[99]]]], "3.6"],
  TestID -> "Untitled-115@@Tests/EIWL.wlt:54,1-57,2"
];

VerificationTest[
  ValidateCorrect[Range[RandomInteger[10]], "3.7"],
  TestID -> "Untitled-116@@Tests/EIWL.wlt:59,1-62,2"
];

VerificationTest[
  ValidateCorrect[Range[10], "3.8"],
  TestID -> "Untitled-117@@Tests/EIWL.wlt:64,1-67,2"
];

VerificationTest[
  ValidateCorrect[Range[5], "3.9"],
  TestID -> "Untitled-118@@Tests/EIWL.wlt:69,1-72,2"
];

VerificationTest[
  ValidateCorrect[Join[{1, 2}, {3, 4}, {5}], "3.9"],
  TestID -> "Untitled-119@@Tests/EIWL.wlt:74,1-77,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Range[10], Range[5]], "3.10"],
  TestID -> "Untitled-120@@Tests/EIWL.wlt:79,1-82,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[20], Reverse[Range[20]]], "3.11"],
  TestID -> "Untitled-121@@Tests/EIWL.wlt:84,1-87,2"
];

VerificationTest[
  ValidateCorrect[Range[4], "x3.1"],
  TestID -> "Untitled-122@@Tests/EIWL.wlt:89,1-92,2"
];

VerificationTest[
  ValidateCorrect[Reverse[Reverse[Range[4]]], "x3.1"],
  TestID -> "Untitled-123@@Tests/EIWL.wlt:94,1-97,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[5], Reverse[Range[4]]], "x3.2"],
  TestID -> "Untitled-124@@Tests/EIWL.wlt:99,1-102,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[4], Reverse[Range[5]]], "x3.2"],
  TestID -> "Untitled-125@@Tests/EIWL.wlt:104,1-107,2"
];

VerificationTest[
  ValidateCorrect[Join[Reverse[Range[3]], Reverse[Range[4]], Reverse[Range[5]]], "x3.3"],
  TestID -> "Untitled-126@@Tests/EIWL.wlt:109,1-112,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10, 14]], "x3.4"],
  TestID -> "Untitled-127@@Tests/EIWL.wlt:114,1-117,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[{10, 11, 12, 13, 14}], "x3.4"],
  TestID -> "Untitled-128@@Tests/EIWL.wlt:119,1-122,2"
];

VerificationTest[
  ValidateCorrect[Join[Range[10], Reverse[Range[10]], Range[10]], "x3.5"],
  TestID -> "Untitled-129@@Tests/EIWL.wlt:124,1-127,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 4*)
VerificationTest[
  ValidateCorrect[BarChart[{1, 1, 2, 3, 5}], "4.1"],
  TestID -> "Untitled-130@@Tests/EIWL.wlt:132,1-135,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Fibonacci[Range[5]]], "4.1"],
  TestID -> "Untitled-131@@Tests/EIWL.wlt:137,1-140,2"
];

VerificationTest[
  ValidateCorrect[PieChart[Range[10]], "4.2"],
  TestID -> "Untitled-132@@Tests/EIWL.wlt:142,1-145,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Reverse[Range[20]]], "4.3"],
  TestID -> "Untitled-133@@Tests/EIWL.wlt:147,1-150,2"
];

VerificationTest[
  ValidateCorrect[Column[{1, 2, 3, 4, 5}], "4.4"],
  TestID -> "Untitled-134@@Tests/EIWL.wlt:152,1-155,2"
];

VerificationTest[
  ValidateCorrect[Column[Range[5]], "4.4"],
  TestID -> "Untitled-135@@Tests/EIWL.wlt:157,1-160,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[{1, 4, 9, 16, 25}], "4.5"],
  TestID -> "Untitled-136@@Tests/EIWL.wlt:162,1-165,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Range[5]^2], "4.5"],
  TestID -> "Untitled-137@@Tests/EIWL.wlt:167,1-170,2"
];

VerificationTest[
  ValidateCorrect[Column[{PieChart[{1}], PieChart[{1, 1}], PieChart[{1, 1, 1}]}], "4.7"],
  TestID -> "Untitled-138@@Tests/EIWL.wlt:172,1-175,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 5*)
VerificationTest[
  ValidateCorrect[Reverse[Range[10]^2], "5.1"],
  TestID -> "Untitled-139@@Tests/EIWL.wlt:180,1-183,2"
];

VerificationTest[
  ValidateCorrect[Total[Range[10]^2], "5.2"],
  TestID -> "Untitled-140@@Tests/EIWL.wlt:185,1-188,2"
];

VerificationTest[
  ValidateCorrect[ListPlot[Range[10]^2], "5.3"],
  TestID -> "Untitled-141@@Tests/EIWL.wlt:190,1-193,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[4], Range[4]]], "5.4"],
  TestID -> "Untitled-142@@Tests/EIWL.wlt:195,1-198,2"
];

VerificationTest[
  ValidateCorrect[Range[11] + 9, "5.5"],
  TestID -> "Untitled-143@@Tests/EIWL.wlt:200,1-203,2"
];

VerificationTest[
  ValidateCorrect[Range[0, 10] + 10, "5.5"],
  TestID -> "Untitled-144@@Tests/EIWL.wlt:205,1-208,2"
];

VerificationTest[
  ValidateCorrect[Sort[Join[Range[5]^2, Range[5]^3]], "5.6"],
  TestID -> "Untitled-145@@Tests/EIWL.wlt:210,1-213,2"
];

VerificationTest[
  ValidateCorrect[Length[IntegerDigits[2^128]], "5.7"],
  TestID -> "Untitled-146@@Tests/EIWL.wlt:215,1-218,2"
];

VerificationTest[
  ValidateCorrect[First[IntegerDigits[2^32]], "5.8"],
  TestID -> "Untitled-147@@Tests/EIWL.wlt:220,1-223,2"
];

VerificationTest[
  ValidateCorrect[Take[IntegerDigits[2^100], 10], "5.9"],
  TestID -> "Untitled-148@@Tests/EIWL.wlt:225,1-228,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-149@@Tests/EIWL.wlt:230,1-233,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[1 ;; 10]], "5.9"],
  TestID -> "Untitled-150@@Tests/EIWL.wlt:235,1-238,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[2^100][[Range[10]]], "5.9"],
  TestID -> "Untitled-151@@Tests/EIWL.wlt:240,1-243,2"
];

VerificationTest[
  ValidateCorrect[Max[IntegerDigits[2^20]], "5.10"],
  TestID -> "Untitled-152@@Tests/EIWL.wlt:245,1-248,2"
];

VerificationTest[
  ValidateCorrect[Count[IntegerDigits[2^1000], 0], "5.11"],
  TestID -> "Untitled-153@@Tests/EIWL.wlt:250,1-253,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-154@@Tests/EIWL.wlt:255,1-258,2"
];

VerificationTest[
  ValidateCorrect[Sort[IntegerDigits[2^20]][[2]], "5.12"],
  TestID -> "Untitled-155@@Tests/EIWL.wlt:260,1-263,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[IntegerDigits[2^128]], "5.13"],
  TestID -> "Untitled-156@@Tests/EIWL.wlt:265,1-268,2"
];

VerificationTest[
  ValidateCorrect[Drop[Take[Range[100], 20], 10], "5.14"],
  TestID -> "Untitled-157@@Tests/EIWL.wlt:270,1-273,2"
];

VerificationTest[
  ValidateCorrect[Take[Drop[Range[100], 10], 10], "5.14"],
  TestID -> "Untitled-158@@Tests/EIWL.wlt:275,1-278,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*3, "x5.1"],
  TestID -> "Untitled-159@@Tests/EIWL.wlt:280,1-283,2"
];

VerificationTest[
  ValidateCorrect[Range[10]*Range[10], "x5.2"],
  TestID -> "Untitled-160@@Tests/EIWL.wlt:285,1-288,2"
];

VerificationTest[
  ValidateCorrect[Last[IntegerDigits[2^37]], "x5.3"],
  TestID -> "Untitled-161@@Tests/EIWL.wlt:290,1-293,2"
];

VerificationTest[
  ValidateCorrect[First[Drop[IntegerDigits[2^32], 2]], "x5.4"],
  TestID -> "Untitled-162@@Tests/EIWL.wlt:295,1-298,2"
];

VerificationTest[
  ValidateCorrect[Reverse[IntegerDigits[2^32]][[2]], "x5.4"],
  TestID -> "Untitled-163@@Tests/EIWL.wlt:300,1-303,2"
];

VerificationTest[
  ValidateCorrect[Total[IntegerDigits[3^126]], "x5.5"],
  TestID -> "Untitled-164@@Tests/EIWL.wlt:305,1-308,2"
];

VerificationTest[
  ValidateCorrect[PieChart[IntegerDigits[2^32]], "x5.6"],
  TestID -> "Untitled-165@@Tests/EIWL.wlt:310,1-313,2"
];

VerificationTest[
  ValidateCorrect[{PieChart[IntegerDigits[2^20]], PieChart[IntegerDigits[2^40]], PieChart[IntegerDigits[2^60]]}, "x5.7"],
  TestID -> "Untitled-166@@Tests/EIWL.wlt:315,1-318,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 6*)
VerificationTest[
  ValidateCorrect[Table[1000, 5], "6.1"],
  TestID -> "Untitled-167@@Tests/EIWL.wlt:323,1-326,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3, {n, 10, 20}], "6.2"],
  TestID -> "Untitled-168@@Tests/EIWL.wlt:328,1-331,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[n^2, {n, 20}]], "6.3"],
  TestID -> "Untitled-169@@Tests/EIWL.wlt:333,1-336,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 20, 2], "6.4"],
  TestID -> "Untitled-170@@Tests/EIWL.wlt:338,1-341,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n, {n, 10}], "6.4"],
  TestID -> "Untitled-171@@Tests/EIWL.wlt:343,1-346,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 2, 20, 2}], "6.4"],
  TestID -> "Untitled-172@@Tests/EIWL.wlt:348,1-351,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 10}], "6.5"],
  TestID -> "Untitled-173@@Tests/EIWL.wlt:353,1-356,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Table[n^2, {n, 10}]], "6.6"],
  TestID -> "Untitled-174@@Tests/EIWL.wlt:358,1-361,2"
];

VerificationTest[
  ValidateCorrect[BarChart[Range[10]^2], "6.6"],
  TestID -> "Untitled-175@@Tests/EIWL.wlt:363,1-366,2"
];

VerificationTest[
  ValidateCorrect[Table[IntegerDigits[n^2], {n, 10}], "6.7"],
  TestID -> "Untitled-176@@Tests/EIWL.wlt:368,1-371,2"
];

VerificationTest[
  ValidateCorrect[IntegerDigits[Range[10]^2], "6.7"],
  TestID -> "Untitled-177@@Tests/EIWL.wlt:373,1-376,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Length[IntegerDigits[n^2]], {n, 100}]], "6.8"],
  TestID -> "Untitled-178@@Tests/EIWL.wlt:378,1-381,2"
];

VerificationTest[
  ValidateCorrect[Table[First[IntegerDigits[n^2]], {n, 20}], "6.9"],
  TestID -> "Untitled-179@@Tests/EIWL.wlt:383,1-386,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[n^2]], {n, 100}]], "6.10"],
  TestID -> "Untitled-180@@Tests/EIWL.wlt:388,1-391,2"
];

VerificationTest[
  ValidateCorrect[Range[10]^3 - Range[10]^2, "x6.1"],
  TestID -> "Untitled-181@@Tests/EIWL.wlt:393,1-396,2"
];

VerificationTest[
  ValidateCorrect[Table[n^3 - n^2, {n, 10}], "x6.1"],
  TestID -> "Untitled-182@@Tests/EIWL.wlt:398,1-401,2"
];

VerificationTest[
  ValidateCorrect[Range[1, 100, 2], "x6.2"],
  TestID -> "Untitled-183@@Tests/EIWL.wlt:403,1-406,2"
];

VerificationTest[
  ValidateCorrect[Table[2*n - 1, {n, 50}], "x6.2"],
  TestID -> "Untitled-184@@Tests/EIWL.wlt:408,1-411,2"
];

VerificationTest[
  ValidateCorrect[Table[n, {n, 1, 100, 2}], "x6.2"],
  TestID -> "Untitled-185@@Tests/EIWL.wlt:413,1-416,2"
];

VerificationTest[
  ValidateCorrect[Range[2, 100, 2]^2, "x6.3"],
  TestID -> "Untitled-186@@Tests/EIWL.wlt:418,1-421,2"
];

VerificationTest[
  ValidateCorrect[Table[n^2, {n, 2, 100, 2}], "x6.3"],
  TestID -> "Untitled-187@@Tests/EIWL.wlt:423,1-426,2"
];

VerificationTest[
  ValidateCorrect[Table[(2*n)^2, {n, 50}], "x6.3"],
  TestID -> "Untitled-188@@Tests/EIWL.wlt:428,1-431,2"
];

VerificationTest[
  ValidateCorrect[Range[-3, 2], "x6.4"],
  TestID -> "Untitled-189@@Tests/EIWL.wlt:433,1-436,2"
];

VerificationTest[
  ValidateCorrect[Table[Column[{n, n^2, n^3}], {n, 20}], "x6.5"],
  TestID -> "Untitled-190@@Tests/EIWL.wlt:438,1-441,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Last[IntegerDigits[n^2]], {n, 100}]], "x6.6"],
  TestID -> "Untitled-191@@Tests/EIWL.wlt:443,1-446,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[First[IntegerDigits[3*n]], {n, 100}]], "x6.7"],
  TestID -> "Untitled-192@@Tests/EIWL.wlt:448,1-451,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n]], {n, 200}]], "x6.8"],
  TestID -> "Untitled-193@@Tests/EIWL.wlt:453,1-456,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[Total[IntegerDigits[n^2]], {n, 100}]], "x6.9"],
  TestID -> "Untitled-194@@Tests/EIWL.wlt:458,1-461,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[1/Range[20]], "x6.10"],
  TestID -> "Untitled-195@@Tests/EIWL.wlt:463,1-466,2"
];

VerificationTest[
  ValidateCorrect[NumberLinePlot[Table[1/n, {n, 20}]], "x6.10"],
  TestID -> "Untitled-196@@Tests/EIWL.wlt:468,1-471,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[Table[RandomInteger[{0, n}], {n, 100}]], "x6.11"],
  TestID -> "Untitled-197@@Tests/EIWL.wlt:473,1-476,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 7*)
VerificationTest[
  ValidateCorrect[Table[{Green, Yellow, Red}[[RandomInteger[{1, 3}]]], 100], "7.13"],
  TestID -> "Untitled-198@@Tests/EIWL.wlt:481,1-484,2"
];

VerificationTest[
  ValidateCorrect[Table[RGBColor[0, 1 - Abs[x], 0], {x, -1, 1, 0.1}], "x7.3"],
  TestID -> "Untitled-199@@Tests/EIWL.wlt:486,1-489,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 11*)
VerificationTest[
  ValidateCorrect[Length[TextWords[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-200@@Tests/EIWL.wlt:494,1-497,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["Computer"]]], "11.9"],
  TestID -> "Untitled-201@@Tests/EIWL.wlt:499,1-502,2"
];

VerificationTest[
  ValidateCorrect[WordCount[StringJoin[WikipediaData["computer"]]], "11.9"],
  TestID -> "Untitled-202@@Tests/EIWL.wlt:504,1-507,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 14*)
VerificationTest[
  ValidateCorrect[Graphics[Table[Circle[{x, y}, 1], {x, 1, 10}, {y, 1, 10}]], "14.3"],
  TestID -> "Untitled-203@@Tests/EIWL.wlt:512,1-515,2"
];

VerificationTest[
  ValidateCorrect[Graphics[Table[Style[Circle[RandomInteger[10, 2], 1], RandomColor[]], 30]], "x14.2"],
  TestID -> "Untitled-204@@Tests/EIWL.wlt:517,1-520,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 17*)
VerificationTest[
  ValidateCorrect[UnitConvert[Quantity[2500, "Yen"], Quantity["USDollars"]], "17.6"],
  TestID -> "Untitled-205@@Tests/EIWL.wlt:525,1-528,2"
];

VerificationTest[
  ValidateCorrect[Rotate[Entity["HistoricalSite", "GreatPyramidOfGiza::kbgx6"]["Image"], 180*Degree], "x17.4"],
  TestID -> "Untitled-206@@Tests/EIWL.wlt:530,1-533,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 22*)
VerificationTest[
  ValidateCorrect[Dendrogram[Table[Rasterize[FromLetterNumber[i]], {i, 10}]], "22.15"],
  TestID -> "Untitled-207@@Tests/EIWL.wlt:538,1-541,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 23*)
VerificationTest[
  ValidateCorrect[Graphics[Table[{RandomColor[], Circle[RandomReal[{0, 10}, 2], RandomReal[{0, 2}]]}, 50]], "23.9"],
  TestID -> "Untitled-208@@Tests/EIWL.wlt:546,1-549,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 25*)
VerificationTest[
  ValidateCorrect[a[b[c[d[x]]]], "25.3"],
  TestID -> "Untitled-209@@Tests/EIWL.wlt:554,1-557,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 30*)
VerificationTest[
  ValidateCorrect[SortBy[Take[WordList[], 50], StringTake[StringReverse[#1], 1] & ], "30.10"],
  TestID -> "Untitled-210@@Tests/EIWL.wlt:562,1-565,2"
];

VerificationTest[
  ValidateCorrect[ListLinePlot[SortBy[Range[200], StringLength[IntegerName[#1]] & ]], "x30.6"],
  TestID -> "Untitled-211@@Tests/EIWL.wlt:567,1-570,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 35*)
VerificationTest[
  ValidateCorrect[Cases[(Interpreter["University"][StringJoin["U of ", #1]] & ) /@ ToUpperCase[Alphabet[]], _Entity], "35.5"],
  TestID -> "Untitled-212@@Tests/EIWL.wlt:575,1-578,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 44*)
VerificationTest[
  ValidateCorrect[Import["https://www.google.com/", {"HTML", "Images"}], "44.1"],
  TestID -> "Untitled-213@@Tests/EIWL.wlt:583,1-586,2"
];

VerificationTest[
  ValidateCorrect[ImageCollage[Import["http://nps.gov", "Images"]], "44.4"],
  TestID -> "Untitled-214@@Tests/EIWL.wlt:588,1-591,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["http://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-215@@Tests/EIWL.wlt:593,1-596,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["https://en.wikipedia.org", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-216@@Tests/EIWL.wlt:598,1-601,2"
];

VerificationTest[
  ValidateCorrect[Length[Import["http://en.wikipedia.org/", "Hyperlinks"]], "44.7"],
  TestID -> "Untitled-217@@Tests/EIWL.wlt:603,1-606,2"
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 45*)
VerificationTest[
  ValidateCorrect[GeoListPlot[TakeLargestBy[Normal[ResourceData["Fireballs and Bolides"]], #Altitude & , 10][[All,"NearestCity"]], GeoLabels -> True], "45.13"],
  TestID -> "Untitled-218@@Tests/EIWL.wlt:611,1-614,2"
];
