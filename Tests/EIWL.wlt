(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$testDataDirectory =
    PacletObject[ "Wolfram__CodeEquivalenceUtilities" ][
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
VerificationTest @ ValidateCorrect[ Range[ 4 ], "3.1" ];

VerificationTest @ ValidateCorrect[ Range[ 100 ], "3.2" ];

VerificationTest @ ValidateCorrect[ Reverse @ Range[ 4 ], "3.3" ];

VerificationTest @ ValidateCorrect[ Reverse @ Range[ 50 ], "3.4" ];

VerificationTest @ ValidateCorrect[ Join[ Range[ 4 ], Reverse @ Range[ 4 ] ], "3.5" ];

VerificationTest @ ValidateCorrect[ ListPlot @ Join[ Range[ 100 ], Reverse @ Range[ 99 ] ], "3.6" ];

VerificationTest @ ValidateCorrect[ Range @ RandomInteger[ 10 ], "3.7" ];

VerificationTest @ ValidateCorrect[ Range[ 10 ], "3.8" ];

VerificationTest @ ValidateCorrect[ Range[ 5 ], "3.9" ];

VerificationTest @ ValidateCorrect[ Join[ { 1, 2 }, { 3, 4 }, { 5 } ], "3.9" ];

VerificationTest @ ValidateCorrect[ Join[ Range[ 10 ], Range[ 10 ], Range[ 5 ] ], "3.10" ];

VerificationTest @ ValidateCorrect[ Join[ Range[ 20 ], Reverse @ Range[ 20 ] ], "3.11" ];

VerificationTest @ ValidateCorrect[ Range[ 4 ], "x3.1" ];

VerificationTest @ ValidateCorrect[ Reverse @ Reverse @ Range[ 4 ], "x3.1" ];

VerificationTest @ ValidateCorrect[ Join[ Range[ 5 ], Reverse @ Range[ 4 ] ], "x3.2" ];

VerificationTest @ ValidateCorrect[ Join[ Range[ 4 ], Reverse @ Range[ 5 ] ], "x3.2" ];

VerificationTest @ ValidateCorrect[ Join[ Reverse @ Range[ 3 ], Reverse @ Range[ 4 ], Reverse @ Range[ 5 ] ], "x3.3" ];

VerificationTest @ ValidateCorrect[ ListPlot @ Range[ 10, 14 ], "x3.4" ];

VerificationTest @ ValidateCorrect[ ListPlot @ { 10, 11, 12, 13, 14 }, "x3.4" ];

VerificationTest @ ValidateCorrect[ Join[ Range[ 10 ], Reverse @ Range[ 10 ], Range[ 10 ] ], "x3.5" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 4*)
VerificationTest @ ValidateCorrect[ BarChart @ { 1, 1, 2, 3, 5 }, "4.1" ];

VerificationTest @ ValidateCorrect[ BarChart @ Fibonacci @ Range[ 5 ], "4.1" ];

VerificationTest @ ValidateCorrect[ PieChart @ Range[ 10 ], "4.2" ];

VerificationTest @ ValidateCorrect[ BarChart @ Reverse @ Range[ 20 ], "4.3" ];

VerificationTest @ ValidateCorrect[ Column @ { 1, 2, 3, 4, 5 }, "4.4" ];

VerificationTest @ ValidateCorrect[ Column @ Range[ 5 ], "4.4" ];

VerificationTest @ ValidateCorrect[ NumberLinePlot @ { 1, 4, 9, 16, 25 }, "4.5" ];

VerificationTest @ ValidateCorrect[ NumberLinePlot[ Range[5]^2 ], "4.5" ];

VerificationTest @ ValidateCorrect[ Column @ { PieChart @ { 1 }, PieChart @ { 1, 1 }, PieChart @ { 1, 1, 1 } }, "4.7" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 5*)
VerificationTest @ ValidateCorrect[ Reverse[ Range[10]^2 ], "5.1" ];

VerificationTest @ ValidateCorrect[ Total[ Range[10]^2 ], "5.2" ];

VerificationTest @ ValidateCorrect[ ListPlot[ Range[10]^2 ], "5.3" ];

VerificationTest @ ValidateCorrect[ Sort @ Join[ Range[ 4 ], Range[ 4 ] ], "5.4" ];

VerificationTest @ ValidateCorrect[ Range[ 11 ] + 9, "5.5" ];

VerificationTest @ ValidateCorrect[ Range[ 0, 10 ] + 10, "5.5" ];

VerificationTest @ ValidateCorrect[ Sort @ Join[ Range[5]^2, Range[5]^3 ], "5.6" ];

VerificationTest @ ValidateCorrect[ Length @ IntegerDigits[ 2^128 ], "5.7" ];

VerificationTest @ ValidateCorrect[ First @ IntegerDigits[ 2^32 ], "5.8" ];

VerificationTest @ ValidateCorrect[ Take[ IntegerDigits[ 2^100 ], 10 ], "5.9" ];

VerificationTest @ ValidateCorrect[ (IntegerDigits[ 2^100 ])[[ Range[ 10 ] ]], "5.9" ];

VerificationTest @ ValidateCorrect[ (IntegerDigits[ 2^100 ])[[ 1;;10 ]], "5.9" ];

VerificationTest @ ValidateCorrect[ (IntegerDigits[ 2^100 ])[[ Range[ 10 ] ]], "5.9" ];

VerificationTest @ ValidateCorrect[ Max @ IntegerDigits[ 2^20 ], "5.10" ];

VerificationTest @ ValidateCorrect[ Count[ IntegerDigits[ 2^1000 ], 0 ], "5.11" ];

VerificationTest @ ValidateCorrect[ (Sort @ IntegerDigits[ 2^20 ])[[ 2 ]], "5.12" ];

VerificationTest @ ValidateCorrect[ (Sort @ IntegerDigits[ 2^20 ])[[ 2 ]], "5.12" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ IntegerDigits[ 2^128 ], "5.13" ];

VerificationTest @ ValidateCorrect[ Drop[ Take[ Range[ 100 ], 20 ], 10 ], "5.14" ];

VerificationTest @ ValidateCorrect[ Take[ Drop[ Range[ 100 ], 10 ], 10 ], "5.14" ];

VerificationTest @ ValidateCorrect[ Range[ 10 ] * 3, "x5.1" ];

VerificationTest @ ValidateCorrect[ Range[ 10 ] * Range[ 10 ], "x5.2" ];

VerificationTest @ ValidateCorrect[ Last @ IntegerDigits[ 2^37 ], "x5.3" ];

VerificationTest @ ValidateCorrect[ First @ Drop[ IntegerDigits[ 2^32 ], 2 ], "x5.4" ];

VerificationTest @ ValidateCorrect[ (Reverse @ IntegerDigits[ 2^32 ])[[ 2 ]], "x5.4" ];

VerificationTest @ ValidateCorrect[ Total @ IntegerDigits[ 3^126 ], "x5.5" ];

VerificationTest @ ValidateCorrect[ PieChart @ IntegerDigits[ 2^32 ], "x5.6" ];

VerificationTest @ ValidateCorrect[ { PieChart @ IntegerDigits[ 2^20 ], PieChart @ IntegerDigits[ 2^40 ], PieChart @ IntegerDigits[ 2^60 ] }, "x5.7" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 6*)
VerificationTest @ ValidateCorrect[ Table[ 1000, 5 ], "6.1" ];

VerificationTest @ ValidateCorrect[ Table[ n^3, { n, 10, 20 } ], "6.2" ];

VerificationTest @ ValidateCorrect[ NumberLinePlot @ Table[ n^2, { n, 20 } ], "6.3" ];

VerificationTest @ ValidateCorrect[ Range[ 2, 20, 2 ], "6.4" ];

VerificationTest @ ValidateCorrect[ Table[ 2 * n, { n, 10 } ], "6.4" ];

VerificationTest @ ValidateCorrect[ Table[ n, { n, 2, 20, 2 } ], "6.4" ];

VerificationTest @ ValidateCorrect[ Table[ n, { n, 10 } ], "6.5" ];

VerificationTest @ ValidateCorrect[ BarChart @ Table[ n^2, { n, 10 } ], "6.6" ];

VerificationTest @ ValidateCorrect[ BarChart[ Range[10]^2 ], "6.6" ];

VerificationTest @ ValidateCorrect[ Table[ IntegerDigits[ n^2 ], { n, 10 } ], "6.7" ];

VerificationTest @ ValidateCorrect[ IntegerDigits[ Range[10]^2 ], "6.7" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ Length @ IntegerDigits[ n^2 ], { n, 100 } ], "6.8" ];

VerificationTest @ ValidateCorrect[ Table[ First @ IntegerDigits[ n^2 ], { n, 20 } ], "6.9" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ First @ IntegerDigits[ n^2 ], { n, 100 } ], "6.10" ];

VerificationTest @ ValidateCorrect[ Range[10]^3 - Range[10]^2, "x6.1" ];

VerificationTest @ ValidateCorrect[ Table[ n^3 - n^2, { n, 10 } ], "x6.1" ];

VerificationTest @ ValidateCorrect[ Range[ 1, 100, 2 ], "x6.2" ];

VerificationTest @ ValidateCorrect[ Table[ 2 * n - 1, { n, 50 } ], "x6.2" ];

VerificationTest @ ValidateCorrect[ Table[ n, { n, 1, 100, 2 } ], "x6.2" ];

VerificationTest @ ValidateCorrect[ Range[2, 100, 2]^2, "x6.3" ];

VerificationTest @ ValidateCorrect[ Table[ n^2, { n, 2, 100, 2 } ], "x6.3" ];

VerificationTest @ ValidateCorrect[ Table[ (2*n)^2, { n, 50 } ], "x6.3" ];

VerificationTest @ ValidateCorrect[ Range[ -3, 2 ], "x6.4" ];

VerificationTest @ ValidateCorrect[ Table[ Column @ { n, n^2, n^3 }, { n, 20 } ], "x6.5" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ Last @ IntegerDigits[ n^2 ], { n, 100 } ], "x6.6" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ First @ IntegerDigits[ 3 * n ], { n, 100 } ], "x6.7" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ Total @ IntegerDigits @ n, { n, 200 } ], "x6.8" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ Total @ IntegerDigits[ n^2 ], { n, 100 } ], "x6.9" ];

VerificationTest @ ValidateCorrect[ NumberLinePlot[ 1 * Range[20]^(-1) ], "x6.10" ];

VerificationTest @ ValidateCorrect[ NumberLinePlot @ Table[ 1 * n^(-1), { n, 20 } ], "x6.10" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ Table[ RandomInteger @ { 0, n }, { n, 100 } ], "x6.11" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 7*)
VerificationTest @ ValidateCorrect[ Table[ ({ Green, Yellow, Red })[[ RandomInteger @ { 1, 3 } ]], 100 ], "7.13" ];

VerificationTest @ ValidateCorrect[ Table[ RGBColor[ 0, 1 - Abs @ x, 0 ], { x, -1, 1, 0.1 } ], "x7.3" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 11*)
VerificationTest @ ValidateCorrect[ Length @ TextWords @ WikipediaData[ "computer" ], "11.9" ];

VerificationTest @ ValidateCorrect[ WordCount @ StringJoin @ WikipediaData[ "Computer" ], "11.9" ];

VerificationTest @ ValidateCorrect[ WordCount @ StringJoin @ WikipediaData[ "computer" ], "11.9" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 14*)
VerificationTest @ ValidateCorrect[ Graphics @ Table[ Circle[ { x, y }, 1 ], { x, 1, 10 }, { y, 1, 10 } ], "14.3" ];

VerificationTest @ ValidateCorrect[ Graphics @ Table[ Style[ Circle[ RandomInteger[ 10, 2 ], 1 ], RandomColor[ ] ], 30 ], "x14.2" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 17*)
VerificationTest @ ValidateCorrect[ UnitConvert[ Quantity[ 2500, "Yen" ], Quantity[ "USDollars" ] ], "17.6" ];

VerificationTest @ ValidateCorrect[ Rotate[ Entity[ "HistoricalSite", "GreatPyramidOfGiza::kbgx6" ][ "Image" ], 180 * Degree ], "x17.4" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 22*)
VerificationTest @ ValidateCorrect[ Dendrogram @ Table[ Rasterize @ FromLetterNumber @ i, { i, 10 } ], "22.15" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 23*)
VerificationTest @ ValidateCorrect[ Graphics @ Table[ { RandomColor[ ], Circle[ RandomReal[ { 0, 10 }, 2 ], RandomReal @ { 0, 2 } ] }, 50 ], "23.9" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 25*)
VerificationTest @ ValidateCorrect[ a @ b @ c @ d @ x, "25.3" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 30*)
VerificationTest @ ValidateCorrect[ SortBy[ Take[ WordList[ ], 50 ], StringTake[ StringReverse[ #1 ], 1 ] & ], "30.10" ];

VerificationTest @ ValidateCorrect[ ListLinePlot @ SortBy[ Range[ 200 ], StringLength @ IntegerName[ #1 ] & ], "x30.6" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 35*)
VerificationTest @ ValidateCorrect[ Cases[ (Interpreter[ "University" ][ "U of " <> #1 ] &) /@ ToUpperCase @ Alphabet[ ], _Entity ], "35.5" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 44*)
VerificationTest @ ValidateCorrect[ Import[ "https://www.google.com/", { "HTML", "Images" } ], "44.1" ];

VerificationTest @ ValidateCorrect[ ImageCollage @ Import[ "http://nps.gov", "Images" ], "44.4" ];

VerificationTest @ ValidateCorrect[ Length @ Import[ "http://en.wikipedia.org", "Hyperlinks" ], "44.7" ];

VerificationTest @ ValidateCorrect[ Length @ Import[ "https://en.wikipedia.org", "Hyperlinks" ], "44.7" ];

VerificationTest @ ValidateCorrect[ Length @ Import[ "http://en.wikipedia.org/", "Hyperlinks" ], "44.7" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*CHAPTER 45*)
VerificationTest @ ValidateCorrect[ GeoListPlot[ (TakeLargestBy[ Normal @ ResourceData[ "Fireballs and Bolides" ], #Altitude &, 10 ])[[ All, "NearestCity" ]], GeoLabels -> True ], "45.13" ];
