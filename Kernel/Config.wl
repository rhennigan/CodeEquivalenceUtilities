(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)
BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

$AllowedEvaluationPatterns;
$RasterBoxErrorThreshold;
$RasterErrorThreshold;
$RasterizeTimeLimit;
$RasterPatterns;
$RasterPerimeter;
$RasterSize;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Configuration values*)
$AllowedEvaluationPatterns = Automatic;
$RasterBoxErrorThreshold   = 0.1;
$RasterErrorThreshold      = 0.01;
$RasterizeTimeLimit        = 5;
$RasterPerimeter           = 0;
$RasterSize                = { 100, 100 };

$RasterPatterns = HoldPattern @ Alternatives[
    ArrayPlot,
    BarChart,
    DateListPlot,
    GeoGraphics,
    GeoListPlot,
    Graph,
    Graphics,
    Graphics3D,
    Histogram,
    Image,
    ListLinePlot,
    ListPlot,
    ListPlot3D,
    Manipulate,
    NumberLinePlot,
    PieChart,
    Plot,
    ReliefPlot,
    Rotate,
    WordCloud
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Argument patterns*)

$$hold := Hold | HoldForm | HoldComplete | HoldPattern | $holdWrapper | topHold;

$$numConst = HoldPattern @ Alternatives[
    Catalan,
    Degree,
    E,
    EulerGamma,
    Glaisher,
    GoldenAngle,
    GoldenRatio,
    Khinchin,
    MachinePrecision,
    Pi
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
