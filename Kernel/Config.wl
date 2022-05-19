(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)
BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

$MemoryLimit;
$RasterBoxErrorThreshold;
$RasterErrorThreshold;
$RasterizeTimeLimit;
$RasterPatterns;
$RasterPerimeter;
$RasterSize;
$TimeLimit;
$TransformationLimit;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Configuration values*)
$MemoryLimit             = 256 * 1048576;
$RasterBoxErrorThreshold = 0.1;
$RasterErrorThreshold    = 0.01;
$RasterizeTimeLimit      = 5;
$RasterPerimeter         = 0;
$RasterSize              = { 100, 100 };
$TimeLimit               = 5;
$TransformationLimit     = 10;

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

$$hold := Hold | HoldForm | HoldComplete;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
