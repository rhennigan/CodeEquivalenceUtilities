#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$inputFileName = Replace[ $InputFileName, "" :> NotebookFileName[ ] ];
$pacletDir     = DirectoryName[ $inputFileName, 2 ];
$dataDir       = GeneralUtilities`EnsureDirectory @ { $pacletDir, "Data" };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)
$systemNames = Names[ "System`*" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*namesWithAttribute*)
namesWithAttribute[ attr_ ] :=
    Select[ $systemNames, MemberQ[ Attributes @ #, attr ] & ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*InertSymbols*)
$mathNames = namesWithAttribute[ NumericFunction|Constant ];

byVersion = Map[
    "System`" <> # &,
    KeySort @ Merge[
        MapApply[
            Rule,
            WolframLanguageData[ $mathNames, { "VersionIntroduced", "Name" } ]
        ],
        Identity
    ],
    { 2 }
];

data = Association[
    0 -> Union @ Flatten @ {
        Values @ KeySelect[ byVersion, LessThan[ 12.2 ] ],
        "System`Hold",
        "System`HoldComplete",
        "System`HoldForm",
        "System`HoldPattern",
        "Wolfram`CodeEquivalenceUtilities`TempHold"
    },
    KeySelect[ byVersion, GreaterEqualThan[ 12.2 ] ]
];

Export[
    FileNameJoin @ { $dataDir, "SimplifySymbols.wxf" },
    data,
    "WXF"
]

(* :!CodeAnalysis::EndBlock:: *)