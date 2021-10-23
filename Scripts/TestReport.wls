#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*jUnitTestResults*)
jUnitTestResults[ files_List ] :=
    XMLObject[ "Document" ][
        { XMLObject[ "Declaration" ][ "Version" -> "1.0" ] },
        XMLElement[ "testsuites", { }, jUnitTestSuite /@ files ],
        { }
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*jUnitTestSuite*)
jUnitTestSuite[ file_? FileExistsQ ] :=
    Module[ { report },
        report = TestReport @ file;

        Export[
            FileNameJoin @ {
                $outDir,
                StringJoin[
                    StringDelete[
                        FileBaseName @ file,
                        StartOfString ~~ $testDir
                    ],
                    ".wxf"
                ]
            },
            report
        ];

        jUnitTestSuite @ report
    ];

jUnitTestSuite[ report_TestReportObject ] :=
    XMLElement[
        "testsuite",
        {
            "errors" -> ToString @ report[ "TestsFailedWithErrorsCount" ],
            "failures" ->
                ToString @ Plus[
                    report[ "TestsFailedWrongResultsCount" ],
                    report[ "TestsFailedWithMessagesCount" ]
                ],
            "name" -> report[ "Title" ],
            "tests" -> ToString @ Length @ report[ "TestResults" ]
        },
        Block[ { $testIndex = 0 },
            jUnitTestCase /@ Values @ report[ "TestResults" ]
        ]
    ];

jUnitTestSuite[ e_, ___ ] := (

    Print[
        "::error::",
        "Expected a file or TestReportObject instead of ",
        Short @ e
    ];

    Exit[ 1 ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getTestID*)
getTestID[ result_TestResultObject ] := (
    $testIndex++;
    ToString @ Replace[
        result[ "TestID" ],
        None :> "Test-"<>ToString[ $testIndex ]
    ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*jUnitTestCase*)
jUnitTestCase[ result_TestResultObject ] :=
    XMLElement[
        "testcase",
        {
            "classname" -> getTestID @ result,
            "name" ->
                StringTake[
                    Replace[
                        result[ "Input" ],
                        HoldForm[ expr_ ] :>
                            ToString[ Unevaluated @ expr, InputForm ]
                    ],
                    UpTo[ 80 ]
                ],
            "time" -> TextString @ QuantityMagnitude @ UnitConvert[
                result[ "AbsoluteTimeUsed" ],
                "Seconds"
            ]
        },
        {
            XMLElement[ "system-out", { }, { } ],
            XMLElement[ "system-err", { }, { (* TODO *) } ]
        }
    ];

jUnitTestCase[ e_, ___ ] := (
    Print[ "::error::", "Expected a TestResultObject instead of ", Short @ e ];
    Exit[ 1 ]
);

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Run*)
$testIndex     = 0;
$inputFileName = Replace[ $InputFileName, "" :> NotebookFileName[ ] ];
$pacletDir     = DirectoryName[ $inputFileName, 2 ];
$testDir       = FileNameJoin @ { $pacletDir, "Tests" };
$testFiles     = FileNames[ "*.wlt", $testDir, Infinity ];
$outDir        = FileNameJoin @ { $pacletDir, "test-results" };

PacletDirectoryLoad @ $pacletDir;

Export[ FileNameJoin @ { $outDir, "results.xml" },
        jUnitTestResults @ $testFiles
];


(* :!CodeAnalysis::EndBlock:: *)