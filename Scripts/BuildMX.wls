#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$inputFileName = Replace[ $InputFileName, "" :> NotebookFileName[ ] ];
$pacletDir     = DirectoryName[ $inputFileName, 2 ];
$mxFile        = FileNameJoin @ {
                     $pacletDir,
                     "Kernel",
                     ToString @ $SystemWordLength <> "Bit",
                     "CodeEquivalenceUtilities.mx"
                 };

If[ FileExistsQ @ $mxFile,
    Print[ "Removing existing MX file..." ];
    DeleteFile @ $mxFile
];

Print[ "Loading paclet..." ];
PacletDirectoryLoad @ $pacletDir;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$mxDir = GeneralUtilities`EnsureDirectory @ DirectoryName @ $mxFile;
Print[ "MX target directory: ", $mxDir ];
Print[ "Building MX file: ", $mxFile ];
DumpSave[
    $mxFile,
    {
        "Wolfram`CodeEquivalenceUtilities`",
        "Wolfram`CodeEquivalenceUtilities`Private`"
    }
];

If[ ! FileExistsQ @ $mxFile,
    Print[ "::error::", "Failed to build MX file!" ];
    If[ $EvaluationEnvironment === "Script", Exit[ 1 ] ]
];

$mxFile

(* :!CodeAnalysis::EndBlock:: *)