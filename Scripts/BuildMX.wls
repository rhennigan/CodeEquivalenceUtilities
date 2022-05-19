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

GeneralUtilities`EnsureDirectory @ DirectoryName @ $mxFile;

If[ FileExistsQ @ $mxFile,
    Print[ "Removing existing MX file..." ];
    DeleteFile @ $mxFile
];

Print[ "Loading paclet..." ];
PacletDirectoryLoad @ $pacletDir;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

Print[ "Building MX file: ", $mxFile ];
DumpSave[
    $mxFile,
    {
        "Wolfram`CodeEquivalenceUtilities`",
        "Wolfram`CodeEquivalenceUtilities`Private`"
    }
]

(* :!CodeAnalysis::EndBlock:: *)