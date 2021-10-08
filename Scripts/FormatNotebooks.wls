#!/usr/bin/env wolframscript
(* ::Package:: *)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Defaults*)
$defaultRoot   = DirectoryName[ $InputFileName, 2 ];
$defaultIgnore = FileNameJoin @ { $defaultRoot, "build" };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Get arguments*)
$root   := $root   = makeRootString @ getArg[ "root"  , $defaultRoot   ];
$ignore := $ignore = makeIgnorePatt @ getArg[ "ignore", $defaultIgnore ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getArg*)
getArg[ name_String, default_: None ] :=
    SelectFirst[ Flatten @ StringCases[
                     $ScriptCommandLine,
                     "--" <> name <> "=" ~~ root__ ~~ EndOfString :> root
                 ],
                 StringQ,
                 default
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*enclose*)
enclose // Attributes = { HoldAll };
enclose[ eval_           ] := enclose[ eval, "An unspecified error occured." ];
enclose[ eval_, msg_     ] := enclose[ eval, msg, 1 ];
enclose[ eval_, msg_, n_ ] := Enclose[
    eval,
    (Print @ ToString @ msg; Exit @ n) &
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeRootString*)
makeRootString[ str_String ] := enclose[
    ConfirmBy[ ExpandFileName @ StringTrim[ str, "\"" ], DirectoryQ ],
    StringForm[ "`1` is not a valid directory", str ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeIgnorePatt*)
makeIgnorePatt[ "None"|"none" ] := None;

makeIgnorePatt[ str_String ] :=
    Alternatives @@ (StringTrim[ #1, "\"" ] & /@ StringSplit[ str, "," ]);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getNotebookFiles*)
getNotebookFiles[             ] := getNotebookFiles @ $root;
getNotebookFiles[ root_       ] := getNotebookFiles[ root, $ignore ];
getNotebookFiles[ root_, None ] := FileNames[ "*.nb", root, Infinity ];

getNotebookFiles[ root_, ignore_ ] :=
    Select[ FileNames[ "*.nb", root, Infinity ],
            Not @* StringMatchQ[ ignore~~___ ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*print*)
print[ ind_Integer, msg__ ] :=
    Print[ StringJoin @ ConstantArray[ " ", ind ],
           DateString @ { "DateTimeShort", ".", "Millisecond" },
           ": ",
           msg
    ];

print[ msg__ ] := print[ 0, msg ];

(******************************************************************************)
(* ::Subsection::Closed:: *)
(*makeReadable*)
makeReadable[ file_ ] := enclose[
    print[ "Formatting: ", StringDelete[ file, StartOfString~~$root ] ];
    ConfirmBy[ makeReadable[ file, $overrideFormats ], FileExistsQ ],
    StringForm[ "Failed to format notebook: `1`", file ]
];

(* :!CodeAnalysis::Disable::VariableError::Block:: *)
makeReadable[ file_, HoldComplete[ overrides___ ] ] :=
    Internal`InheritedBlock[ { RawArray, NumericArray, overrides },
        Unprotect[ RawArray, NumericArray, overrides ];
        ReleaseHold[ overrideFormat /@ HoldComplete @ overrides ];

        Format[ x_RawArray? rawArrayQ, InputForm ] :=
            OutputForm[ "CompressedData[\"" <> Compress @ x <> "\"]" ];


        Format[ x_NumericArray? numericArrayQ, InputForm ] :=
            OutputForm[ "CompressedData[\"" <> Compress @ x <> "\"]" ];

        ResourceFunction[ "SaveReadableNotebook" ][
            ReplaceAll[
                Import @ file,
                a: _RawArray | _NumericArray :>
                    With[ { b = Check[ a, $Failed ] }, b /; ! FailureQ @ b ]
            ],
            file,
            "ExcludedNotebookOptions" -> { WindowMargins, WindowSize }
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rawArrayQ*)
rawArrayQ // Attributes = { HoldFirst };
rawArrayQ[ arr_RawArray ] := Developer`RawArrayQ @ Unevaluated @ arr;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*numericArrayQ*)
numericArrayQ // Attributes = { HoldFirst };
numericArrayQ[ arr_RawArray ] := NumericArrayQ @ Unevaluated @ arr;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*overrideFormat*)
overrideFormat // Attributes = { HoldAllComplete };

overrideFormat[ sym_Symbol ] := (
    Unprotect @ sym;
    Format[ x_sym, InputForm ] :=
        OutputForm @ ToString @ Unevaluated @ FullForm @ x
);

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$overrideFormats*)
$overrideFormats = HoldComplete[
    Alternatives,
    Apply,
    Composition,
    Decrement,
    Increment,
    Map,
    MessageName,
    Not,
    Out,
    Part,
    Pattern,
    PatternTest,
    PreDecrement,
    PreIncrement,
    RightComposition
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Run*)

makeReadable /@ getNotebookFiles[ ]


(* :!CodeAnalysis::EndBlock:: *)