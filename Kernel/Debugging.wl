Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


BeginPackage[ "Wolfram`CodeEquivalenceUtilities`Debugging`" ];
(* Exported symbols added here with SymbolName::usage *)
Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


$DebugDataPath;
$StackExport;



Begin[ "`Private`" ];



(******************************************************************************)



$DebugDataPath := FileNameJoin @ { $UserBaseDirectory,
                                   "ApplicationData",
                                   "CodeEquivalenceUtilities",
                                   "Debug"
                                 };



(******************************************************************************)



$StackExport // Attributes = { ReadProtected };


$StackExport :=
  Module[ { timestamp, stackFile, stackPath },

      timestamp = ToString @ Round[ 1000 * AbsoluteTime[ ] ];
      stackFile = StringJoin[ timestamp, "_StackData.mx" ];
      stackPath = FileNameJoin @ { $DebugDataPath, stackFile };

      Export[ stackPath, Stack[ _ ] ]
  ];

$StackExport // Protect;



(******************************************************************************)



End[ ]; (* `Private` *)

EndPackage[ ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
