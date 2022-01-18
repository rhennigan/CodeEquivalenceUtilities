Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


BeginPackage[ "Wrong`CodeEquivalenceUtilities`CachedValues`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`",
        "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`",
        "Wolfram`CodeEquivalenceUtilities`EvaluationControl`"
    }
];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(******************************************************************************)
(* Exported symbols added here with SymbolName::usage *)

Cached;
CacheFileData;
CacheFileDataLookup;
CachePath;
KeyHash;
UseCache;
InvalidateCacheFile;
ClearEvalCache;
CachedQ;
InitializeCache;


(******************************************************************************)


(* Hey CodeInspector, look at this: *)
If[ a, b, b ]


Begin[ "`Private`" ];


$packageRoot = First @ StringSplit[ $Context, "`" ] ;


(******************************************************************************)



CacheFileData[ a_Association ][ key_ ] := a[ key ];



(******************************************************************************)



KeyHash // Attributes = { };
KeyHash // Options    = { };


KeyHash[ expr_ ] :=
  Key @ StringPadLeft[ StringReverse[ Hash @ expr ~IntegerString~ 16 ],
                       16,
                       "0"
        ];


KeyHash[ expr_, exprs__ ] :=
  KeyHash @ { expr, exprs };



(******************************************************************************)



CacheFileDataLookup // Attributes = { };
CacheFileDataLookup // Options    = { "Depth" -> 3 };


CacheFileDataLookup[ Key[ keyHash_String ], opts : OptionsPattern[ ] ] :=

  Module[ { depth, partitionedHash, hashDirPath, hashFile },

      depth             = OptionValue @ "Depth";
      partitionedHash   = StringPartition[ keyHash, 2 ];
      hashDirPath       = FileNameJoin @ Take[ partitionedHash, depth ];
      hashFile          = Drop[ partitionedHash, depth ];

      CacheFileData @ <|
          "Index"       -> hashDirPath,
          "Payload"     -> StringJoin[ hashFile, "_p.mx" ],
          "Metadata"    -> StringJoin[ hashFile, "_m.mx" ]
      |>

  ];


CacheFileDataLookup[ keyExpression : Except[ Key[ _String ] ],
                     opts : OptionsPattern[ ]
                   ] :=

  CacheFileDataLookup[ KeyHash @ keyExpression, opts ];



(******************************************************************************)



CachePath // Attributes = { };
CachePath // Options    = { "PackageRoot" -> $packageRoot };


CachePath[ key_, opts : OptionsPattern[ ] ] :=

  Module[ { cacheData, cacheDirectory, payload, metadata },

      cacheData = CacheFileDataLookup @ key;

      cacheDirectory = FileNameJoin @ { $UserBaseDirectory,
                                        "ApplicationData",
                                        OptionValue @ "PackageRoot",
                                        "Index",
                                        cacheData @ "Index"
                                      };

      payload  = FileNameJoin @ { cacheDirectory, cacheData @ "Payload"  };
      metadata = FileNameJoin @ { cacheDirectory, cacheData @ "Metadata" };

      <|
          "Payload"     -> payload,
          "Metadata"    -> metadata
      |>

  ];


CachePath[ key_, lookup_, opts : OptionsPattern[ ] ] :=
  CachePath[ key, opts ][ lookup ];



(******************************************************************************)


contentString // ClearAll;
contentString[ expr_ ] :=
  Apply[
      ToFullString,
      Fold[
          Function[
              Replace[
                  #1,
                  e_ /; ByteCount[Unevaluated[e]] > 50000 :> With[
                      {b = ByteCount[Unevaluated[e]]},
                      Wolfram`CodeEquivalenceUtilities`Utilities`$Redacted[b] /; True
                  ],
                  {#2}
              ]
          ],
          expr,
          Reverse[Range[Min[5, Depth[expr]]]]
      ]
  ];



Cached // Attributes = { HoldAllComplete, SequenceHold };
Cached // Options    = {
    "Expiration"     -> Quantity[ 7, "Days" ],
    "NormalizeNames" -> False
};


Cached[ expr_, opts : OptionsPattern[ ] ] :=
  Module[
      {
          hexpr, pathData, payloadPath, metadataPath,
          cachedQ, $failed = False, timing, value, new, result, metadata
      },

      hexpr = HoldComplete @ expr //. {
          HoldPattern[ Cached[ e_, ___ ] ] :> e
      };

      pathData      = If[ OptionValue @ "NormalizeNames",
                          CachePath @ NormalizeNames @ hexpr,
                          CachePath @ hexpr
                      ];

      payloadPath   = pathData  @ "Payload";
      metadataPath  = pathData  @ "Metadata";

      cachedQ :=
        And[ FileExistsQ @ payloadPath,
             FileExistsQ @ metadataPath,
             DateObject[ TimeZone -> 0 ] - Import[ metadataPath ][ "LastUsedDate" ] <
               OptionValue @ "Expiration"
        ];

      new := (
          $failed = FailureQ @
            Check[ { timing, value } = AbsoluteTiming @ expr,
                   $Failed
            ] || FailureQ @ value;

          If[ ! $failed,
              GeneralUtilities`EnsureDirectory @ DirectoryName @ payloadPath;
              Export[ payloadPath, value ];
          ];

          value
      );

      result =
        If[ Quiet @ TrueQ @ Check[ cachedQ, False ]
            ,
            Quiet @ Check[ Import @ payloadPath, new ]
            ,
            new
        ];

      If[ ! TrueQ @ $failed
          ,

          Cached[ Verbatim @ expr, opts ] = result;

          metadata = Quiet @ CheckPattern[
              Import @ metadataPath,
              _? AssociationQ,
              <|
                  "CreationDate" -> DateObject[ TimeZone -> 0 ],
                  "AccessCount"  -> 0,
                  "Contents"     -> contentString @ hexpr,
                  "PathData"     -> pathData,
                  "Timing"       -> timing
              |>
          ];

          GeneralUtilities`EnsureDirectory @ DirectoryName @ metadataPath;

          metadata = Join[ metadata,
                           <|
                               "LastUsedDate"  -> DateObject[ TimeZone -> 0 ],
                               "AccessCount"   -> metadata[ "AccessCount" ] + 1
                           |>
                     ];

          Sow[ metadata, $cacheMetadata ];

          Export[ metadataPath, metadata ];
      ];

      result

  ];


$OriginalCachedDV = DownValues @ Cached;



(******************************************************************************)



UseCache // Attributes = { HoldAllComplete };
UseCache // Options    = {
    "CacheSymbols" -> {
        "DiscretizeGraphics",
        "DominantColors",
        "EntityList",
        "EntityValue",
        "ExampleData",
        "GenomeData",
        "GeoElevationData",
        "GeoListPlot",
        "GeoWithinQ",
        "ImageCollage",
        "Import",
        "Nearest",
        "NetModel",
        "ResourceData",
        "ResourceObject",
        "ResourceSearch",
        "SolarEclipse",
        "TextSentences",
        "WikipediaData",
        "WordCloud",
        "WordList"
    },
    "Expiration" -> Quantity[ 1, "Days" ]
};

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::VariableError::Block:: *)
UseCache[ expression_, opts: OptionsPattern[ ] ] :=
  Module[ { cacheSymbols, cacheReady },

      cacheSymbols = Flatten @ Apply[
          HoldComplete,
          ToExpression[ OptionValue[ "CacheSymbols" ],
                        InputForm,
                        HoldComplete
          ]
      ];

      Replace[
          cacheSymbols,
          HoldComplete[ syms___ ] :>
            Internal`InheritedBlock[ { syms },
                ReleaseHold[ override /@ HoldComplete[syms] ];
                Cached @ expression
            ]
      ]
  ];
(* :!CodeAnalysis::EndBlock:: *)


override // Attributes = { HoldAllComplete };

override[ sym_ ] := (
    Unprotect[sym];
    PrependTo[
        DownValues[sym],
        HoldPattern[sym[args___] /; ! TrueQ@$usingCache ] :>
          Block[{$usingCache = True}, Cached[sym[args]]]
    ];
    Protect[sym];
);



(******************************************************************************)



InvalidateCacheFile // Attributes = { };
InvalidateCacheFile // Options    = { };


InvalidateCacheFile[ metafile_String? FileExistsQ ] :=

  With[
      {
          meta = Append[ Import @ metafile,
                         "LastUsedDate" -> DateObject @ 0 ]
      },

      Export[ metafile, meta ];
      meta
  ];



(******************************************************************************)



ClearEvalCache // Attributes = { HoldAllComplete, SequenceHold };
ClearEvalCache // Options    = { "PackageRoot" -> $packageRoot };


ClearEvalCache[ OptionsPattern[ ] ] :=
    Quiet[

        DeleteDirectory[
            FileNameJoin @ {
                $UserBaseDirectory,
                "ApplicationData",
                OptionValue[ "PackageRoot" ],
                "Index"
            },
            DeleteContents -> True
        ];

        DownValues[ Cached ] = $OriginalCachedDV;
        ,
        DeleteDirectory::nodir
    ];


ClearEvalCache[ expr_, opts: OptionsPattern[ ] ] :=
  Scan[ DeleteFile,
        Select[ CachePath @ HoldComplete @ expr, FileExistsQ ]
  ];



(******************************************************************************)



CachedQ // Attributes = { HoldAllComplete, SequenceHold };
CachedQ // Options    = { };


CachedQ[ expr_ ] :=
  AllTrue[ CachePath @ HoldComplete @ expr, FileExistsQ ];


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*InitializeCache*)
InitializeCache[ ] := InitializeCache @ All;

InitializeCache[ "Local" ] :=
    Module[ { list },
        Quiet @ ResourceObject;
        list = List @@ (HoldComplete /@ $PrecachedExpressions);
        precacheValue /@ list
    ];

InitializeCache[ "Cloud" ] := (
    DownValues[ Cached ] = $OriginalCachedDV;
    CloudEvaluate @ InitializeCache[ "Local" ]
);

InitializeCache[ All ] := <|
    "Cloud" -> InitializeCache[ "Cloud" ],
    "Local" -> InitializeCache[ "Local" ]
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*precacheValue*)
precacheValue // Attributes = { HoldAllComplete };

precacheValue[ HoldComplete[ expr_ ] ] :=
    precacheValue @ expr;

precacheValue[ e_ ] :=
    Enclose @ Module[ { held, file, meta },
        Cached[ e, "Expiration" -> Quantity[ 0, "Days" ] ];
        held = HoldComplete @ e;
        file = ConfirmBy[ CachePath[ held, "Metadata" ], FileExistsQ ];
        meta = ConfirmBy[ Import @ file, AssociationQ ];
        meta
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$PrecachedExpressions*)
$PrecachedExpressions = HoldComplete[
    ResourceData[ "Fireballs and Bolides" ],
    WikipediaData[ "computer" ]
];


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*End Package*)
End[ ]; (* `Private` *)

EndPackage[ ];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
