(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
$CachePersistence;
Cached;
CachedQ;
CacheFileData;
CacheFileDataLookup;
CachePath;
ClearEvalCache;
InitializeCache;
InvalidateCacheFile;
KeyHash;
UseCache;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Declarations*)
NormalizeNames;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

$CachePersistence = "Local";
$packageRoot      = First @ StringSplit[ $Context, "`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cached*)
Cached // Attributes = { HoldAllComplete, SequenceHold };

Cached // Options    = {
    "Expiration"     :> Quantity[ 7, "Days" ],
    "NormalizeNames" -> False
};

Cached[ expr_, ___ ] :=
    With[ { res = Lookup[ $sessionCache, sessionCacheKey @ expr, $missing ] },
        res /; res =!= $missing
    ];

Cached[ expr_, opts: OptionsPattern[ ] ] /; $localCaching :=
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

          $sessionCache[ sessionCacheKey @ expr ] = result;

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

Cached[ expr_, opts: OptionsPattern[ ] ] /; $sessionCaching :=
    Module[ { $failed, res },
        $failed = False;
        Check[ res = expr, $failed = True ];
        If[ TrueQ @ $failed || FailureQ @ res,
            res,
            $sessionCache[ sessionCacheKey @ expr ] = res
        ]
    ];

Cached[ expr_, opts: OptionsPattern[ ] ] := expr;

$sessionCache = <| |>;

sessionCacheKey // Attributes = { HoldAllComplete };
sessionCacheKey[ eval_ ] :=
    With[ { h = Hash @ $cacheStateValues },
        HoldComplete[ eval, h ]
    ];


$localCaching := localCachingQ[ ];
localCachingQ[ ] := localCachingQ @ $CachePersistence;
localCachingQ[ "Local" ] := True;
localCachingQ[ Full    ] := True;
localCachingQ[ All     ] := True;
localCachingQ[ ___     ] := False;


$sessionCaching := sessionCachingQ[ ];
sessionCachingQ[ ] := sessionCachingQ @ $CachePersistence;
sessionCachingQ[ "Session"       ] := True;
sessionCachingQ[ "Kernel"        ] := True;
sessionCachingQ[ "KernelSession" ] := True;
sessionCachingQ[ Automatic       ] := True;
sessionCachingQ[ ___             ] := False;


(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*contentString*)
contentString[ expr_ ] :=
    Apply[
        ToFullString,
        Fold[
            Function[
                Replace[
                    #1,
                    e_ /; ByteCount @ Unevaluated @ e > 50000 :>
                        With[ { b = ByteCount @ Unevaluated @ e },
                            Wolfram`CodeEquivalenceUtilities`$Redacted @ b /;
                                True
                        ],
                    { #2 }
                ]
            ],
            expr,
            Reverse @ Range @ Min[ 5, Depth @ expr ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CachedQ*)
CachedQ // Attributes = { HoldAllComplete, SequenceHold };
CachedQ[ expr_ ] := AllTrue[ CachePath @ HoldComplete @ expr, FileExistsQ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CacheFileData*)
CacheFileData[ a_Association ][ key_ ] := a[ key ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CacheFileDataLookup*)
CacheFileDataLookup // Options = { "Depth" -> 3 };

CacheFileDataLookup[ Key[ keyHash_String ], opts: OptionsPattern[ ] ] :=
    Module[ { depth, partitionedHash, hashDirPath, hashFile },
        depth           = OptionValue[ "Depth" ];
        partitionedHash = StringPartition[ keyHash, 2 ];
        hashDirPath     = FileNameJoin @ Take[ partitionedHash, depth ];
        hashFile        = Drop[ partitionedHash, depth ];

        CacheFileData @ <|
            "Index"    -> hashDirPath,
            "Payload"  -> hashFile <> "_p.mx",
            "Metadata" -> hashFile <> "_m.mx"
        |>
    ];

CacheFileDataLookup[ key: Except[ Key[ _String ] ], opts: OptionsPattern[ ] ] :=
    CacheFileDataLookup[ KeyHash @ key, opts ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CachePath*)
CachePath // Options = { "PackageRoot" -> $packageRoot };

CachePath[ key_, opts: OptionsPattern[ ] ] :=
    Module[ { cacheData, cacheDirectory, payload, metadata },
        cacheData = CacheFileDataLookup @ key;

        cacheDirectory =
            FileNameJoin @ {
                $UserBaseDirectory,
                "ApplicationData",
                OptionValue[ "PackageRoot" ],
                "Index",
                cacheData[ "Index" ]
            };

        payload  = FileNameJoin @ { cacheDirectory, cacheData[ "Payload"  ] };
        metadata = FileNameJoin @ { cacheDirectory, cacheData[ "Metadata" ] };
        <| "Payload" -> payload, "Metadata" -> metadata |>
    ];

CachePath[ key_, lookup_, opts: OptionsPattern[ ] ] :=
    CachePath[ key, opts ][ lookup ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ClearEvalCache*)
ClearEvalCache // Attributes = { HoldAllComplete, SequenceHold };
ClearEvalCache // Options    = { "PackageRoot" -> $packageRoot };

ClearEvalCache[ OptionsPattern[ ] ] /; $localCaching :=
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

        $sessionCache = <| |>;
        ,
        DeleteDirectory::nodir
    ];

ClearEvalCache[ OptionsPattern[ ] ] /; $sessionCaching :=
    ($sessionCache = <| |>; Null);

ClearEvalCache[ expr_, opts: OptionsPattern[ ] ] :=
    Scan[ DeleteFile, Select[ CachePath @ HoldComplete @ expr, FileExistsQ ] ];

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
    $sessionCache = <| |>;
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
(*InvalidateCacheFile*)
InvalidateCacheFile[ metafile_String? FileExistsQ ] :=
    Module[ { meta },
        meta = Append[ Import @ metafile, "LastUsedDate" -> DateObject[ 0 ] ];
        Export[ metafile, meta ];
        meta
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*KeyHash*)
KeyHash[ expr_ ] := keyHash @ { expr, $cacheStateValues };

KeyHash[ expr_, exprs__ ] := KeyHash @ { expr, exprs };

keyHash[ expr_ ] :=
    Key @ StringPadLeft[ StringReverse[ Hash @ expr ~IntegerString~ 16 ],
                         16,
                         "0"
          ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$cacheStateValues*)
$cacheStateValues := {
    $AllowedEvaluationPatterns,
    $dispatchHash,
    $NormalizeNames,
    $PostProcessingFunction
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*UseCache*)
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
    "Expiration" :> Quantity[ 1, "Days" ]
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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*override*)
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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)