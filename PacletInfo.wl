PacletObject[ <|
    "Name"             -> "Wolfram/CodeEquivalenceUtilities",
    "Description"      -> "Utilities for testing code equivalence",
    "Creator"          -> "Richard Hennigan <richardh@wolfram.com>",
    "URL"              -> "https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/CodeEquivalenceUtilities",
    "SourceControlURL" -> "https://github.com/rhennigan/CodeEquivalenceUtilities",
    "License"          -> "MIT",
    "PublisherID"      -> "Wolfram",
    "Version"          -> "1.3.3",
    "WolframVersion"   -> "12.2+",
    "Keywords"         -> { "code transformations", "equivalent input" },
    "Extensions"       -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Symbols" -> {
                (* "Wolfram`CodeEquivalenceUtilities`CodeEquivalentQ", *)
                "Wolfram`CodeEquivalenceUtilities`EquivalenceTestData",
                "Wolfram`CodeEquivalenceUtilities`FromCanonicalForm",
                "Wolfram`CodeEquivalenceUtilities`MakeCanonicalForm",
                "Wolfram`CodeEquivalenceUtilities`ToCanonicalForm"
            },
            "Context" -> {
                { "Wolfram`CodeEquivalenceUtilities`"                          , "init.wl"                      },
                { "Wolfram`CodeEquivalenceUtilities`CachedValues`"             , "CachedValues.wl"              },
                { "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Attributes`", "CanonicalForms/Attributes.wl" },
                { "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`"    , "CanonicalForms/Common.wl"     },
                { "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Graphics`"  , "CanonicalForms/Graphics.wl"   },
                { "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Rules`"     , "CanonicalForms/Rules.wl"      },
                { "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`"     , "CanonicalForms/Scope.wl"      },
                { "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Structural`", "CanonicalForms/Structural.wl" },
                { "Wolfram`CodeEquivalenceUtilities`CodeEquivalenceUtilities`" , "CodeEquivalenceUtilities.wl"  },
                { "Wolfram`CodeEquivalenceUtilities`Debugging`"                , "Debugging.wl"                 },
                { "Wolfram`CodeEquivalenceUtilities`EvaluationControl`"        , "EvaluationControl.wl"         },
                { "Wolfram`CodeEquivalenceUtilities`Formatting`"               , "Formatting.wl"                },
                { "Wolfram`CodeEquivalenceUtilities`Types`"                    , "Types.wl"                     },
                { "Wolfram`CodeEquivalenceUtilities`Utilities`"                , "Utilities.wl"                 }
            }
        },
        {
            "Documentation",
            "Root" -> "Documentation"
        },
        {
            "Asset",
            "Assets" -> {
                { "License" , "./LICENSE"    },
                { "TestData", "./Tests/Data" }
            }
        }(*,
        {
            "Tests",
            "Method" -> "Experimental-v1"
        } *)
    }
|> ]
