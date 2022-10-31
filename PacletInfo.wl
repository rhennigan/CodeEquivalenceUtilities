PacletObject[ <|
    "Name"             -> "Wolfram/CodeEquivalenceUtilities",
    "Description"      -> "Utilities for testing code equivalence",
    "Creator"          -> "Richard Hennigan <richardh@wolfram.com>",
    "URL"              -> "https://paclets.com/Wolfram/CodeEquivalenceUtilities",
    "SourceControlURL" -> "https://github.com/rhennigan/CodeEquivalenceUtilities",
    "DocumentationURL" -> "https://paclets.com",
    "License"          -> "MIT",
    "PublisherID"      -> "Wolfram",
    "Version"          -> "2.3.0",
    "WolframVersion"   -> "12.2+",
    "Keywords"         -> {
        "canonical forms",
        "code comparison",
        "code transformations",
        "education",
        "equality",
        "equivalence",
        "code grading",
        "intension",
        "metaprogramming",
        "type theory",
        "wolfram language"
    },
    "Extensions"       -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> "Wolfram`CodeEquivalenceUtilities`",
            "Symbols" -> {
                "Wolfram`CodeEquivalenceUtilities`$AllowedEvaluationPatterns",
                "Wolfram`CodeEquivalenceUtilities`CodeEquivalentQ",
                "Wolfram`CodeEquivalenceUtilities`EquivalenceTestData",
                "Wolfram`CodeEquivalenceUtilities`FromCanonicalForm",
                "Wolfram`CodeEquivalenceUtilities`MakeCanonicalForm",
                "Wolfram`CodeEquivalenceUtilities`ToCanonicalForm"
            }
        },
        {
            "Documentation",
            "Language" -> "English",
            "Root"     -> "Documentation"
        },
        {
            "Asset",
            "Assets"   -> {
                { "License"        , "./LICENSE"                  },
                { "SimplifySymbols", "./Data/SimplifySymbols.wxf" }
            }
        }
    }
|> ]
