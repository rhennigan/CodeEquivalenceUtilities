PacletObject[ <|
    "Name"             -> "Wolfram/CodeEquivalenceUtilities",
    "Description"      -> "Utilities for testing code equivalence",
    "Creator"          -> "Richard Hennigan <richardh@wolfram.com>",
    "URL"              -> "https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/CodeEquivalenceUtilities",
    "SourceControlURL" -> "https://github.com/rhennigan/CodeEquivalenceUtilities",
    "License"          -> "MIT",
    "PublisherID"      -> "Wolfram",
    "Version"          -> "2.0.2",
    "WolframVersion"   -> "12.2+",
    "DocumentationURL" -> "https://resources.wolframcloud.com/PacletRepository/resources",
    "Keywords"         -> { "code transformations", "equivalent input" },
    "Extensions"       -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> { "Wolfram`CodeEquivalenceUtilities`" },
            "Symbols" -> {
                "Wolfram`CodeEquivalenceUtilities`CodeEquivalentQ",
                "Wolfram`CodeEquivalenceUtilities`EquivalenceTestData",
                "Wolfram`CodeEquivalenceUtilities`FromCanonicalForm",
                "Wolfram`CodeEquivalenceUtilities`MakeCanonicalForm",
                "Wolfram`CodeEquivalenceUtilities`ToCanonicalForm"
            }
        },
        {
            "Documentation",
            "Root"     -> "Documentation"
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
