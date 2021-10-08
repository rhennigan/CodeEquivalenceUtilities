(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet @ ResourceObject;
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest @ SameQ[
    Replace[
        ToCanonicalForm @ HoldComplete @ Reverse @ RandomColor[ 100 ],
        HoldComplete[ e_ ] :>
            EvaluateSafely[
                e,
                "SymbolList" -> Join[ $UnsafeSymbols, $RandomSymbols ]
            ]
    ],
    Replace[
        ToCanonicalForm @ HoldComplete @ Table[ RandomColor[ ], 100 ],
        HoldComplete[ e_ ] :>
            EvaluateSafely[
                e,
                "SymbolList" -> Join[ $UnsafeSymbols, $RandomSymbols ]
            ]
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ n + Range @ n, { n, 10 } ],
    ToCanonicalForm @ HoldComplete @ Table[ (n + 1) + Range[ n + 1 ], { n, 0, 9 } ]
];

VerificationTest @ SameQ[
    Apply[
        EvaluateSafely,
        ToCanonicalForm @ HoldComplete @ Table[ n + Range @ n, { n, 10 } ]
    ],
    Table[ n + Range @ n, { n, 10 } ],
    Apply[
        EvaluateSafely,
        ToCanonicalForm @ HoldComplete @ Table[ (n + 1) + Range[ n + 1 ], { n, 0, 9 } ]
    ],
    Table[ (n + 1) + Range[ n + 1 ], { n, 0, 9 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ Range[2^3 + 2]^(n + 1), { n, 3 } ],
    ToCanonicalForm @ HoldComplete @ Table[ n^p, { p, 2, 4 }, { n, 10 } ]
];

VerificationTest @ SameQ[
    Apply[
        EvaluateSafely,
        ToCanonicalForm @ HoldComplete @ Table[ Range[2^3 + 2]^(n + 1), { n, 3 } ]
    ],
    Table[ Range[2^3 + 2]^(n + 1), { n, 3 } ],
    Apply[
        EvaluateSafely,
        ToCanonicalForm @ HoldComplete @ Table[ n^p, { p, 2, 4 }, { n, 10 } ]
    ],
    Table[ n^p, { p, 2, 4 }, { n, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), { n, 3 } ],
    ToCanonicalForm @ HoldComplete @ Table[ n^p, { p, 2, 4 }, { n, 10 } ]
];

VerificationTest @ SameQ[
    Apply[
        EvaluateSafely,
        ToCanonicalForm @ HoldComplete @ Table[ {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), { n, 3 } ]
    ],
    Table[ {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}^(n + 1), { n, 3 } ],
    Apply[
        EvaluateSafely,
        ToCanonicalForm @ HoldComplete @ Table[ n^p, { p, 2, 4 }, { n, 10 } ]
    ],
    Table[ n^p, { p, 2, 4 }, { n, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Reverse[ RandomInteger /@ Reverse @ Range[ 5 ] ],
    ToCanonicalForm @ HoldComplete[ RandomInteger /@ Range[ 5 ] ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ (Reverse @ IntegerDigits[ 4294967296 ])[[ 2 ]] ],
    ToCanonicalForm @ HoldComplete[ (IntegerDigits[ 4294967296 ])[[ -2 ]] ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ Range[10]^3 - Range[10]^2 ],
    ToCanonicalForm @ HoldComplete @ Table[ i^3 - i^2, { i, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm @ Style[ "A", 100 ],
    ToCanonicalForm @ HoldForm @ Style[ "A", FontSize -> 100 ]
];

VerificationTest @ CodeEquivalentQ[ #1 + 2 &, Function[ n, n + 2 ] ];

VerificationTest @ CodeEquivalentQ[ 2 * (x + 1), 2 * x + 2 ];

VerificationTest @ CodeEquivalentQ[ 2 * (#1 + 1) &, 2 * #1 + 2 & ];

VerificationTest @ CodeEquivalentQ[
    Grid @ RandomColor @ { 10, 10 },
    Grid @ Array[ RandomColor[ ] &, { 10, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Range[ 4 ],
    ToCanonicalForm @ HoldComplete @ Reverse @ Reverse @ { 1, 2, 3, 4 }
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Reverse @ Reverse @ Range[ 4 ],
    ToCanonicalForm @ HoldComplete @ Reverse @ Reverse @ { 1, 2, 3, 4 }
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ ListPlot @ Range[ 10, 14 ],
    ToCanonicalForm @ HoldComplete @ ListPlot @ { 10, 11, 12, 13, 14 }
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Column @ { 1, 2, 3, 4, 5 },
    ToCanonicalForm @ HoldComplete @ Column @ Range[ 5 ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ NumberLinePlot[ Range[5]^2 ],
    ToCanonicalForm @ HoldComplete @ NumberLinePlot @ { 1, 4, 9, 16, 25 }
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ Range[ 11 ] + 9 ],
    ToCanonicalForm @ HoldComplete[ 9 + Range[ 11 ] ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ Range[ 0, 10 ] + 10 ],
    ToCanonicalForm @ HoldComplete[ 9 + Range[ 11 ] ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ Range[ 10 ] * 3 ],
    ToCanonicalForm @ HoldComplete[ 3 * Range[ 10 ] ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ n, { n, 2, 20, 2 } ],
    ToCanonicalForm @ HoldComplete @ Range[ 2, 20, 2 ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ BarChart[ Range[10]^2 ],
    ToCanonicalForm @ HoldComplete @ BarChart @ Table[ n^2, { n, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ IntegerDigits[ Range[10]^2 ],
    ToCanonicalForm @ HoldComplete @ Table[ IntegerDigits[ n^2 ], { n, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ Range[10]^3 - Range[10]^2 ],
    ToCanonicalForm @ HoldComplete @ Table[ n^3 - n^2, { n, 10 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ n, { n, 1, 100, 2 } ],
    ToCanonicalForm @ HoldComplete @ Range[ 1, 100, 2 ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ n^2, { n, 2, 100, 2 } ],
    ToCanonicalForm @ HoldComplete[ Range[2, 100, 2]^2 ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ NumberLinePlot[ 1 * Range[20]^(-1) ],
    ToCanonicalForm @ HoldComplete @ NumberLinePlot @ Table[ 1 * n^(-1), { n, 20 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ ListLinePlot @ Table[ RandomInteger @ { 0, n }, { n, 100 } ],
    ToCanonicalForm @ HoldComplete @ ListLinePlot @ Table[ RandomInteger @ n, { n, 100 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[ ({ Red, Yellow, Green })[[ RandomInteger[ 2 ] + 1 ]], 100 ],
    ToCanonicalForm @ HoldComplete @ Table[ ({ Red, Green, Yellow })[[ RandomInteger[ 2 ] + 1 ]], 100 ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Reverse @ Array[ Range, 5, 2 ],
    HoldComplete @ Table[
        Table[
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ],
            {
                TypedSymbol[
                    CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                    _Integer
                ],
                1,
                Plus[
                    7,
                    Times[
                        -1,
                        TypedSymbol[
                            CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S2,
                            _Integer
                        ]
                    ]
                ],
                1
            }
        ],
        {
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S2,
                _Integer
            ],
            1,
            5,
            1
        }
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete[ Range[ 11 ] + 9 ],
    HoldComplete @ Table[
        Plus[
            9,
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ]
        ],
        {
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ],
            1,
            11,
            1
        }
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm @ Reverse @ Table[ Floor[ 3 * i^2 + (-1) * 2 * i ], { i, 5 } ],
    HoldForm @ Table[
        Plus[
            96,
            Times[
                -34,
                TypedSymbol[
                    CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                    _Integer
                ]
            ],
            Times[
                3,
                Power[
                    TypedSymbol[
                        CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                        _Integer
                    ],
                    2
                ]
            ]
        ],
        {
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ],
            1,
            5,
            1
        }
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm @ Reverse @ Table[ 3 * i^2 + (-1) * 4 * i + 3, { i, -4, 13, 1 } ],
    ToCanonicalForm @ HoldForm @ { 458, 387, 322, 263, 210, 163, 122, 87, 58, 35, 18, 7, 2, 3, 10, 23, 42, 67 }
];

VerificationTest[
    ReleaseHold @ FromCanonicalForm @ ToCanonicalForm @ HoldForm @ Table[ i, { i, 1, -3, -1 } ],
    Table[ i, { i, 1, -3, -1 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm @ Reverse @ Table[ i^2, { i, 3, 9, 2 } ],
    ToCanonicalForm @ HoldForm @ Table[ i^2, { i, 9, 3, -2 } ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm @ Array[ Composition[ Range, RandomInteger ], 10, 3 ],
    HoldForm @ Table[
        Table[
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ],
            {
                TypedSymbol[
                    CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                    _Integer
                ],
                1,
                RandomValue @ DiscreteUniformDistribution @ {
                    0,
                    Plus[
                        2,
                        TypedSymbol[
                            CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S2,
                            _Integer
                        ]
                    ]
                },
                1
            }
        ],
        {
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S2,
                _Integer
            ],
            1,
            10,
            1
        }
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm[ (#1 - 1 &)[ Range[ 5 ] ] ],
    HoldForm @ Table[
        Plus[
            -1,
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ]
        ],
        {
            TypedSymbol[
                CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                _Integer
            ],
            1,
            5,
            1
        }
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldForm[ (#1 - RandomInteger[ #1 ] &)[ Range[ 3, 7, 3 ] ] ],
    HoldForm[
        (-RandomInteger[ #1 ] + #1 &)[
            Table[
                Times[
                    3,
                    TypedSymbol[
                        CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                        _Integer
                    ]
                ],
                {
                    TypedSymbol[
                        CodeEquivalence`CanonicalForms`Scope`LocalSymbols`S1,
                        _Integer
                    ],
                    1,
                    2,
                    1
                }
            ]
        ]
    ]
];

VerificationTest[
    CodeEquivalentQ[
        Take[
            Sort[
                ({ GenomeData[ #1, "SequenceLength" ], #1 } &) /@ GenomeData[ ]
            ],
            10
        ],
        Sort @ TakeSmallestBy[
            ({ GenomeData[ #1, "SequenceLength" ], #1 } &) /@ GenomeData[ ],
            First,
            10
        ]
    ],
    True
];

VerificationTest[
    CodeEquivalentQ[
        Graphics3D @ { Orange, Sphere[ RandomReal[ 1, { 100, 3 } ], 0.05 ] },
        Graphics3D @ { Orange, Table[ Sphere[ RandomReal[ 1, { 3 } ], 0.05 ], { 100 } ] }
    ],
    True
];

VerificationTest @ CodeEquivalentQ[
    Map[
        Framed[ Style[ #1, RandomColor[ ] ], Background -> RandomColor[ ] ] &,
        Alphabet[ ]
    ],
    Map[
        Framed[ #1, Background -> RandomColor[ ] ] &,
        (Style[ #1, RandomColor[ ] ] &) /@ Alphabet[ ]
    ]
];

VerificationTest @ CodeEquivalentQ[
    NestList[ #1 + First @ RandomReal[ { -1, 1 }, { 1, 3 } ] &, { 0, 0, 0 }, 1000 ],
    NestList[ #1 + RandomReal[ { -1, 1 }, 3 ] &, { 0, 0, 0 }, 1000 ]
];

VerificationTest @ CodeEquivalentQ[
    TakeLargestBy[ WordList[ ], StringLength, 10 ],
    Take[ SortBy[ WordList[ ], StringLength[ #1 ] & ], -10 ]
];

VerificationTest[
    MakeCanonicalForm @ Dilation[ Dilation[ img, { { 1, 1, 1 } } ], { { 1 }, { 1 }, { 1 } } ],
    MakeCanonicalForm @ Dilation[
        Dilation[ img, { { 0, 0, 0 }, { 1, 1, 1 }, { 0, 0, 0 } } ],
        { { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } }
    ]
];

VerificationTest[
    MakeCanonicalForm @ Dilation[ Dilation[ img, { { 1, 1, 1 } } ], { { 1 }, { 1 }, { 1 } } ],
    MakeCanonicalForm @ Dilation[ img, 1 ]
];

VerificationTest[
    MakeCanonicalForm @ Erosion[ Erosion[ img, { { 1, 1, 1 } } ], { { 1 }, { 1 }, { 1 } } ],
    MakeCanonicalForm @ Erosion[
        Erosion[ img, { { 0, 0, 0 }, { 1, 1, 1 }, { 0, 0, 0 } } ],
        { { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } }
    ]
];

VerificationTest[
    MakeCanonicalForm @ Erosion[ Erosion[ img, { { 1, 1, 1 } } ], { { 1 }, { 1 }, { 1 } } ],
    MakeCanonicalForm @ Erosion[ img, 1 ]
];

VerificationTest[
    MakeCanonicalForm @ StringTake[ "test string", 4 ],
    MakeCanonicalForm @ StringJoin @ Take[ Characters[ "test string" ], 4 ]
]

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[
        StringJoin @ Table[ FromLetterNumber[ RandomInteger[ 25 ] + 1 ], 5 ],
        100
    ],
    ToCanonicalForm @ HoldComplete @ Table[
        StringJoin @ FromLetterNumber @ Table[ RandomInteger[ 25 ] + 1, 5 ],
        100
    ]
];

VerificationTest[
    ToCanonicalForm @ HoldComplete @ Table[
        Column @ { Style[ n, Plain ], Style[ n, Bold ], Style[ n, Italic ] },
        { n, 10 }
    ],
    ToCanonicalForm @ HoldComplete @ Table[
        Column @ { n, Style[ n, Bold ], Style[ n, Italic ] },
        { n, 10 }
    ]
];
