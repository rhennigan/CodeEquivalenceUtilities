Quiet @ Scan[
    (Unprotect @ #; ClearAll @ #) &,
    Names[ "Wolfram`CodeEquivalenceUtilities`" ~~ ___ ]
];


Wolfram`CodeEquivalenceUtilities`Debugging`$Debug = False;


Wolfram`CodeEquivalenceUtilities`Debugging`$LoadOrder // ClearAll;
Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad // ClearAll;

If[ TrueQ @ Wolfram`CodeEquivalenceUtilities`Debugging`$Debug,
    Wolfram`CodeEquivalenceUtilities`Debugging`$LoadOrder = Internal`Bag[ ];
    Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad :=
      Internal`StuffBag[ Wolfram`CodeEquivalenceUtilities`Debugging`$LoadOrder,
          { SessionTime[ ], $InputFileName }
      ]
];


<< "Wolfram`CodeEquivalenceUtilities`Utilities`";
<< "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Attributes`";
<< "Wolfram`CodeEquivalenceUtilities`Types`";
<< "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Scope`";
<< "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Common`";
<< "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Graphics`";
<< "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Structural`";
<< "Wolfram`CodeEquivalenceUtilities`EvaluationControl`";
<< "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Rules`";
<< "Wolfram`CodeEquivalenceUtilities`Formatting`";
<< "Wolfram`CodeEquivalenceUtilities`CachedValues`";
<< "Wolfram`CodeEquivalenceUtilities`CodeEquivalenceUtilities`";



Wolfram`CodeEquivalenceUtilities`Package`Version // ClearAll;
Wolfram`CodeEquivalenceUtilities`Package`Version // Attributes = { HoldFirst };


ToExpression[ Select[ Join[ Names[ "Wolfram`CodeEquivalenceUtilities`*" ],
                            Names[ "Wolfram`CodeEquivalenceUtilities`*`*" ]
                      ],
                      DefinedQ
              ],
              StandardForm,
              Function[ symbol,
                        If[ MemberQ[ Attributes @ symbol, Protected ]
                            ,
                            Unprotect @ symbol;
                            TagSet[ symbol,
                                    Wolfram`CodeEquivalenceUtilities`Package`Version @ symbol,
                                    Wolfram`CodeEquivalenceUtilities`Package`$Version
                            ];
                            Protect @ symbol;
                            ,
                            TagSet[ symbol,
                                    Wolfram`CodeEquivalenceUtilities`Package`Version @ symbol,
                                    Wolfram`CodeEquivalenceUtilities`Package`$Version
                            ]
                        ],
                        { HoldAllComplete }
              ]
];
