(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Default Values*)
SetRuleDefaults[
    "Description" -> "Expand simple definitions",
    "Usage"       -> { "EquivalenceTesting", "Optimization" }
];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Resolve System symbols from their name",
    "Symbols"     :> { Symbol },
    "Rule"        :>
        Symbol[ name_String? UStringQ ] /; NameQ @ name && Context @ name === "System`" :>
            RuleCondition @ ToExpression[ name, InputForm, TempHold ]
|>

(**********************************************************************************************************************)
HoldComplete[
    Dashed                 :> Dashing @ { Small, Small },
    DotDashed              :> Dashing @ { 0, Small, Small, Small },
    Dotted                 :> Dashing @ { 0, Small },
    Thick                  :> Thickness @ Large,
    Thin                   :> Thickness @ Tiny,
    Yesterday              :> DateObject[ Take[ DateList[ ], 3 ] - { 0, 0, 1 } ],
    Today                  :> DateObject @ Take[ DateList[ ], 3 ],
    Tomorrow               :> DateObject[ Take[ DateList[ ], 3 ] + { 0, 0, 1 } ],
    Now                    :> DateObject @ DateList[ ],
    InfiniteFuture         :> DateObject[ { Infinity }, "Eternity", "Gregorian", None ],
    InfinitePast           :> DateObject[ { -Infinity }, "Eternity", "Gregorian", None ],
    Infinity               :> DirectedInfinity[ 1 ],
    Here                   :> $GeoLocation,
    Black                  :> GrayLevel[ 0 ],
    Gray                   :> GrayLevel[ 0.5 ],
    LightGray              :> GrayLevel[ 0.85 ],
    White                  :> GrayLevel[ 1 ],
    Transparent            :> GrayLevel[ 0, 0 ],
    Blue                   :> RGBColor[ 0, 0, 1 ],
    Green                  :> RGBColor[ 0, 1, 0 ],
    Cyan                   :> RGBColor[ 0, 1, 1 ],
    Purple                 :> RGBColor[ 0.5, 0, 0.5 ],
    LightGreen             :> RGBColor[ 0.88, 1, 0.88 ],
    Red                    :> RGBColor[ 1, 0, 0 ],
    Magenta                :> RGBColor[ 1, 0, 1 ],
    Orange                 :> RGBColor[ 1, 0.5, 0 ],
    Pink                   :> RGBColor[ 1, 0.5, 0.5 ],
    Yellow                 :> RGBColor[ 1, 1, 0 ],
    LightYellow            :> RGBColor[ 1, 1, 0.85 ],
    $CloudCreditsAvailable :> If[ TrueQ @ $CloudConnected, CloudAccountData[ "CloudCreditsAvailable" ], Indeterminate ],
    $Linked                :> $ParentLink =!= Null,
    $PerformanceGoal       :> If[ $ControlActiveSetting, "Speed", "Quality" ]
]

HoldComplete[
    Circle[ { 0, 0 } ] :> Circle[ { 0, 0 }, 1 ],
    Circle[ ]          :> Circle @ { 0, 0 },
    Cone[ ]            :> Cone @ { { 0, 0, -1 }, { 0, 0, 1 } },
    Cylinder[ ]        :> Cylinder @ { { 0, 0, -1 }, { 0, 0, 1 } },
    Disk[ ]            :> Disk @ { 0, 0 },
    Graphics[ ]        :> Graphics @ { },
    Graphics3D[ ]      :> Graphics3D @ { },
    Rectangle[ ]       :> Rectangle @ { 0, 0 },
    Sphere[ ]          :> Sphere @ { 0, 0, 0 },
    DateObject[ ]      :> DateObject @ DateList[ ]
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)