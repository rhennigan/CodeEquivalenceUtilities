Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


BeginPackage[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Graphics`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`"
    }
];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)
$GraphicsTransformations        ::usage = "";
$NamedColorRules                ::usage = "";
CanonicalTransformFromGraphics  ::usage = "";



Begin["`Private`"];



(******************************************************************************)



$GraphicsTransformations =
  {
      replaceNamedColors,
      canonicalRGBColor,
      listLinePlotsToListPlots,
      listPlot2DSortedPoints,
      canonicalSort2DPoints,
      canonical2DPolyOrdering
  };



(******************************************************************************)



$NamedColorRules =
  Module[ { readableQ, readableNames, getVal, values, colorPatt },

      readableQ     = FreeQ[ Attributes @ #, ReadProtected ] &;
      readableNames = Select[ Names @ "System`*", readableQ ];
      getVal        = ToExpression[ #, StandardForm, OwnValues ] &;
      values        = getVal /@ readableNames;
      colorPatt     = HoldPattern[ Verbatim[ HoldPattern ][ _ ] :> _RGBColor ];

      Cases[ values, colorPatt, Infinity ]
  ];



(******************************************************************************)




replaceNamedColors // ClearAll;
replaceNamedColors // Attributes = { };
replaceNamedColors // Options    = { };


replaceNamedColors[ expression_ ] :=
  Inline[ $NamedColorRules, expression //. $NamedColorRules ];



(******************************************************************************)



canonicalRGBColor // ClearAll;
canonicalRGBColor // Attributes = { };
canonicalRGBColor // Options    = { };


canonicalRGBColor[ expression_ ] :=
  expression //. {
      RGBColor[ { colors__ } ] :> RGBColor[ colors ],
      RGBColor[ r_, g_, b_ ] :> RGBColor[ r, g, b, 1 ]
  };



(******************************************************************************)



listLinePlotsToListPlots // ClearAll;
listLinePlotsToListPlots // Attributes = { HoldFirst };
listLinePlotsToListPlots // Options    = { };


listLinePlotsToListPlots[ expression_ ] :=
  expression //.
    HoldPattern @ ListLinePlot[ args___ ] :>
      ListPlot[ args, Joined -> True ];


listLinePlotsToListPlots[ expression_ ] :=
  expression //.
    HoldPattern @ ListLinePlot[ { d__? UNumericQ }, a___ ] :>
      TrEval @ With[
          {
              s1 = MapIndexed[ { First @ #2, #1 } &, { d } ],
              s2 = Sort @ TempHold[ a, Joined -> True ]
          },
          TempHold[ ListPlot ][ s1, s2 ]
      ] //. TempHold[ ListPlot ][ data_, TempHold[ opts___ ] ] :>
              ListPlot[ data, opts ];



(******************************************************************************)



listPlot2DSortedPoints // ClearAll;
listPlot2DSortedPoints // Attributes = { HoldFirst };
listPlot2DSortedPoints // Options    = { };


listPlot2DSortedPoints[ expression_ ] :=
  expression //.
    HoldPattern @
      ListPlot[ l : { d : { _? UAtomQ, _? UAtomQ } .. }, a___ ] /;
        ! UOrderedQ @ l :>
          TrEval @ With[ { s1 = Sort @ TempHold @ d, s2 = Sort @ TempHold @ a },
              TempHold[ ListPlot ][ s1, s2 ]
          ] //. TempHold[ ListPlot ][ TempHold[ s1___ ], TempHold[ s2___ ] ] :>
                  ListPlot[ { s1 }, s2 ];



(******************************************************************************)



canonicalSort2DPoints // ClearAll;
canonicalSort2DPoints // Attributes = { };
canonicalSort2DPoints // Options    = { };

canonicalSort2DPoints[ expression_ ] :=
  expression //. Point[ { data__ }, args___ ] /; ! UOrderedQ @ { data } :>
    TrEval @ With[ { sortPts = Sort @ TempHold @ data },
                   Point[ sortPts, args ]
             ] //. TempHold -> List;



(******************************************************************************)



unordered2dPolyTempQ // ClearAll;
unordered2dPolyTempQ // Attributes = { HoldFirst };
unordered2dPolyTempQ // Options    = { };

unordered2dPolyTempQ[ Polygon[ { { _, _ } .. } ] ] := True;
unordered2dPolyTempQ[ ___ ] := False;



rotate2dPolypoints // ClearAll;
rotate2dPolypoints // Attributes = { HoldFirst };
rotate2dPolypoints // Options    = { };

rotate2dPolypoints[ Polygon[ { points : { _, _ } .. } ] ] :=
  Polygon @ RotateLeft[ TempHold @ points,
                        First @ Ordering @ HoldComplete @ points - 1
            ];



canonical2dPoly // ClearAll;
canonical2dPoly // Attributes = { HoldFirst };
canonical2dPoly // Options    = { };

canonical2dPoly[ Polygon[ { points : { _, _ } .. } ] ] :=

  Module[ { rotateForward, rotateReverse },

      With[ { forward = { points }, reverse = Reverse @ { points } },

          rotateForward = rotate2dPolypoints @ Polygon @ forward;
          rotateReverse = rotate2dPolypoints @ Polygon @ reverse;

          First @ MaximalBy[ { rotateForward, rotateReverse }, Hash ]
      ]
  ];



canonical2DPolyOrdering // ClearAll;
canonical2DPolyOrdering // Attributes = { };
canonical2DPolyOrdering // Options    = { };


canonical2DPolyOrdering[ expression_ ] :=
  expression //.
    p_? unordered2dPolyTempQ :>
      TrEval @ canonical2dPoly @ p //.
        TempHold -> List;



(******************************************************************************)



CanonicalTransformFromGraphics // Attributes = { };
CanonicalTransformFromGraphics // Options    = { };


CanonicalTransformFromGraphics[ exp_ ] :=
  exp // RightComposition @@ $GraphicsTransformations;



(******************************************************************************)



End[ ];

EndPackage[ ];
Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
