(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

$GraphicsTransformations;
$NamedColorRules;
CanonicalTransformFromGraphics;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$GraphicsTransformations*)
$GraphicsTransformations = {
    replaceNamedColors,
    canonicalRGBColor,
    listLinePlotsToListPlots,
    listPlot2DSortedPoints,
    canonicalSort2DPoints,
    canonical2DPolyOrdering
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*replaceNamedColors*)
replaceNamedColors[ expression_ ] := expression //. $NamedColorRules;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*canonicalRGBColor*)
canonicalRGBColor[ expression_ ] :=
    ReplaceRepeated[
        expression,
        {
            RGBColor @ { colors__ } :> RGBColor @ colors,
            RGBColor[ r_, g_, b_ ]  :> RGBColor[ r, g, b, 1 ]
        }
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*listLinePlotsToListPlots*)
listLinePlotsToListPlots // Attributes = { HoldFirst };

listLinePlotsToListPlots[ expression_ ] :=
  expression //.
    HoldPattern @ ListLinePlot[ args___ ] :>
      ListPlot[ args, Joined -> True ];


listLinePlotsToListPlots[ expression_ ] :=
  expression //.
    HoldPattern @ ListLinePlot[ { d__? UNumericQ }, a___ ] :>
      RuleCondition @ With[
          {
              s1 = MapIndexed[ { First @ #2, #1 } &, { d } ],
              s2 = Sort @ TempHold[ a, Joined -> True ]
          },
          TempHold[ ListPlot ][ s1, s2 ]
      ] //. TempHold[ ListPlot ][ data_, TempHold[ opts___ ] ] :>
              ListPlot[ data, opts ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*listPlot2DSortedPoints*)
listPlot2DSortedPoints // Attributes = { HoldFirst };

listPlot2DSortedPoints[ expression_ ] :=
  expression //.
    HoldPattern @
      ListPlot[ l : { d : { _? UAtomQ, _? UAtomQ } .. }, a___ ] /;
        ! UOrderedQ @ l :>
          RuleCondition @ With[ { s1 = Sort @ TempHold @ d, s2 = Sort @ TempHold @ a },
              TempHold[ ListPlot ][ s1, s2 ]
          ] //. TempHold[ ListPlot ][ TempHold[ s1___ ], TempHold[ s2___ ] ] :>
                  ListPlot[ { s1 }, s2 ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*canonicalSort2DPoints*)
canonicalSort2DPoints[ expression_ ] :=
  expression //. Point[ { data__ }, args___ ] /; ! UOrderedQ @ { data } :>
    RuleCondition @ With[ { sortPts = Sort @ TempHold @ data },
                   Point[ sortPts, args ]
             ] //. TempHold -> List;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*canonical2DPolyOrdering*)
canonical2DPolyOrdering[ expression_ ] :=
  expression //.
    p_? unordered2dPolyTempQ :>
      RuleCondition @ canonical2dPoly @ p //.
        TempHold -> List;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*unordered2dPolyTempQ*)
unordered2dPolyTempQ // Attributes = { HoldFirst };
unordered2dPolyTempQ[ Polygon[ { { _, _ } .. } ] ] := True;
unordered2dPolyTempQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*canonical2dPoly*)
canonical2dPoly // Attributes = { HoldFirst };
canonical2dPoly[ Polygon @ { points: { _, _ }.. } ] :=
    Module[ { rotateForward, rotateReverse },
        With[ { forward = { points }, reverse = Reverse @ { points } },
            rotateForward = rotate2dPolypoints @ Polygon @ forward;
            rotateReverse = rotate2dPolypoints @ Polygon @ reverse;
            First @ MaximalBy[ { rotateForward, rotateReverse }, Hash ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rotate2dPolypoints*)
rotate2dPolypoints // Attributes = { HoldFirst };
rotate2dPolypoints[ Polygon[ { points : { _, _ } .. } ] ] :=
    Polygon @ RotateLeft[ TempHold @ points,
                          First @ Ordering @ HoldComplete @ points - 1
              ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$NamedColorRules*)
$NamedColorRules =
    Module[ { readableQ, names, getVal, values, colorPatt },

        readableQ = FreeQ[ Attributes @ #, ReadProtected ] &;
        names     = Select[ Names @ "System`*", readableQ ];
        getVal    = ToExpression[ #, StandardForm, OwnValues ] &;
        values    = getVal /@ names;
        colorPatt = HoldPattern[ Verbatim[ HoldPattern ][ _ ] :> _RGBColor ];

        Dispatch @ Cases[ values, colorPatt, Infinity ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformFromGraphics*)
CanonicalTransformFromGraphics[ exp_ ] :=
    exp // RightComposition @@ $GraphicsTransformations;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)
