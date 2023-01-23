(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

TypedSymbol;
$LocalContext;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Style data*)
$LiteralTypeStyles =
  <|
      "Text" ->
        <|
            "Value" -> Sequence[
                FontColor -> RGBColor[ {  71,  88,  25 } / 255 ]
            ],

            "Separator" -> Sequence[
                FontColor -> RGBColor[ { 143, 176,  50 } / 255 ]
            ],

            "Type" -> Sequence[
                FontColor -> RGBColor[ { 107, 132,  37 } / 255 ]
            ]
        |>,

      "FrameBoxOptions" -> Sequence[
          ContentPadding -> False,
          FrameStyle -> None,
          RoundingRadius -> 5,
          Background -> Lighter[ColorData[97][3], .75]
      ]
  |>;

$SymbolTypeStyles =
  <|
      "Text" ->
        <|
            "Value" -> Sequence[
                FontColor -> RGBColor[ {  47,  65,  90 } / 255 ]
            ],

            "Separator" -> Sequence[
                FontColor -> RGBColor[ {  94, 129, 181 } / 255 ]
            ],

            "Type" -> Sequence[
                FontColor -> RGBColor[ {  70,  97, 136 } / 255 ]
            ]
        |>,

      "FrameBoxOptions" -> Sequence[
          ContentPadding -> False,
          FrameStyle -> None,
          RoundingRadius -> 5,
          Background -> Lighter[ColorData[97][1], .75]
      ]
  |>;

$ExprTypeStyles =
  <|
      "Text" ->
        <|
            "Value" -> Sequence[
                FontColor -> RGBColor[ { 112,  78,  18 } / 255 ]
            ],

            "Separator" -> Sequence[
                FontColor -> RGBColor[ { 225, 156,  36 } / 255 ]
            ],

            "Type" -> Sequence[
                FontColor -> RGBColor[ { 168, 117,  27 } / 255 ]
            ]
        |>,

      "FrameBoxOptions" -> Sequence[
          ContentPadding -> False,
          FrameStyle -> None,
          RoundingRadius -> 5,
          Background -> Lighter[ColorData[97][2], .75]
      ]
  |>;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*MakeAtomicTypeBox*)
MakeAtomicTypeBox // Attributes = { HoldAll };

MakeAtomicTypeBox[ value_, typeName_, tooltip_, styles_ ] :=

  Module[

      {
          separator = "\[ThinSpace]\[Proportion]\[ThinSpace]",
          valString, typString, strLength,
          valBox, sepBox, typBox,
          txtBox, txtCel, frmBox, ttpStr, ttpBox
      },

      (*valString = If[ StringQ @ value,
                      value,
                      ToString[ Unevaluated @ value, InputForm ]
                  ];*)

      valString = Which[
            StringQ @ value,
            value
            ,
            LocalContextQ @ value,
            MakeBoxes @ value,

            True,
            StringTake[ ToString[
                HoldComplete @ value /. s_Symbol? LocalContextQ :>
                  RuleCondition @ Symbol[ "Global`" <> SymbolName @ Unevaluated @ s ],
                InputForm
            ], { 14, -2 } ]
      ];

      typString = ToString[ typeName, InputForm ];
      strLength = StringLength @ typString;
      typString = typString ~StringTake~ Min[ 3, strLength ];

      valBox = valString ~StyleBox~ styles[ "Text", "Value"     ];
      sepBox = separator ~StyleBox~ styles[ "Text", "Separator" ];
      typBox = typString ~StyleBox~ styles[ "Text", "Type"      ];

      txtBox = TextData @ { valBox, sepBox, typBox };
      txtCel = Cell[ txtBox, "Text" ];
      frmBox = FrameBox[ txtCel, styles @ "FrameBoxOptions" ];
      ttpStr = ToBoxes @ ToString[ Unevaluated @ tooltip, InputForm ];
      ttpBox = TooltipBox[ frmBox, ttpStr ];

      TagBox[ ttpBox, #1 & ]

];

MakeAtomicTypeBox[ value_, typeName_, tooltip_, styles_ ] :=

  Module[

      {
          separator = "\[Proportion]",
          valString, typString,
          valBox, sepBox, typBox,
          txtBox, frmBox, ttpStr, ttpBox
      },

      (*valString = If[ StringQ @ value,
                      value,
                      ToString[ Unevaluated @ value, InputForm ]
                  ];*)

      valString = Which[
            StringQ @ value,
            value
            ,
            LocalContextQ @ value,
            MakeBoxes @ value
            ,
            True,
            StringTake[ ToString[
                HoldComplete @ value /. s_Symbol? LocalContextQ :>
                  RuleCondition @ Symbol[ "Global`" <> SymbolName @ Unevaluated @ s ],
                InputForm
            ], { 14, -2 } ]
      ];

      typString = makeTypeString @ typeName;

      valBox = valString ~StyleBox~ styles[ "Text", "Value"     ];
      sepBox = separator ~StyleBox~ styles[ "Text", "Separator" ];
      typBox = typString ~StyleBox~ styles[ "Text", "Type"      ];

      txtBox = RowBox @ {
          AdjustmentBox[ valBox, BoxBaselineShift -> -0.25 ],
          sepBox,
          typBox
      };

      frmBox = FrameBox[ txtBox, styles @ "FrameBoxOptions" ];
      ttpStr = ToBoxes @ ToString[ Unevaluated @ tooltip, InputForm ];
      ttpBox = TooltipBox[ StyleBox[ frmBox, FontSize -> Inherited*0.9 ], ttpStr ];

      TagBox[ ttpBox, #1 & ]

];

makeTypeString[ Integer  ] := "\[DoubleStruckCapitalZ]";
makeTypeString[ Real     ] := "\[DoubleStruckCapitalR]";
makeTypeString[ Rational ] := "\[DoubleStruckCapitalQ]";
makeTypeString[ Complex  ] := "\[DoubleStruckCapitalC]";
makeTypeString[ String   ] := "\[DoubleStruckCapitalS]";
makeTypeString[ str_     ] :=
    ToLowerCase @ StringTake[ ToString @ str, UpTo[ 3 ] ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedLiteral*)
TypedLiteral /:
  MakeBoxes[ t : TypedLiteral[ value_, Verbatim[ Blank ][ type_ ] ],
      StandardForm
  ] :=
  With[ { tagBox = MakeAtomicTypeBox[ value, type, t, $LiteralTypeStyles ] },
      InterpretationBox[ tagBox, t ]
  ];


TypedLiteral /:
  MakeBoxes[ t : TypedLiteral[ value_, type_ ], StandardForm ] :=
    With[ { tagBox = MakeAtomicTypeBox[ value, type, t, $LiteralTypeStyles ] },
        InterpretationBox[ tagBox, t ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedSymbol*)
TypedSymbol /: MakeBoxes[
    t: TypedSymbol[ value_, Verbatim[ Blank ][ type_ ] ],
    StandardForm
] :=
    With[ { tagBox = MakeAtomicTypeBox[ value, type, t, $SymbolTypeStyles ] },
        InterpretationBox[ tagBox, t ]
    ];


TypedSymbol /: MakeBoxes[
    TypedSymbol[ value_, type_Verbatim ],
    StandardForm
] :=
    With[ { t = First @ type },
        MakeBoxes[ TypedSymbol[ value, t ], StandardForm ]
    ];


TypedSymbol /: MakeBoxes[
    t: TypedSymbol[ value_, type_ ],
    StandardForm
] :=
    With[ { tagBox = MakeAtomicTypeBox[ value, type, t, $SymbolTypeStyles ] },
        InterpretationBox[ tagBox, t ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedExpression*)
TypedExpression /:
  MakeBoxes[ t : TypedExpression[ value_, type_ ], StandardForm ] :=
    With[ { tagBox = MakeAtomicTypeBox[ value, type, t, $ExprTypeStyles ] },
        InterpretationBox[ tagBox, t ]
    ];

TypedExpression /: MakeBoxes[ t : TypedExpression[ exp_ ], StandardForm ] :=
  With[ { boxes = typedExpFrameBox @ t }, InterpretationBox[ boxes, t ] ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*typedExpFrameBox*)
typedExpFrameBox // Attributes = { HoldAll };
typedExpFrameBox[ t : TypedExpression[ exp___ ] ] :=
  With[ { boxes = MakeBoxes[ exp, StandardForm ] },
      FrameBox[ StyleBox[ boxes, GrayLevel @ 0, StripOnInput -> False ],
          Background -> GrayLevel @ 0.975,
          FrameMargins -> 2,
          RoundingRadius -> 3,
          ContentPadding -> False,
          FrameStyle -> Directive[ Thickness @ 0.001, GrayLevel @ 0.85 ],
          StripOnInput -> False
      ]
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Local symbols*)
$localSymbolHashMod = 64;
$localSymbolColorFunction = ColorData @ 97;


(* MakeBoxes[ s_? SymbolQ /; Context @ Unevaluated @ s === $LocalContext,
           StandardForm
] :=

  Module[ { symHash, color, simpleName },

      symHash = Mod[ Hash @ Unevaluated @ s, $localSymbolHashMod ];
      color = $localSymbolColorFunction @ symHash;

      (*simpleName = StringDelete[ SymbolName @ Unevaluated @ s,
                                 "$" | DigitCharacter
                   ];*)

      simpleName = SymbolName @ Unevaluated @ s;

      With[{n = simpleName},
          With[{styled = StyleBox[n, $SymbolTypeStyles["Text", "Value"]]},
              InterpretationBox[styled, s]]]
  ]; *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CodeTransformerObject*)
MakeBoxes[ifun : CodeTransformerObject[obj_Association], fmt_] ^:=
  BoxForm`ArrangeSummaryBox[
      CodeTransformerObject, ifun, obj["GraphIcon"],
      {Row@{Style["Transformations: ", "SummaryItemAnnotation"],
          Style[obj["Count"], "SummaryItem"]},
          Row@{Style["Equivalence classes: ", "SummaryItemAnnotation"],
              Style[obj["EquivalenceClasses"], "SummaryItem"]}},
      {Grid[{{"", ""},
          {"", ""},
          {Style["Graph: ", "SummaryItemAnnotation"],
              Style[Show@obj["Graph"], "SummaryItem"]}
      }, Alignment -> {Left, Top}]},
      fmt
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Unsafe*)
Unsafe /:
MakeBoxes[Unsafe[str_String][args___],StandardForm]:=
  With[{ebox=Replace[ToExpression[str,StandardForm,HoldComplete],HoldComplete[exp_]:>MakeBoxes[exp,StandardForm]]},
      Replace[MakeBoxes[HoldComplete[args]],RowBox[{"HoldComplete",app___}]:>
        RowBox[{InterpretationBox[TagBox[TooltipBox[StyleBox[ebox,RGBColor[1.,0.2,0.2],Bold],"Unsafe"],#1&] ,Unsafe[str]],app}]]
  ];


Unsafe /:
MakeBoxes[Unsafe[str_String],StandardForm]:=
  With[{ebox=Replace[ToExpression[str,StandardForm,HoldComplete],HoldComplete[exp_]:>MakeBoxes[exp,StandardForm]]},
      InterpretationBox[TagBox[TooltipBox[StyleBox[ebox,RGBColor[1.,0.2,0.2],Bold,ShowStringCharacters -> False],"Unsafe"],#1&] ,Unsafe[str]]
  ];

Unsafe /:
MakeBoxes[Unsafe[str_String, Null],StandardForm]:=MakeBoxes[Unsafe[str]];

Unsafe /:
MakeBoxes[Unsafe[expr:Except[_String]],StandardForm]:=
  With[{s=ToFullString[expr]},MakeBoxes[Unsafe[s],StandardForm]];


Unsafe /:
MakeBoxes[HoldPattern[u : Unsafe[s_String, _[args___]]], fmt_] :=
  Module[{argBoxes, headBox},
      argBoxes =
        Replace[MakeBoxes[{args}, fmt], RowBox[{"{", a___, "}"}] :> a];
      headBox =
        ToBoxes[Tooltip[Style[Last[StringSplit[s, "`"]], Red, Bold, ShowStringCharacters -> False],
            "Unsafe: " <> s], fmt];
      With[{h = headBox, a = argBoxes},
          InterpretationBox[#, u] & [RowBox[{h, "[", a, "]"}]]
      ]
  ];

Unsafe /:
MakeBoxes[
    HoldPattern[u : Unsafe[s_String, _[args___], "Bindings" -> bind_]],
    fmt_] :=
  Module[{argBoxes, headBox},
      argBoxes =
        Replace[MakeBoxes[{args}, fmt], RowBox[{"{", a___, "}"}] :> a];
      headBox =
        ToBoxes[Tooltip[Style[Last[StringSplit[s, "`"]], Red, Bold, ShowStringCharacters -> False],
            formatClosure @ bind
        ], fmt];
      With[{h = headBox, a = argBoxes},
          InterpretationBox[RowBox[{h, "[", a, "]"}], u]]
  ];

formatClosure[Closure[a_Association]] :=
  Grid[
      KeyValueMap[Function[{key, val},
          {key,
              Grid[KeyValueMap[Prepend[#2, Style[#1, Bold]] &, val],
                  Alignment -> {Left, Top}] /. HoldPattern -> HoldForm}
      ],
          a
      ],
      Alignment -> {Left, Top},
      Dividers -> All
  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CompressedExpression*)
CompressedExpression /:
  MakeBoxes[ CompressedExpression[ compressed_ ], fmt_ ] :=

  Module[
      {
          head, size1, size2, ratio, percent, length, depth,
          version, shown, hidden
      },

      head      = compressed[ "Head" ];
      size1     = Quantity[ compressed[ "Size" ][ "Original"   ], "Bytes" ];
      size2     = Quantity[ compressed[ "Size" ][ "Compressed" ], "Bytes" ];
      ratio     = N[ size2 / size1 ];
      percent   = ToString @ NumberForm[ 100 * ratio, { 3, 1 } ] <> " %";
      length    = compressed[ "Size" ][ "Length" ];
      depth     = compressed[ "Size" ][ "Depth"  ];
      version   = Lookup[ compressed, "Version", 1 ];

      shown = {
          {
              BoxForm`MakeSummaryItem[ { "Head: ", head }, fmt ],
              SpanFromLeft
          },
          {
              BoxForm`MakeSummaryItem[ { "Compression: ", percent }, fmt ]
          }
      };

      hidden = {
          {
              BoxForm`MakeSummaryItem[
                  { "Original: ", ToString @ size1 },
                  fmt
              ]
              ,
              BoxForm`MakeSummaryItem[
                  { "Compressed: ", ToString @ size2 },
                  fmt
              ]
          }
          ,
          {
              BoxForm`MakeSummaryItem[
                  { "Length: ", length },
                  fmt
              ]
              ,
              BoxForm`MakeSummaryItem[
                  { "Depth: ", depth },
                  fmt
              ]
          }
          ,
          {
              BoxForm`MakeSummaryItem[
                  { "Version: ", version },
                  fmt
              ]
              ,
              SpanFromLeft
          }
      };

      BoxForm`ArrangeSummaryBox[ SymbolName @ CompressedExpression,
                                 CompressedExpression @ compressed,
                                 icon,
                                 shown,
                                 hidden,
                                 fmt
      ]
  ];

compressionIcon =
  Graphics[{
      EdgeForm[None], FaceForm[GrayLevel[.5]],
      FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1,
          0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0,
          1, 0}, {1, 3, 3}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1,
          0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, {{0, 2, 0}, {0, 1,
          0}, {0, 1, 0}, {0, 1, 0}}}, {{{509.34000000000003`,
          588.9599999999999`}, {475.01`, 630.61`}, {468.07`,
          638.7900000000001`}, {457.78`, 644.13`}, {446.25`,
          644.13`}, {148.75`, 644.13`}, {137.22`, 644.13`}, {126.93`,
          638.7900000000001`}, {120.11999999999999`, 630.61`}, {85.78`,
          588.9599999999999`}, {78.59`, 580.2900000000001`}, {74.38`,
          569.38`}, {74.38`, 557.3499999999999`}, {74.38`,
          247.46`}, {74.38`, 220.06`}, {96.56`,
          197.88000000000002`}, {123.96000000000001`,
          197.88000000000002`}, {471.04`, 197.88000000000002`}, {498.44`,
          197.88000000000002`}, {520.63`, 220.06`}, {520.63`,
          247.46`}, {520.63`, 557.3499999999999`}, {520.63`,
          569.38`}, {516.41`, 580.2900000000001`}, {509.34000000000003`,
          588.9599999999999`}}, {{297.5`, 284.65000000000003`}, {161.15`,
          421.`}, {247.92000000000002`, 421.`}, {247.92000000000002`,
          470.58`}, {347.08`, 470.58`}, {347.08`, 421.`}, {433.85`,
          421.`}, {297.5`, 284.65000000000003`}}, {{127.06`,
          594.54`}, {147.26`, 619.3299999999999`}, {444.76`,
          619.3299999999999`}, {467.94`, 594.54`}, {127.06`,
          594.54`}}}]}, ImageSize -> 32, Background -> GrayLevel[14/15],
      PlotRangePadding -> 150, ImagePadding -> 1, Frame -> True,
      FrameTicks -> None, FrameStyle -> GrayLevel[.7]];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)