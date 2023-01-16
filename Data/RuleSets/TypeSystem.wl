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
SetRuleDefaults @ <|
    "Description" -> "Annotate expressions with inferred type information",
    "Usage"       -> { "EquivalenceTesting", "TypeSystem" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*iterMaxTypeQ*)
iterMaxTypeQ // ClearAll;
iterMaxTypeQ // Attributes = { HoldAllComplete };
iterMaxTypeQ[ _? IntTypeQ  ] := True;
iterMaxTypeQ[ _? RealTypeQ ] := True;
iterMaxTypeQ[ $$numConst   ] := Inline[ $$numConst, True ];
iterMaxTypeQ[ ___          ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*replaceTableFromListType*)
replaceTableFromListType // ClearAll;
replaceTableFromListType // Attributes = { HoldAllComplete };

replaceTableFromListType[ exp_, i_, ii_ ] :=
    With[ { local = newTypedLocal @ ListType @ ii },
        If[ FailureQ @ local,
            $Failed,
            replaceTableFromListType[ exp, i, ii, local ]
        ]
    ];

replaceTableFromListType[ exp_, i_, ii_, j_ ] :=
    With[ { held = TempHold @ exp /. HoldPattern @ i -> j },
        TempHold @ Table[ held, { j, ii } ]
    ];

replaceTableFromListType[ ___ ] := $Failed;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*newTypedLocal*)
newTypedLocal // ClearAll;
newTypedLocal[ Integer ] := Int @@ { NewLocalSymbol[ ] };
newTypedLocal[ Real    ] := Rea @@ { NewLocalSymbol[ ] };
newTypedLocal[ String  ] := Str @@ { NewLocalSymbol[ ] };
newTypedLocal[ ___     ] := $Failed;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*constantArrayQ*)
constantArrayQ // ClearAll;
constantArrayQ // Attributes = { HoldAllComplete };

constantArrayQ[ m_List ] := TrueQ[
    ArrayQ @ Unevaluated @ m &&
    Length @ Unevaluated @ m >= 4 &&
    SameQ @@ (HoldComplete /@ Unevaluated @ m)
];

constantArrayQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
Inline[ { $intType, $reaType }, HoldComplete[

    (* Table iterators *)

    Table[ exp_, n_? IntTypeQ ] :>
      WithHolding[
          {
              i = NewLocalSymbol[ ]
          },
          Table[ exp, { i, 1, n, 1 } ]
      ],



    Table[ exp_, { i_Symbol? SymbolQ, i2_? IntTypeQ } ] :>
      WithHolding[
          {
              j = Int @@ { NewLocalSymbol[ ] },
              held = TempHold[ exp ] /. HoldPattern[ i ] -> j
          },
          Table[ held, { j, 1, i2, 1 } ]
      ],



    Table[ exp_, { i_Symbol? SymbolQ, i1_? IntTypeQ, i2_? IntTypeQ } ] :>
      WithHolding[
          {
              j = Int @@ { NewLocalSymbol[ ] },
              held = TempHold[ exp ] /. HoldPattern[ i ] -> j
          },
          Table[ held, { j, i1, i2, 1 } ]
      ],



    Table[ exp_, { i_Symbol? SymbolQ, i1_? IntTypeQ, i2_? iterMaxTypeQ, di_? IntTypeQ } ] :>
      WithHolding[
          {
              j = Int @@ { NewLocalSymbol[ ] },
              held = TempHold[ exp ] /. HoldPattern[ i ] -> j
          },
          Table[ held, { j, i1, i2, di } ]
      ],

     Table[ exp_, { i_Symbol? SymbolQ, ii_? ListTypeQ } ] :>
        With[ { new = replaceTableFromListType[ exp, i, ii ] },
            new /; ! FailureQ @ new
        ],

    Table[ exp_, { i: Except[ _Integer, _? IntTypeQ ], n_? IntTypeQ } ] :>
      Table[ exp, { i, 1, n, 1 } ],


    Table[ exp_, { i_Symbol? SymbolQ, i1 : $reaType, i2 : $reaType, di_ } ] :>
      WithHolding[
          {
              held = TempHold[ exp ] /. HoldPattern[ i ] -> Rea @ i
          },
          Table[ held, { Rea @ i, i1, i2, di } ]
      ],

    Table[ exp_,
           { i_Symbol? SymbolQ, i1_? HoldNumericQ, i2_? HoldNumericQ, di : $reaType }
    ] :>
      WithHolding[ { held = TempHold[ exp ] /. HoldPattern[ i ] -> Rea @ i },
                   Table[ held, { Rea @ i, i1, i2, di } ]
      ],



    Array[ f_, n_? IntTypeQ ] :>
      Table[ f[i], { i, 1, n, 1 } ],


    Array[ fun_, { iter__? IntTypeQ } ] :>
      WithHolding[
          {
              iterators = Replace[ TempHold @ iter,
                                   e_ :> WithHolding[ { i = NewLocalSymbol[ ] },
                                                      { i, 1, e, 1 }
                                         ],
                                   { 1 }
                          ]
              ,
              args = TempHold @@ Cases[ iterators, { sym_, _, _, _ } :>
                                                     TempHold @ sym
                                 ]
          },
          Table[ fun @ args, iterators ]
      ],

    BoxMatrix[ r_? IntTypeQ ] /; Positive @ Unevaluated @ r :>
      WithHolding[
          {
              n = 2 r + 1
          },
          Table[ 1, n, n ]
      ],

    ConstantArray[ x_, n_? IntTypeQ ] :>
      WithHolding[
          {
              i = NewLocalSymbol[ ]
          },
          Table[ x, { i, 1, n, 1 } ]
      ],

    ConstantArray[ x_, { n___? IntTypeQ } ] :>
      Table[ x, n ]
    ,

    arr_List? constantArrayQ :>
      WithHolding[
        {
            dims = Dimensions @ Unevaluated @ arr,
            elem = Extract[ Unevaluated @ arr, 1 & /@ dims, TempHold ]
        },
        ConstantArray[ elem, dims ]
    ],

    Reverse[ exp_ ][[ i_? IntTypeQ ]] :> exp[[ -i ]]
    ,


    RandomColor[ { x__? IntTypeQ } ] :>
      Table[RandomColor[], x]
    ,


    i_Int :> RuleCondition @ i,
    i_Rea :> RuleCondition @ i
]]

(**********************************************************************************************************************)
<|
    "Description" -> "Convert SetDelayed to a typed expression",
    "Symbols"     :> { SetDelayed },
    "Rule"        :> sd_SetDelayed /; ! TypedDefinitionQ @ sd :> RuleCondition @ ToTypedBinding @ sd
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Convert RuleDelayed to a typed expression",
    "Symbols"     :> { RuleDelayed },
    "Rule"        :> rd_RuleDelayed /; ! TypedDefinitionQ @ rd :> RuleCondition @ ToTypedBinding @ rd
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)