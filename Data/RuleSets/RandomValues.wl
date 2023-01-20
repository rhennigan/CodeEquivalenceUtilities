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
    "Usage" -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*randomColorByteQ*)
randomColorByteQ // ClearAll;
randomColorByteQ // Attributes = { HoldAllComplete };
randomColorByteQ[ RandomValue @ UniformDistribution @ { 0, 1 } ] := True;
randomColorByteQ[ Verbatim[ Times ][ a_, RandomValue @ DiscreteUniformDistribution @ { 0, 255 } ] ] :=
    With[ { b = Rational[ 1, 255 ] }, HoldComplete @ a === HoldComplete @ b ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-efFM8U",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomInteger[ ] :> RandomInteger @ { 0, 1 }
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-dIMHY9",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomInteger[ n_? IntTypeQ ] :> RandomInteger @ { 0, n }
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-dVu0OP",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        { r1: RandomInteger[ n_? IntTypeQ ], r2: RandomInteger[ n_ ].. } :>
            With[ { len = Length @ HoldComplete[ r1, r2 ] }, RandomInteger[ n, len ] /; len >= 4 ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-dRgHc4",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomReal[ ] :> RandomReal[ 1 ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-mOidi",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomReal[ n_? HoldNumericQ ] :> RandomReal @ { 0, n }
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-cOEZrK",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        (r: RandomReal|RandomInteger)[ range_, { n_? IntTypeQ, rest___ } ] :>
            Table[ r[ range, { rest } ], n ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-MMDBs",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> (r: RandomReal|RandomInteger)[ range_, n_? IntTypeQ ] :> r[ range, { n } ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-dsG4eI",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> (r: RandomReal|RandomInteger)[ range_, { } ] :> r @ range
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-eiQSLA",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomColor[ n_? IntTypeQ ] :> Table[ RandomColor[ ], n ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-deOYNE",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        RandomColor[ ] :>
            RGBColor[
                RandomInteger @ { 0, 255 } / 255,
                RandomInteger @ { 0, 255 } / 255,
                RandomInteger @ { 0, 255 } / 255,
                1
            ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-dsRuZC",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        RGBColor @ Table[ x_? randomColorByteQ, { i_, 1, 3, 1 } ] /;
            FreeQ[ HoldComplete @ x, HoldPattern @ i ] :>
                RandomColor[ ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-DkZoH",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        RGBColor[ a1___, RandomReal @ { 0, 1 }, a2___ ] :>
            RGBColor[ a1, RandomInteger @ { 0, 255 } / 255, a2 ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-cCIvo1",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        RandomInteger[ { a_? IntTypeQ, b_? IntTypeQ }, n_? IntTypeQ ] :>
            Table[ RandomInteger @ { a, b }, n ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-M9IjG",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomInteger[ b_? IntTypeQ ] + a_? IntTypeQ :> RandomInteger @ { a, a + b }
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-X3dDw",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        RandomSample @ Table[ rv: RandomValue @ DiscreteUniformDistribution @ { _Integer, _Integer }, iters___ ] :>
            Table[ rv, iters ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-VbkB9",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        Unsafe[ "System`RandomInteger", { { a_Integer, b_Integer } } ] :>
            With[ { c = b - a }, Unsafe[ "System`RandomInteger", { { 0, c } } ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-HjOry",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomInteger @ { a_, b_ } :> RandomValue @ DiscreteUniformDistribution @ { a, b }
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-ep0Dzf",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :> RandomReal @ { a_, b_ } :> RandomValue @ UniformDistribution @ { a, b }
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-cPBlcE",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        ({ a___ })[[ RandomValue @ DiscreteUniformDistribution @ { 1, n_Integer } ]] /;
            Length @ HoldComplete @ a === n && ! OrderedQ @ Unevaluated @ { a } :>
                With[ { sorted = Sort @ TempHold @ a },
                    ({ sorted })[[ RandomValue @ DiscreteUniformDistribution @ { 1, n } ]] /; True
                ]
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "RandomValues-b0sVLN",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { },
    "Rule"        :>
        Verbatim[ Plus ][
            a___,
            i_? IntTypeQ,
            RandomValue @ DiscreteUniformDistribution @ { imin_? IntTypeQ, imax_? IntTypeQ },
            b___
        ] :>
            a + RandomValue @ DiscreteUniformDistribution @ { imin + i, imax + i } + b
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)