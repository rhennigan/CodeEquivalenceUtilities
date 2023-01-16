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
    "Description" -> "Transform color specifications into an equivalent form",
    "Usage"       -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Convert GrayLevel to an equivalent RGBColor",
    "Symbols"     :> { GrayLevel, RGBColor },
    "Rule"        :> GrayLevel[ p_ ] :> RGBColor[ p, p, p ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Add an alpha channel to RGBColor",
    "Symbols"     :> { RGBColor },
    "Rule"        :> RGBColor[ r_, g_, b_ ] :> RGBColor[ r, g, b, 1 ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Add default arguments to Hue",
    "Symbols"     :> { Hue },
    "Rule"        :> Hue[ p_? UNumericQ ] :> Hue[ p, 1, 1 ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Add an alpha channel to Hue",
    "Symbols"     :> { Hue },
    "Rule"        :> Hue[ h_, s_, v_ ] :> Hue[ h, s, v, 1 ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Convert Hue to an equivalent RGBColor",
    "Symbols"     :> { Hue, RGBColor },
    "Rule"        :>
        Hue[ a___? UNumericQ ] :>
            With[ { c = Quiet @ ColorConvert[ Hue @ a, RGBColor ] }, c /; ColorQ @ c ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Round RGBColor values to the nearest byte value",
    "Symbols"     :> { RGBColor },
    "Rule"        :>
        RGBColor[ a___? UNumericQ ] /; Round[ { a }, 1 / 255 ] =!= { a } :>
            With[ { r = Round[ { a }, 1 / 255 ] }, { c = RGBColor @@ r }, c /; True ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)