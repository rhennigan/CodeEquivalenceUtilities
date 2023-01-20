(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)
BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Inert Wrappers*)
TransformHold    // Attributes = { HoldAllComplete };
TransformRelease // Attributes = { HoldAllComplete };

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rule Helper Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Type Patterns*)
$intAtom = _Integer? UAtomQ | TypedSymbol[ _, Verbatim[ _Integer ] ];
$intType = (Plus|Times)[ $intAtom... ] | $intAtom | RandomValue[ _DiscreteUniformDistribution ];

$reaAtom = _Real? UAtomQ | TypedSymbol[ _, Verbatim[ _Real ] ];
$realMix = OrderlessPatternSequence[ $intAtom..., $reaAtom.. ];
$reaType = (Plus|Times)[ $realMix ] | $reaAtom | RandomValue[ _UniformDistribution ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Type Shortcuts*)

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Int*)
Int // ClearAll;
Int // Attributes = { HoldAllComplete };
Int[ i_ ] := TypedSymbol[ i, _Integer ];

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Rea*)
Rea // ClearAll;
Rea // Attributes = { HoldAllComplete };
Rea[ i_ ] := TypedSymbol[ i, _Real ];

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Str*)
Str // ClearAll;
Str // Attributes = { HoldAllComplete };
Str[ i_ ] := TypedSymbol[ i, _String ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Held Pattern Tests*)

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*associationQ*)
associationQ // ClearAll;
associationQ // Attributes = { HoldAllComplete };
associationQ[ Association[ a___ ] ] := AllTrue[ Unevaluated @ { a }, associationRulesQ ];

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*associationRulesQ*)
associationRulesQ // ClearAll;
associationRulesQ // Attributes = { HoldAllComplete };
associationRulesQ[ (Rule|RuleDelayed)[ _, _ ] ] := True;
associationRulesQ[ a_List ] := AllTrue[ Unevaluated @ a, associationRulesQ ];
associationRulesQ[ a_Association ] := associationQ @ a;
associationRulesQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*atomQ*)
atomQ // ClearAll;
atomQ // Attributes = { HoldAllComplete };
atomQ[ _? UAtomQ       ] := True;
atomQ[ _? IntTypeQ     ] := True;
atomQ[ _? RealTypeQ    ] := True;
atomQ[ _? StringTypeQ  ] := True;
atomQ[ _? associationQ ] := True;
atomQ[ ___             ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*inertAtomQ*)
inertAtomQ // ClearAll;
inertAtomQ // Attributes = { HoldAllComplete };
inertAtomQ[ _Symbol ] := False;
inertAtomQ[ expr_   ] := AtomQ @ Unevaluated @ expr;
inertAtomQ[ ___     ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*urlQ*)
urlQ // ClearAll;
urlQ // Attributes = { HoldAllComplete };
urlQ[ url_String /; StringQ @ Unevaluated @ url ] := StringStartsQ[ url, "http://" | "https://" ];
urlQ[ _URLBuild ] := True;
urlQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Load Rules*)
LoadTransformationRules[ All ];

$TransformationRules = GetRules[ "EquivalenceTesting" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];