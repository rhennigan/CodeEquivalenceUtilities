(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

`CachedValues`ClearEvalCache                   = ClearEvalCache;
`CanonicalForms`Common`MakeTransformationRules = MakeTransformationRules;
`CanonicalForms`Rules`$TransformationRules     = $TransformationRules;

Begin[ "`Private`" ];
(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Legacy Hacks*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*legacyRulesQ*)
legacyRulesQ // ClearAll;
legacyRulesQ[ { ___, last_ } ] := legacyRulesQ @ last;
legacyRulesQ[ Verbatim[ HoldPattern ][ _Wolfram`CodeEquivalenceUtilities`Utilities`HoldApply ] :> _ ] := True;
legacyRulesQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
