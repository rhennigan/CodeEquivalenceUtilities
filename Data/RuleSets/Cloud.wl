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
    "Description" -> "Transform cloud operations into a form that is equivalent to the original.",
    "Usage"       -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*canonicalizeCloudObjectURL*)
canonicalizeCloudObjectURL // ClearAll;

canonicalizeCloudObjectURL[ CloudObject[ url_String, opts___ ] ] :=
    CloudObject[ canonicalizeCloudObjectURL @ url, opts ];

canonicalizeCloudObjectURL[ url_String ] :=
    Module[ { obj, expanded, normal },
        obj = CloudObject @ url;
        expanded = First @ obj;
        If[ StringEndsQ[ expanded, "/" ~~ uuid__ ~~ EndOfString /; UUIDStringQ @ uuid ],
            Canonical[ ],
            expanded
        ]
    ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizePermissions*)
normalizePermissions // ClearAll;
normalizePermissions[ permissions_ ] := (
    Needs[ "CloudObject`" -> None ];
    CloudObject`Private`normalizePermissions[ permissions, "application/mathematica", CloudObject ]
);

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Standardize Permissions specifications",
    "Symbols"     :> { Permissions },
    "Rule"        :>
        HoldPattern[ Permissions -> permissions: Except[ _Canonical, _? SafeEvaluatedQ ] ] :>
            With[ { normal = normalizePermissions @ permissions },
                (Permissions -> Canonical @ normal) /; ListQ @ normal
            ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Standardize Permissions specifications",
    "Symbols"     :> { Permissions, $Permissions },
    "Rule"        :> HoldPattern[ Permissions -> $Permissions ] :> (Permissions -> "Private")
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Standardize Permissions specifications",
    "Symbols"     :> { Permissions, $Permissions },
    "Rule"        :> HoldPattern[ Permissions :> permissions_ ] :> (Permissions -> permissions)
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Add default Permissions specification",
    "Symbols"     :> { Permissions, $Permissions, CloudPut, CloudDeploy, CloudPublish },
    "Rule"        :>
        (h: CloudPut|CloudDeploy |CloudPublish)[ a: Except[ Permissions -> _ ]... ] :>
            h[ a, Permissions -> $Permissions ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Standardize anonymous cloud objects",
    "Symbols"     :> { CloudObject },
    "Rule"        :>
        CloudObject[ url_String? UStringQ, opts___ ] :>
            With[ { new = canonicalizeCloudObjectURL @ url }, CloudObject[ Canonical @ new, opts ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Standardize cloud deployment targets",
    "Symbols"     :> { CloudPut, CloudDeploy, CloudPublish, CloudObject },
    "Rule"        :>
        (h: CloudPut | CloudDeploy | CloudPublish)[ a_, b: Except[ _Canonical | _Rule | _RuleDelayed ], c___ ] :>
            With[ { std = canonicalizeCloudObjectURL @ b }, h[ a, Canonical @ std, c ] /; True ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Standardize cloud deployment targets",
    "Symbols"     :> { CloudPut, CloudDeploy, CloudPublish, CloudObject },
    "Rule"        :>
        (h: CloudPut | CloudDeploy | CloudPublish)[ a_, b: (_Rule | _RuleDelayed)... ] :>
            h[ a, CloudObject[ ], b ]
|>

(**********************************************************************************************************************)
<|
    "Description" -> "Add default argument to EmbedCode",
    "Symbols"     :> { EmbedCode },
    "Rule"        :> EmbedCode[ code_ ] :> EmbedCode[ code, "HTML" ]
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)