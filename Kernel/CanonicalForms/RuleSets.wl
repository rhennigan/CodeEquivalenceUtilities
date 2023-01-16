(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];
Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Messages*)
CodeEquivalenceUtilities::SetRuleDefaultsNotLoading =
"SetRuleDefaults can only be used within a rule set file.";

CodeEquivalenceUtilities::InvalidAssociationRules =
"Invalid rules in `1`.";

CodeEquivalenceUtilities::InvalidRuleFile =
"Cannot read rules from file `1`.";

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Argument Patterns*)
$$ruleUsage = _String? StringQ | All;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$RuleData = <| |>;
$RuleData // Protect;

$RuleDefaults = <|
    "Name"              -> None,
    "Description"       -> "Transform expression with an anonymous replacement rule",
    "Category"          -> "General",
    "Usage"             -> { },
    "Symbols"           :> { },
    "Priority"          -> 0,
    "Experimental"      -> False,
    "Internal"          -> False,
    "IncompatibleRules" -> { },
    "RelatedRules"      -> { }
|>;
$RuleDefaults // Protect;

$defaultRuleDefaults  = $RuleDefaults;
$ruleSetPath         := PacletObject[ "Wolfram/CodeEquivalenceUtilities" ][ "AssetLocation", "RuleSets" ];
$namedRuleSets        = DeleteCases[ FileBaseName /@ FileNames[ { "*.wl", "*.wxf" }, $ruleSetPath ], "_SAMPLE" ];
$loadingRules         = False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*GetRules*)
GetRules[ usage_String ] :=
    Cases[ SortBy[ Join[ Lookup[ $RuleData, All, { } ], Lookup[ $RuleData, usage, { } ] ], priorityOrder ],
           KeyValuePattern[ "Rule" :> rule_ ] :> rule
    ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*SetRuleDefaults*)
SetRuleDefaults[ as_Association? AssociationQ ] /; $loadingRules :=
    catchTop @ blockProtected[ { $RuleDefaults },
        $RuleDefaults = Association[ $RuleDefaults, as ];
        Null
    ];

SetRuleDefaults[ rules___ ] /; $loadingRules :=
    With[ { as = Association @ rules },
        If[ AssociationQ @ as,
            SetRuleDefaults @ as,
            throwFailure[ "InvalidAssociationRules", HoldForm @ SetRuleDefaults @ rules ]
        ]
    ];

SetRuleDefaults[ args___ ] := throwFailure[ "SetRuleDefaultsNotLoading", HoldForm @ SetRuleDefaults @ args ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*LoadTransformationRules*)
LoadTransformationRules[ name_String? namedRuleSetQ ] :=
    catchMine @ addToRuleData @ loadNamedRuleSet @ name;

LoadTransformationRules[ All ] := catchMine[
    LoadTransformationRules /@ $namedRuleSets;
    $RuleData
];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*namedRuleSetQ*)
namedRuleSetQ // ClearAll;
namedRuleSetQ[ name_String ] := MemberQ[ $namedRuleSets, name ];
namedRuleSetQ[ ___         ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*loadNamedRuleSet*)
loadNamedRuleSet // beginDefinition;

loadNamedRuleSet[ name_String ] := Enclose[
    Block[ { $loadingRules = True },
        loadNamedRuleSet[ name ] =
            Module[ { file0, wxf, file, data },
                file0 = FileNameJoin @ { $ruleSetPath, name };
                wxf   = file0 <> ".wxf";
                file  = If[ FileExistsQ @ wxf, wxf, file0 <> ".wl" ];
                ConfirmAssert @ FileExistsQ @ file;
                data = ConfirmMatch[ loadRuleFile @ file, { __Association? AssociationQ } ];
                SortBy[ data, priorityOrder ]
            ]
    ],
    throwInternalFailure[ HoldForm @ loadNamedRuleSet @ name, ##1 ] &
];

loadNamedRuleSet // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*priorityOrder*)
priorityOrder // beginDefinition;
priorityOrder[ KeyValuePattern[ "Priority" :> p_ ] ] := priorityOrder @ p;
priorityOrder[ KeyValuePattern[ "Priority" -> p_ ] ] := priorityOrder @ p;
priorityOrder[ -Infinity ] := { -1, 0 };
priorityOrder[  Infinity ] := {  1, 0 };
priorityOrder[  n_       ] := {  0, n };
priorityOrder // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*loadRuleFile*)
loadRuleFile // beginDefinition;

loadRuleFile[ file_? FileExistsQ ] :=
    Internal`InheritedBlock[ { $RuleDefaults },
        blockProtected[ { $RuleDefaults }, $RuleDefaults[ "Category" ] = FileBaseName @ file ];
        Block[ { $ContextPath = DeleteDuplicates @ Prepend[ $ContextPath, "Wolfram`CodeEquivalenceUtilities`" ] },
            readRuleExpressions @ ExpandFileName @ file
        ]
    ];

loadRuleFile // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*readRuleExpressions*)
readRuleExpressions // beginDefinition;
readRuleExpressions[ file_String  ] := readRuleExpressions[ file, ToUpperCase @ FileExtension @ file ];
readRuleExpressions[ file_, "WXF" ] := checkRuleExpressions[ file, Developer`ReadWXFFile @ file ];
readRuleExpressions[ file_, "WL"  ] := checkRuleExpressions[ file, toRuleData /@ ReadList @ file ];
readRuleExpressions // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkRuleExpressions*)
checkRuleExpressions // beginDefinition;
checkRuleExpressions[ file_, rules_List ] := Cases[ Flatten @ rules, KeyValuePattern[ "Rule" :> _RuleDelayed ] ];
checkRuleExpressions[ file_, _ ] := throwFailure[ "InvalidRuleFile", file ];
checkRuleExpressions // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toRuleData*)
toRuleData // ClearAll;

toRuleData[ rule_RuleDelayed ] := toRuleData @ <| "Rule" :> rule |>;

toRuleData[ as_Association ] /; KeyExistsQ[ as, "Rule" ] := standardizeRuleData @ as;

toRuleData[ expr_ ] :=
    With[ { rules = MakeTransformationRules @ expr },
        toRuleData /@ rules /; MatchQ[ rules, { __RuleDelayed } ]
    ];

toRuleData[ ___ ] := Nothing;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*standardizeRuleData*)
standardizeRuleData // beginDefinition;

standardizeRuleData[ as: KeyValuePattern[ a_ -> b: Except[ _String|_Integer|True|False ] ] ] :=
    standardizeRuleData @ Append[ as, a :> b ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> Inherited ] ] :=
    standardizeRuleData @ Append[ as, key :> { Inherited } ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> { ___, Inherited, ___ } ] ] :=
    standardizeRuleData @ Merge[ { $RuleDefaults, as }, inheritRuleValues ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> inheritRuleValues[ { { defaults___ }, { a___, Inherited, b___ } } ] ] ] :=
    standardizeRuleData @ Append[ as, key :> { a, defaults, b } ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> inheritRuleValues[ { ___, values_ } ] ] ] :=
    standardizeRuleData @ Append[ as, key :> values ];

standardizeRuleData[ as: KeyValuePattern[ "Rule" :> rule0_ ] ] :=
    With[ { rule = MakeTransformationRules @ rule0 },
        Association[ $RuleDefaults, as, "Rule" :> rule ]
    ];

standardizeRuleData // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*inheritRuleValues*)
inheritRuleValues // beginDefinition;
inheritRuleValues[ { { defaults___ }, { a___, Inherited, b___ } } ] := DeleteDuplicates @ { a, defaults, b };
inheritRuleValues[ { _, values_ } ] := values;
inheritRuleValues[ { values_ } ] := values;
inheritRuleValues // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*addToRuleData*)
addToRuleData // beginDefinition;

addToRuleData[ data_ ] :=(
    KeyValueMap[
        addToRuleData,
        Merge[
            (Thread[ Replace[ Lookup[ #1, "Usage", { } ], a: Except[ _List ] :> { a } ] -> #1 ] &) /@ data,
            DeleteDuplicates
        ]
    ];
    data
);

addToRuleData[ usage: $$ruleUsage, data: { __Association } ] :=
    Module[ { current, combined },
        current  = Lookup[ $RuleData, usage, { } ];
        combined = SortBy[ Union[ current, data ], priorityOrder ];
        blockProtected[ { $RuleData }, $RuleData[ usage ] = combined ]
    ];

addToRuleData // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)