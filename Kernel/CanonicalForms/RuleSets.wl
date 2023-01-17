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
$$boolean   = True|False;
$$integer   = _Integer? IntegerQ;
$$string    = _String? StringQ;
$$ruleUsage = $$string | All;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$RuleData = <| |>;
$RuleData // Protect;

$RuleDefaults = <|
    "Name"              -> None,
    "Description"       -> "Transform expression with an anonymous replacement rule",
    "Group"             -> "General",
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
$rulesNeedSorting     = False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*GetRules*)
GetRules[ usage: $$string ] := (
    sortRules[ ];
    Cases[ Lookup[ $RuleData, usage, { } ], KeyValuePattern[ "Rule" :> rule_ ] :> rule ]
);

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*sortRules*)
sortRules // beginDefinition;

sortRules[ ] :=
    If[ TrueQ @ $rulesNeedSorting,
        blockProtected[ { $RuleData }, $RuleData = sortRules0 /@ $RuleData ];
        $rulesNeedSorting = False;
    ];

sortRules // endDefinition;

sortRules0 // beginDefinition;
sortRules0[ rules_ ] := SortBy[ rules, priorityOrder ];
sortRules0 // endDefinition;

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

loadNamedRuleSet[ name: $$string ] := Enclose[
    Block[ { $loadingRules = True },
        loadNamedRuleSet[ name ] =
            Module[ { file0, wxf, file, data },
                file0 = FileNameJoin @ { $ruleSetPath, name };
                wxf   = file0 <> ".wxf";
                file  = If[ FileExistsQ @ wxf, wxf, file0 <> ".wl" ];
                ConfirmAssert @ FileExistsQ @ file;
                data = ConfirmMatch[ loadRuleFile @ file, { __Association? AssociationQ } ];
                data
            ]
    ],
    throwInternalFailure[ HoldForm @ loadNamedRuleSet @ name, ##1 ] &
];

loadNamedRuleSet // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*priorityOrder*)
priorityOrder // beginDefinition;
priorityOrder[ as_Association ] :={ priorityOrder @ as[ "Priority" ], as[ "Index" ] };
priorityOrder[ -Infinity      ] := { -1, 0 };
priorityOrder[  Infinity      ] := {  1, 0 };
priorityOrder[  n_            ] := {  0, n };
priorityOrder // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*loadRuleFile*)
loadRuleFile // beginDefinition;

loadRuleFile[ file_? FileExistsQ ] :=
    Internal`InheritedBlock[ { $RuleDefaults },
        blockProtected[ { $RuleDefaults }, $RuleDefaults[ "Group" ] = FileBaseName @ file ];
        Block[ { $ContextPath = DeleteDuplicates @ Prepend[ $ContextPath, "Wolfram`CodeEquivalenceUtilities`" ] },
            readRuleExpressions @ ExpandFileName @ file
        ]
    ];

loadRuleFile // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*readRuleExpressions*)
readRuleExpressions // beginDefinition;
readRuleExpressions[ file: $$string ] := readRuleExpressions[ file, ToUpperCase @ FileExtension @ file ];
readRuleExpressions[ file_, "WXF"   ] := checkRuleExpressions[ file, Developer`ReadWXFFile @ file ];
readRuleExpressions[ file_, "WL"    ] := checkRuleExpressions[ file, toRuleData /@ ReadList @ file ];
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

standardizeRuleData[ as: KeyValuePattern[ a_ -> b: Except[ $$string|$$integer|$$boolean ] ] ] :=
    standardizeRuleData @ Append[ as, a :> b ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> Inherited ] ] :=
    standardizeRuleData @ Append[ as, key :> { Inherited } ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> { ___, Inherited, ___ } ] ] :=
    standardizeRuleData @ Merge[ { $RuleDefaults, as }, inheritRuleValues ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> inheritRuleValues[ { { defaults___ }, { a___, Inherited, b___ } } ] ] ] :=
    standardizeRuleData @ Append[ as, key :> { a, defaults, b } ];

standardizeRuleData[ as: KeyValuePattern[ key_ :> inheritRuleValues[ { ___, values_ } ] ] ] :=
    standardizeRuleData @ Append[ as, key :> values ];

standardizeRuleData[ as0: KeyValuePattern[ "Rule" :> rule0_ ] ] :=
    Module[ { rule, defaults, as, new },
        rule     = MakeTransformationRules @ rule0;
        defaults = $RuleDefaults;
        as       = Association[ defaults, as0, "Rule" :> Evaluate @ rule ];
        new      = generateRuleName @ as;

        While[ $usedRuleNames @ new[ "Name" ] && StringQ @ new[ "Name" ],
               new[ "Name" ] = generateRuleName0[ new[ "Group" ], new ]
        ];

        $usedRuleNames[ new[ "Name" ] ] = True;
        new[ "Index" ] = $ruleIndex++;
        Association[ KeyTake[ new, Keys @ defaults ], KeySort @ new ]
    ];

standardizeRuleData // endDefinition;

$usedRuleNames = <| |>;
$ruleIndex     = 1;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*generateRuleName*)
generateRuleName // beginDefinition;
generateRuleName[ as: KeyValuePattern[ "Name" :> name_ ] ] := generateRuleName @ Association[ as, "Name" -> name ];
generateRuleName[ as: KeyValuePattern[ "Name" -> name: $$string ] ] := as;
generateRuleName[ as: KeyValuePattern[ "Name" -> Except[ $$string ] ] ] := generateRuleName @ KeyDrop[ as, "Name" ];

generateRuleName[ as: KeyValuePattern @ { "Group" -> group_, "Rule" :> rule_ } ] :=
    <| "Name" -> generateRuleName0[ group, as ], as |>;

generateRuleName // endDefinition;

generateRuleName0 // beginDefinition;
generateRuleName0[ group: $$string, expr_ ] := group <> "-" <> b62Hash[ expr, "MD5", 32 ];
generateRuleName0 // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*b62Hash*)
b62Hash // beginDefinition;
b62Hash[ expr_ ] := b62Hash[ Unevaluated @ expr, "MD5" ];
b62Hash[ expr_, type_ ] := b62Hash[ Unevaluated @ expr, type, Infinity ];
b62Hash[ expr_, type_, mod_ ] := b62Encode @ Mod[ Hash[ Unevaluated @ expr, type ], 2^mod ];
b62Hash // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*b62Encode*)
b62Encode // beginDefinition;
b62Encode[ int: $$integer ] := StringJoin @ Replace[ IntegerDigits[ int, 62 ], $b62IntToChar, { 1 } ];
b62Encode // endDefinition;

$lcChars  = CharacterRange[ "a", "z" ];
$ucChars  = CharacterRange[ "A", "Z" ];
$numChars = CharacterRange[ "0", "9" ];

$b62Chars     = Join[ $lcChars, $ucChars, $numChars ];
$b62IntToChar = Association @ MapIndexed[ First[ #2 ] - 1 -> #1 &, $b62Chars ];
$b62CharToInt = AssociationMap[ Reverse, $b62IntToChar ];

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
        combined = SortBy[ DeleteDuplicates @ Join[ current, data ], priorityOrder ];
        blockProtected[ { $RuleData }, $RuleData[ usage ] = combined ];
        $rulesNeedSorting = True;
        combined
    ];

addToRuleData // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)