(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)

(*
Rules are specified as top level associations with the following keys:
    * Category          (optional): Defaults to FileBaseName of the current file if omitted.
    * Description       (optional): A short description of the rule.
    * IncompatibleRules (optional): A list of rule names that are not compatible with this rule.
    * Name              (optional): Specifies a name for the rule which can be referenced from other rules.
    * Priority          (optional): Rules are sorted by this value, so set lower to prioritize (default: 0).
    * RelatedRules      (optional): A list of rule names that are related to this rule.
    * Rule              (required): The transformation rule.
    * Symbols           (optional): Any symbols that are particularly relevant to the transformation rule.
    * Usage             (required): Specifies which rule sets a rule should belong to.

The function `SetRuleDefaults` can be used to set default values for remaining rules in the current file. If a required
key is set by `SetRuleDefaults`, it becomes optional in subsequent rule associations. For values that are specified as
lists, `Inherited` can be used to splice in default values.

Some typical "Usage" values:
    * CodeStyle
    * EquivalenceTesting
    * Optimization
    * Scoping
    * Simplification
*)

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Default Values*)
SetRuleDefaults @ <|
    "Category" -> "SampleCategory",
    "Usage"    :> { "SampleUsage" },
    "Priority" -> 10000
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Description" -> "Simplify double Reverse",
    "Usage"       -> { Inherited, "Simplification", "Optimization" },
    "Symbols"     :> { Reverse },
    "Rule"        :> Reverse @ Reverse[ list_ ] :> list
|>

(**********************************************************************************************************************)
<|
    "Name"              -> "PartToFirst",
    "IncompatibleRules" -> { "FirstToPart" },
    "Description"       -> "Replace Part with First when possible",
    "Usage"             -> { Inherited, "Simplification", "CodeStyle" },
    "Symbols"           :> { Part, First },
    "Rule"              :> Part[ list_, 1 ] :> First @ list
|>

(**********************************************************************************************************************)
<|
    "Name"              -> "FirstToPart",
    "IncompatibleRules" -> { "PartToFirst" },
    "Description"       -> "Replace First with Part when possible",
    "Usage"             -> { Inherited, "EquivalenceTesting" },
    "Symbols"           :> { Part, First },
    "Rule"              :> First[ list_ ] :> Part[ list, 1 ]
|>

(**********************************************************************************************************************)
(* Wolfram`CodeEquivalenceUtilities` will automatically be included on the $ContextPath when loading these files: *)
<|
    "Description" -> "Convert Range into an equivalent Table",
    "Usage"       -> { Inherited, "EquivalenceTesting", "Scoping" },
    "Priority"    -> 0,
    "Symbols"     :> { Table, Range },
    "Rule"        :> Range[ args___ ] :> WithHolding[ { i = NewLocalSymbol[ ] }, Table[ i, { i, args } ] ]
|>

(**********************************************************************************************************************)
(* Supporting definitions can be included in the rule file: *)
numberQ // Attributes = { HoldAllComplete };
numberQ[ n: _Integer|_Real ] := AtomQ @ Unevaluated @ n;
numberQ[ ___ ] := False;

<|
    "Description" -> "Replace Table with ConstantArray",
    "Usage"       -> { "Optimization", "Simplification", "CodeStyle" },
    "Symbols"     :> { Table, ConstantArray },
    "Rule"        :> Table[ a_? numberQ, b__Integer? numberQ ] :> ConstantArray[ a, { b } ]
|>

(* :!CodeAnalysis::EndBlock:: *)