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
    "Description" -> "Transform string operations into a form that is equivalent to the original.",
    "Usage"       -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*characterQ*)
characterQ // ClearAll;
characterQ[ c_String? UStringQ ] := StringLength @ c === 1;
characterQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
HoldComplete[
    WordCount[ s_? StringTypeQ ] :>
        Length @ TextCases[ s, "Word" ],

    StringLength[ s_? StringTypeQ ] :>
        Length @ Characters @ s,

    Characters[ s_String? UStringQ ] :>
        RuleCondition @ Characters @ s,

    StringTake[ s_? StringTypeQ, spec_ ] :>
        StringJoin @ Take[ Characters @ s, spec ],

    StringDrop[ s_? StringTypeQ, spec_ ] :>
        StringJoin @ Drop[ Characters @ s, spec ],

    StringJoin[ a___, "", b___ ] :>
        StringJoin[ a, b ],

    StringJoin[ a1___, { a2___ }, a3___ ] :>
        StringJoin[ a1, a2, a3 ],

    StringLength /@ TextCases[ e_, "Word" ] :>
        StringLength @ TextCases[ e, "Word" ],

    HoldPattern[ StringJoin ][ str__? UStringQ ] :>
        RuleCondition @ StringJoin @ str,

    HoldPattern[ StringJoin ][ a1___, a2_String? UStringQ, a3_String? UStringQ, a4___ ] :>
        With[ { joined = StringJoin[ a2, a3 ] },
            StringJoin[ a1, joined, a4 ] /; UStringQ @ joined
        ],

    StringLength[ s_String? UStringQ ] :>
        RuleCondition @ StringLength @ s,

    StringReplace[ a_, b_ -> c_String? UStringQ, d___ ] :>
        StringReplace[ a, b :> c, d ],

    StringReplace[ a_, { b___String? UStringQ } :> c_, d___ ] :>
        StringReplace[ a, Alternatives @ b :> c, d ],

    StringTake[ s_, 1 ] :>
        StringPart[ s, 1 ],

    StringTake[ s_, -1 ] :>
        StringPart[ s, -1 ],

    StringTake[ Part[ list_, span_Span ], i_? IntTypeQ ] :>
        Part[ Characters @ list, span, i ],

    TextSentences[ str_? StringTypeQ ] :>
        TextCases[ str, "Sentence" ],

    TextSentences[ str_? StringTypeQ, n_? IntTypeQ ] :>
        TextCases[ str, "Sentence", n ],

    TextWords[ str_? StringTypeQ ] :>
        TextCases[ str, "Word" ],

    TextWords[ str_? StringTypeQ, n_? IntTypeQ ] :>
        TextCases[ str, "Word", n ],

    HoldPattern[ StringJoin ][ str_? StringTypeQ ] :>
        str,

    StringReverse[ str_ ] :>
        StringJoin @ Reverse @ Characters @ str,

    Characters[ FromCharacterCode[ list_ ] ] /; ListTypeQ[ list, Integer ] :>
        FromCharacterCode /@ list,

    CharacterRange[ c1_? characterQ, c2_? characterQ ] :>
        With[
            {
                n1 = First @ ToCharacterCode @ c1,
                n2 = First @ ToCharacterCode @ c2
            },
            FromCharacterCode /@ Range[ n1, n2 ] /; True
        ]
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)