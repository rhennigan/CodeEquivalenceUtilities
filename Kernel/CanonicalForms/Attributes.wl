(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

CanonicalFlat;
CanonicalOneIdentity;
CanonicalOrderless;
CanonicalTransformFromAttributes;
FlatQ;
GetAttributes;
HoldingQ;
HoldNumericQ;
ListableQ;
NonFlatQ;
NonHoldingQ;
NonListableQ;
NonOneIdentityQ;
NonOrderlessQ;
OneIdentityQ;
OrderlessQ;

Begin[ "`Private`" ];

$Holding = HoldFirst | HoldRest | HoldAll | HoldAllComplete;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalFlat*)
(* Flatten all f[args] in expression where f has the attribute Flat: *)
CanonicalFlat[ expression_ ] :=
  expression //. HoldPattern @ f_[ a1___, f_ @ a2___, a3___ ] /;
                   Attributes @ f ~MemberQ~ Flat :>
                     f[ a1, a2, a3 ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalOneIdentity*)
CanonicalOneIdentity[ expression_ ] :=
  expression //.
    HoldPattern @ f_[ x : Except[ _Symbol, _? UAtomQ ] ] /;
      Attributes @ f ~MemberQ~ OneIdentity :>
        x;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalOrderless*)
(* Sort the arguments of any Orderless functions: *)
CanonicalOrderless[ expression_ ] :=
  Module[ { heldSorted },
      heldSorted = expression //. f_[ a___ ] /; OrderlessQ @ Unevaluated @ f :>
        TrEval @ sortArgs @ f @ a;
      heldSorted //. HoldApply[ f_, { args___ } ] :> f @ args
  ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*sortArgs*)
sortArgs // Attributes = { HoldAllComplete };

sortArgs[ f_[ args___ ] ] :=
    Module[ { formatted, order },
        formatted = sortFmt /@ Unevaluated @ { args };
        order     = Ordering @ formatted;
        With[ { sortedArgs = (TempHold @ args)[[ order ]] },
            ReplacePart[ HoldApply[ f, sortedArgs ], { 2, 0 } -> List ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*sortFmt*)
sortFmt = Function[
    e,
    { Head @ Unevaluated @ e, HoldComplete @ e },
    HoldAllComplete
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CanonicalTransformFromAttributes*)
(* Puts expressions into a canonical form while preventing evaluation leaks: *)
CanonicalTransformFromAttributes[ expression_ ] :=
  CanonicalOrderless @ CanonicalFlat @ CanonicalOneIdentity @ expression;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FlatQ*)
FlatQ // Attributes = { HoldAllComplete };
FlatQ[ f_  ] := GetAttributes @ f ~MemberQ~ Flat;
FlatQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*GetAttributes*)
GetAttributes // Attributes = { HoldAllComplete, Listable };
GetAttributes[ Function[ _, _, a_Symbol ] ] := { a };
GetAttributes[ Function[ _, _, a_ ] ] := a;
GetAttributes[ s_? SymbolQ ] := Attributes @ s;
GetAttributes[ s_? stringQ ] := Attributes @ s;
GetAttributes[ list_List ] := GetAttributes /@ list;
GetAttributes[ ___ ] := { };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*stringQ*)
stringQ // Attributes = { HoldAllComplete };
stringQ[ s_String ] := StringQ @ Unevaluated @ s;
stringQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*HoldingQ*)
HoldingQ // Attributes = { HoldAllComplete };
HoldingQ[ f_  ] := GetAttributes @ f ~MemberQ~ $Holding;
HoldingQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*HoldNumericQ*)
HoldNumericQ // Attributes = { HoldAllComplete };

HoldNumericQ[ x_ /; NumericQ @ Unevaluated @ x ] := True;

HoldNumericQ[ _? IntTypeQ ] := True;

HoldNumericQ[ f_[ args___ ] ] :=
    Module[ { fNumeric, argsNumeric },
        fNumeric    = MemberQ[ GetAttributes @ f, NumericFunction ];
        argsNumeric = And @@ (HoldNumericQ /@ HoldComplete @ args);
        fNumeric && argsNumeric
    ];

HoldNumericQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ListableQ*)
ListableQ // Attributes = { HoldAllComplete };

ListableQ[ fun : Function[ expr_ ] ] :=
  And @@ Cases[ fun,
                f_[ ___, _Slot, ___ ] :> ListableQ @ f,
                Infinity,
                Heads -> True
         ];

ListableQ[ f_  ] := GetAttributes @ f ~MemberQ~ Listable;
ListableQ[ ___ ] := False;

ListableQ[ FromLetterNumber ] = True;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NonFlatQ*)
NonFlatQ // Attributes = { HoldAllComplete };
NonFlatQ[ f_  ] := GetAttributes @ f ~FreeQ~ Flat;
NonFlatQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NonHoldingQ*)
NonHoldingQ // Attributes = { HoldAllComplete };
NonHoldingQ[ f_  ] := GetAttributes @ f ~FreeQ~ $Holding;
NonHoldingQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NonListableQ*)
NonListableQ // Attributes = { HoldAllComplete };
NonListableQ[ f_  ] := GetAttributes @ f ~FreeQ~ Listable;
NonListableQ[ ___ ] := False;
NonListableQ[ FromLetterNumber ] = False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NonOneIdentityQ*)
NonOneIdentityQ // Attributes = { HoldAllComplete };
NonOneIdentityQ[ f_  ] := GetAttributes @ f ~FreeQ~ OneIdentity;
NonOneIdentityQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NonOrderlessQ*)
NonOrderlessQ // Attributes = { HoldAllComplete };
NonOrderlessQ[ f_  ] := GetAttributes @ f ~FreeQ~ Orderless;
NonOrderlessQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*OneIdentityQ*)
OneIdentityQ // Attributes = { HoldAllComplete };
OneIdentityQ[ f_  ] := GetAttributes @ f ~MemberQ~ OneIdentity;
OneIdentityQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*OrderlessQ*)
OrderlessQ // Attributes = { HoldAllComplete };
OrderlessQ[ f_  ] := GetAttributes @ f ~MemberQ~ Orderless;
OrderlessQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)
