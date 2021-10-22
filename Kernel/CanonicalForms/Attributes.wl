Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`CanonicalForms`Attributes`",
    {
        "Wolfram`CodeEquivalenceUtilities`Utilities`"
    }
];

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;


(* Exported symbols added here with SymbolName::usage *)
GetAttributes;
ListableQ                           ::usage = "";
NonListableQ                        ::usage = "";
HoldingQ                            ::usage = "";
NonHoldingQ                         ::usage = "";
OrderlessQ                          ::usage = "";
NonOrderlessQ;
FlatQ;
NonFlatQ;
OneIdentityQ;
NonOneIdentityQ;
CanonicalFlat                       ::usage = "";
CanonicalOrderless                  ::usage = "";
CanonicalOneIdentity                ::usage = "";
CanonicalTransformFromAttributes    ::usage = "";
HoldNumericQ;



Begin[ "`Private`" ];



(******************************************************************************)



stringQ // Attributes = { HoldAllComplete };
stringQ // Options    = { };


stringQ[ s_String ] := Depth @ HoldComplete @ s === 2;
stringQ[ ___ ] := False;



GetAttributes // Attributes = { HoldAllComplete, Listable };
GetAttributes // Options    = { };


GetAttributes[ Function[ _, _, attr_ ] ] :=
  Replace[ attr, s_? SymbolQ :> { s } ];


GetAttributes[ s_? SymbolQ ] :=
  Attributes @ s;


GetAttributes[ s_? stringQ ] :=
  Attributes @ s;


GetAttributes[ { x : HoldPattern[ Function[ _, _, _ ] |
                                  _? SymbolQ |
                                  _? stringQ
                     ] ... }
] :=
  {
      ReleaseHold[ GetAttributes /@ HoldComplete @ x ]
  };


GetAttributes[ ___ ] := { };



(******************************************************************************)




ListableQ // Attributes = { HoldAllComplete };
ListableQ // Options    = { };

ListableQ[ fun : Function[ expr_ ] ] :=
  And @@ Cases[ fun,
                f_[ ___, _Slot, ___ ] :> ListableQ @ f,
                Infinity,
                Heads -> True
         ];


ListableQ[ f_  ] := GetAttributes @ f ~MemberQ~ Listable;
ListableQ[ ___ ] := False;


NonListableQ // Attributes = { HoldAllComplete };
NonListableQ // Options    = { };

NonListableQ[ f_  ] := GetAttributes @ f ~FreeQ~ Listable;
NonListableQ[ ___ ] := False;



(******************************************************************************)

(* Overrides *)


ListableQ[ FromLetterNumber ] = True;

NonListableQ[ FromLetterNumber ] = False;



(******************************************************************************)



$Holding = HoldFirst | HoldRest | HoldAll | HoldAllComplete;



HoldingQ // Attributes = { HoldAllComplete };
HoldingQ // Options    = { };


HoldingQ[ f_  ] := GetAttributes @ f ~MemberQ~ $Holding;
HoldingQ[ ___ ] := False;



NonHoldingQ // Attributes = { HoldAllComplete };
NonHoldingQ // Options    = { };


NonHoldingQ[ f_  ] := GetAttributes @ f ~FreeQ~ $Holding;
NonHoldingQ[ ___ ] := False;






(*HoldingQ // Attributes = { HoldAll };
HoldingQ // Options    = { };


HoldingQ[ f : (_Symbol | _String) ] :=
  Inline[ $Holding,
          GetAttributes @ f ~MemberQ~ $Holding
  ];


HoldingQ[ ___ ] := False;


NonHoldingQ // Attributes = { HoldAll };
NonHoldingQ // Options    = { };


NonHoldingQ[ f : (_Symbol | _String) ] :=
  Inline[ $Holding,
          GetAttributes @ f ~FreeQ~ $Holding
  ];


NonHoldingQ[ ___      ] := True;*)



(******************************************************************************)



OrderlessQ // Attributes = { HoldAllComplete };
OrderlessQ // Options    = { };

OrderlessQ[ f_  ] := GetAttributes @ f ~MemberQ~ Orderless;
OrderlessQ[ ___ ] := False;


NonOrderlessQ // Attributes = { HoldAllComplete };
NonOrderlessQ // Options    = { };

NonOrderlessQ[ f_  ] := GetAttributes @ f ~FreeQ~ Orderless;
NonOrderlessQ[ ___ ] := False;



(*OrderlessQ // Attributes = { HoldAll };
OrderlessQ // Options    = { };

OrderlessQ[ f_Symbol ] := Attributes @ f ~MemberQ~ Orderless;
OrderlessQ[ ___      ] := False;*)



(******************************************************************************)



FlatQ // Attributes = { HoldAllComplete };
FlatQ // Options    = { };

FlatQ[ f_  ] := GetAttributes @ f ~MemberQ~ Flat;
FlatQ[ ___ ] := False;


NonFlatQ // Attributes = { HoldAllComplete };
NonFlatQ // Options    = { };

NonFlatQ[ f_  ] := GetAttributes @ f ~FreeQ~ Flat;
NonFlatQ[ ___ ] := False;



(******************************************************************************)



OneIdentityQ // Attributes = { HoldAllComplete };
OneIdentityQ // Options    = { };

OneIdentityQ[ f_  ] := GetAttributes @ f ~MemberQ~ OneIdentity;
OneIdentityQ[ ___ ] := False;


NonOneIdentityQ // Attributes = { HoldAllComplete };
NonOneIdentityQ // Options    = { };

NonOneIdentityQ[ f_  ] := GetAttributes @ f ~FreeQ~ OneIdentity;
NonOneIdentityQ[ ___ ] := False;



(******************************************************************************)



HoldNumericQ // Attributes = { HoldAllComplete };
HoldNumericQ // Options    = { };


HoldNumericQ[ x_ /; NumericQ @ Unevaluated @ x ] := True;


HoldNumericQ[ _? Wolfram`CodeEquivalenceUtilities`Types`IntTypeQ ] := True;


HoldNumericQ[ f_[ args___ ] ] :=

  FoldModule @ Module[ { fNumeric, argsNumeric },

      fNumeric = MemberQ[ GetAttributes @ f, NumericFunction ];

      argsNumeric = And @@ HoldNumericQ /@ HoldComplete @ args;

      fNumeric && argsNumeric
  ];



(*HoldNumericQ[ HoldPattern @ TypedSymbol[ _, Verbatim[ _Integer ] ] ] :=
  True;


HoldNumericQ[ HoldPattern @ TypedSymbol[ _, Verbatim[ _Real ] ] ] :=
  True;


HoldNumericQ[ HoldPattern @ TypedSymbol[ _, Verbatim[ _Rational ] ] ] :=
  True;


HoldNumericQ[ HoldPattern @ TypedSymbol[ _, Verbatim[ _Complex ] ] ] :=
  True;*)


HoldNumericQ[ ___ ] := False;



(******************************************************************************)



(* Flatten all f[args] in expression where f has the attribute Flat: *)
CanonicalFlat // Attributes = { };
CanonicalFlat // Options    = { };


CanonicalFlat[ expression_ ] :=
  expression //. HoldPattern @ f_[ a1___, f_ @ a2___, a3___ ] /;
                   Attributes @ f ~MemberQ~ Flat :>
                     f[ a1, a2, a3 ];



(******************************************************************************)



(* Sort the arguments of any Orderless functions: *)
CanonicalOrderless // Attributes = { };
CanonicalOrderless // Options    = { };


sortArgs // ClearAll;
sortArgs // Attributes = { HoldAllComplete };
sortArgs // Options    = { };

sortFmt = Function[ e,
                    { Head @ Unevaluated @ e, HoldComplete @ e },
                    HoldAllComplete ];

sortArgs[ f_[ args___ ] ] :=
  Module[ { formatted, order },
      formatted = sortFmt /@ Unevaluated @ { args };
      order = Ordering @ formatted;
      With[ { sortedArgs = TempHold[ args ][[ order ]] },
          ReplacePart[ HoldApply[ f, sortedArgs ], { 2, 0 } -> List ]
      ]
  ];


CanonicalOrderless[ expression_ ] :=
  Module[ { heldSorted },
      heldSorted = expression //. f_[ a___ ] /; OrderlessQ @ Unevaluated @ f :>
        TrEval @ sortArgs @ f @ a;
      heldSorted //. HoldApply[ f_, { args___ } ] :> f @ args
  ];


(*CanonicalOrderless[ expression_ ] :=

  Module[ { heldSorted },

      heldSorted = expression //.
        (f : Except[ HoldComplete | TempHold, _Symbol ])[ x___ ] /;
          Attributes @ f ~MemberQ~ Orderless :>
            TrEval @ With[ { sorted = Sort @ TempHold @ x },
                           TempHold[ "Apply", f, sorted ]
                     ];

      heldSorted //. TempHold[ "Apply", f_, TempHold @ args___ ] :> f @ args
  ];*)



(******************************************************************************)



CanonicalOneIdentity // Attributes = { };
CanonicalOneIdentity // Options    = { };


CanonicalOneIdentity[ expression_ ] :=
  expression //.
    HoldPattern @ f_[ x : Except[ _Symbol, _? UAtomQ ] ] /;
      Attributes @ f ~MemberQ~ OneIdentity :>
        x;



(******************************************************************************)



(* Puts expressions into a canonical form while preventing evaluation leaks: *)
CanonicalTransformFromAttributes // Attributes = { };
CanonicalTransformFromAttributes // Options    = { };


CanonicalTransformFromAttributes[ expression_ ] :=
  CanonicalOrderless @ CanonicalFlat @ CanonicalOneIdentity @ expression;



(******************************************************************************)



End[]; (* `Private` *)

EndPackage[];

(* :!CodeAnalysis::EndBlock:: *)

Wolfram`CodeEquivalenceUtilities`Debugging`$DebugLoad;
