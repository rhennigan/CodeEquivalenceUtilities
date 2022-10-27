(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::BadSymbol::SymbolQ:: *)

BeginPackage[ "Wolfram`CodeEquivalenceUtilities`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Defined symbols*)
$Q;
$R;
$S;
$Z;
Algebraic;
CastAllLiterals;
CreateTypeClass;
EnumerableQ;
EnumerableTypeQ;
IntTypeQ;
IteratorType;
IteratorTypePrecedence;
KnownTypeQ;
ListType;
ListTypeQ;
LiteralQ;
MakeTypeTransformation;
NewInheritedTypedFunction;
NumericUnionType;
StringTypeQ;
T;
ToTypedBinding;
ToTypedExpression;
TransformAndPostProcess;
Type;
TypeCast;
TypedDefinitionQ;
TypedExpression;
TypedLiteral;
TypedQ;
TypedSymbol;
Typeof;
UnionType;
UnknownTypeQ;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Declarations*)
ListableQ;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Private*)
Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Type*)
Type // Attributes = { };
Type // Options    = { };


Type[ t_Type ] := t;


Type[ type_ ][ s_Symbol   ] :=  TypedSymbol[ s, type ];


Type[ type_ ][ s_? UAtomQ ] := TypedLiteral[ s, type ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreateTypeClass*)
(* TODO: remove string naming due to pattern conflicts *)

CreateTypeClass // ClearAll;
CreateTypeClass // Attributes = { HoldFirst };

CreateTypeClass[ symbol_ ] := (

    symbol // Attributes = { HoldAllComplete };

    symbol[ i_, Type[ type_ ] ] := symbol[ i, type ];

    symbol[ Unevaluated[ i_ ], type_ ] := symbol[ i, type ];

(*    symbol[ i_Symbol, type_ ] :=
      With[ { i$ = ToString[ Unevaluated @ i, InputForm ] },
          symbol[ i$, type ]
      ];*)

    symbol[t : symbol[_, _], type_] := t;

    symbol

);


CreateTypeClass @ TypedSymbol;
CreateTypeClass @ TypedLiteral;
CreateTypeClass @ TypedExpression;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*UnionType*)
UnionType // Attributes = { OneIdentity };
UnionType // Options    = { };



NumericUnionType // Attributes = { OneIdentity };
NumericUnionType // Options    = { };



Algebraic // Attributes = { OneIdentity };
Algebraic // Options    = { };


Algebraic[ f_, t1___, Type[ t_ ], t2___ ] := Algebraic[ f, t1, t, t2 ];


Algebraic[ f_, t_ ] /; MemberQ[ Attributes @ f, OneIdentity ] := Type @ t;


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedExpression*)
TypedExpression // Attributes = { HoldAllComplete };
TypedExpression[ te_TypedExpression ] := te;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedSymbol*)
TypedSymbol /: Normal @ HoldPattern @ TypedSymbol[ s_String, _ ] :=
  Symbol @ s;

TypedSymbol /: Normal @ HoldPattern @ TypedSymbol[ s_, _ ] :=
  s;

TypedLiteral /: Normal @ HoldPattern @ TypedLiteral[ s_String, String ] :=
  s;

TypedLiteral /: Normal @ HoldPattern @ TypedLiteral[ s_String, _ ] :=
  ToExpression @ s;

TypedExpression /: Normal @ TypedExpression[ exp_ ] :=
  HoldComplete[ exp ] //. TypedExpression[ e_ ] :> e //.
    t : (_TypedSymbol | _TypedLiteral) :> TrEval @ Normal @ t;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedLiteral*)
TypedLiteral /:
  HoldPattern @ NumericQ @ TypedLiteral[ s_String, _ ] :=
    ToExpression[ s, StandardForm, UNumericQ ];


TypedSymbol /:
  HoldPattern @ NumericQ @ TypedSymbol[ s_, Verbatim @ _Integer ] := True;


TypedSymbol /:
  HoldPattern @ NumericQ @ TypedSymbol[ s_, Verbatim @ _Real ] := True;


TypedSymbol /:
  HoldPattern @ NumericQ @ TypedSymbol[ s_, Verbatim @ _Rational ] := True;


TypedSymbol /:
  HoldPattern @ NumericQ @ TypedSymbol[ s_, Verbatim @ _Complex ] := True;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*IntTypeQ*)
IntTypeQ // Attributes = { HoldAllComplete };


Wolfram`CodeEquivalenceUtilities`RandomValue;
Wolfram`CodeEquivalenceUtilities`HoldNumericQ;


IntTypeQ[ x_Integer ] := IntegerQ @ Unevaluated @ x;
IntTypeQ[ TypedSymbol[ _, Verbatim[ _Integer ] ] ] := True;
IntTypeQ[ RandomValue[ _DiscreteUniformDistribution ] ] := True;


(* Arithmetic *)
IntTypeQ[ (Plus | Times | Subtract)[ __? IntTypeQ ] ] := True;
IntTypeQ[ Power[ _? IntTypeQ, _Integer? Positive ] ] := True;
IntTypeQ[ Floor[ _? HoldNumericQ ] ] := True;
IntTypeQ[ Ceiling[ _? HoldNumericQ ] ] := True;
IntTypeQ[ Round[ _? HoldNumericQ ] ] := True;

(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* Misc *)
IntTypeQ[ Echo[ _? IntTypeQ ] ] := True;
IntTypeQ[ Echo[ _? IntTypeQ, ___ ] ] := True;
IntTypeQ[ Identity[ _? IntTypeQ ] ] := True;
IntTypeQ[ StringLength[ _? StringTypeQ ] ] := True;
IntTypeQ[ Length[ Characters[ _? StringTypeQ ] ] ] := True;

(* Default *)
IntTypeQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*RealTypeQ*)
RealTypeQ // Attributes = { HoldAllComplete };

RealTypeQ[ x_Real ] := AtomQ @ Unevaluated @ x;
RealTypeQ[ TypedSymbol[ _, Verbatim[ _Real ] ] ] := True;
RealTypeQ[ RandomValue[ _UniformDistribution ] ] := True;

(* Arithmetic *)
RealTypeQ[ (Plus|Times|Subtract)[ __? RealTypeQ ] ] := True;
(* TODO: this should work when mixed with integers too *)

(* Misc *)
RealTypeQ[ Echo[ _? RealTypeQ ]      ] := True;
RealTypeQ[ Echo[ _? RealTypeQ, ___ ] ] := True;
RealTypeQ[ Identity[ _? RealTypeQ ]  ] := True;

(* Default *)
RealTypeQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*StringTypeQ*)
StringTypeQ // Attributes = { HoldAllComplete };

StringTypeQ[ _String? UStringQ ] := True;
StringTypeQ[ TypedSymbol[ _, Verbatim[ _String ] ] ] := True;

(* String Operations *)
StringTypeQ[ StringJoin[  ___? StringTypeQ    ] ] := True;
StringTypeQ[ StringTake[    _? StringTypeQ, _ ] ] := True;
StringTypeQ[ StringPart[    _? StringTypeQ, _ ] ] := True;
StringTypeQ[ StringReplace[ _? StringTypeQ, _ ] ] := True;

(* From Lists *)
StringTypeQ[ RandomChoice[ { __? StringTypeQ } ] ] := True;
StringTypeQ[ First[ { __? StringTypeQ } ] ] := True;
StringTypeQ[ Last[ { __? StringTypeQ } ] ] := True;
StringTypeQ[ Part[ { __? StringTypeQ }, _? IntTypeQ ] ] := True;

(* Misc *)
StringTypeQ[ Echo[     _? StringTypeQ ] ] := True;
StringTypeQ[ Identity[ _? StringTypeQ ] ] := True;
StringTypeQ[ WikipediaData[ _? StringTypeQ ] ] := True;

(* Default *)
StringTypeQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ListTypeQ*)
ListTypeQ // Attributes = { HoldAllComplete };

ListTypeQ[ list_ ] := ListTypeQ[ list, Except[ None ] ];
ListTypeQ[ list_, type_ ] := MatchQ[ ListType @ list, type ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ListType*)
ListType // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Literal Lists*)
ListType[ { __? IntTypeQ    } ] := Integer;
ListType[ { __? StringTypeQ } ] := String;
ListType[ { __? RealTypeQ   } ] := Real;
ListType[ { ___             } ] := Undefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Expected List Output*)
ListType[ Table[ _? IntTypeQ   , __ ] ] := Integer;
ListType[ Table[ _? StringTypeQ, __ ] ] := String;
ListType[ Table[ _? RealTypeQ  , __ ] ] := Real;
ListType[ Table[ _             , __ ] ] := Undefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*String to StringList Functions*)
ListType[ WordList[ _? StringTypeQ ] ] := String;
ListType[ Characters[ _? StringTypeQ ] ] := String;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*TextCases*)
ListType[ TextCases[ _? StringTypeQ, _? StringTypeQ ] ] := String;

ListType[
    TextCases[
        _? StringTypeQ,
        Verbatim[ Alternatives ][ __? StringTypeQ ]
    ]
] :=
    String;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(* Recursive Definitions *)
ListType[ Part[ list_, _Span ] ] := ListType @ list;
ListType[ Echo[ list_, ___   ] ] := ListType @ list;
ListType[ Identity[ list_    ] ] := ListType @ list;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(* Listable Functions *)
ListType[ (_? ListableQ)[ _? ListTypeQ ] ] := Undefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(* Default *)
ListType[ ___ ] := None;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Typeof*)
Typeof // Attributes = { HoldAll };

Typeof[  TypedSymbol[ _, type_ ] ] := Type @ type;
Typeof[ TypedLiteral[ _, type_ ] ] := Type @ type;

Typeof[ (f_)[ args___? EnumerableQ ] ] /;
  MemberQ[ Attributes @ f, NumericFunction ] :=
    With[ { argTypes = Typeof /@ Hold @ args // ReleaseHold // List // Union },
        Type @ Algebraic[ NumericUnionType, Sequence @@ argTypes ]
    ];

Typeof[ val : Except[ _Symbol ] /; UAtomQ @ val ] :=
  Type @ Head @ Unevaluated @ val;

(* TODO: Test for other types here *)

Typeof[ _Symbol ] := Type @ Undefined;
Typeof[ _TypedExpression ] := Type @ Undefined;

(* Catch remaining *)
Typeof[ arg_   ] := Type @ Head @ Unevaluated @ arg;
Typeof[ arg___ ] := Type @ Sequence;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*KnownTypeQ*)
KnownTypeQ // Attributes = { HoldAllComplete };
KnownTypeQ // Options    = { };


KnownTypeQ[ s_ ] := Typeof[ s ] =!= Type[ Undefined ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*UnknownTypeQ*)
UnknownTypeQ // Attributes = { HoldAllComplete };
UnknownTypeQ // Options    = { };


UnknownTypeQ[ s_ ] := Typeof[ s ] === Type[ Undefined ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*LiteralQ*)
LiteralQ // Attributes = { HoldAll };
LiteralQ // Options    = { };


LiteralQ[ HoldPattern @ Times[ _Integer, Power[ _Integer, -1 ] ] ] := True;


LiteralQ[ exp : Except @ _Symbol ] := UAtomQ @ exp;


LiteralQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*EnumerableTypeQ*)
$classType = TypedSymbol | TypedLiteral;
$enumType  = Integer | Real | Rational;
$enumPType = Verbatim[ Blank ] /@ $enumType;



EnumerableTypeQ // Attributes = { };
EnumerableTypeQ // Options    = { };


EnumerableTypeQ[ $enumType  ] := Inline[ $enumType , True ];
EnumerableTypeQ[ $enumPType ] := Inline[ $enumPType, True ];


EnumerableTypeQ[ HoldPattern @ Algebraic[ f_, t__? EnumerableTypeQ ] ] /;
  MemberQ[ Attributes @ f, NumericFunction ] :=
    True;


EnumerableTypeQ[ HoldPattern @ Algebraic[ UnionType, t__? EnumerableTypeQ ] ] /;
  MemberQ[ Attributes @ f, NumericFunction ] :=
    True;


EnumerableTypeQ[ HoldPattern @ Algebraic[ NumericUnionType, ___ ] ] := True;


EnumerableTypeQ[ Type[ t_ ] ] := EnumerableTypeQ @ t;


EnumerableTypeQ[ ___ ] := False;


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*EnumerableQ*)
EnumerableQ // Attributes = { };
EnumerableQ // Options    = { };


EnumerableQ[ $classType[ _, $enumType ] | $classType[ _, $enumPType ] ] :=
  Inline[ { $classType, $enumType, $enumPType }, True ];


EnumerableQ[ HoldPattern @ (f_)[ args___? EnumerableQ ] ] /;
  MemberQ[ Attributes @ f, NumericFunction ] := True;


EnumerableQ[ HoldPattern @
  $classType[ _, Algebraic[ f_, t___? EnumerableTypeQ ] ] ] /;
    MemberQ[ Attributes @ f, NumericFunction ] :=
      Inline[ $classType, True ];


EnumerableQ[ x : Blank /@ $enumType ] := Inline[ $enumType, True ];


EnumerableQ[ ___ ] := False;


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*IteratorTypePrecedence*)
IteratorTypePrecedence // Attributes = { };
IteratorTypePrecedence // Options    = { };


Inline[ $classType,
    IteratorTypePrecedence[ $classType[ _, Symbol   ] | _Symbol   ] := 1;
    IteratorTypePrecedence[ $classType[ _, Integer  ] | _Integer  ] := 1;
    IteratorTypePrecedence[ $classType[ _, Rational ] | _Rational ] := 1;
    IteratorTypePrecedence[ $classType[ _, Real     ] | _Real     ] := 2;
];


(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*IteratorType*)
IteratorType // Attributes = { HoldFirst };
IteratorType // Options    = { };

IteratorType::form = "Iterator types are undefined for Tables that are not " <>
                     "in canonical form.";


$heldQ = HoldPattern @ Times[ _, Power[ _, -1 | TypedLiteral[ -1, Integer ] ] ];

$R = $classType[ _, Real     ] | _Real;
$Z = $classType[ _, Integer  ] | _Integer;
$Q = $classType[ _, Rational ] | _Rational | $heldQ ;
$S = $classType[ _, Symbol   ] | _Symbol;


Inline[ { $R, $Z, $Q },
    IteratorType[ { _, $R,  _,  _ } ] := Type @ Real;

    IteratorType[ { _,  _,  _, $R } ] := Type @ Real;

    IteratorType[ { _, $Z,  _, $Z } ] := Type @ Integer;

    IteratorType[ { _, $Z,  _, $Q } ] :=
      Type @ Algebraic[ NumericUnionType, Integer, Rational ];

    IteratorType[ { _, $Q,  _, $Z } ] :=
      Type @ Algebraic[ NumericUnionType, Integer, Rational ];

    IteratorType[ { _, $Q,  _, $Q } ] :=
      Type @ Algebraic[ NumericUnionType, Integer, Rational ];
];


IteratorType[ { _, iter__? EnumerableQ } ] :=
  Type[ Algebraic[ NumericUnionType, Sequence @@ Union @ { ## } ] & @@
    First /@ Typeof /@ MaximalBy[ { iter }, IteratorTypePrecedence ]
  ];


IteratorType[ { _, { iter__? EnumerableQ } } ] :=
  Type[ Algebraic[ NumericUnionType, Sequence @@ Union @ { ## } ] & @@
    First /@ Typeof /@ MaximalBy[ { iter }, IteratorTypePrecedence ]
  ];


(*IteratorType[ { iter_ } ] := Typeof @ iter;*)


(*IteratorType[ iter : Except[ _List | _Table ] ] := Type @ Null;*)


IteratorType[ HoldPattern @ Table[ _, iter_ ] ] := IteratorType @ iter;


IteratorType[ HoldPattern @ Table[ _, _, __ ] ] := Null /; (
    Message[ IteratorType::form ];
    False
);


IteratorType[ ___ ] := Type @ Undefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToTypedExpression*)
ToTypedExpression // Attributes = { HoldAllComplete };


ToTypedExpression[ s_Symbol ] :=
  With[ { type = Typeof[ s ] },
      TypedSymbol[ s, type ]
  ];


ToTypedExpression[ s_ /; UAtomQ[ s ] ] :=
  With[ { type = Typeof[ s ] },
      TypedLiteral[ s, type ]
  ];


ToTypedExpression[ t_TypedSymbol  ] := t;


ToTypedExpression[ t_TypedLiteral ] := t;


ToTypedExpression[ HoldPattern @ Times[ n_Integer, Power[ d_Integer, -1 ] ] ] :=
  With[ { r = n / d },
      ToTypedExpression @ r /;
        Hold @ r =!= Hold @ Times[ n, Power[ d, -1 ] ]
  ];


ToTypedExpression[ (f_)[ args___? EnumerableQ ] ] /;
  MemberQ[ Attributes @ f, NumericFunction ] :=
    TypedExpression[ f[ args ],
                     Type @ Algebraic[ NumericUnionType, args ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*T*)
T // Attributes = { HoldAllComplete };

T[ s_ ] := ToTypedExpression @ s;

T[ s_, t_ ] := t @ s;

T[ expr_, HoldPattern[ s_ -> t_ ] ] :=
  TypedExpression @ expr /.
    With[ { typedS = t @ s },
        HoldPattern @ s :> typedS
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*MakeTypeTransformation*)
MakeTypeTransformation // Attributes = { HoldAll };

MakeTypeTransformation[ HoldPattern[ patt_ :> With[ { v__ }, exp_ ] ] ] :=

  Module[ { assignments, replacements, spec },

      assignments = Hold @ v;

      replacements = Cases[ assignments,
          HoldPattern[ s_ = e_ ] :> HoldPattern @ s :> e,
          Infinity
      ];

      spec = TypedExpression[ exp ] /. replacements;

      With[ { spec$ = spec },
          Hold[ MakeTypeTransformation[ patt ~rd~ spec$ ] ] /.
            rd -> RuleDelayed //
            ReleaseHold
      ]
  ];

MakeTypeTransformation[ (patt_ :> spec_) ] :=
  With[ {
      epatt = HoldPattern @ patt /.
        HoldPattern @ Verbatim[ Blank ][ head___ ] :>
          Except[ _TypedExpression, Blank @ head ]
  },
      With[ { rule = HoldPattern @ epatt :> spec },
          expression : patt :>
            TrEval[ TypedExpression @ expression /. rule /. t_T :> TrEval @ t ]
      ]
  ];

MakeTypeTransformation[ patt_, spec_ ] :=
  MakeTypeTransformation[ patt :> spec ];

MakeTypeTransformation[ { patterns__ } ] :=
  MakeTypeTransformation[ Hold[ patterns ] ];

$evalTypes = _TypedSymbol | _TypedLiteral | _? UAtomQ;

MakeTypeTransformation[ Hold[ patterns__ ] ] :=

  Module[ { transformations, cleanup, fulltransformations },

      transformations =
        Cases[ Hold @ patterns,
            HoldPattern[ patt_ :> spec_ ] :>
              MakeTypeTransformation[ HoldPattern @ patt :> spec ]
        ];

      cleanup =
        With[ { evalTypes = $evalTypes },
            {
            (*                Proportion[ s : evalTypes, Typeof @ s_ ] :> s,

                Proportion[ s_, t : HoldPattern @ Typeof[ evalTypes ] ] :>
                  TrEval @ Proportion[ s, t ],

                p : Proportion[ _, evalTypes ] :> TrEval @ p,*)

                TypedExpression[ t_ ] :> t
            }
        ];

      fulltransformations = transformations ~Join~ cleanup;

      Dispatch @ fulltransformations
  ];


castLiterals[transformed_] :=
  Module[{pos, rules},
      pos = Position[
          transformed /. (_TypedSymbol | _TypedLiteral) -> Null[], _?
            UAtomQ, Heads -> False];
      rules = Thread[pos -> Extract[transformed, pos, ToTypedExpression]];
      ReplacePart[transformed, rules]
  ];


TransformAndPostProcess[ transformations_, expression_ ] :=
  Module[ { cleanupRules, fp },

      cleanupRules =
        With[ { evalTypes = _TypedSymbol | _TypedLiteral | _? UAtomQ },
            {
                Proportion[ s : evalTypes, Typeof @ s_ ] :> s,

                Proportion[ s_, t : HoldPattern @ Typeof @ evalTypes ] :>
                  TrEval @ Proportion[ s, t ],

                p : Proportion[ _, evalTypes ] :> TrEval @ p,

                HoldPattern @
                  Proportion[ s : TypedLiteral[ _, t_ ], Type[ t_ ] ] :> s,

                HoldPattern @
                  Proportion[ s : TypedSymbol[ _, t_ ], Type[ t_ ] ] :> s,

                TypedExpression[ t_ ] :> t,

                Verbatim[HoldPattern][ t : (_TypedSymbol | _TypedLiteral) ] :> t
            }
        ];

      fp = FixedPoint[ # /. transformations //. cleanupRules &, expression ];

      castLiterals @ fp

  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypeCast*)
TypeCast // Attributes = { HoldAll };
TypeCast // SyntaxInformation =
  {
      "ArgumentsPattern" -> { _, _ },
      "LocalVariables"   -> { "Limit", { 2, 2 } }
  };

TypeCast[ s_Symbol, type_Type ] := TypedSymbol[ s, type ];

TypeCast[ s_ /; UAtomQ[ s ], type_Type ] := TypedLiteral[ s, type ];

TypeCast[ s_, type_ ] :=
  With[ { t = type },
      TypeCast[ s, t ] /; Head[ t ] === Type
  ];

TypeCast[ expression_, s_ -> type_Type ] :=
  TypedExpression @ expression /.
    HoldPattern @ s :> TrEval @ TypeCast[ s, type ];

TypeCast[ expression_, s_ -> type_ ] :=
  With[ { t = type },
      TypeCast[ expression, s -> t ] /; Head[ t ] === Type
  ];

TypeCast[ expression_, { x : HoldPattern[ _ -> _ ], xs___ } ] :=
  TypeCast[ TypeCast[ expression, { xs } ], x ];

TypeCast[ expression_, {} ] := TypedExpression @ expression;

TypedExpression[ tc_TypeCast ] := tc;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedDefinitionQ*)
TypedDefinitionQ // ClearAll;
TypedDefinitionQ // Attributes = {HoldAllComplete};
TypedDefinitionQ // Options = {};


TypedDefinitionQ[(RuleDelayed | SetDelayed)[patt_, _]] :=
    FreeQ[HoldComplete[patt] /. _TypedSymbol -> Null, _Pattern];


TypedDefinitionQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TypedQ*)
TypedQ // Attributes = { HoldAllComplete };

TypedQ[ s_TypedSymbol     ] := True;
TypedQ[ s_TypedLiteral    ] := True;
TypedQ[ s_TypedExpression ] := True;
TypedQ[ ___               ] := False;


UntypedQ // Attributes = { HoldAllComplete };

UntypedQ[ s_ ] := ! TypedQ @ s;



(******************************************************************************)



patternToType // ClearAll;
patternToType // Attributes = { };
patternToType // Options    = { };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::ParameterError:: *)
patternToType[ container_ ] :=
  HoldPattern @ Verbatim[ Pattern ][ n_, Verbatim[ Blank ][ h_ ] ] :>
    container[ Pattern[ n, Blank[ ] ], h ];



generalizePattern // ClearAll;
generalizePattern // Attributes = { HoldAll };
generalizePattern // Options    = { };


generalizePattern[ RuleDelayed[ patt0_, exp_ ] ] :=

  Inline[ patternToType,

      Module[ { patt, alt1, alt2 },

          patt = HoldPattern @ patt0;

          alt1 = patt /. patternToType @ TypedLiteral;
          alt2 = patt /. patternToType @ TypedSymbol;

          DeleteDuplicates @ { alt1 :> exp,
                               alt2 :> exp,
                               patt :> exp }
      ]
  ];



generalizeDownValues // ClearAll;
generalizeDownValues // Attributes = { HoldAll };
generalizeDownValues // Options    = { };


generalizeDownValues[ f_ ] :=
  Flatten[ generalizePattern /@ DownValues @ f ] //.
    Verbatim[ HoldPattern ][ h_HoldPattern ] :> h;



(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*NewInheritedTypedFunction*)
NewInheritedTypedFunction // Attributes = { HoldAll };
NewInheritedTypedFunction // Options    = { };


NewInheritedTypedFunction[ new_, old_ ] := (
    Attributes @ new = Attributes @ old;
    Options    @ new = Options    @ old;

    DownValues @ new = generalizeDownValues @ old //.
                         HoldPattern @ old -> new
);

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CastAllLiterals*)
CastAllLiterals // Attributes = { };
CastAllLiterals // Options    = { };


CastAllLiterals[ expression_ ] :=
  TypedExpression @@ List @
    ReplaceMasked[ expression,
                   s_? LiteralQ :> ToTypedExpression @ s,
                   All,
                   Evaluate -> True,
                   Masking -> { _? TypedQ }
    ];



(******************************************************************************)



(* Make sure numeric constants in System` are treated as literal values *)
defLiteralSymbol[ str_String ] :=
  With[ { s = ToExpression @ str },
      ToTypedExpression[ s ] = TypedLiteral[ s, Symbol ];
      LiteralQ[ s ] = True;
      Typeof[ s ] = Symbol;
      EnumerableQ[ TypedLiteral[ str, Symbol ] ] = True;
      EnumerableQ[ s ] = True;
  ];

Scan[ defLiteralSymbol,
    Select[ Names[ "System`*" ],
        ToExpression[ #, StandardForm, UNumericQ ] &
    ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToTypedBinding*)
Quiet[

toTypedPatterns // ClearAll;
toTypedPatterns // Attributes = {HoldAll};
toTypedPatterns // Options = {"Messages" -> False};


toTypedPatterns[Verbatim[Pattern][s_, t_]] :=
  TypedSymbol[s_, Verbatim[t]];


ToTypedBinding // ClearAll;
ToTypedBinding // Attributes = {HoldAllComplete};
ToTypedBinding // Options = {};

ToTypedBinding[bind_[patt_, defn_]] :=
    Quiet[Module[{newPatt, dRepl, newDefn},
      newPatt =
          HoldComplete[patt] /. p_Pattern :> TrEval@toTypedPatterns[p];

      dRepl =
        Cases[HoldComplete[patt],
          Verbatim[Pattern][s_, t_] :> HoldPattern[s] :> TypedSymbol[s, t],
          Infinity
        ];

      newDefn = HoldComplete[defn] /. dRepl;

      Delete[
        bind @@@ TempHold @@ {{newPatt, newDefn}}, {{1, 1, 0}, {1, 2, 0}}]
    ], RuleDelayed::rhs];

    ,
    RuleDelayed::rhs
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package footer*)
End[ ];
EndPackage[ ];
(* :!CodeAnalysis::EndBlock:: *)