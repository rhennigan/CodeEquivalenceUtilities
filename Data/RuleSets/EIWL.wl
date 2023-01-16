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
    "Usage" -> { "EquivalenceTesting" }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
Inline[ { $intType }, HoldComplete[

    Import[ url_? urlQ, element: "Images"|"Hyperlinks" ] :>
      Import[ url, { "HTML", element } ]
    ,

    url_String? urlQ :> WithHolding[
        {
            expanded = FixedPoint[ expandURL, url, 5 ],
            urlData = KeySort @ Join[ URLParse @ expanded,
                                      <|
                                          "Scheme"   -> "https",
                                          "User"     -> None,
                                          "Port"     -> None,
                                          "Fragment" -> None
                                      |>
                                ]
        },
        URLBuild @ urlData
    ]
    ,

    Entity[ "HistoricalSite", "GreatPyramidOfGiza::kbgx6" ][ "Image"|EntityProperty[ "HistoricalSite", "Image" ] ] :>
      Entity[ "Building", "GreatPyramidOfGiza::jbm66" ][ EntityProperty[ "Building", "Image" ] ]
    ,

    EntityValue[
        Entity[ "HistoricalSite", "GreatPyramidOfGiza::kbgx6" ],
        EntityProperty[ "HistoricalSite", "Image" ]
    ] :>
      EntityValue[
          Entity[ "Building", "GreatPyramidOfGiza::jbm66" ],
          EntityProperty[ "Building", "Image" ]
      ]
    ,


    CountryData[ "G5" ] :> EntityList @ EntityClass[ "Country", "GroupOf5" ],

    Column[ c_ ] :> Column[ c, ItemSize -> { Automatic, Automatic } ]
    ,

    Circle[ pt_ ] :> Circle[ pt, 1 ]
    ,

    CurrencyConvert[ a_, Quantity[ b_, "USDollars" ] ] :>
      UnitConvert[ a, Quantity[ b, "USDollars" ] ]
    ,
    Style[ s_String, a___, n_? IntTypeQ, b___ ] :>
      Style[ s, a, FontSize -> n, b ]
    ,

    FromLetterNumber[ Table[ expr_, iter_ ] ] :>
      Table[ FromLetterNumber @ expr, iter ]
    ,

    Style[ t_? IntTypeQ, Plain ] :> t
    ,
    UnitConvert[ q_, unit_String ] :> UnitConvert[ q, Quantity @ unit ]
    ,
    Style[ a_, b___, n_? IntTypeQ, c___ ] :> Style[ a, b, FontSize -> n, c ]
    ,
    Round[ N[ a_ ], 1 ] :> Round[ a, 1 ]
    ,
    Round[ a_ ] :> Round[ a, 1 ]
    ,
    {items__}[[RandomValue[DiscreteUniformDistribution[{1, n_?IntTypeQ}]]]] /; (HoldComplete[#] === HoldComplete[n] &[Length[HoldComplete[items]]]) :>
      RandomChoice[{items}]
    ,
    Style[ a_, args__ ] /; ! OrderedQ[Unevaluated[{args}]] :>
      WithHolding[
          {
              sorted = Sort[ TempHold[ args ] ]
          },
          Style[ a, sorted ]
      ]
    ,
    Alphabet[ ][[ n_? IntTypeQ ]] :> FromLetterNumber[ n ]
    ,
    Alphabet[ ] :> CharacterRange[ "a", "z" ]
    ,
    ToUpperCase[ CharacterRange[ a_String, b_String ] ] :>
      WithHolding[ { au = ToUpperCase[ a ], bu = ToUpperCase[ b ] },
                   CharacterRange[ au, bu ]
      ]
    ,
    CharacterRange[ "a", "z" ][[ n_? IntTypeQ ]] :> FromLetterNumber[ n ]
    ,
    Quantity[ 7, "Days" ] :> Quantity[ 1, "Weeks" ]
    ,
    x_?IntTypeQ * RandomValue[DiscreteUniformDistribution[{0, y_}]] :>
      RandomValue[DiscreteUniformDistribution[{0, x * y}]]
    ,
    x_?HoldNumericQ * RandomValue[UniformDistribution[{0, y_}]] :>
      RandomValue[UniformDistribution[{0, x * y}]]
    ,
    360 * Degree :> 2 * Pi
    ,
    LetterNumber /@ Characters[ x_ ] :> LetterNumber[ x ]
    ,
    LetterNumber[ Characters[ x_ ] ] :> LetterNumber[ x ]
    ,
    HoldPattern[
        (f: FormFunction|FormPage|APIFunction)[
            { a___, (Rule|RuleDelayed)[ x_String, t_ ], b___ },
            body_,
            rest___
        ]
    ] /; ! LocalContextQ @ x :> WithHolding[
        {
            xx = NewLocalSymbol[ ],
            xn = StringArg @ xx,
            body2 = TempHold @ body /. x -> xn
        },
        f[ { a, xn -> t, b }, body2, rest ]
    ]
    ,
    Delayed[ (f:FormFunction|FormPage)[ args___ ] ] :> f[ args ]
    ,
    WordList[ ] :> WordList[ Language -> "English" ]
    ,
    Style[ a_ ] :> a
    ,
    Quantity[u_String] :> Quantity[1, u]
    ,
(*
    BarChart[ t : Table[ _? HoldNumericQ, { __? IntTypeQ } ] ] :>
      WithHolding[ { list = ReleaseHold @ CodeEquivalenceUtilities`FromCanonicalForm @ Hold @t },
                   BarChart @ list
      ]*)

    (* Hardcoded Overrides: *)
    CharacterRange["A", "Z"][[ n_ ]] :>
      ToUpperCase[FromLetterNumber[n]]
    ,
    Length[CharacterRange["a", "z"]] :> 26
    ,
    Length[CharacterRange["A", "Z"]] :> 26
    ,
    ToUpperCase[ ToUpperCase[ arg_ ] ] :> ToUpperCase[ arg ]
    ,
    DateObject[ d_DateObject ] :> d
    ,


    (* x19.1 *)
    UnitConvert[-DateObject[{1900, 1, 1}] +
      DateObject[DateList[][[1 ;; 3]]], Quantity[1, "Weeks"]] :>
      UnitConvert[DateObject[DateList[]] - DateObject[{1900, 1, 1}],
          Quantity[1, "Weeks"]]
    ,
    N[UnitConvert[DateObject[DateList[]] - DateObject[{1900, 1, 1}],
        Quantity[1, "Weeks"]]] :>
      UnitConvert[DateObject[DateList[]] - DateObject[{1900, 1, 1}],
          Quantity[1, "Weeks"]]
    ,
    DateObject[DateObject[DateList[][[1 ;; 3]]] - Quantity[1, "Weeks"]] :>
      DateObject[DateList[][[1 ;; 3]]] - Quantity[1, "Weeks"]
    ,

    (* 19.11 *)
    AirTemperatureData[
        Entity["Building",
            "EiffelTower::5h9w8"], {DateObject[DateList[]] -
      Quantity[1, "Weeks"], DateObject[DateList[]]}] :>
      AirTemperatureData[
          Entity["Building",
              "EiffelTower::5h9w8"], {DateObject[DateList[][[1 ;; 3]]] -
        Quantity[1, "Weeks"], DateObject[DateList[][[1 ;; 3]]]}]
    ,
    DatePlus[DateObject[d_], q_] :> DateObject[d] + q
    ,
    AirTemperatureData[
        Entity["Building",
            "EiffelTower::5h9w8"], {DateObject[DateList[][[1 ;; 3]]] -
      Quantity[1, "Weeks"], DateObject[DateList[]]}] :>
      AirTemperatureData[
          Entity["Building",
              "EiffelTower::5h9w8"], {DateObject[DateList[][[1 ;; 3]]] -
        Quantity[1, "Weeks"], DateObject[DateList[][[1 ;; 3]]]}]
    ,


    (* 35.7 *)
    Permutations[{"l", "i", "m", "a"}] :> Permutations[{"a", "i", "l", "m"}]
    ,
    Cases[(Interpreter["City"][#1] &) /@
      StringJoin /@ Permutations[{"a", "i", "l", "m"}], _Entity, {1}] :>
      Cases[Interpreter["City"][
          StringJoin /@ Permutations[{"a", "i", "l", "m"}]], _Entity, {1}]

    ,

    WikipediaData[ str_String? UStringQ ] :>
        With[ { lower = ToLowerCase @ str },
            WikipediaData @ lower /; lower =!= str
        ]
] ]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)