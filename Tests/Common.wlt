(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
Quiet[ ResourceObject, General::shdw ];
Needs[ "Wolfram`CodeEquivalenceUtilities`" ];

$CachePersistence = "Session";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Tests*)
VerificationTest[
  Block[{f, g, h, x, y, z}, HoldPattern[f[x_]] := g[x + h[x]]; HoldPattern[g[x_]] := Replace[x, y_Integer :> y + 1]; y = "Hello world"; HoldPattern[h[x_]] := f[x][x]; HoldPattern[f[x_][y_]] := h[g[x + y]]; g /: HoldPattern[h[x_g]] := Replace[x, z_Integer :> z^2]; z /: HoldPattern[Verbatim[Pattern][z, Verbatim[Blank][Integer]]] := z_Real; Attributes[f] = {HoldFirst}; f::message = "This is a message"; MinimalFullDefinition[f]],
  Language`DefinitionList[HoldForm[f] -> {SubValues -> {HoldPattern[HoldPattern[f[x_][y_]]] :> h[g[x + y]]}, DownValues -> {HoldPattern[HoldPattern[f[x_]]] :> g[x + h[x]]}, Messages -> {HoldPattern[f::message] -> "This is a message"}, Attributes -> {HoldFirst}}, HoldForm[g] -> {UpValues -> {HoldPattern[HoldPattern[h[x_g]]] :> Replace[x, z_Integer :> z^2]}, DownValues -> {HoldPattern[HoldPattern[g[x_]]] :> Replace[x, y_Integer :> y + 1]}}, HoldForm[h] -> {DownValues -> {HoldPattern[HoldPattern[h[x_]]] :> f[x][x]}}],
  TestID -> "Untitled-2@@Tests/Common.wlt:12,1-16,2"
];
