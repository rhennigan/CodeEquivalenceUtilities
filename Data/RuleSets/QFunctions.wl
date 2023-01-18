(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::TopLevelAssociation:: *)
(* :!CodeAnalysis::Disable::SymbolVersionTooNew:: *)

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
(*Definitions*)

$booleanFunctions1 = HoldPattern @ Alternatives[
    AcyclicGraphQ, AlgebraicIntegerQ, AlgebraicUnitQ, \
    AntihermitianMatrixQ, AntisymmetricMatrixQ, ArrayQ, AssociationQ, \
    AtomQ, AudioQ, BinaryImageQ, BioSequenceQ, BipartiteGraphQ, BooleanQ, \
    BoundaryMeshRegionQ, BoundedRegionQ, BusinessDayQ, ByteArrayQ, \
    ColorQ, CompatibleUnitQ, CompleteGraphQ, CompositeQ, ConnectedGraphQ, \
    ConnectedMoleculeQ, ConstantRegionQ, ContinuousTimeModelQ, \
    ConvexPolygonQ, ConvexPolyhedronQ, ConvexRegionQ, CoprimeQ, \
    System`CSGRegionQ, DataStructureQ, DateObjectQ, DeviceOpenQ, \
    DiagonalizableMatrixQ, DiagonalMatrixQ, DirectedGraphQ, DirectoryQ, \
    DiscreteTimeModelQ, DispatchQ, EdgeTaggedGraphQ, \
    System`EdgeTransitiveGraphQ, EdgeWeightedGraphQ, EmptyGraphQ, \
    EulerianGraphQ, EvenQ, ExactNumberQ, FailureQ, FileExistsQ, GraphQ, \
    HamiltonianGraphQ, HermitianMatrixQ, ImageQ, IndefiniteMatrixQ, \
    InexactNumberQ, IntegerQ, IrreduciblePolynomialQ, KnownUnitQ, ListQ, \
    LoopFreeGraphQ, LowerTriangularMatrixQ, MachineNumberQ, \
    ManagedLibraryExpressionQ, MatrixQ, MersennePrimeExponentQ, \
    MeshRegionQ, MissingQ, MixedGraphQ, MoleculeEquivalentQ, MoleculeQ, \
    MultigraphQ, NegativeDefiniteMatrixQ, NegativeSemidefiniteMatrixQ, \
    NormalMatrixQ, NumberQ, NumericArrayQ, NumericQ, OddQ, OptionQ, \
    OrthogonalMatrixQ, PacletObjectQ, PathGraphQ, PerfectNumberQ, \
    PermutationCyclesQ, PermutationListQ, PlanarGraphQ, PolynomialQ, \
    PositiveDefiniteMatrixQ, PositiveSemidefiniteMatrixQ, PossibleZeroQ, \
    PrimePowerQ, PrimeQ, QuadraticIrrationalQ, QuantityQ, \
    System`ReactionBalancedQ, RegionQ, RootOfUnityQ, SameQ, SatisfiableQ, \
    SimpleGraphQ, SimplePolygonQ, SimplePolyhedronQ, SolidRegionQ, \
    System`SparseArrayQ, SpatialObservationRegionQ, SquareFreeQ, SquareMatrixQ, \
    StringQ, StructuredArrayHeadQ, SymmetricMatrixQ, TautologyQ, TensorQ, \
    TimeObjectQ, TreeGraphQ, System`TreeLeafQ, System`TreeQ, TrueQ, UnateQ, \
    UndirectedGraphQ, UnitaryMatrixQ, UnsameQ, UpperTriangularMatrixQ, \
    ValueQ, VectorQ, System`VertexTransitiveGraphQ, VertexWeightedGraphQ, \
    VideoQ, WeaklyConnectedGraphQ, WeightedGraphQ
];

$booleanFunctionsN = HoldPattern @ Alternatives[
    SameQ, UnsameQ
];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-J1Hx",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { TrueQ },
    "Inline"      :> { $booleanFunctions1 },
    "Rule"        :> TrueQ[ (h: $booleanFunctions1)[ x_ ] ] :> h @ x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-dT7TWk",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { TrueQ },
    "Inline"      :> { $booleanFunctionsN },
    "Rule"        :> TrueQ[ (h: $booleanFunctionsN)[ x___ ] ] :> h @ x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-c5t5aa",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { SameQ, True, TrueQ },
    "Rule"        :> (x_) === True :> TrueQ @ x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-eefpzJ",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { True, SameQ, TrueQ },
    "Rule"        :> True === (x_) :> TrueQ @ x
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-rudVk",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { AtomQ, True },
    "Rule"        :> AtomQ[ _? atomQ ] :> True
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-bRW95c",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { IntegerQ, True },
    "Rule"        :> IntegerQ[ _? IntTypeQ ] :> True
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-b5cXk6",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { StringQ, True },
    "Rule"        :> StringQ[ _? StringTypeQ ] :> True
|>

(**********************************************************************************************************************)
<|
    "Name"        -> "QFunctions-dyteEQ",
    "Description" -> "Transform expression with an anonymous replacement rule",
    "Usage"       -> { "EquivalenceTesting" },
    "Symbols"     :> { AssociationQ, Association, True },
    "Rule"        :> AssociationQ[ _? associationQ ] :> True
|>

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)