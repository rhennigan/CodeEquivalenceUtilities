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
(*Definitions*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SymbolVersionTooNew:: *)
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
(* :!CodeAnalysis::EndBlock:: *)

$booleanFunctionsN = HoldPattern @ Alternatives[
    SameQ, UnsameQ
];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Rules*)
Inline[ { $booleanFunctions1, $booleanFunctionsN },
    HoldComplete[
        TrueQ[ (h:$booleanFunctions1)[ x_ ] ] :> h @ x,
        TrueQ[ (h:$booleanFunctionsN)[ x___ ] ] :> h @ x,
        SameQ[ x_, True ] :> TrueQ @ x,
        SameQ[ True, x_ ] :> TrueQ @ x,
        AtomQ[ _? atomQ ] :> True,
        IntegerQ[ _? IntTypeQ ] :> True,
        StringQ[ _? StringTypeQ ] :> True,
        AssociationQ[ _? associationQ ] :> True
    ]
]

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];

(* :!CodeAnalysis::EndBlock:: *)