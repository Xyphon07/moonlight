module TopologySpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Moonlight.Homology as H
import qualified Moonlight.Homology.Boundary.Finite as H (mkFiniteChainComplex)
import Moonlight.Homology.Pure.Topology.Algebra (mkQuotientPresentation)
import Moonlight.Homology.Pure.Topology.Core qualified as HomologyCore
import Moonlight.Homology.Pure.Topology.Harmonic (harmonicBasisAt)
import TestFixtures
  ( intervalComplex,
    mooreComplex,
    projectivePlaneComplex,
    tetrahedronBoundaryComplex,
    triangleCycleComplex,
  )
import Moonlight.Pale.Test.Assertions (assertApproxEqual, expectRight, expectSome)
import Moonlight.Pale.Test.Core (mkTolerance)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertEqual, assertFailure, testCase)


tests :: TestTree
tests =
  testGroup
    "topology"
    [ testCase "exact witness recovers Betti and Euler for triangle cycle" testExactWitnessTriangle,
      testCase "tetrahedron boundary computes the 2-sphere H2 anchor" testTetrahedronBoundarySphereAnchor,
      testCase "graph adjacency algebra inserts absent keys and counts connected components" testGraphAdjacencyAlgebra,
      testCase "sparse H0 graph representatives choose one vertex per component" testSparseGraphHomologyRepresentatives,
      testCase "sparse H0 cohomology representatives are component-constant" testSparseGraphCohomologyRepresentatives,
      testCase "malformed graph-shaped boundaries use generic sparse fallback" testSparseGraphFallbackOnMalformedBoundaries,
      testCase "graph skeleton preserves parallel one-cell multiplicity" testGraphSkeletonParallelMultiplicity,
      testCase "exact witness recovers torsion and exact classes for Moore complex" testExactWitnessMoore,
      testCase "projective plane cellular attaching map exposes Z/2 torsion" testProjectivePlaneTorsionAnchor,
      testCase "persistent witness tracks the essential loop birth" testPersistentTriangleLoop,
      testCase "persistence retains a non-binary64 exact filtration order" testExactOrderedPersistence,
      testCase "critical Betti tables cancel same-rank interval pairs" testCriticalBettiSameRankCancellation,
      testCase "persistence falls back for non-graph-shaped degree-one columns" testPersistentGraphFallback,
      testCase "graph specialization preserves higher-degree clearing" testPersistentTetrahedronClearing,
      testCase "graph witness extracts scaffold and low modes on interval" testGraphWitnessInterval,
      testCase "graph witness remains convergent on branched five-vertex skeletons" testGraphWitnessBranchedFiveVertexSkeleton,
      testCase "graph witness seed enriches graph scaffolds with exact loop data" testGraphWitnessSeedTriangle,
      testCase "harmonic loop discovery enriches scaffold from exact cycle data" testHarmonicLoopDiscovery,
      scaffoldCompositionTests,
      testCase "topology targets validate witness constraints through the target algebra" testTopologyTargets,
      testCase "direction field constructor rejects coverage mismatch" testDirectionFieldConstructorRejectsCoverageMismatch,
      testCase "direction angle constructor normalizes phase modulo symmetry" testDirectionAngleConstructorNormalizesPhase,
      testCase "direction phase constructor rejects non-finite values" testDirectionPhaseConstructorRejectsNonFiniteValues,
      testCase "direction coefficient constructor rejects non-finite values" testDirectionCoefficientConstructorRejectsNonFiniteValues,
      testCase "potential value constructor rejects non-finite values" testPotentialValueConstructorRejectsNonFiniteValues,
      testCase "scalar potential raw constructor rejects non-finite samples" testScalarPotentialFieldRawConstructorRejectsNonFiniteSamples,
      testCase "graded torsion family reuses canonical degree aggregation laws" testGradedTorsionFamily,
      testCase "graded query kernel reuses selection across homology, persistence, and harmonics" testGradedQueryKernel,
      testCase "topology observers interpret witness data without raw record spelunking" testTopologyObserver,
      testCase "cohomologyBasisAt degree 1 is non-empty for triangle cycle" testCohomologyBasisTriangle,
      testCase "cohomologyBasisAt degree 1 is empty for contractible interval" testCohomologyBasisInterval,
      testCase "cohomologyBasisAt degree 1 is empty for degenerate zero-boundary complex" testCohomologyBasisDegenerate,
      presentationCoordinateTests
    ]

scaffoldCompositionTests :: TestTree
scaffoldCompositionTests =
  testGroup
    "macro scaffold composition"
    [ testCase "is identity on singleton inputs" testMacroScaffoldCompositionSingletonIdentity,
      testCase "preserves basis reindex stability" testMacroScaffoldCompositionBasisReindexing,
      testCase "keeps Reeb, singularity, and loop ids disjoint" testMacroScaffoldCompositionIdDisjointness,
      testCase "stitches routed regions with a seam arc" testMacroScaffoldRouteStitching,
      testCase "keeps route-kind seams distinct" testMacroScaffoldRouteKindsRemainDistinct,
      testCase "widens weighted corridor seams" testMacroScaffoldWeightedCorridorSeams,
      testCase "preserves scalar and direction compatibility checks" testMacroScaffoldCompositionCompatibilityChecks,
      testCase "adds scaffold cardinalities across disjoint union" testMacroScaffoldCompositionCardinalityAdditivity,
      testCase "rejects incompatible inputs symmetrically" testMacroScaffoldCompositionCompatibilitySymmetry,
      testCase "adds Reeb cycle rank across disjoint union" testMacroScaffoldCompositionCycleRank
    ]

withExactWitness ::
  H.FiniteChainComplex Integer ->
  (H.TopologyWitness H.MacroScaffoldIR H.GraphSpectralMode H.FiltrationValue Rational Int -> Assertion) ->
  Assertion
withExactWitness finite assertion = do
  witnessValue <- expectRight (H.exactTopologyWitness finite)
  assertion witnessValue

testExactWitnessTriangle :: Assertion
testExactWitnessTriangle =
  withExactWitness triangleCycleComplex $ \witnessValue ->
    let degreeOneCycles =
          H.topologyCoefficientRepresentativeCycles witnessValue
            & filter ((== H.HomologicalDegree 1) . H.representativeDegree)
        degreeOneCocycles =
          H.topologyCoefficientRepresentativeCocycles witnessValue
            & filter ((== H.HomologicalDegree 1) . H.representativeDegree)
     in do
          fmap H.unEulerCharacteristic (H.topologyEulerCharacteristic witnessValue) @?= Just 0
          H.topologyBettiVector witnessValue @?= [1, 1]
          fmap H.freeRank (H.topologyIntegralHomologyGroups witnessValue) @?= [1, 1]
          fmap H.torsionInvariants (H.topologyIntegralHomologyGroups witnessValue) @?= [[], []]
          length degreeOneCycles @?= 1
          length degreeOneCocycles @?= 1

testTetrahedronBoundarySphereAnchor :: Assertion
testTetrahedronBoundarySphereAnchor = do
  integralGroups <-
    expectRight
      ( H.runHomologyBackend
          (H.IntegralSmithBackend :: H.HomologyBackend Integer Integer)
          tetrahedronBoundaryComplex
      )
  rationalGroups <-
    expectRight
      ( H.runHomologyBackend
          H.RationalRankBackend
          (H.rationalizeFiniteChainComplex tetrahedronBoundaryComplex)
      )
  witnessValue <- expectRight (H.exactTopologyWitness tetrahedronBoundaryComplex)
  fmap (H.degreeCardinality tetrahedronBoundaryComplex . H.HomologicalDegree) [0, 1, 2] @?= [4, 6, 4]
  length (H.boundaryEntries (H.incidenceMatrixAt tetrahedronBoundaryComplex (H.HomologicalDegree 2))) @?= 12
  let tetrahedronBoundaryComposite =
        H.composeBoundaryIncidence
          (H.incidenceMatrixAt tetrahedronBoundaryComplex (H.HomologicalDegree 1))
          (H.incidenceMatrixAt tetrahedronBoundaryComplex (H.HomologicalDegree 2))
  case tetrahedronBoundaryComposite of
    Left shapeError ->
      assertFailure ("unexpected tetrahedron boundary composition shape failure: " <> show shapeError)
    Right compositeBoundary ->
      assertBool
        "tetrahedron boundary satisfies d1 . d2 = 0"
        (all ((== 0) . H.boundaryCoefficient) (H.boundaryEntries compositeBoundary))
  fmap H.freeRank integralGroups @?= [1, 0, 1]
  fmap H.torsionInvariants integralGroups @?= [[], [], []]
  fmap H.freeRank rationalGroups @?= [1, 0, 1]
  H.topologyBettiVector witnessValue @?= [1, 0, 1]
  fmap H.unEulerCharacteristic (H.topologyEulerCharacteristic witnessValue) @?= Just 2
  length (harmonicBasisAt tetrahedronBoundaryComplex (H.HomologicalDegree 2)) @?= 1

testGraphAdjacencyAlgebra :: Assertion
testGraphAdjacencyAlgebra =
  let adjacency =
        ( H.addUndirectedAdjacency 0 1
            . H.addUndirectedAdjacency 0 1
            . H.addUndirectedAdjacency 2 3
            $ Map.empty
        ) ::
          Map.Map Integer [Integer]
   in do
        adjacency
          @?= Map.fromList
            [ (0, [1, 1]),
              (1, [0, 0]),
              (2, [3]),
              (3, [2])
            ]
        H.connectedComponentsFromAdjacency adjacency @?= 2

testSparseGraphHomologyRepresentatives :: Assertion
testSparseGraphHomologyRepresentatives = do
  disconnectedGraph <- expectRight (graphChainComplex 4 [(0, 1), (2, 3)])
  fmap H.representativeTerms (H.sparseHomologyBasisAt intervalComplex (H.HomologicalDegree 0))
    @?= [[(1, 0)]]
  fmap H.representativeTerms (H.sparseHomologyBasisAt disconnectedGraph (H.HomologicalDegree 0))
    @?= [[(1, 0)], [(1, 2)]]

testSparseGraphCohomologyRepresentatives :: Assertion
testSparseGraphCohomologyRepresentatives = do
  disconnectedGraph <- expectRight (graphChainComplex 4 [(0, 1), (2, 3)])
  fmap H.representativeTerms (H.sparseCohomologyBasisAt intervalComplex (H.HomologicalDegree 0))
    @?= [[(1, 0), (1, 1)]]
  fmap H.representativeTerms (H.sparseCohomologyBasisAt disconnectedGraph (H.HomologicalDegree 0))
    @?= [[(1, 0), (1, 1)], [(1, 2), (1, 3)]]

testSparseGraphFallbackOnMalformedBoundaries :: Assertion
testSparseGraphFallbackOnMalformedBoundaries = do
  nonUnitGraph <- expectRight (boundaryOneComplex 2 1 [boundaryEntry 0 0 (-2), boundaryEntry 0 1 2])
  singleEndpointGraph <- expectRight (boundaryOneComplex 2 1 [boundaryEntry 0 0 1])
  H.graph1SkeletonFromComplex nonUnitGraph @?= Left (H.InvalidOrientedUnitGraphEdgeBoundary 0)
  H.graph1SkeletonFromComplex singleEndpointGraph @?= Left (H.InvalidOrientedUnitGraphEdgeBoundary 0)
  assertBool
    "non-unit graph-shaped boundary still falls back to generic sparse H0 representatives"
    (not (null (H.sparseHomologyBasisAt nonUnitGraph (H.HomologicalDegree 0))))
  fmap H.representativeTerms (H.sparseHomologyBasisAt singleEndpointGraph (H.HomologicalDegree 0))
    @?= [[(1, 1)]]

testGraphSkeletonParallelMultiplicity :: Assertion
testGraphSkeletonParallelMultiplicity = do
  parallelPath <- expectRight (graphChainComplex 3 [(0, 1), (0, 1), (1, 2)])
  skeleton <- expectRight (H.graph1SkeletonFromComplex parallelPath)
  potentialField <- expectRight parallelPathPotentialField
  scaffoldValue <- expectRight (H.graphMacroScaffold potentialField skeleton)
  let potentials = Map.fromList [(0, 0.0), (1, 1.0), (2, 2.0)]
      edgeIndicesAt vertexValue =
        fmap H.graphEdgeIndex (Map.findWithDefault [] vertexValue (H.graphEdgeAdjacency skeleton))
      reebValue = H.macroScaffoldReeb scaffoldValue
  H.graphEdges skeleton
    @?= [ H.GraphEdge 0 0 1,
          H.GraphEdge 1 0 1,
          H.GraphEdge 2 1 2
        ]
  edgeIndicesAt 0 @?= [0, 1]
  edgeIndicesAt 1 @?= [0, 1, 2]
  H.higherNeighbors potentials skeleton 0 @?= [1, 1]
  H.lowerNeighbors potentials skeleton 1 @?= [0, 0]
  H.higherNeighbors potentials skeleton 1 @?= [2]
  H.criticalKindAt potentials skeleton 1 @?= Just H.Merge
  fmap H.morseReebNodeKind (H.morseReebNodes reebValue) @?= [H.Basin, H.Merge, H.Peak]
  fmap H.morseReebArcSupport (H.morseReebArcs reebValue)
    @?= [ [ H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0},
            H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 0},
            H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}
          ],
          [ H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0},
            H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 1},
            H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}
          ],
          [ H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1},
            H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 2},
            H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2}
          ]
        ]
  spectralModes <- expectRight (H.graphSpectralModes 3 skeleton)
  length spectralModes @?= 3

testExactWitnessMoore :: Assertion
testExactWitnessMoore =
  withExactWitness mooreComplex $ \witnessValue ->
    let degreeOneClasses =
          H.topologyExactRepresentativeClasses witnessValue
            & filter ((== H.HomologicalDegree 1) . H.exactClassDegree)
     in do
          fmap H.unEulerCharacteristic (H.topologyEulerCharacteristic witnessValue) @?= Just 1
          H.topologyBettiVector witnessValue @?= [1, 0, 0]
          fmap H.freeRank (H.topologyIntegralHomologyGroups witnessValue) @?= [1, 0, 0]
          fmap H.torsionInvariants (H.topologyIntegralHomologyGroups witnessValue) @?= [[], [2], []]
          fmap H.exactClassOrder degreeOneClasses @?= [Just 2]
          fmap (H.representativeTerms . H.exactClassRepresentative) degreeOneClasses @?= [[(1, 0)]]

testProjectivePlaneTorsionAnchor :: Assertion
testProjectivePlaneTorsionAnchor = do
  integralGroups <-
    expectRight
      ( H.runHomologyBackend
          (H.IntegralSmithBackend :: H.HomologyBackend Integer Integer)
          projectivePlaneComplex
      )
  fmap (H.degreeCardinality projectivePlaneComplex . H.HomologicalDegree) [0, 1, 2] @?= [1, 1, 1]
  fmap H.boundaryCoefficient (H.boundaryEntries (H.incidenceMatrixAt projectivePlaneComplex (H.HomologicalDegree 2))) @?= [2]
  fmap H.freeRank integralGroups @?= [1, 0, 0]
  fmap H.torsionInvariants integralGroups @?= [[], [2], []]

testPersistentTriangleLoop :: Assertion
testPersistentTriangleLoop = do
  filteredComplex <- expectRight (H.mkFilteredFiniteChainComplex triangleCycleComplex triangleFiltration)
  witnessValue <- expectRight (H.observeTopologyWitness defaultTopologyConfig {H.observationFiltration = Just filteredComplex} triangleCycleComplex)
  let essentialLoops =
        H.topologyPersistencePairs witnessValue
          & filter (\pairValue -> H.persistenceDegree pairValue == H.HomologicalDegree 1)
          & filter (isNothing . H.persistenceDeath)
  length essentialLoops @?= 1
  fmap H.persistenceBirth essentialLoops @?= [H.FiltrationValue 2.0]

newtype ExactTestBirth = ExactTestBirth Rational
  deriving stock (Eq, Ord, Show)

testExactOrderedPersistence :: Assertion
testExactOrderedPersistence = do
  filteredComplex <-
    expectRight
      (H.mkFilteredFiniteChainComplex triangleCycleComplex exactTriangleFiltration)
  persistencePairs <- expectRight (H.mod2PersistentPairs filteredComplex)
  assertCriticalBettiTableAgrees filteredComplex persistencePairs
  let criticalValues = fmap ExactTestBirth [0, 1, 2]
  H.filteredCriticalValues filteredComplex @?= criticalValues
  HomologyCore.filteredBirthValueRanks filteredComplex
    @?= Map.fromList (zip criticalValues [0, 1, 2])
  fmap
    (\(cellRefValue, _) -> Map.lookup cellRefValue (HomologyCore.filteredCellBirthRanks filteredComplex))
    exactTriangleFiltration
    @?= fmap Just [0, 0, 0, 1, 1, 2]
  let essentialLoops =
        persistencePairs
          & filter (\pairValue -> H.persistenceDegree pairValue == H.HomologicalDegree 1)
          & filter (isNothing . H.persistenceDeath)
  fmap H.persistenceBirth essentialLoops @?= [ExactTestBirth 2]
  H.persistentBettiAt (ExactTestBirth 1) persistencePairs
    @?= Map.fromList [(H.HomologicalDegree 0, 1)]
  H.persistentBettiAt (ExactTestBirth 2) persistencePairs
    @?= Map.fromList
      [ (H.HomologicalDegree 0, 1),
        (H.HomologicalDegree 1, 1)
      ]
  let requestedThresholds = fmap ExactTestBirth [2, 0, 1, 2]
  H.persistentBettiAtMany requestedThresholds persistencePairs
    @?= fmap (`H.persistentBettiAt` persistencePairs) requestedThresholds
  criticalProfiles <-
    expectRight (H.persistentBettiAtCriticalValues filteredComplex persistencePairs)
  criticalProfiles
    @?= fmap (`H.persistentBettiAt` persistencePairs) criticalValues
  let malformedPairs =
        [ H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 0,
              H.persistenceBirth = ExactTestBirth 1,
              H.persistenceDeath = Just (ExactTestBirth 1)
            },
          H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 1,
              H.persistenceBirth = ExactTestBirth 2,
              H.persistenceDeath = Just (ExactTestBirth 1)
            }
        ]
  H.persistentBettiAtMany requestedThresholds malformedPairs
    @?= fmap (`H.persistentBettiAt` malformedPairs) requestedThresholds

testCriticalBettiSameRankCancellation :: Assertion
testCriticalBettiSameRankCancellation = do
  filteredComplex <-
    expectRight
      ( H.mkFilteredFiniteChainComplex
          intervalComplex
          (filtrationByDegree intervalComplex (const (ExactTestBirth 0)))
      )
  (persistencePairs, criticalBettiTable) <-
    expectRight (H.mod2PersistentPairsWithCriticalBettiTable filteredComplex)
  persistencePairs
    @?= [ H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 0,
              H.persistenceBirth = ExactTestBirth 0,
              H.persistenceDeath = Just (ExactTestBirth 0)
            },
          H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 0,
              H.persistenceBirth = ExactTestBirth 0,
              H.persistenceDeath = Nothing
            }
        ]
  H.criticalBettiVectors criticalBettiTable @?= [[1, 0]]

testPersistentGraphFallback :: Assertion
testPersistentGraphFallback = do
  singleEndpointGraph <-
    expectRight (boundaryOneComplex 2 1 [boundaryEntry 0 0 1])
  filteredComplex <-
    expectRight
      ( H.mkFilteredFiniteChainComplex
          singleEndpointGraph
          ( filtrationByDegree singleEndpointGraph $ \degreeValue ->
              ExactTestBirth
                (if degreeValue == H.HomologicalDegree 0 then 0 else 1)
          )
      )
  (persistencePairs, criticalBettiTable) <-
    expectRight (H.mod2PersistentPairsWithCriticalBettiTable filteredComplex)
  persistencePairs
    @?= [ H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 0,
              H.persistenceBirth = ExactTestBirth 0,
              H.persistenceDeath = Just (ExactTestBirth 1)
            },
          H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 0,
              H.persistenceBirth = ExactTestBirth 0,
              H.persistenceDeath = Nothing
            }
        ]
  H.criticalBettiVectors criticalBettiTable @?= [[2, 0], [1, 0]]

testPersistentTetrahedronClearing :: Assertion
testPersistentTetrahedronClearing = do
  filteredComplex <-
    expectRight
      ( H.mkFilteredFiniteChainComplex
          tetrahedronBoundaryComplex
          ( filtrationByDegree
              tetrahedronBoundaryComplex
              (ExactTestBirth . fromIntegral . H.unHomologicalDegree)
          )
      )
  persistencePairs <- expectRight (H.mod2PersistentPairs filteredComplex)
  assertCriticalBettiTableAgrees filteredComplex persistencePairs
  let pairsAt degreeValue =
        filter ((== degreeValue) . H.persistenceDegree) persistencePairs
      degreeOnePairs = pairsAt (H.HomologicalDegree 1)
      degreeTwoPairs = pairsAt (H.HomologicalDegree 2)
  length degreeOnePairs @?= 3
  fmap H.persistenceBirth degreeOnePairs @?= replicate 3 (ExactTestBirth 1)
  fmap H.persistenceDeath degreeOnePairs @?= replicate 3 (Just (ExactTestBirth 2))
  fmap H.persistenceBirth degreeTwoPairs @?= [ExactTestBirth 2]
  fmap H.persistenceDeath degreeTwoPairs @?= [Nothing]

assertCriticalBettiTableAgrees ::
  (Integral r, Ord filtration, Show filtration) =>
  H.FilteredFiniteChainComplex filtration r ->
  [H.PersistencePair filtration] ->
  Assertion
assertCriticalBettiTableAgrees filteredComplex persistencePairs = do
  (fusedPairs, criticalBettiTable) <-
    expectRight (H.mod2PersistentPairsWithCriticalBettiTable filteredComplex)
  criticalProfiles <-
    expectRight (H.persistentBettiAtCriticalValues filteredComplex persistencePairs)
  let H.HomologicalDegree maximumDegree =
        H.maxHomologicalDegree (H.filteredBaseComplex filteredComplex)
      degreeValues = fmap H.HomologicalDegree [0 .. maximumDegree]
      denseProfiles =
        fmap
          (\profile -> fmap (\degreeValue -> Map.findWithDefault 0 degreeValue profile) degreeValues)
          criticalProfiles
  fusedPairs @?= persistencePairs
  H.criticalBettiDegreeCount criticalBettiTable @?= maximumDegree + 1
  H.criticalBettiRankCount criticalBettiTable @?= length criticalProfiles
  H.criticalBettiVectors criticalBettiTable @?= denseProfiles

testGraphWitnessInterval :: Assertion
testGraphWitnessInterval =
  withIntervalObservation $ \intervalObservationValue -> do
    _ <- expectRight (H.graph1SkeletonFromComplex intervalComplex)
    witnessValue <- expectRight (H.observeTopologyWitness intervalObservationValue intervalComplex)
    H.topologyBettiVector witnessValue @?= [1, 0]
    scaffoldValue <- expectSome "expected macro scaffold" (H.topologyMacroScaffold witnessValue)
    length (H.carrierCells (H.scalarPotentialCarrier (H.macroScaffoldScalarPotential scaffoldValue))) @?= 2
    H.unDirectionSymmetryOrder (H.directionFieldSymmetryOrder (H.macroScaffoldDirectionField scaffoldValue)) @?= 1
    length (H.morseReebNodes (H.macroScaffoldReeb scaffoldValue)) @?= 2
    fmap H.morseReebNodeKind (H.morseReebNodes (H.macroScaffoldReeb scaffoldValue)) @?= [H.Basin, H.Peak]
    length (H.morseReebArcs (H.macroScaffoldReeb scaffoldValue)) @?= 1
    fmap H.singularityKind (H.macroScaffoldSingularities scaffoldValue) @?= [H.Basin, H.Peak]
    let spectralModes = H.topologyLowSpectralModes witnessValue
    length spectralModes @?= 2
    assertBool
      "contains zero mode"
      (spectralModes & any (\modeValue -> abs (H.spectralEigenvalue modeValue) < 1.0e-6))

testGraphWitnessBranchedFiveVertexSkeleton :: Assertion
testGraphWitnessBranchedFiveVertexSkeleton =
  let branchedSkeleton =
        H.graphFromEdgeSupports
          5
          [ (0, 1),
            (1, 2),
            (2, 3),
            (3, 4),
            (0, 2),
            (1, 3)
          ]
   in do
        witnessValue <- expectRight (H.graphTopologyWitness 2 Nothing branchedSkeleton)
        length (H.topologyLowSpectralModes witnessValue) @?= 2
        assertBool
          "contains zero mode for connected branched skeleton"
          (H.topologyLowSpectralModes witnessValue & any (\modeValue -> abs (H.spectralEigenvalue modeValue) < 1.0e-6))

testGraphWitnessSeedTriangle :: Assertion
testGraphWitnessSeedTriangle =
  withTriangleObservation $ \triangleObservationValue -> do
    triangleSkeleton <- expectRight (H.graph1SkeletonFromComplex triangleCycleComplex)
    witnessValue <-
      expectRight
        ( H.observeTopologyWitnessSeed
            ( ( H.GraphTopologySeed
                  triangleSkeleton
                  Nothing
                  (H.observationPotential triangleObservationValue)
                  (H.observationLowModeCount triangleObservationValue)
              ) ::
                H.TopologyWitnessSeed Integer
            )
        )
    H.topologyBettiVector witnessValue @?= [1, 1]
    scaffoldValue <- expectSome "expected graph seed scaffold" (H.topologyMacroScaffold witnessValue)
    length (H.macroScaffoldHarmonicLoops scaffoldValue) @?= 1

testHarmonicLoopDiscovery :: Assertion
testHarmonicLoopDiscovery =
  withTriangleObservation $ \triangleObservationValue -> do
    witnessValue <- expectRight (H.observeTopologyWitness triangleObservationValue triangleCycleComplex)
    scaffoldValue <- expectSome "expected macro scaffold with harmonic loop enrichment" (H.topologyMacroScaffold witnessValue)
    case H.macroScaffoldHarmonicLoops scaffoldValue of
      [H.HarmonicLoop {H.harmonicLoopDegree = degreeValue, H.harmonicLoopSupport = supportValue, H.harmonicLoopCycle = cycleValue}] -> do
        degreeValue @?= H.HomologicalDegree 1
        assertEqual "harmonic loop support count" 2 (length supportValue)
        assertEqual "harmonic loop cycle term count" 3 (length (H.representativeTerms cycleValue))
      observedLoops ->
        assertFailure ("expected one harmonic loop, observed " <> show observedLoops)

testMacroScaffoldCompositionSingletonIdentity :: Assertion
testMacroScaffoldCompositionSingletonIdentity = do
  _ <- expectRight intervalMacroScaffold
  _ <- expectRight triangleMacroScaffold
  assertSingletonIdentity intervalCompositionFixture
  assertSingletonIdentity triangleCompositionFixture

testMacroScaffoldCompositionBasisReindexing :: Assertion
testMacroScaffoldCompositionBasisReindexing =
  withSelfComposition intervalCompositionFixture $ \_ composedScaffold -> do
          H.carrierCells (H.scalarPotentialCarrier (H.macroScaffoldScalarPotential composedScaffold))
            @?= basisRefs (H.HomologicalDegree 0) [0, 1, 2, 3]
          Map.keys (H.scalarPotentialSamples (H.macroScaffoldScalarPotential composedScaffold))
            @?= basisRefs (H.HomologicalDegree 0) [0, 1, 2, 3]
          H.carrierCells (H.directionFieldCarrier (H.macroScaffoldDirectionField composedScaffold))
            @?= basisRefs (H.HomologicalDegree 1) [0, 1]
          fmap H.morseReebNodeAnchor (H.morseReebNodes (H.macroScaffoldReeb composedScaffold))
            @?= basisRefs (H.HomologicalDegree 0) [0, 1, 2, 3]
          (H.morseReebArcs (H.macroScaffoldReeb composedScaffold) >>= H.morseReebArcSupport)
            @?= [ H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0},
                  H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 0},
                  H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1},
                  H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2},
                  H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 1},
                  H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 3}
                ]

testMacroScaffoldCompositionIdDisjointness :: Assertion
testMacroScaffoldCompositionIdDisjointness =
  withSelfComposition triangleCompositionFixture $ \_ composedScaffold -> do
          assertDistinct "Reeb node ids remain disjoint" (fmap H.morseReebNodeId (H.morseReebNodes (H.macroScaffoldReeb composedScaffold)))
          assertDistinct "Reeb arc ids remain disjoint" (fmap H.morseReebArcId (H.morseReebArcs (H.macroScaffoldReeb composedScaffold)))
          assertDistinct "singularity ids remain disjoint" (fmap H.singularityId (H.macroScaffoldSingularities composedScaffold))
          assertDistinct "harmonic loop ids remain disjoint" (fmap H.harmonicLoopId (H.macroScaffoldHarmonicLoops composedScaffold))
          fmap H.harmonicLoopId (H.macroScaffoldHarmonicLoops composedScaffold)
            @?= [H.HarmonicLoopId 0, H.HarmonicLoopId 1]

testMacroScaffoldRouteStitching :: Assertion
testMacroScaffoldRouteStitching =
  withFixture intervalCompositionFixture $ \leftScaffold ->
    withFixture intervalCompositionFixture $ \rightScaffold -> do
      (composedScaffold, regionScopes) <-
        expectRight (H.composeMacroScaffoldsWithScopes ((Text.pack "left", leftScaffold) :| [(Text.pack "right", rightScaffold)]))
      (stitchedScaffold, stitchScopes) <-
        expectRight
          ( H.stitchMacroScaffoldRoutes
              stitchSemantics
              regionScopes
              [H.StitchRoute (Text.pack "trail") (Text.pack "left" :| [Text.pack "right"])]
              composedScaffold
          )
      length (H.morseReebArcs (H.macroScaffoldReeb stitchedScaffold))
        @?= length (H.morseReebArcs (H.macroScaffoldReeb composedScaffold)) + 1
      let trailKey = H.StitchRouteKey (Text.pack "trail") (Text.pack "left") (Text.pack "right")
          trailScope = Map.findWithDefault Set.empty trailKey stitchScopes
          leftRegionRefs = Map.findWithDefault Set.empty (Text.pack "left") regionScopes
          rightRegionRefs = Map.findWithDefault Set.empty (Text.pack "right") regionScopes
      assertBool "trail stitch scope is non-empty" (not (Set.null trailScope))
      assertBool "trail scope contains refs from left region"
        (not (Set.null (Set.intersection trailScope leftRegionRefs)))
      assertBool "trail scope contains refs from right region"
        (not (Set.null (Set.intersection trailScope rightRegionRefs)))
      assertEqual "trail stitch basis scope size" 6 (Set.size trailScope)
  where
    stitchSemantics routeKind =
      if routeKind == Text.pack "trail"
        then
          H.StitchSemantics
            { H.ssSourceBoundary = H.UpperBoundary,
              H.ssTargetBoundary = H.LowerBoundary,
              H.ssSupportSelection = H.BoundarySupport,
              H.ssSupportRefinement = H.BoundaryEnvelopeRefinement
            }
        else
          H.StitchSemantics
            { H.ssSourceBoundary = H.LowerBoundary,
              H.ssTargetBoundary = H.LowerBoundary,
              H.ssSupportSelection = H.AnchorSupport,
              H.ssSupportRefinement = H.KernelSupportRefinement
            }

testMacroScaffoldRouteKindsRemainDistinct :: Assertion
testMacroScaffoldRouteKindsRemainDistinct =
  withFixture intervalCompositionFixture $ \leftScaffold ->
    withFixture intervalCompositionFixture $ \rightScaffold -> do
      (composedScaffold, regionScopes) <-
        expectRight (H.composeMacroScaffoldsWithScopes ((Text.pack "left", leftScaffold) :| [(Text.pack "right", rightScaffold)]))
      (stitchedScaffold, stitchScopes) <-
        expectRight
          ( H.stitchMacroScaffoldRoutes
              stitchSemantics
              regionScopes
              [ H.StitchRoute (Text.pack "trail") (Text.pack "left" :| [Text.pack "right"]),
                H.StitchRoute (Text.pack "waterway") (Text.pack "left" :| [Text.pack "right"])
              ]
              composedScaffold
          )
      let trailKey = H.StitchRouteKey (Text.pack "trail") (Text.pack "left") (Text.pack "right")
          waterwayKey = H.StitchRouteKey (Text.pack "waterway") (Text.pack "left") (Text.pack "right")
          trailScope = Map.findWithDefault Set.empty trailKey stitchScopes
          waterwayScope = Map.findWithDefault Set.empty waterwayKey stitchScopes
          leftRegionRefs = Map.findWithDefault Set.empty (Text.pack "left") regionScopes
          rightRegionRefs = Map.findWithDefault Set.empty (Text.pack "right") regionScopes
      length (H.morseReebArcs (H.macroScaffoldReeb stitchedScaffold))
        @?= length (H.morseReebArcs (H.macroScaffoldReeb composedScaffold)) + 2
      assertBool "trail scope contains refs from left region"
        (not (Set.null (Set.intersection trailScope leftRegionRefs)))
      assertBool "trail scope contains refs from right region"
        (not (Set.null (Set.intersection trailScope rightRegionRefs)))
      assertEqual "trail stitch basis scope size" 6 (Set.size trailScope)
      assertEqual "waterway stitch basis scope size" 2 (Set.size waterwayScope)
      assertBool "expected route kinds to retain distinct stitch supports" (trailScope /= waterwayScope)
      assertBool "waterway scope is a subset of trail scope" (Set.isSubsetOf waterwayScope trailScope)
      assertBool "expected anchor waterway support to be no larger than trail support" (Set.size waterwayScope <= Set.size trailScope)
  where
    stitchSemantics routeKind =
      if routeKind == Text.pack "trail"
        then
          H.StitchSemantics
            { H.ssSourceBoundary = H.UpperBoundary,
              H.ssTargetBoundary = H.LowerBoundary,
              H.ssSupportSelection = H.BoundarySupport,
              H.ssSupportRefinement = H.BoundaryEnvelopeRefinement
            }
        else
          H.StitchSemantics
            { H.ssSourceBoundary = H.LowerBoundary,
              H.ssTargetBoundary = H.LowerBoundary,
              H.ssSupportSelection = H.AnchorSupport,
              H.ssSupportRefinement = H.KernelSupportRefinement
            }

testMacroScaffoldWeightedCorridorSeams :: Assertion
testMacroScaffoldWeightedCorridorSeams =
  withFixture intervalCompositionFixture $ \leftScaffold ->
    withFixture intervalCompositionFixture $ \rightScaffold -> do
      (composedScaffold, regionScopes) <-
        expectRight (H.composeMacroScaffoldsWithScopes ((Text.pack "left", leftScaffold) :| [(Text.pack "right", rightScaffold)]))
      (_, stitchScopes) <-
        expectRight
          ( H.stitchMacroScaffoldRoutes
              stitchSemantics
              regionScopes
              [ H.StitchRoute (Text.pack "waterway-light") (Text.pack "left" :| [Text.pack "right"]),
                H.StitchRoute (Text.pack "waterway-heavy") (Text.pack "left" :| [Text.pack "right"])
              ]
              composedScaffold
          )
      let lightKey = H.StitchRouteKey (Text.pack "waterway-light") (Text.pack "left") (Text.pack "right")
          heavyKey = H.StitchRouteKey (Text.pack "waterway-heavy") (Text.pack "left") (Text.pack "right")
          lightScope = Map.findWithDefault Set.empty lightKey stitchScopes
          heavyScope = Map.findWithDefault Set.empty heavyKey stitchScopes
          leftRegionRefs = Map.findWithDefault Set.empty (Text.pack "left") regionScopes
          rightRegionRefs = Map.findWithDefault Set.empty (Text.pack "right") regionScopes
      assertBool "light seam scope is non-empty" (not (Set.null lightScope))
      assertEqual "light seam scope size" 2 (Set.size lightScope)
      assertBool "heavy scope contains refs from left region"
        (not (Set.null (Set.intersection heavyScope leftRegionRefs)))
      assertBool "heavy scope contains refs from right region"
        (not (Set.null (Set.intersection heavyScope rightRegionRefs)))
      assertEqual "heavy seam scope size" 6 (Set.size heavyScope)
      assertBool "light scope is a subset of heavy scope" (Set.isSubsetOf lightScope heavyScope)
      assertBool "expected heavy corridor to widen support" (Set.size heavyScope >= Set.size lightScope)
      assertBool "expected weighted seams to remain distinct" (lightScope /= heavyScope)
  where
    stitchSemantics routeKind =
      if routeKind == Text.pack "waterway-light"
        then
          H.StitchSemantics
            { H.ssSourceBoundary = H.LowerBoundary,
              H.ssTargetBoundary = H.LowerBoundary,
              H.ssSupportSelection = H.AnchorSupport,
              H.ssSupportRefinement = H.KernelSupportRefinement
            }
        else
          H.StitchSemantics
            { H.ssSourceBoundary = H.LowerBoundary,
              H.ssTargetBoundary = H.LowerBoundary,
              H.ssSupportSelection = H.AnchorSupport,
              H.ssSupportRefinement = H.RegionalEnvelopeRefinement
            }

testMacroScaffoldCompositionCompatibilityChecks :: Assertion
testMacroScaffoldCompositionCompatibilityChecks =
  withFixture intervalCompositionFixture $ \scaffoldValue -> do
    angleDirectionField <- expectRight (rebuildAngleDirectionField (H.macroScaffoldDirectionField scaffoldValue))
    let normalizationMismatch =
          scaffoldValue
            { H.macroScaffoldScalarPotential =
                (H.macroScaffoldScalarPotential scaffoldValue)
                  { H.scalarPotentialNormalization = H.UnitIntervalPotentialScale
                  }
            }
        encodingMismatch =
          scaffoldValue
            { H.macroScaffoldDirectionField = angleDirectionField
            }
    H.composeMacroScaffolds (scaffoldValue :| [normalizationMismatch])
      @?= Left H.MismatchedScalarPotentialNormalizations
    H.composeMacroScaffolds (scaffoldValue :| [encodingMismatch])
      @?= Left H.MismatchedDirectionEncodingFamilies

testMacroScaffoldCompositionCardinalityAdditivity :: Assertion
testMacroScaffoldCompositionCardinalityAdditivity =
  withComposedScaffolds intervalCompositionFixture triangleCompositionFixture $ \intervalScaffold triangleScaffold composedScaffold ->
          scaffoldCardinalities composedScaffold
            @?= addScaffoldCardinalities
              (scaffoldCardinalities intervalScaffold)
              (scaffoldCardinalities triangleScaffold)

testMacroScaffoldCompositionCompatibilitySymmetry :: Assertion
testMacroScaffoldCompositionCompatibilitySymmetry =
  withFixture intervalCompositionFixture $ \scaffoldValue -> do
    angleDirectionField <- expectRight (rebuildAngleDirectionField (H.macroScaffoldDirectionField scaffoldValue))
    let normalizationMismatch =
          scaffoldValue
            { H.macroScaffoldScalarPotential =
                (H.macroScaffoldScalarPotential scaffoldValue)
                  { H.scalarPotentialNormalization = H.UnitIntervalPotentialScale
                  }
            }
        encodingMismatch =
          scaffoldValue
            { H.macroScaffoldDirectionField = angleDirectionField
            }
    assertSymmetricCompositionFailure
      H.MismatchedScalarPotentialNormalizations
      scaffoldValue
      normalizationMismatch
    assertSymmetricCompositionFailure
      H.MismatchedDirectionEncodingFamilies
      scaffoldValue
      encodingMismatch

testMacroScaffoldCompositionCycleRank :: Assertion
testMacroScaffoldCompositionCycleRank =
  withSelfComposition triangleCompositionFixture $ \scaffoldValue composedScaffold ->
          reebCycleRank (H.macroScaffoldReeb composedScaffold)
            @?= 2 * reebCycleRank (H.macroScaffoldReeb scaffoldValue)

testTopologyTargets :: Assertion
testTopologyTargets =
  withIntervalObservation $ \intervalObservationValue -> do
    witnessValue <- expectRight (H.observeTopologyWitness intervalObservationValue intervalComplex)
    let targetValues =
          [ H.EulerTarget (H.EulerBound (H.Exactly 1)),
            H.BettiTarget (H.TargetBetti [1, 0]),
            H.SkeletonTarget
              H.SkeletonAdherence
                { H.skeletonTargetSignature =
                    H.SkeletonSignature
                      { H.signatureCriticalCounts = Map.fromList [(H.Basin, 1), (H.Peak, 1)],
                        H.signatureArcCount = 1
                      },
                  H.skeletonTolerance = 0
                }
          ]
    H.validateTopologyTargets witnessValue targetValues @?= Right ()
    case H.validateTopologyTarget witnessValue (H.BettiTarget (H.TargetBetti [0, 1])) of
      Left (H.TargetViolation {H.violatedTarget = H.BettiTarget _, H.targetViolationCause = H.BettiViolation _ observedBetti}) ->
        observedBetti @?= [1, 0]
      validationResult ->
        assertFailure ("expected target violation for incorrect Betti target, observed " <> show validationResult)

testDirectionFieldConstructorRejectsCoverageMismatch :: Assertion
testDirectionFieldConstructorRejectsCoverageMismatch =
  let supportedCell =
        H.BasisCellRef
          { H.cellDegree = H.HomologicalDegree 1,
            H.cellIndex = 0
          }
      missingCell =
        H.BasisCellRef
          { H.cellDegree = H.HomologicalDegree 1,
            H.cellIndex = 1
          }
      extraneousCell =
        H.BasisCellRef
          { H.cellDegree = H.HomologicalDegree 1,
            H.cellIndex = 2
          }
   in do
        carrierValue <- expectRight (H.mkCellCarrier (H.HomologicalDegree 1) [supportedCell, missingCell])
        symmetryOrderValue <- expectRight (H.mkDirectionSymmetryOrder 2)
        supportedCoefficient <- expectRight (first show (H.mkDirectionCoefficient 1.0))
        extraneousCoefficient <- expectRight (first show (H.mkDirectionCoefficient (-1.0)))
        H.mkDirectionCochainField
          carrierValue
          symmetryOrderValue
          ( Map.fromList
              [ (supportedCell, supportedCoefficient),
                (extraneousCell, extraneousCoefficient)
              ]
          )
          @?= Left
            H.DirectionFieldCoverageMismatch
              { H.directionFieldMissingCells = [missingCell],
                H.directionFieldExtraneousCells = [extraneousCell]
              }

testDirectionAngleConstructorNormalizesPhase :: Assertion
testDirectionAngleConstructorNormalizesPhase =
  let supportedCell =
        H.BasisCellRef
          { H.cellDegree = H.HomologicalDegree 1,
            H.cellIndex = 0
          }
      expectedPhase = pi / 2
   in do
        carrierValue <- expectRight (H.mkCellCarrier (H.HomologicalDegree 1) [supportedCell])
        symmetryOrderValue <- expectRight (H.mkDirectionSymmetryOrder 2)
        phaseValue <- expectRight (first show (H.mkDirectionPhase ((-3) * pi / 2)))
        directionFieldValue <-
          expectRight
            ( H.mkDirectionAngleField
                carrierValue
                symmetryOrderValue
                (Map.fromList [(supportedCell, phaseValue)])
            )
        case H.directionFieldEncoding directionFieldValue of
          H.DirectionAngleEncoding phaseMap -> do
            normalizedPhaseValue <-
              expectSome "normalized direction phase missing from angle encoding" (Map.lookup supportedCell phaseMap)
            assertApproxEqual
              "angle phase normalized modulo symmetry order"
              (mkTolerance 1.0e-9 0)
              expectedPhase
              (H.unDirectionPhase normalizedPhaseValue)
          H.DirectionCochainEncoding _ ->
            assertFailure "expected angle encoding after angle field construction"

testDirectionPhaseConstructorRejectsNonFiniteValues :: Assertion
testDirectionPhaseConstructorRejectsNonFiniteValues =
  case (H.mkDirectionPhase (0 / 0), H.mkDirectionPhase (1 / 0)) of
    (Left (H.NonFiniteDirectionPhase notANumberValue), Left (H.NonFiniteDirectionPhase infiniteValue)) -> do
      assertBool "direction phase rejects NaN" (isNaN notANumberValue)
      assertBool "direction phase rejects infinity" (isInfinite infiniteValue)
    (nanResult, infinityResult) ->
      assertFailure
        ( "expected non-finite direction phase failures, observed "
            <> show (nanResult, infinityResult)
        )

testDirectionCoefficientConstructorRejectsNonFiniteValues :: Assertion
testDirectionCoefficientConstructorRejectsNonFiniteValues =
  case (H.mkDirectionCoefficient (0 / 0), H.mkDirectionCoefficient ((-1) / 0)) of
    (Left (H.NonFiniteDirectionCoefficient notANumberValue), Left (H.NonFiniteDirectionCoefficient infiniteValue)) -> do
      assertBool "direction coefficient rejects NaN" (isNaN notANumberValue)
      assertBool "direction coefficient rejects infinity" (isInfinite infiniteValue)
    (nanResult, infinityResult) ->
      assertFailure
        ( "expected non-finite direction coefficient failures, observed "
            <> show (nanResult, infinityResult)
        )

testPotentialValueConstructorRejectsNonFiniteValues :: Assertion
testPotentialValueConstructorRejectsNonFiniteValues =
  case (H.mkPotentialValue (0 / 0), H.mkPotentialValue (1 / 0)) of
    (Left (H.NonFinitePotentialValue notANumberValue), Left (H.NonFinitePotentialValue infiniteValue)) -> do
      assertBool "potential value rejects NaN" (isNaN notANumberValue)
      assertBool "potential value rejects infinity" (isInfinite infiniteValue)
    (nanResult, infinityResult) ->
      assertFailure
        ( "expected non-finite potential value failures, observed "
            <> show (nanResult, infinityResult)
        )

testScalarPotentialFieldRawConstructorRejectsNonFiniteSamples :: Assertion
testScalarPotentialFieldRawConstructorRejectsNonFiniteSamples =
  let supportedCell =
        H.BasisCellRef
          { H.cellDegree = H.HomologicalDegree 0,
            H.cellIndex = 0
          }
   in do
        carrierValue <- expectRight (H.mkCellCarrier (H.HomologicalDegree 0) [supportedCell])
        case
          H.mkScalarPotentialFieldFromSamples
            carrierValue
            H.NativePotentialScale
            (Map.fromList [(supportedCell, 0 / 0)])
          of
          Left (H.ScalarPotentialFieldInvalidSamples [(invalidCell, H.NonFinitePotentialValue invalidValue)]) -> do
            invalidCell @?= supportedCell
            assertBool "scalar potential raw constructor rejects NaN sample" (isNaN invalidValue)
          otherResult ->
            assertFailure
              ( "expected scalar potential invalid-sample failure, observed "
                  <> show otherResult
              )

testGradedTorsionFamily :: Assertion
testGradedTorsionFamily =
  let torsionFamily =
        H.mkGradedTorsionFamily
          [ H.HomologyGroup {H.freeRank = 1, H.torsionInvariants = [] :: [Integer]},
            H.HomologyGroup {H.freeRank = 0, H.torsionInvariants = [4 :: Integer]},
            H.HomologyGroup {H.freeRank = 0, H.torsionInvariants = [3 :: Integer]}
          ]
      combinedTorsion = H.gradedTorsionCombined H.selectAllDegrees torsionFamily
      supportQuery = H.degreewiseUnionQuery H.selectAllDegrees
      productQuery = H.directProductQuery H.selectAllDegrees
   in do
        fmap H.finiteAbelianInvariants (H.gradedTorsionAtDegree (H.HomologicalDegree 1) torsionFamily) @?= Just [4]
        H.gradedTorsionOrderSupport supportQuery torsionFamily @?= [2, 3, 4]
        H.gradedTorsionOrderSupport productQuery torsionFamily @?= [2, 3, 4, 6, 12]
        H.gradedTorsionPrimaryOrderSupport 2 supportQuery torsionFamily @?= Just [2, 4]
        H.finiteAbelianInvariants combinedTorsion @?= [3, 4]
        H.finiteAbelianExactOrderElementCount 12 combinedTorsion @?= 4

testGradedQueryKernel :: Assertion
testGradedQueryKernel =
  let degreeOneSelection = H.selectDegree (H.HomologicalDegree 1)
      degreeIndexedGroups =
        H.enumerateDegreeIndexed
          [ H.HomologyGroup {H.freeRank = 1, H.torsionInvariants = [] :: [Integer]},
            H.HomologyGroup {H.freeRank = 0, H.torsionInvariants = [4 :: Integer]},
            H.HomologyGroup {H.freeRank = 0, H.torsionInvariants = [3 :: Integer]}
          ]
      persistencePairs =
        [ H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 0,
              H.persistenceBirth = H.FiltrationValue 0.0,
              H.persistenceDeath = Nothing
            },
          H.PersistencePair
            { H.persistenceDegree = H.HomologicalDegree 1,
              H.persistenceBirth = H.FiltrationValue 2.0,
              H.persistenceDeath = Nothing
            }
        ]
      harmonicBasis =
        [ H.HarmonicBasisElement
            { H.harmonicDegree = H.HomologicalDegree 1,
              H.harmonicRepresentative =
                H.RepresentativeChain
                  { H.representativeDegree = H.HomologicalDegree 1,
                    H.representativeTerms = [(1 :: Integer, 0 :: Int)]
                  }
            },
          H.HarmonicBasisElement
            { H.harmonicDegree = H.HomologicalDegree 2,
              H.harmonicRepresentative =
                H.RepresentativeChain
                  { H.representativeDegree = H.HomologicalDegree 2,
                    H.representativeTerms = [(1 :: Integer, 1 :: Int)]
                  }
            }
        ]
   in do
        fmap H.torsionInvariants (H.selectDegreeIndexed degreeOneSelection degreeIndexedGroups) @?= [[4 :: Integer]]
        fmap H.torsionInvariants (H.lookupDegreeIndexed (H.HomologicalDegree 2) degreeIndexedGroups) @?= Just [3 :: Integer]
        H.countGradedMembers H.persistenceDegree degreeOneSelection persistencePairs @?= 1
        H.countGradedMembers H.harmonicDegree degreeOneSelection harmonicBasis @?= 1

testTopologyObserver :: Assertion
testTopologyObserver =
  withIntervalObservation $ \intervalObservationValue -> do
    _ <- expectRight (H.graph1SkeletonFromComplex intervalComplex)
    witnessValue <- expectRight (H.observeTopologyWitness intervalObservationValue intervalComplex)
    let topologyView =
          H.mkMacroScaffoldTopologyView witnessValue
    H.runTopologyObserver H.observeBettiVector topologyView @?= [1, 0]
    fmap H.freeRank (H.runTopologyObserver (H.observeIntegralHomology H.selectAllDegrees) topologyView) @?= [1, 0]
    H.runTopologyObserver (H.observePersistenceCount H.selectAllDegrees) topologyView @?= 0
    H.runTopologyObserver (H.observeHarmonicCount (H.selectDegree (H.HomologicalDegree 0))) topologyView @?= 0
    H.runWitnessInterpreter (H.mkMacroScaffoldWitnessInterpreter H.observeScaffoldSummary) witnessValue
      @?= Just
        H.SkeletonSignature
          { H.signatureCriticalCounts = Map.fromList [(H.Basin, 1), (H.Peak, 1)],
            H.signatureArcCount = 1
          }

graphChainComplex :: Int -> [(Int, Int)] -> Either String (H.FiniteChainComplex Integer)
graphChainComplex vertexCount edgeSupports =
  boundaryOneComplex
    vertexCount
    (length edgeSupports)
    (zipWith orientedEdgeBoundaryEntries [0 ..] edgeSupports >>= id)

boundaryOneComplex ::
  Int ->
  Int ->
  [H.BoundaryEntry Integer] ->
  Either String (H.FiniteChainComplex Integer)
boundaryOneComplex vertexCount edgeCount entries = do
  edgeBoundary <-
    first show $
      H.mkBoundaryIncidence
        (fromIntegral edgeCount)
        (fromIntegral vertexCount)
        entries
  pure
    ( H.mkFiniteChainComplex (H.HomologicalDegree 1) $ \dimensionValue ->
        case dimensionValue of
          H.HomologicalDegree 1 ->
            edgeBoundary
          H.HomologicalDegree 0 ->
            H.emptyBoundaryIncidenceOf (fromIntegral vertexCount) 0
          _ ->
            H.emptyBoundaryIncidence
    )

orientedEdgeBoundaryEntries :: Int -> (Int, Int) -> [H.BoundaryEntry Integer]
orientedEdgeBoundaryEntries edgeIndexValue (sourceVertex, targetVertex) =
  [ boundaryEntry edgeIndexValue sourceVertex (-1),
    boundaryEntry edgeIndexValue targetVertex 1
  ]

boundaryEntry :: Int -> Int -> coefficient -> H.BoundaryEntry coefficient
boundaryEntry sourceIndexValue targetIndexValue =
  H.mkBoundaryEntry (fromIntegral sourceIndexValue) (fromIntegral targetIndexValue)

triangleFiltration :: [(H.BasisCellRef, H.FiltrationValue)]
triangleFiltration =
  [ (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0}, H.FiltrationValue 0.0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}, H.FiltrationValue 0.0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2}, H.FiltrationValue 0.0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 0}, H.FiltrationValue 1.0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 1}, H.FiltrationValue 1.0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 2}, H.FiltrationValue 2.0)
  ]

exactTriangleFiltration :: [(H.BasisCellRef, ExactTestBirth)]
exactTriangleFiltration =
  [ (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0}, ExactTestBirth 0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}, ExactTestBirth 0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2}, ExactTestBirth 0),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 0}, ExactTestBirth 1),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 1}, ExactTestBirth 1),
    (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 1, H.cellIndex = 2}, ExactTestBirth 2)
  ]

filtrationByDegree ::
  H.FiniteChainComplex coefficient ->
  (H.HomologicalDegree -> filtration) ->
  [(H.BasisCellRef, filtration)]
filtrationByDegree finite birthAtDegree =
  let H.HomologicalDegree maximumDegree = H.maxHomologicalDegree finite
   in concatMap
        ( \degreeIndex ->
            let degreeValue = H.HomologicalDegree degreeIndex
             in fmap
                  (\cellRefValue -> (cellRefValue, birthAtDegree degreeValue))
                  (H.finiteChainBasisRefsAtDegree finite degreeValue)
        )
        [0 .. maximumDegree]

withIntervalObservation :: (H.TopologyObservationConfig Integer -> Assertion) -> Assertion
withIntervalObservation assertion = do
  observationValue <- expectRight intervalObservation
  assertion observationValue

intervalObservation :: Either String (H.TopologyObservationConfig Integer)
intervalObservation = do
  potentialField <- intervalPotentialField
  pure
    defaultTopologyConfig
      { H.observationPotential = Just potentialField,
        H.observationLowModeCount = 2
      }

withTriangleObservation :: (H.TopologyObservationConfig Integer -> Assertion) -> Assertion
withTriangleObservation assertion = do
  observationValue <- expectRight triangleObservation
  assertion observationValue

triangleObservation :: Either String (H.TopologyObservationConfig Integer)
triangleObservation = do
  potentialField <- trianglePotentialField
  pure
    defaultTopologyConfig
      { H.observationPotential = Just potentialField,
        H.observationLowModeCount = 0
      }

trianglePotentialField :: Either String H.ScalarPotentialField
trianglePotentialField = do
  carrierValue <-
    first show $
      H.mkCellCarrier
        (H.HomologicalDegree 0)
        [ H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0},
          H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1},
          H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2}
        ]
  first show $
    H.mkScalarPotentialFieldFromSamples
      carrierValue
      H.NativePotentialScale
      ( Map.fromList
          [ (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0}, 0.0),
            (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}, 1.0),
            (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2}, 2.0)
          ]
      )

parallelPathPotentialField :: Either String H.ScalarPotentialField
parallelPathPotentialField = do
  carrierValue <-
    first show $
      H.mkCellCarrier
        (H.HomologicalDegree 0)
        (basisRefs (H.HomologicalDegree 0) [0, 1, 2])
  first show $
    H.mkScalarPotentialFieldFromSamples
      carrierValue
      H.NativePotentialScale
      ( Map.fromList
          [ (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0}, 0.0),
            (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}, 1.0),
            (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 2}, 2.0)
          ]
      )

intervalPotentialField :: Either String H.ScalarPotentialField
intervalPotentialField = do
  carrierValue <-
    first show $
      H.mkCellCarrier
        (H.HomologicalDegree 0)
        [ H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0},
          H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}
        ]
  first show $
    H.mkScalarPotentialFieldFromSamples
      carrierValue
      H.NativePotentialScale
      ( Map.fromList
          [ (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0}, 0.0),
            (H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 1}, 1.0)
          ]
      )

defaultTopologyConfig :: H.TopologyObservationConfig Integer
defaultTopologyConfig =
  H.TopologyObservationConfig
    { H.observationFiltration = Nothing,
      H.observationPotential = Nothing,
      H.observationLowModeCount = 0
    }

intervalMacroScaffold :: Either String H.MacroScaffoldIR
intervalMacroScaffold =
  observedMacroScaffold intervalObservation intervalComplex

intervalCompositionFixture :: CompositionFixture
intervalCompositionFixture =
  CompositionFixture
    { compositionFixtureLabel = "interval scaffold",
      compositionFixtureScaffold = intervalMacroScaffold
    }

triangleMacroScaffold :: Either String H.MacroScaffoldIR
triangleMacroScaffold =
  observedMacroScaffold triangleObservation triangleCycleComplex

triangleCompositionFixture :: CompositionFixture
triangleCompositionFixture =
  CompositionFixture
    { compositionFixtureLabel = "triangle scaffold",
      compositionFixtureScaffold = triangleMacroScaffold
    }

observedMacroScaffold ::
  Either String (H.TopologyObservationConfig Integer) ->
  H.FiniteChainComplex Integer ->
  Either String H.MacroScaffoldIR
observedMacroScaffold observationResult finiteComplex = do
  observationValue <- observationResult
  witnessValue <- first show (H.observeTopologyWitness observationValue finiteComplex)
  maybe
    (Left "expected macro scaffold")
    Right
    (H.topologyMacroScaffold witnessValue)

rebuildAngleDirectionField :: H.DirectionField -> Either String H.DirectionField
rebuildAngleDirectionField directionFieldValue = do
  phaseValue <- first show (H.mkDirectionPhase 0.0)
  first show $
    H.mkDirectionAngleField
      (H.directionFieldCarrier directionFieldValue)
      (H.directionFieldSymmetryOrder directionFieldValue)
      ( Map.fromList
          ( fmap
              (\basisCellRef -> (basisCellRef, phaseValue))
              (H.carrierCells (H.directionFieldCarrier directionFieldValue))
          )
      )

basisRefs :: H.HomologicalDegree -> [Int] -> [H.BasisCellRef]
basisRefs degreeValue =
  fmap
    (\indexValue -> H.BasisCellRef {H.cellDegree = degreeValue, H.cellIndex = indexValue})

type CompositionFixture :: Type
data CompositionFixture = CompositionFixture
  { compositionFixtureLabel :: String,
    compositionFixtureScaffold :: Either String H.MacroScaffoldIR
  }

withFixture :: CompositionFixture -> (H.MacroScaffoldIR -> Assertion) -> Assertion
withFixture fixtureValue assertion = do
  scaffoldValue <- expectRight (compositionFixtureScaffold fixtureValue)
  assertion scaffoldValue

withComposedScaffolds ::
  CompositionFixture ->
  CompositionFixture ->
  (H.MacroScaffoldIR -> H.MacroScaffoldIR -> H.MacroScaffoldIR -> Assertion) ->
  Assertion
withComposedScaffolds leftFixture rightFixture assertion =
  withFixture leftFixture $ \leftScaffold ->
    withFixture rightFixture $ \rightScaffold -> do
      composedScaffold <- expectRight (H.composeMacroScaffolds (leftScaffold :| [rightScaffold]))
      assertion leftScaffold rightScaffold composedScaffold

withSelfComposition ::
  CompositionFixture ->
  (H.MacroScaffoldIR -> H.MacroScaffoldIR -> Assertion) ->
  Assertion
withSelfComposition fixtureValue assertion =
  withComposedScaffolds fixtureValue fixtureValue (\scaffoldValue _ composedScaffold -> assertion scaffoldValue composedScaffold)

assertSingletonIdentity :: CompositionFixture -> Assertion
assertSingletonIdentity fixtureValue =
  withFixture fixtureValue $ \scaffoldValue ->
    H.composeMacroScaffolds (scaffoldValue :| []) @?= Right scaffoldValue

type ScaffoldCardinalities :: Type
data ScaffoldCardinalities = ScaffoldCardinalities
  { scaffoldScalarCarrierCount :: Int,
    scaffoldDirectionCarrierCount :: Int,
    scaffoldReebNodeCount :: Int,
    scaffoldReebArcCount :: Int,
    scaffoldSingularityCount :: Int,
    scaffoldHarmonicLoopCount :: Int
  }
  deriving stock (Eq, Show)

scaffoldCardinalities :: H.MacroScaffoldIR -> ScaffoldCardinalities
scaffoldCardinalities scaffoldValue =
  ScaffoldCardinalities
    { scaffoldScalarCarrierCount =
        length (H.carrierCells (H.scalarPotentialCarrier (H.macroScaffoldScalarPotential scaffoldValue))),
      scaffoldDirectionCarrierCount =
        length (H.carrierCells (H.directionFieldCarrier (H.macroScaffoldDirectionField scaffoldValue))),
      scaffoldReebNodeCount =
        length (H.morseReebNodes (H.macroScaffoldReeb scaffoldValue)),
      scaffoldReebArcCount =
        length (H.morseReebArcs (H.macroScaffoldReeb scaffoldValue)),
      scaffoldSingularityCount =
        length (H.macroScaffoldSingularities scaffoldValue),
      scaffoldHarmonicLoopCount =
        length (H.macroScaffoldHarmonicLoops scaffoldValue)
    }

addScaffoldCardinalities :: ScaffoldCardinalities -> ScaffoldCardinalities -> ScaffoldCardinalities
addScaffoldCardinalities leftCounts rightCounts =
  ScaffoldCardinalities
    { scaffoldScalarCarrierCount =
        scaffoldScalarCarrierCount leftCounts + scaffoldScalarCarrierCount rightCounts,
      scaffoldDirectionCarrierCount =
        scaffoldDirectionCarrierCount leftCounts + scaffoldDirectionCarrierCount rightCounts,
      scaffoldReebNodeCount =
        scaffoldReebNodeCount leftCounts + scaffoldReebNodeCount rightCounts,
      scaffoldReebArcCount =
        scaffoldReebArcCount leftCounts + scaffoldReebArcCount rightCounts,
      scaffoldSingularityCount =
        scaffoldSingularityCount leftCounts + scaffoldSingularityCount rightCounts,
      scaffoldHarmonicLoopCount =
        scaffoldHarmonicLoopCount leftCounts + scaffoldHarmonicLoopCount rightCounts
    }

assertDistinct :: Ord a => String -> [a] -> Assertion
assertDistinct message values =
  assertBool message (Set.size (Set.fromList values) == length values)

assertSymmetricCompositionFailure ::
  H.MacroScaffoldCompositionError ->
  H.MacroScaffoldIR ->
  H.MacroScaffoldIR ->
  Assertion
assertSymmetricCompositionFailure expectedFailure leftScaffold rightScaffold = do
  H.composeMacroScaffolds (leftScaffold :| [rightScaffold]) @?= Left expectedFailure
  H.composeMacroScaffolds (rightScaffold :| [leftScaffold]) @?= Left expectedFailure

reebCycleRank :: H.MorseReebScaffold -> Int
reebCycleRank reebValue =
  let nodes = H.morseReebNodes reebValue
      arcs = H.morseReebArcs reebValue
      adjacency = foldr insertArc (Map.fromList (fmap (\nodeValue -> (H.morseReebNodeId nodeValue, Set.empty)) nodes)) arcs
      componentCount = H.connectedComponentsFromAdjacency adjacency
   in max 0 (length arcs - length nodes + componentCount)
  where
    insertArc arcValue =
      connectNodes
        (H.morseReebArcSource arcValue)
        (H.morseReebArcTarget arcValue)

connectNodes ::
  H.ReebNodeId ->
  H.ReebNodeId ->
  Map.Map H.ReebNodeId (Set.Set H.ReebNodeId) ->
  Map.Map H.ReebNodeId (Set.Set H.ReebNodeId)
connectNodes sourceNode targetNode =
  Map.alter (Just . Set.insert targetNode . maybe Set.empty id) sourceNode
    . Map.alter (Just . Set.insert sourceNode . maybe Set.empty id) targetNode

testCohomologyBasisTriangle :: Assertion
testCohomologyBasisTriangle =
  let cocycles = H.cohomologyBasisAt triangleCycleComplex (H.HomologicalDegree 1)
   in assertEqual "H^1 cocycle count for triangle cycle" 1 (length cocycles)

testCohomologyBasisInterval :: Assertion
testCohomologyBasisInterval =
  let cocycles = H.cohomologyBasisAt intervalComplex (H.HomologicalDegree 1)
   in cocycles @?= []

testCohomologyBasisDegenerate :: Assertion
testCohomologyBasisDegenerate =
  let degenerateComplex :: H.FiniteChainComplex Integer
      degenerateComplex =
        H.mkFiniteChainComplex (H.HomologicalDegree 1) $ \dimensionValue ->
          case dimensionValue of
            H.HomologicalDegree 1 -> H.emptyBoundaryIncidenceOf 0 100
            H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 100 0
            _ -> H.emptyBoundaryIncidence
      cocycles = H.cohomologyBasisAt degenerateComplex (H.HomologicalDegree 1)
   in cocycles @?= []

presentationCoordinateTests :: TestTree
presentationCoordinateTests =
  testGroup
    "presentation coordinates (solveLinearCombination regression)"
    [ testCase "zero-dimensional quotient accepts empty target" $
        H.presentationCoordinates (mkQuotientPresentation 0 [] [] []) [] @?= Just [],
      testCase "trivial quotient in nonzero ambient space accepts zero vector" $
        H.presentationCoordinates (mkQuotientPresentation 3 [] [] []) [0, 0, 0] @?= Just [],
      testCase "trivial quotient in nonzero ambient space rejects nonzero vector" $
        H.presentationCoordinates (mkQuotientPresentation 3 [] [] []) [1, 0, 0] @?= Nothing,
      testCase "single basis vector yields correct coordinate" $
        H.presentationCoordinates (mkQuotientPresentation 2 [[1, 0]] [] []) [3, 0] @?= Just [3],
      testCase "vector outside basis span returns Nothing" $
        H.presentationCoordinates (mkQuotientPresentation 2 [[1, 0]] [] []) [0, 1] @?= Nothing,
      testCase "denominator absorbs component leaving quotient coordinates" $
        H.presentationCoordinates (mkQuotientPresentation 2 [[1, 0]] [] [[0, 1]]) [5, 7] @?= Just [5]
    ]
