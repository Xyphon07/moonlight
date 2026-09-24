module Main (main) where

import Data.Bits (shiftL)
import Data.Bifunctor (first)
import Data.Either (rights)
import Data.Function ((&))
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Hex.CellComplex (finiteHexCellComplex)
import Moonlight.Hex.Coordinate (HexCoord (..), allHexDirections, hexStepCoord)
import Moonlight.Hex.Region (hexLayout, hexRegionFromCoords)
import Moonlight.Homology.Boundary
  ( BoundaryEntry
  , boundaryCoefficient
  , boundaryEntries
  , degreeCardinality
  , incidenceMatrixAt
  , sourceIndex
  , targetIndex
  )
import Moonlight.Homology.Chain
  ( HomologicalDegree (..)
  , PersistencePair (..)
  )
import Moonlight.Homology.Persistence
  ( FilteredFiniteChainComplex
  , filteredBaseComplex
  , filteredCellBirths
  , mod2PersistentPairs
  )
import Moonlight.Homology.Topology
  ( BasisCellRef (..)
  , freeBettiVector
  )
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
    ValidateComplex2D (..),
    eulerCharacteristic,
    isBoundaryEdge,
  )
import Moonlight.Planar.Alpha
  ( AlphaBirth
  , AlphaFiltration
  , alphaBirthDenominator
  , alphaBirthNumerator
  , alphaBirthFromRadiusSquared
  , alphaComplexAtBirth
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaFiltration
  , alphaFiltrationCellSet
  , alphaFiltrationCriticalBirths
  , alphaVertexBirth
  )
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.CellSet
  ( ExactCellSet,
    CellSelectionError (..),
    closeFaceCellSet,
    exactCellSet,
    exactCellSetEdgeCount,
    exactCellSetFaceCount,
    exactCellSetVertexCount,
    foldExactCellEdges,
    foldExactCellFaces,
    foldExactCellVertices,
  )
import Moonlight.Planar.CellComplex
  ( DCELComplex
  , DCELCellEdge (..)
  , filteredAlphaComplex
  , finiteChainComplex
  , fromExactCellSet
  )
import Moonlight.Planar.Dcel
  ( undirectedEndpoints
  , vertexPoint
  )
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  )
import Moonlight.Planar.Internal.CellSet (closeExactCellSetWith)
import Moonlight.Planar.Internal.Incidence (admitPlanarIncidence)
import Moonlight.Planar.Internal.PackedIndex (noIndex)
import Moonlight.Planar.Internal.Paged qualified as Paged
import Moonlight.Planar.Exact (ExactPoint, exactPointFromPoint)
import Moonlight.Planar.Handles.Iterators.FixedIterators qualified as Resident
import Moonlight.Planar.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  )
import Moonlight.Planar.Internal.Dyadic (exactDiametralDot)
import Moonlight.Planar.Internal.ExactRational
  ( exactRational
  , exactRationalFromDyadic
  , exactRationalFromDyadicRatio
  )
import Moonlight.Planar.Internal.Predicates (inDiametralCircle)
import Moonlight.Planar.LabelledComplex (filteredPlanarComplex)
import Moonlight.Planar.Simplex
  ( planarComplex
  , planarEdge
  , planarFace
  , planarVertex
  )
import Moonlight.Planar.Types (DelaunayTriangulation)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Scalar (mkRadiusSquared)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "ExactCellSet bridge"
    [ testCase "preserves the admitted cell inventory" preserveCellInventory,
      testCase "preserves downward-closed incidence" preserveClosedIncidence,
      testCase "preserves ascending incidence for every square subcomplex and absent handle" exhaustiveSelectedIncidence,
      testCase "preserves sparse selections around a high-degree resident vertex" sparseHighDegreeIncidence,
      testCase "marks the triangular exterior as absent" preserveExteriorAdjacency,
      testCase "lowers oriented DCEL incidence to a checked integral chain complex" lowerIntegralChainComplex,
      testCase "lowers a native hexagonal ring through the same chain owner" lowerHexagonalRing,
      testCase "assigns exact Gabriel and non-Gabriel alpha births" exactAlphaBirths,
      testCase "derives the constant vertex birth section over exactly its resident support" constantAlphaVertexSection,
      testCase "preserves exact dyadic rational normalization" exactDyadicRationalNormalization,
      testCase "orders cached alpha keys by their exact rational births" alphaBirthOrderMatchesExactRationals,
      testCase "agrees with the exact diametral predicate" diametralPredicateMatchesExactSign,
      testCase "keeps every boundary cell no later than its coface" alphaBirthsAreFaceMonotone,
      testCase "finds the square alpha hole from radius one to radius two" squareAlphaPersistence,
      testCase "lowers a labelled planar filtration through the same Homology owner" labelledPlanarPersistence,
      testCase "keeps point and collinear alpha filtrations total" degenerateSupportFiltrations
    , testCase "refines annular faces without confusing strata with disk cells" exactAnnulusRefinement
    , testCase "refines isolated vertices and floating tree boundary components" exactDisconnectedBoundaryRefinement
    , testCase "retains repeated slit darts and vertex-only face contacts" exactSlitAndContactRefinement
    ]

-- Literal embedded planar maps, independent of the arrangement constructor.
-- Darts use (origin, next, previous, incident face); even/odd darts are twins.
data CellularFixture = CellularFixture
  { fixturePoints :: ![Point]
  , fixtureVertexRoots :: ![Maybe Int]
  , fixtureDarts :: ![(Int, Int, Int, Int)]
  , fixtureFaceRoots :: ![Maybe Int]
  , fixtureInnerRoots :: ![(Int, [Int])]
  , fixtureIsolatedVertices :: ![(Int, [Int])]
  }

refineCellularFixture
  :: CellularFixture
  -> [Int]
  -> [Int]
  -> [Int]
  -> IO (ExactCellSet, DCELComplex)
refineCellularFixture fixture selectedVertices selectedEdges selectedFaces = do
  incidence <- requireRight "literal planar incidence"
    (admitPlanarIncidence
      (Paged.fromVector noIndex (Unboxed.fromList (fmap (maybe noIndex fromIntegral) (fixtureVertexRoots fixture))))
      (Paged.fromVector noIndex (Unboxed.fromList
        (concatMap (\(origin, next, previous, face) -> fmap fromIntegral [origin, next, previous, face]) (fixtureDarts fixture))))
      (Paged.fromVector noIndex (Unboxed.fromList (fmap (maybe noIndex fromIntegral) (fixtureFaceRoots fixture))))
      (IntMap.fromList (fmap (fmap (fmap (DirectedEdgeId . fromIntegral))) (fixtureInnerRoots fixture)))
      (IntMap.fromList (fmap (fmap IntSet.fromList) (fixtureIsolatedVertices fixture))))
  let points = Vector.fromList (fixturePoints fixture)
      exactPointAt vertex@(VertexId rawVertex) =
        case points Vector.!? fromIntegral rawVertex of
          Nothing -> Left (CellVertexOutOfRange vertex (Vector.length points))
          Just point -> first (CellCoordinateInvalid vertex) (exactPointFromPoint point)
  selection <- requireRight "literal closed cell selection"
    (closeExactCellSetWith exactPointAt incidence
      (fmap (VertexId . fromIntegral) selectedVertices)
      (fmap (UndirectedEdgeId . fromIntegral) selectedEdges)
      (fmap (FaceId . fromIntegral) selectedFaces))
  refined <- requireRight "literal cellular refinement" (fromExactCellSet selection)
  pure (selection, refined)

assertCellularBetti :: [Int] -> DCELComplex -> Assertion
assertCellularBetti expected complexValue = do
  finite <- requireRight "refined cellular chains" (finiteChainComplex complexValue)
  freeBettiVector finite @?= expected
  eulerCharacteristic complexValue @?= sum (zipWith (*) (cycle [1, -1]) expected)
  assertIncidenceQueries complexValue (vertices complexValue)

exactAnnulusRefinement :: Assertion
exactAnnulusRefinement = do
  (selection, annulus) <- refineCellularFixture annulusFixture [] [] [1]
  (exactCellSetVertexCount selection, exactCellSetEdgeCount selection, exactCellSetFaceCount selection) @?= (8, 8, 1)
  (length (vertices annulus), length (edges annulus), length (faces annulus)) @?= (8, 9, 1)
  assertCellularBetti [1, 1, 0] annulus
  (_, filled) <- refineCellularFixture annulusFixture [] [] [1, 2]
  assertCellularBetti [1, 0, 0] filled
  let cuts :: DCELComplex -> [DCELCellEdge]
      cuts complexValue = [edge | edge@FaceBoundaryCut {} <- edges complexValue]
  cuts annulus @?= cuts filled
  traverse_ (\edge -> facesAtEdge annulus edge @?= (Just (FaceId 1), Just (FaceId 1))) (cuts annulus)
  (_, boundaryOnly) <- refineCellularFixture annulusFixture [] [0 .. 7] []
  assertCellularBetti [2, 2, 0] boundaryOnly
  (_, emptySelection) <- refineCellularFixture annulusFixture [] [] []
  assertCellularBetti [0, 0, 0] emptySelection

exactDisconnectedBoundaryRefinement :: Assertion
exactDisconnectedBoundaryRefinement = do
  (_, markedDisk) <- refineCellularFixture isolatedPointFixture [] [] [1]
  (length (vertices markedDisk), length (edges markedDisk), length (faces markedDisk)) @?= (5, 5, 1)
  assertCellularBetti [1, 0, 0] markedDisk
  (_, floatingSlitDisk) <- refineCellularFixture floatingSlitFixture [] [] [1]
  (length (vertices floatingSlitDisk), length (edges floatingSlitDisk), length (faces floatingSlitDisk)) @?= (6, 6, 1)
  assertCellularBetti [1, 0, 0] floatingSlitDisk
  (_, isolatedSelection) <- refineCellularFixture isolatedPointFixture [4] [] []
  assertCellularBetti [1, 0, 0] isolatedSelection
  (_, slitSelection) <- refineCellularFixture floatingSlitFixture [] [4] []
  assertCellularBetti [1, 0, 0] slitSelection

exactSlitAndContactRefinement :: Assertion
exactSlitAndContactRefinement = do
  (_, slitDisk) <- refineCellularFixture attachedSlitFixture [] [] [1]
  length (edges slitDisk) @?= 5
  length (faceBoundary slitDisk (FaceId 1)) @?= 6
  assertCellularBetti [1, 0, 0] slitDisk
  (_, pointContact) <- refineCellularFixture pointContactFixture [] [] [1, 2]
  (length (vertices pointContact), length (edges pointContact), length (faces pointContact)) @?= (7, 8, 2)
  assertCellularBetti [1, 0, 0] pointContact

outerSquarePoints :: [Point]
outerSquarePoints = [Point (-3) (-3), Point 3 (-3), Point 3 3, Point (-3) 3]

outerSquareDarts :: [(Int, Int, Int, Int)]
outerSquareDarts =
  [ (0, 2, 6, 1), (1, 7, 3, 0), (1, 4, 0, 1), (2, 1, 5, 0)
  , (2, 6, 2, 1), (3, 3, 7, 0), (3, 0, 4, 1), (0, 5, 1, 0)
  ]

annulusFixture :: CellularFixture
annulusFixture = CellularFixture
  (outerSquarePoints <> [Point (-1) (-1), Point 1 (-1), Point 1 1, Point (-1) 1])
  (fmap Just [0, 2, 4, 6, 8, 10, 12, 14])
  (outerSquareDarts <>
    [ (4, 10, 14, 2), (5, 15, 11, 1), (5, 12, 8, 2), (6, 9, 13, 1)
    , (6, 14, 10, 2), (7, 11, 15, 1), (7, 8, 12, 2), (4, 13, 9, 1)
    ])
  (fmap Just [1, 0, 8])
  [(1, [9])]
  []

isolatedPointFixture :: CellularFixture
isolatedPointFixture = CellularFixture
  (outerSquarePoints <> [Point 0 0])
  (fmap Just [0, 2, 4, 6] <> [Nothing])
  outerSquareDarts
  (fmap Just [1, 0])
  []
  [(1, [4])]

floatingSlitFixture :: CellularFixture
floatingSlitFixture = CellularFixture
  (outerSquarePoints <> [Point (-1) 0, Point 1 0])
  (fmap Just [0, 2, 4, 6, 8, 9])
  (outerSquareDarts <> [(4, 9, 9, 1), (5, 8, 8, 1)])
  (fmap Just [1, 0])
  [(1, [8])]
  []

attachedSlitFixture :: CellularFixture
attachedSlitFixture = CellularFixture
  (outerSquarePoints <> [Point 0 0])
  (fmap Just [0, 2, 4, 6, 9])
  [ (0, 2, 9, 1), (1, 7, 3, 0), (1, 4, 0, 1), (2, 1, 5, 0)
  , (2, 6, 2, 1), (3, 3, 7, 0), (3, 8, 4, 1), (0, 5, 1, 0)
  , (0, 9, 6, 1), (4, 0, 8, 1)
  ]
  (fmap Just [1, 0])
  []
  []

pointContactFixture :: CellularFixture
pointContactFixture = CellularFixture
  [Point (-2) (-2), Point 0 (-2), Point 0 0, Point (-2) 0, Point 2 0, Point 2 2, Point 0 2]
  (fmap Just [0, 2, 4, 6, 10, 12, 14])
  [ (0, 2, 6, 1), (1, 7, 3, 0), (1, 4, 0, 1), (2, 1, 9, 0)
  , (2, 6, 2, 1), (3, 15, 7, 0), (3, 0, 4, 1), (0, 5, 1, 0)
  , (2, 10, 14, 2), (4, 3, 11, 0), (4, 12, 8, 2), (5, 9, 13, 0)
  , (5, 14, 10, 2), (6, 11, 15, 0), (6, 8, 12, 2), (2, 13, 5, 0)
  ]
  (fmap Just [1, 0, 8])
  []
  []

preserveCellInventory :: Assertion
preserveCellInventory =
  withTriangleComplex $ \cellSet complexValue -> do
    length (vertices complexValue) @?= exactCellSetVertexCount cellSet
    length (edges complexValue) @?= exactCellSetEdgeCount cellSet
    length (faces complexValue) @?= exactCellSetFaceCount cellSet
    (length (vertices complexValue), length (edges complexValue), length (faces complexValue))
      @?= (3, 3, 1)
    eulerCharacteristic complexValue @?= 1
    validateComplex complexValue @?= []

preserveClosedIncidence :: Assertion
preserveClosedIncidence =
  withTriangleComplex $ \_ complexValue -> do
    traverse_ (assertSelectedEdgeEndpoints complexValue) (edges complexValue)
    traverse_ (assertSelectedFaceBoundary complexValue) (faces complexValue)
    fmap (length . edgesAtVertex complexValue) (vertices complexValue) @?= [2, 2, 2]

exhaustiveSelectedIncidence :: Assertion
exhaustiveSelectedIncidence = do
  triangulation <- requireDelaunay "incidence square" squarePoints
  let selections =
        rights
          [ exactCellSet triangulation selectedVertices selectedEdges selectedFaces
          | selectedVertices <- List.subsequences (Resident.vertices triangulation)
          , selectedEdges <- List.subsequences (undirectedEdges triangulation)
          , selectedFaces <- List.subsequences (innerFaces triangulation)
          ]
      queries = Resident.vertices triangulation <> [VertexId 4, VertexId maxBound]
  length selections @?= 83
  traverse_
    (\selection -> do
      complexValue <- requireRight "selected cellular refinement" (fromExactCellSet selection)
      assertIncidenceQueries complexValue queries)
    selections

sparseHighDegreeIncidence :: Assertion
sparseHighDegreeIncidence = do
  triangulation <- requireDelaunay "high-degree incidence fan" fanPoints
  centralVertex <-
    requireSome "fan central resident handle"
      (List.find ((== Point 0 0) . vertexPoint triangulation) (Resident.vertices triangulation))
  let centralEdges =
        filter
          (\edge -> let (source, target) = undirectedEndpoints triangulation edge in centralVertex == source || centralVertex == target)
          (undirectedEdges triangulation)
      selectedFamilies = [[], take 1 centralEdges, take 3 centralEdges, centralEdges]
  assertBool "fixture retains a high-degree central star" (length centralEdges >= 64)
  traverse_
    (\selectedEdges -> do
        let selectedVertices =
              Set.toAscList
                ( Set.fromList
                    (centralVertex : concatMap (\edge -> let (source, target) = undirectedEndpoints triangulation edge in [source, target]) selectedEdges)
                )
        selection <- requireRight "sparse fan selection" (exactCellSet triangulation selectedVertices selectedEdges [])
        complexValue <- requireRight "sparse fan cellular refinement" (fromExactCellSet selection)
        assertIncidenceQueries complexValue (Resident.vertices triangulation <> [VertexId maxBound]))
    selectedFamilies
 where
  fanPoints =
    Vector.fromList
      ( Point 0 0
          : [ Point (100 * cos angle) (100 * sin angle)
            | index <- [0 .. 127 :: Int]
            , let angle = 2 * pi * fromIntegral index / 128
            ]
      )

assertIncidenceQueries :: DCELComplex -> [VertexId] -> Assertion
assertIncidenceQueries complexValue =
  traverse_
    (\vertex ->
        edgesAtVertex complexValue vertex
          @?= filter
            (\edge -> let (source, target) = edgeBoundary complexValue edge in vertex == source || vertex == target)
            (edges complexValue))

preserveExteriorAdjacency :: Assertion
preserveExteriorAdjacency =
  withTriangleComplex $ \_ complexValue ->
    assertBool
      "every edge of a single selected triangle has one exterior incident face"
      (all (isBoundaryEdge complexValue) (edges complexValue))

lowerIntegralChainComplex :: Assertion
lowerIntegralChainComplex =
  withTriangleComplex $ \_ complexValue -> do
    finite <- requireRight "triangle finite chain lowering" (finiteChainComplex complexValue)
    freeBettiVector finite @?= [1, 0, 0]

lowerHexagonalRing :: Assertion
lowerHexagonalRing = do
  layout <- requireRight "hexagonal ring layout" (hexLayout (HexCoord (-1) (-1)) 3 3)
  let ringCoordinates =
        [ coordinate
        | direction <- NonEmpty.toList allHexDirections
        , Just coordinate <- [hexStepCoord (HexCoord 0 0) direction]
        ]
  region <- requireRight "hexagonal ring region" (hexRegionFromCoords layout ringCoordinates)
  finite <- requireRight "hexagonal ring chain lowering" (finiteHexCellComplex region)
  freeBettiVector finite @?= [1, 1, 0]

exactAlphaBirths :: Assertion
exactAlphaBirths = do
  triangulation <- requireDelaunay "obtuse triangle" obtuseTrianglePoints
  filtration <- requireRight "obtuse triangle alpha filtration" (alphaFiltration triangulation)
  complexValue <- requireRight "obtuse triangle cellular refinement" (fromExactCellSet (alphaFiltrationCellSet filtration))
  let vertexBirthRatios =
        vertices complexValue
          & fmap (fmap alphaBirthRatio . alphaVertexBirth filtration)
      faceBirthRatios =
        faces complexValue
          & fmap (fmap alphaBirthRatio . alphaFaceBirth filtration)
  assertBool "every vertex is born at zero" (all (== Just (0, 1)) vertexBirthRatios)
  faceBirthRatios @?= [Just (25, 4)]
  longEdge <- requireSome "obtuse triangle long edge" (edgeBetween triangulation (Point (-2) 0) (Point 2 0))
  fmap alphaBirthRatio (alphaEdgeBirth filtration longEdge) @?= Just (25, 4)
  let shortEdgeBirths =
        undirectedEdges triangulation
          & filter (/= longEdge)
          & fmap (fmap alphaBirthRatio . alphaEdgeBirth filtration)
  shortEdgeBirths @?= [Just (5, 4), Just (5, 4)]

constantAlphaVertexSection :: Assertion
constantAlphaVertexSection = do
  zeroBirth <- alphaBirthFromRadiusSquared <$> requireRight "zero alpha radius" (mkRadiusSquared 0)
  traverse_ (assertConstantSection zeroBirth)
    [ ("empty alpha support", Vector.empty)
    , ("singleton alpha support", Vector.singleton (Point 3 7))
    , ("collinear alpha support", Vector.fromList [Point 0 0, Point 1 0, Point 3 0])
    , ("square alpha support", squarePoints)
    ]
 where
  assertConstantSection :: AlphaBirth -> (String, Vector.Vector Point) -> Assertion
  assertConstantSection zeroBirth (label, points) = do
    triangulation <- requireDelaunay label points
    filtration <- requireRight (label <> " filtration") (alphaFiltration triangulation)
    let fullCells = alphaFiltrationCellSet filtration
        vertexCount = exactCellSetVertexCount fullCells
        zeroCells = alphaComplexAtBirth zeroBirth filtration
        exactVertices :: ExactCellSet -> [(VertexId, ExactPoint)]
        exactVertices = foldExactCellVertices (\values vertex point -> (vertex, point) : values) []
    traverse_
      (\index -> alphaVertexBirth filtration (VertexId (fromIntegral index)) @?= Just zeroBirth)
      [0 .. vertexCount - 1]
    alphaVertexBirth filtration (VertexId (fromIntegral vertexCount)) @?= Nothing
    alphaVertexBirth filtration (VertexId maxBound) @?= Nothing
    alphaFiltrationCriticalBirths filtration @?= Set.toAscList (Set.fromList (allAlphaBirths filtration))
    exactVertices zeroCells @?= exactVertices fullCells
    exactCellSetEdgeCount zeroCells @?= 0
    exactCellSetFaceCount zeroCells @?= 0
    traverse_
      (\birth -> exactVertices (alphaComplexAtBirth birth filtration) @?= exactVertices fullCells)
      (alphaFiltrationCriticalBirths filtration)

exactDyadicRationalNormalization :: Assertion
exactDyadicRationalNormalization = do
  traverse_ assertDyadic
    [ (0, -200)
    , (12, -5)
    , (-12, -5)
    , (3, 4)
    , (2 ^ (80 :: Int) + 8, -70)
    ]
  traverse_ assertDyadicRatio
    [ (0, 7, -200)
    , (96, 40, -11)
    , (-96, 40, 9)
    , (45, -28, -3)
    , (2 ^ (120 :: Int) + 24, 2 ^ (75 :: Int) + 12, -51)
    ]
 where
  assertDyadic (numerator, power) =
    Right (exactRationalFromDyadic numerator power)
      @?= ( if power >= 0
              then exactRational (numerator `shiftL` power) 1
              else exactRational numerator (1 `shiftL` negate power)
          )
  assertDyadicRatio (numerator, denominator, power) =
    exactRationalFromDyadicRatio numerator denominator power
      @?= ( if power >= 0
              then exactRational (numerator `shiftL` power) denominator
              else exactRational numerator (denominator `shiftL` negate power)
          )

alphaBirthOrderMatchesExactRationals :: Assertion
alphaBirthOrderMatchesExactRationals = do
  filtrations <-
    traverse
      (\(label, points) -> requireDelaunay label points >>= requireRight (label <> " alpha filtration") . alphaFiltration)
      [ ("exact-order obtuse triangle", obtuseTrianglePoints)
      , ("exact-order square", squarePoints)
      ]
  let births = filtrations >>= allAlphaBirths
  traverse_
    ( \(leftBirth, rightBirth) ->
        compare leftBirth rightBirth
          @?= compare (alphaBirthRational leftBirth) (alphaBirthRational rightBirth)
    )
    [(leftBirth, rightBirth) | leftBirth <- births, rightBirth <- births]

diametralPredicateMatchesExactSign :: Assertion
diametralPredicateMatchesExactSign =
  traverse_
    ( \(firstPoint, secondPoint, witnessPoint) ->
        inDiametralCircle firstPoint secondPoint witnessPoint
          @?= exactDiametralMembership firstPoint secondPoint witnessPoint
    )
    [ (Point (-1) 0, Point 1 0, Point 0 0)
    , (Point (-1) 0, Point 1 0, Point 0 2)
    , (Point (-1) 0, Point 1 0, Point 0 1)
    , (Point (-1) 0, Point 1 0, Point 0 (1 + encodeFloat 1 (-52)))
    , (Point (-1e40) 0, Point 1e40 0, Point 0 1e40)
    ]

allAlphaBirths :: AlphaFiltration -> [AlphaBirth]
allAlphaBirths filtration =
  let cellSet = alphaFiltrationCellSet filtration
   in mapMaybe (alphaVertexBirth filtration) (foldExactCellVertices (\values vertex _ -> vertex : values) [] cellSet)
        <> mapMaybe (alphaEdgeBirth filtration) (foldExactCellEdges (flip (:)) [] cellSet)
        <> mapMaybe (alphaFaceBirth filtration) (foldExactCellFaces (flip (:)) [] cellSet)

alphaBirthRational :: AlphaBirth -> Rational
alphaBirthRational birth =
  alphaBirthNumerator birth % alphaBirthDenominator birth

exactDiametralMembership :: Point -> Point -> Point -> Bool
exactDiametralMembership (Point ax ay) (Point bx by) (Point px py) =
  exactDiametralDot ax ay bx by px py <= 0

alphaBirthsAreFaceMonotone :: Assertion
alphaBirthsAreFaceMonotone = do
  triangulation <- requireDelaunay "alpha monotonicity square" squarePoints
  filtration <- requireRight "alpha monotonicity filtration" (alphaFiltration triangulation)
  filtered <- requireRight "alpha monotonicity filtered complex" (filteredAlphaComplex filtration)
  traverse_
    (assertBoundaryDegreeMonotone filtered)
    [HomologicalDegree 1, HomologicalDegree 2]

assertBoundaryDegreeMonotone
  :: FilteredFiniteChainComplex AlphaBirth Int
  -> HomologicalDegree
  -> Assertion
assertBoundaryDegreeMonotone filtered degreeValue =
  traverse_
    (assertBoundaryEntryMonotone filtered degreeValue)
    ( boundaryEntries
        (incidenceMatrixAt (filteredBaseComplex filtered) degreeValue)
    )

assertBoundaryEntryMonotone
  :: FilteredFiniteChainComplex AlphaBirth Int
  -> HomologicalDegree
  -> BoundaryEntry Int
  -> Assertion
assertBoundaryEntryMonotone filtered degreeValue@(HomologicalDegree degreeIndex) entryValue = do
  let sourceCell =
        BasisCellRef
          { cellDegree = degreeValue
          , cellIndex = sourceIndex entryValue
          }
      targetCell =
        BasisCellRef
          { cellDegree = HomologicalDegree (degreeIndex - 1)
          , cellIndex = targetIndex entryValue
          }
      births = filteredCellBirths filtered
  sourceBirth <- requireSome "source alpha birth" (Map.lookup sourceCell births)
  targetBirth <- requireSome "boundary alpha birth" (Map.lookup targetCell births)
  assertBool
    ("boundary birth exceeds coface birth for " <> show (targetCell, sourceCell, boundaryCoefficient entryValue))
    (targetBirth <= sourceBirth)

squareAlphaPersistence :: Assertion
squareAlphaPersistence = do
  triangulation <- requireDelaunay "cocircular square" squarePoints
  filtration <- requireRight "cocircular square alpha filtration" (alphaFiltration triangulation)
  filtered <- requireRight "cocircular square filtered chain complex" (filteredAlphaComplex filtration)
  pairs <- requireRight "cocircular square persistence" (mod2PersistentPairs filtered)
  let positiveDegreeOnePairs =
        pairs
          & filter ((== HomologicalDegree 1) . persistenceDegree)
          & mapMaybe positiveBirthAndDeath
  positiveDegreeOnePairs @?= [((1, 1), (2, 1))]

labelledPlanarPersistence :: Assertion
labelledPlanarPersistence = do
  edgeAB <- requireRight "labelled edge ab" (planarEdge 'a' 'b')
  edgeAC <- requireRight "labelled edge ac" (planarEdge 'a' 'c')
  edgeBC <- requireRight "labelled edge bc" (planarEdge 'b' 'c')
  faceABC <- requireRight "labelled face abc" (planarFace 'a' 'b' 'c')
  complexValue <-
    requireRight
      "labelled triangle complex"
      ( planarComplex
          ( Set.fromList
              [ planarVertex 'a'
              , planarVertex 'b'
              , planarVertex 'c'
              , edgeAB
              , edgeAC
              , edgeBC
              , faceABC
              ]
          )
      )
  filtered <-
    requireRight
      "labelled triangle filtration"
      ( filteredPlanarComplex
          complexValue
          ( Map.fromList
              [ (planarVertex 'a', 0 :: Int)
              , (planarVertex 'b', 0)
              , (planarVertex 'c', 0)
              , (edgeAB, 1)
              , (edgeAC, 1)
              , (edgeBC, 1)
              , (faceABC, 2)
              ]
          )
      )
  pairs <- requireRight "labelled triangle persistence" (mod2PersistentPairs filtered)
  let oneCycles =
        [ pair
        | pair <- pairs
        , persistenceDegree pair == HomologicalDegree 1
        ]
  fmap (\pair -> (persistenceBirth pair, persistenceDeath pair)) oneCycles
    @?= [(1, Just 2)]

degenerateSupportFiltrations :: Assertion
degenerateSupportFiltrations =
  traverse_
    assertSupport
    [ ("two sites", Vector.fromList [Point 0 0, Point 2 0], (2, 1, 0))
    , ("three collinear sites", Vector.fromList [Point 0 0, Point 1 0, Point 3 0], (3, 2, 0))
    ]
 where
  assertSupport (label, points, expectedCounts) = do
    triangulation <- requireDelaunay label points
    filtration <- requireRight (label <> " alpha filtration") (alphaFiltration triangulation)
    filtered <- requireRight (label <> " filtered chain complex") (filteredAlphaComplex filtration)
    let finite = filteredBaseComplex filtered
        counts =
          ( degreeCardinality finite (HomologicalDegree 0)
          , degreeCardinality finite (HomologicalDegree 1)
          , degreeCardinality finite (HomologicalDegree 2)
          )
    counts @?= expectedCounts

positiveBirthAndDeath
  :: PersistencePair AlphaBirth
  -> Maybe ((Integer, Integer), (Integer, Integer))
positiveBirthAndDeath pairValue =
  case persistenceDeath pairValue of
    Just deathBirth
      | persistenceBirth pairValue < deathBirth ->
          Just
            ( alphaBirthRatio (persistenceBirth pairValue)
            , alphaBirthRatio deathBirth
            )
    _ -> Nothing

alphaBirthRatio :: AlphaBirth -> (Integer, Integer)
alphaBirthRatio birth =
  (alphaBirthNumerator birth, alphaBirthDenominator birth)

edgeBetween
  :: DelaunayTriangulation ()
  -> Point
  -> Point
  -> Maybe UndirectedEdgeId
edgeBetween triangulation firstPoint secondPoint =
  List.find hasEndpoints (undirectedEdges triangulation)
 where
  hasEndpoints edgeValue =
    let (fromVertex, toVertex) = undirectedEndpoints triangulation edgeValue
        fromPoint = vertexPoint triangulation fromVertex
        toPoint = vertexPoint triangulation toVertex
     in (fromPoint == firstPoint && toPoint == secondPoint)
          || (fromPoint == secondPoint && toPoint == firstPoint)

requireDelaunay
  :: String
  -> Vector.Vector Point
  -> IO (DelaunayTriangulation ())
requireDelaunay label points =
  requireRight (label <> " Delaunay construction") (delaunayGeometry points)

requireRight :: Show failure => String -> Either failure value -> IO value
requireRight label result =
  case result of
    Left failureValue -> assertFailure (label <> " failed: " <> show failureValue)
    Right value -> pure value

requireSome :: String -> Maybe value -> IO value
requireSome label maybeValue =
  case maybeValue of
    Nothing -> assertFailure (label <> " was absent")
    Just value -> pure value

obtuseTrianglePoints :: Vector.Vector Point
obtuseTrianglePoints =
  Vector.fromList
    [ Point (-2) 0
    , Point 2 0
    , Point 0 1
    ]

squarePoints :: Vector.Vector Point
squarePoints =
  Vector.fromList
    [ Point (-1) (-1)
    , Point 1 (-1)
    , Point 1 1
    , Point (-1) 1
    ]

assertSelectedEdgeEndpoints :: DCELComplex -> Edge DCELComplex -> Assertion
assertSelectedEdgeEndpoints complexValue edgeValue = do
  let selectedVertices = vertices complexValue
      (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
  assertBool "edge source is selected" (sourceVertex `elem` selectedVertices)
  assertBool "edge target is selected" (targetVertex `elem` selectedVertices)

assertSelectedFaceBoundary :: DCELComplex -> Face DCELComplex -> Assertion
assertSelectedFaceBoundary complexValue faceValue = do
  let selectedEdges = edges complexValue
      boundary = faceBoundary complexValue faceValue
  length boundary @?= 3
  assertBool
    "every oriented boundary edge is selected"
    (all ((`elem` selectedEdges) . orientedEdge) boundary)

withTriangleComplex :: (ExactCellSet -> DCELComplex -> Assertion) -> Assertion
withTriangleComplex assertion =
  case delaunayGeometry trianglePoints of
    Left buildFailure -> assertFailure ("triangle construction failed: " <> show buildFailure)
    Right triangulation ->
      case closeFaceCellSet triangulation (innerFaces triangulation) of
        Left selectionFailure -> assertFailure ("triangle selection failed: " <> show selectionFailure)
        Right cellSet -> do
          complexValue <- requireRight "triangle cellular refinement" (fromExactCellSet cellSet)
          assertion cellSet complexValue

trianglePoints :: Vector.Vector Point
trianglePoints =
  Vector.fromList
    [ Point 0 0,
      Point 2 0,
      Point 0 2
    ]
