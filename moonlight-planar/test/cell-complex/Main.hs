module Main (main) where

import Data.Bits (shiftL)
import Data.Function ((&))
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
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
import Moonlight.Triangulation.Alpha
  ( AlphaBirth
  , AlphaFiltration
  , alphaBirthDenominator
  , alphaBirthNumerator
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaFiltration
  , alphaFiltrationCellSet
  , alphaVertexBirth
  )
import Moonlight.Triangulation.BulkLoad (delaunayGeometry)
import Moonlight.Triangulation.CellSet
  ( ExactCellSet,
    closeFaceCellSet,
    exactCellSetEdgeCount,
    exactCellSetFaceCount,
    exactCellSetVertexCount,
  )
import Moonlight.Triangulation.CellComplex
  ( DCELComplex
  , filteredAlphaComplex
  , finiteChainComplex
  , fromExactCellSet
  )
import Moonlight.Triangulation.Dcel
  ( undirectedEndpoints
  , vertexPoint
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( UndirectedEdgeId
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  )
import Moonlight.Triangulation.Internal.Dyadic (exactDiametralDot)
import Moonlight.Triangulation.Internal.ExactRational
  ( exactRational
  , exactRationalFromDyadic
  , exactRationalFromDyadicRatio
  )
import Moonlight.Triangulation.Internal.Predicates (inDiametralCircle)
import Moonlight.Triangulation.LabelledComplex (filteredPlanarComplex)
import Moonlight.Triangulation.Simplex
  ( planarComplex
  , planarEdge
  , planarFace
  , planarVertex
  )
import Moonlight.Triangulation.Types
  ( DelaunayTriangulation
  , Point (..)
  )
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
      testCase "marks the triangular exterior as absent" preserveExteriorAdjacency,
      testCase "lowers oriented DCEL incidence to a checked integral chain complex" lowerIntegralChainComplex,
      testCase "lowers a native hexagonal ring through the same chain owner" lowerHexagonalRing,
      testCase "assigns exact Gabriel and non-Gabriel alpha births" exactAlphaBirths,
      testCase "preserves exact dyadic rational normalization" exactDyadicRationalNormalization,
      testCase "orders cached alpha keys by their exact rational births" alphaBirthOrderMatchesExactRationals,
      testCase "agrees with the exact diametral predicate" diametralPredicateMatchesExactSign,
      testCase "keeps every boundary cell no later than its coface" alphaBirthsAreFaceMonotone,
      testCase "finds the square alpha hole from radius one to radius two" squareAlphaPersistence,
      testCase "lowers a labelled planar filtration through the same Homology owner" labelledPlanarPersistence,
      testCase "keeps point and collinear alpha filtrations total" degenerateSupportFiltrations
    ]

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
  let vertexBirthRatios =
        vertices (fromExactCellSet (alphaFiltrationCellSet filtration))
          & fmap (fmap alphaBirthRatio . alphaVertexBirth filtration)
      faceBirthRatios =
        faces (fromExactCellSet (alphaFiltrationCellSet filtration))
          & fmap (fmap alphaBirthRatio . alphaFaceBirth filtration)
  assertBool "every vertex is born at zero" (all (== Just (0, 1)) vertexBirthRatios)
  faceBirthRatios @?= [Just (25, 4)]
  longEdge <- requireSome "obtuse triangle long edge" (edgeBetween triangulation (Point (-2) 0) (Point 2 0))
  fmap alphaBirthRatio (alphaEdgeBirth filtration longEdge) @?= Just (25, 4)
  let shortEdgeBirths =
        edges (fromExactCellSet (alphaFiltrationCellSet filtration))
          & filter (/= longEdge)
          & fmap (fmap alphaBirthRatio . alphaEdgeBirth filtration)
  shortEdgeBirths @?= [Just (5, 4), Just (5, 4)]

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
  let complexValue = fromExactCellSet (alphaFiltrationCellSet filtration)
   in mapMaybe (alphaVertexBirth filtration) (vertices complexValue)
        <> mapMaybe (alphaEdgeBirth filtration) (edges complexValue)
        <> mapMaybe (alphaFaceBirth filtration) (faces complexValue)

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
        Right cellSet -> assertion cellSet (fromExactCellSet cellSet)

trianglePoints :: Vector.Vector Point
trianglePoints =
  Vector.fromList
    [ Point 0 0,
      Point 2 0,
      Point 0 2
    ]
