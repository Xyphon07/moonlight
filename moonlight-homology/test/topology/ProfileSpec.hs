module ProfileSpec (tests) where

import Data.Bifunctor (first)
import Data.Bits (testBit)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import Moonlight.Homology qualified as H
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2 (..))
import Moonlight.Pale.Test.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "batched profile rank oracles"
    [ testCase "all 83 planar square subcomplexes agree at complete exact birth batches" testPlanarProfiles,
      testCase "union of two disks is the tetrahedral sphere, not a planar disk" testSphereUnion,
      testCase "all 512 binary 3-by-3 boundaries agree with independent packed ranks" testGenericMatrices,
      testCase "full-rank face boundary without a free row retains correct Betti" testNonpeelableMatrix,
      testCase "even coefficients vanish and odd nonunit coefficients are units in GF(2)" testActualField,
      testCase "three-dimensional simplex agrees with independent critical-sublevel ranks" testHigherDimension
    ]

type Simplex = [Int]

nonemptyFaces :: Simplex -> [Simplex]
nonemptyFaces = filter (not . null) . List.subsequences

downwardClosure :: [Simplex] -> [Simplex]
downwardClosure = Set.toAscList . Set.fromList . concatMap nonemptyFaces

squareSubcomplexes :: [[Simplex]]
squareSubcomplexes =
  filter
    (\simplices -> Set.fromList (downwardClosure simplices) == Set.fromList simplices)
    (List.subsequences (downwardClosure [[0, 1, 2], [0, 2, 3]]))

simplicialComplex :: [Simplex] -> Either String (H.FiniteChainComplex Integer, [(H.BasisCellRef, Simplex)])
simplicialComplex simplices = do
  let maximumDegree = maximum (0 : fmap (subtract 1 . length) simplices)
      cellsAt degreeValue = filter ((== degreeValue + 1) . length) simplices
      cellSections =
        [ (H.BasisCellRef (H.HomologicalDegree degreeValue) indexValue, simplex)
        | degreeValue <- [0 .. maximumDegree],
          (indexValue, simplex) <- zip [0 ..] (cellsAt degreeValue)
        ]
      incidenceAt degreeValue =
        first show
          ( H.mkBoundaryIncidence
              (fromIntegral (length (cellsAt degreeValue)))
              (fromIntegral (length (cellsAt (degreeValue - 1))))
              [ H.mkBoundaryEntryFromInts columnIndex rowIndex (if even omittedIndex then 1 else -1)
              | degreeValue > 0,
                (columnIndex, simplex) <- zip [0 ..] (cellsAt degreeValue),
                omittedIndex <- [0 .. length simplex - 1],
                let face = take omittedIndex simplex <> drop (omittedIndex + 1) simplex,
                Just rowIndex <- [List.elemIndex face (cellsAt (degreeValue - 1))]
              ]
          )
  boundaries <- traverse incidenceAt [0 .. maximumDegree]
  let boundaryByDegree = Map.fromList (zip (fmap H.HomologicalDegree [0 ..]) boundaries)
  finite <-
    first show
      ( H.mkFiniteChainComplexChecked
          (H.HomologicalDegree maximumDegree)
          (\degreeValue -> Map.findWithDefault H.emptyBoundaryIncidence degreeValue boundaryByDegree)
      )
  pure (finite, cellSections)

filteredSimplicialComplex :: Int -> [Simplex] -> Either String (H.FilteredFiniteChainComplex Rational Integer)
filteredSimplicialComplex seed simplices = do
  (finite, cells) <- simplicialComplex simplices
  let birth simplex =
        ( maximum (-5 : fmap (\vertex -> toInteger ((vertex * seed) `mod` 5) - 5) simplex)
            + toInteger (length simplex * (seed `mod` 3))
        ) % 7
  first show (H.mkFilteredFiniteChainComplex finite (fmap (\(cell, simplex) -> (cell, birth simplex)) cells))

assertProfileAgrees :: H.FilteredFiniteChainComplex Rational Integer -> Assertion
assertProfileAgrees filtered = do
  (_, table) <- expectRight (H.mod2PersistentPairsWithCriticalBettiTable filtered)
  expectedProfiles <- traverse (independentSublevelBetti filtered) (H.filteredCriticalValues filtered)
  H.criticalBettiVectors table @?= expectedProfiles

-- Independent test oracle: reconstruct each closed sublevel from its admitted
-- incidence, then use packed GF(2) matrix ranks. The production observation
-- instead performs sparse ordered persistence reduction once for all births.
independentSublevelBetti ::
  H.FilteredFiniteChainComplex Rational Integer ->
  Rational ->
  IO [Int]
independentSublevelBetti filtered threshold = do
  let finite = H.filteredBaseComplex filtered
      maximumDegree = H.unHomologicalDegree (H.maxHomologicalDegree finite)
      activeCells = Map.keys (Map.filter (<= threshold) (H.filteredCellBirths filtered))
      activeCellsAt degreeValue = filter ((== H.HomologicalDegree degreeValue) . H.cellDegree) activeCells
      coordinatesAt degreeValue = Map.fromList (zip (fmap H.cellIndex (activeCellsAt degreeValue)) [0 ..])
      rankAt degreeValue = do
        let sourceCoordinates = coordinatesAt degreeValue
            targetCoordinates = coordinatesAt (degreeValue - 1)
        restrictedBoundary <- expectRight
          ( H.mkBoundaryIncidence
              (fromIntegral (Map.size sourceCoordinates))
              (fromIntegral (Map.size targetCoordinates))
              [ H.mkBoundaryEntryFromInts sourceCoordinate targetCoordinate GF2One
              | entry <- H.boundaryEntries (H.incidenceMatrixAt finite (H.HomologicalDegree degreeValue)),
                odd (H.boundaryCoefficient entry),
                Just sourceCoordinate <- [Map.lookup (H.sourceIndex entry) sourceCoordinates],
                Just targetCoordinate <- [Map.lookup (H.targetIndex entry) targetCoordinates]
              ]
          )
        expectRight (H.gf2BoundaryRank restrictedBoundary)
  ranks <- traverse rankAt [0 .. maximumDegree + 1]
  pure
    ( zipWith3
        (\degreeValue rankHere rankAbove -> length (activeCellsAt degreeValue) - rankHere - rankAbove)
        [0 .. maximumDegree]
        ranks
        (drop 1 ranks)
    )

testPlanarProfiles :: Assertion
testPlanarProfiles = do
  length squareSubcomplexes @?= 83
  _ <- traverse
    (\(seed, simplices) -> do
      filtered <- expectRight (filteredSimplicialComplex seed simplices)
      assertProfileAgrees filtered)
    [(seed, simplices) | seed <- [0 .. 13], simplices <- squareSubcomplexes]
  pure ()

testSphereUnion :: Assertion
testSphereUnion = do
  let firstDisk = downwardClosure [[0, 1, 2], [0, 2, 3]]
      secondDisk = downwardClosure [[0, 1, 3], [1, 2, 3]]
      sphere = Set.toAscList (Set.fromList (firstDisk <> secondDisk))
  _ <- traverse
    (\disk -> do
      filtered <- expectRight (filteredSimplicialComplex 0 disk)
      (_, profile) <- expectRight (H.mod2PersistentPairsWithCriticalBettiTable filtered)
      H.criticalBettiVectors profile @?= [[1, 0, 0]])
    [firstDisk, secondDisk]
  filtered <- expectRight (filteredSimplicialComplex 0 sphere)
  (_, profile) <- expectRight (H.mod2PersistentPairsWithCriticalBettiTable filtered)
  H.criticalBettiVectors profile @?= [[1, 0, 1]]
  assertProfileAgrees filtered

binaryMatrix :: Int -> Either H.BoundaryIncidenceShapeError (H.BoundaryIncidence GF2)
binaryMatrix mask =
  H.mkBoundaryIncidence 3 3
    [ H.mkBoundaryEntryFromInts columnIndex rowIndex GF2One
    | columnIndex <- [0 .. 2], rowIndex <- [0 .. 2], testBit mask (3 * columnIndex + rowIndex)
    ]

testGenericMatrices :: Assertion
testGenericMatrices = do
  _ <- traverse
    (\mask -> do
      matrix <- expectRight (binaryMatrix mask)
      finite <- expectRight
        ( H.mkFiniteChainComplexChecked (H.HomologicalDegree 1) $ \degreeValue ->
            case degreeValue of
              H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 3 0
              H.HomologicalDegree 1 -> H.mapBoundaryCoefficients (\coefficient -> if coefficient == GF2One then 1 else 0) matrix
              _ -> H.emptyBoundaryIncidence
        )
      filtered <- expectRight
        ( H.mkFilteredFiniteChainComplex finite
            ( [ (H.BasisCellRef (H.HomologicalDegree 0) indexValue, -2) | indexValue <- [0 .. 2] ]
                <> [ (H.BasisCellRef (H.HomologicalDegree 1) indexValue, fromIntegral indexValue - 1) | indexValue <- [0 .. 2] ]
            )
        )
      assertProfileAgrees filtered)
    [0 .. 511]
  pure ()

testNonpeelableMatrix :: Assertion
testNonpeelableMatrix = do
  matrix <- expectRight (binaryMatrix 351)
  rankValue <- expectRight (H.gf2BoundaryRank matrix)
  rankValue @?= 3
  assertBool "the invertible matrix has no free row"
    (all (\rowIndex -> length (filter ((== rowIndex) . H.targetIndex) (H.boundaryEntries matrix)) > 1) [0 .. 2])
  -- Three independent cycles among five parallel edges, but no free face
  -- row. A free-row failure cannot justify dropping these face ranks.
  edgeBoundary <- expectRight
    ( H.mkBoundaryIncidence 5 2
        [ H.mkBoundaryEntryFromInts edgeIndex vertexIndex coefficient
        | edgeIndex <- [0 .. 4], (vertexIndex, coefficient) <- [(0, -1), (1, 1)]
        ]
    )
  faceBoundary <- expectRight
    ( H.mkBoundaryIncidence 3 5
        [ H.mkBoundaryEntryFromInts faceIndex edgeIndex coefficient
        | (faceIndex, edgeIndices) <- zip [0 ..] [[0, 1], [0, 2], [1, 2]],
          (edgeIndex, coefficient) <- fmap (, 1) edgeIndices <> [(3, -1), (4, -1)]
        ]
    )
  faceRank <- expectRight (H.gf2BoundaryRank (mod2Boundary faceBoundary))
  faceRank @?= 3
  finite <- expectRight
    ( H.mkFiniteChainComplexChecked (H.HomologicalDegree 2) $ \degreeValue ->
        case degreeValue of
          H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 2 0
          H.HomologicalDegree 1 -> edgeBoundary
          H.HomologicalDegree 2 -> faceBoundary
          _ -> H.emptyBoundaryIncidence
    )
  filtered <- expectRight
    ( H.mkFilteredFiniteChainComplex finite
        [ (H.BasisCellRef (H.HomologicalDegree degreeValue) cellIndex, 0 :: Rational)
        | (degreeValue, count) <- [(0, 2), (1, 5), (2, 3)], cellIndex <- [0 .. count - 1]
        ]
    )
  (_, profile) <- expectRight (H.mod2PersistentPairsWithCriticalBettiTable filtered)
  H.criticalBettiVectors profile @?= [[1, 1, 0]]
  assertProfileAgrees filtered

mod2Boundary :: H.BoundaryIncidence Integer -> H.BoundaryIncidence GF2
mod2Boundary = H.mapBoundaryCoefficients (\coefficient -> if odd coefficient then GF2One else GF2Zero)

testActualField :: Assertion
testActualField = do
  (triangle, cells) <- expectRight (simplicialComplex (downwardClosure [[0, 1, 2]]))
  _ <- traverse
    (\scaleValue -> do
      scaled <- expectRight
        ( H.mkFiniteChainComplexChecked (H.HomologicalDegree 2) $ \degreeValue ->
            H.mapBoundaryCoefficients
              (* scaleValue)
              (H.incidenceMatrixAt triangle degreeValue)
        )
      filtered <- expectRight (H.mkFilteredFiniteChainComplex scaled (fmap (\(cell, _) -> (cell, 0 :: Rational)) cells))
      (_, profile) <- expectRight (H.mod2PersistentPairsWithCriticalBettiTable filtered)
      H.criticalBettiVectors profile @?= [if even scaleValue then [3, 3, 1] else [1, 0, 0]]
      assertProfileAgrees filtered)
    [2, 3, -3]
  pure ()

testHigherDimension :: Assertion
testHigherDimension = do
  filtered <- expectRight (filteredSimplicialComplex 5 (downwardClosure [[0, 1, 2, 3]]))
  assertProfileAgrees filtered
