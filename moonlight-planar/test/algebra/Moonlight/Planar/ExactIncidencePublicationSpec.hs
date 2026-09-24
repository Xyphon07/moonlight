-- | Strict polygon observations of exact, non-triangular planar incidence.
-- Raw cycles include multiple boundary components, bridges and pinches.
module Moonlight.Planar.ExactIncidencePublicationSpec (tests) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import Moonlight.Planar.Exact (ExactPoint, exactPoint, exactPointCoordinates)
import Moonlight.Planar.FloodFillIterator (BoundaryObstruction (..))
import Moonlight.Planar.Internal.BoundaryCycle (cyclePairsNonEmpty, cyclicTriples, orderedPair)
import Moonlight.Planar.Internal.HandleDefs (DirectedEdgeId (..), FaceId (..), VertexId (..))
import Moonlight.Planar.Internal.Incidence (PlanarIncidence, admitPlanarIncidence)
import Moonlight.Planar.Internal.PackedIndex (noIndex)
import Moonlight.Planar.Internal.Paged (fromVector)
import Moonlight.Planar.Internal.Region.Publication (planarRegionFromSelectedIncidence)
import Moonlight.Planar.Region
  ( PlanarRegion
  , RegionPublicationError (..)
  , exactLoop
  , planarRegion
  , polygonComponent
  )
import Support (assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = do
  testHoleCancellationAndInteriorDebris
  testNestedIsland
  testVertexContact
  testConnectedPinchRefusal
  testExactCoordinateAndRefusalBoundaries

testHoleCancellationAndInteriorDebris :: IO ()
testHoleCancellationAndInteriorDebris = do
  let outer = square 0 0 6 6
      inner = square 2 2 4 4
      points = NonEmpty.toList outer <> NonEmpty.toList inner <> [integerPoint 1 1, integerPoint 5 5]
  fixture <- admitFixture "annulus with bridge and isolated vertex" points
    [ (0, 0 :| [3, 2, 1])
    , (1, 0 :| [8, 0, 1, 2, 3])
    , (1, 4 :| [7, 6, 5])
    , (2, 4 :| [5, 6, 7])
    ]
    [(1, [9])]
  annulus <- publishFixture "annulus publication" fixture (== FaceId 1)
  expectedAnnulus <- expectedRegion [(outer, [NonEmpty.reverse inner])]
  assertEqual "hole, bridge and isolated point publication" expectedAnnulus annulus
  filled <- publishFixture "hole cancellation publication" fixture (/= FaceId 0)
  expectedFilled <- expectedRegion [(outer, [])]
  assertEqual "selected hole face cancels shared boundary darts" expectedFilled filled

testNestedIsland :: IO ()
testNestedIsland = do
  let outer = square 0 0 10 10
      hole = square 2 2 8 8
      island = square 4 4 6 6
  fixture <- admitFixture "nested island" (concatMap NonEmpty.toList [outer, hole, island])
    [ (0, 0 :| [3, 2, 1])
    , (1, 0 :| [1, 2, 3])
    , (1, 4 :| [7, 6, 5])
    , (2, 4 :| [5, 6, 7])
    , (2, 8 :| [11, 10, 9])
    , (3, 8 :| [9, 10, 11])
    ]
    []
  published <- publishFixture "nested island publication" fixture (\face -> face == FaceId 1 || face == FaceId 3)
  expected <- expectedRegion [(outer, [NonEmpty.reverse hole]), (island, [])]
  assertEqual "disconnected island retains the containing component's hole" expected published

testVertexContact :: IO ()
testVertexContact = do
  let left = square 0 0 2 2
      right = square 2 2 4 4
      points = NonEmpty.toList left <> [integerPoint 4 2, integerPoint 4 4, integerPoint 2 4]
  fixture <- admitFixture "vertex-contact components" points
    [ (0, 0 :| [3, 2, 6, 5, 4, 2, 1])
    , (1, 0 :| [1, 2, 3])
    , (2, 2 :| [4, 5, 6])
    ]
    []
  published <- publishFixture "vertex contact publication" fixture (/= FaceId 0)
  expected <- expectedRegion [(left, []), (right, [])]
  assertEqual "vertex contact is two polygon components, not a pinch" expected published

testConnectedPinchRefusal :: IO ()
testConnectedPinchRefusal = do
  fixture <- admitFixture "pinched annulus"
    (NonEmpty.toList (square 0 0 6 6) <> [integerPoint 2 1, integerPoint 1 2])
    [ (0, 0 :| [3, 2, 1])
    , (1, 0 :| [1, 2, 3, 0, 5, 4])
    , (2, 0 :| [4, 5])
    ]
    []
  case uncurry planarRegionFromSelectedIncidence fixture (== FaceId 1) of
    Left (RegionBoundaryObstruction (BoundaryPinch (VertexId 0) _ _)) -> pure ()
    result -> fail ("connected pinch must be a publication refusal: " <> show result)
  filled <- publishFixture "filled pinch publication" fixture (/= FaceId 0)
  expected <- expectedRegion [(square 0 0 6 6, [])]
  assertEqual "a refused observation does not invalidate exact incidence" expected filled

testExactCoordinateAndRefusalBoundaries :: IO ()
testExactCoordinateAndRefusalBoundaries = do
  let shifted = fmap
        (\point -> let (x, y) = exactPointCoordinates point in exactPoint (x + 2 ^ (60 :: Int)) y)
        (square 0 0 1 1)
  fixture@(incidence, exactPointAt) <- admitFixture "binary64-colliding coordinates"
    (NonEmpty.toList shifted)
    [(0, 0 :| [3, 2, 1]), (1, 0 :| [1, 2, 3])]
    []
  published <- publishFixture "exact-only coordinate publication" fixture (== FaceId 1)
  expected <- expectedRegion [(shifted, [])]
  assertEqual "no binary64 coordinate admission is required" expected published
  assertEqual "unbounded selection refuses before publication"
    (Left RegionUnboundedSelection)
    (planarRegionFromSelectedIncidence incidence exactPointAt (const True))
  empty <- requireRight "empty region" (planarRegion [])
  assertEqual "empty selection never asks for coordinates"
    (Right empty)
    (planarRegionFromSelectedIncidence incidence (Left . RegionCoordinateMissing) (const False))
  case planarRegionFromSelectedIncidence incidence (Left . RegionCoordinateMissing) (== FaceId 1) of
    Left (RegionCoordinateMissing _) -> pure ()
    result -> fail ("missing selected boundary coordinate must remain typed: " <> show result)

square :: Integer -> Integer -> Integer -> Integer -> NonEmpty ExactPoint
square minimumX minimumY maximumX maximumY =
  integerPoint minimumX minimumY
    :| [integerPoint maximumX minimumY, integerPoint maximumX maximumY, integerPoint minimumX maximumY]

expectedRegion :: [(NonEmpty ExactPoint, [NonEmpty ExactPoint])] -> IO PlanarRegion
expectedRegion boundaries = do
  components <- traverse
    (\(outer, holes) -> do
      admittedOuter <- requireRight "expected outer loop" (exactLoop outer)
      admittedHoles <- traverse (requireRight "expected hole loop" . exactLoop) holes
      requireRight "expected polygon component" (polygonComponent admittedOuter admittedHoles))
    boundaries
  requireRight "expected region" (planarRegion components)

type PublicationFixture =
  (PlanarIncidence, VertexId -> Either RegionPublicationError ExactPoint)

publishFixture :: String -> PublicationFixture -> (FaceId -> Bool) -> IO PlanarRegion
publishFixture label fixture selected =
  requireRight label (uncurry planarRegionFromSelectedIncidence fixture selected)

-- | Test-only packing of explicitly supplied oriented face walks. It does not
-- discover faces: the fixture supplies all incidence and containment choices.
admitFixture
  :: String
  -> [ExactPoint]
  -> [(Int, NonEmpty Int)]
  -> [(Int, [Int])]
  -> IO PublicationFixture
admitFixture label points submittedCycles isolatedVertices = do
  cycles <- maybe (fail (label <> ": missing fixture edge")) pure
    (traverse numberCycle submittedCycles)
  let records = Map.fromList
        [ (edge, (vertex, [fromIntegral vertex, fromIntegral nextEdge, fromIntegral previousEdge, fromIntegral face]))
        | (face, cycleVertices) <- cycles
        , ((previousEdge, _), (edge, vertex), (nextEdge, _)) <- cyclicTriples (NonEmpty.toList cycleVertices)
        ]
      vertexRoots = Map.fromListWith min [(vertex, edge) | (edge, (vertex, _)) <- Map.toAscList records]
      faceRoots = Map.fromListWith (flip (<>)) [(face, [fst (NonEmpty.head cycleVertices)]) | (face, cycleVertices) <- cycles]
      packed = fromVector noIndex . Vector.fromList
      rootAt index roots = maybe noIndex fromIntegral (Map.lookup index roots)
      primaryFaceRoots = Map.mapMaybe listToMaybe faceRoots
  incidence <- requireRight (label <> ": incidence admission")
    ( admitPlanarIncidence
        (packed [rootAt index vertexRoots | index <- [0 .. length points - 1]])
        (packed (concatMap (snd . snd) (Map.toAscList records)))
        (packed [rootAt index primaryFaceRoots | index <- [0 .. maximumFace]])
        (IntMap.fromList [(face, map (DirectedEdgeId . fromIntegral) remaining) | (face, _ : remaining) <- Map.toAscList faceRoots, not (null remaining)])
        (IntMap.fromList [(face, IntSet.fromList vertices) | (face, vertices) <- isolatedVertices])
    )
  let exactPoints = Map.fromList (zip (map (VertexId . fromIntegral) ([0 ..] :: [Int])) points)
  pure (incidence, \vertex -> maybe (Left (RegionCoordinateMissing vertex)) Right (Map.lookup vertex exactPoints))
 where
  maximumFace = foldr (max . fst) 0 submittedCycles
  undirected = Map.fromList
    (zip
      (Set.toAscList (Set.fromList
        [orderedPair origin target | (_, vertices) <- submittedCycles, (origin, target) <- NonEmpty.toList (cyclePairsNonEmpty vertices)]))
      ([0 ..] :: [Int]))
  numberCycle (face, vertices) =
    (face,) <$> traverse
      (\(origin, target) ->
        (\index -> (2 * index + (if origin < target then 0 else 1), origin))
          <$> Map.lookup (orderedPair origin target) undirected)
      (cyclePairsNonEmpty vertices)
