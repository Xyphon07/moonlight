{-# LANGUAGE NumericUnderscores #-}

-- | Construction, degenerate cardinality, and admission laws.
module Moonlight.Planar.ConstructionSpec
  ( tests
  ) where

import Control.Monad ( forM_ )
import Control.Monad.ST (runST)
import qualified Moonlight.Planar.BuildStats as Build
import qualified Moonlight.Planar.Internal.OperationState as Operation
import Data.Foldable ( traverse_ )
import Data.Primitive.PrimArray ( primArrayFromList, sizeofPrimArray )
import GHC.Float ( castDoubleToWord64 )
import Moonlight.Planar.BulkLoad ( clear, delaunay, delaunayGeometry, empty, insert, insertMany )
import Moonlight.Planar.Dcel ( numDirectedEdges, numFaces, numInnerFaces, numUndirectedEdges,
  numVertices, vertexPoint )
import Moonlight.Planar.Internal.HandleDefs ( VertexId(VertexId) )
import Moonlight.Planar.Internal.Types ( RefinementParameters(..) )
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.MeshFixtures ( requirePointBuild, pointKey, edgeKeys )
import Moonlight.Planar.PointLocation ( locatePoint )
import Moonlight.Planar.Refinement ( refine, withMinimumAngle )
import Moonlight.Planar.Session ( withSession, insertVertex )
import Moonlight.Planar.Point (Point(..), PointValidationError(InvalidPointX,
  InvalidPointY))
import Moonlight.Planar.Types (defaultRefinementParameters, unitElementDefaults, BuildError(RefinementMinimumAreaExceedsMaximum, InvalidCoordinate,
  RefinementMinimumAngleNotFinite, RefinementMinimumAngleOutOfRange,
  RefinementMinimumAngleDerivedRatioNotFinite, RefinementMaximumAdditionalVerticesNegative,
  RefinementMinimumAreaNotFinite, RefinementMinimumAreaNegative, RefinementMaximumAreaNotFinite,
  RefinementMaximumAreaNotPositive, RefinementMaximumRadiusEdgeRatioNotFinite,
  RefinementMaximumRadiusEdgeRatioNotPositive, RefinementMaximumEdgeLengthNotFinite,
  RefinementMaximumEdgeLengthNotPositive), Location(OnVertex, EmptyTriangulation), BuildResult(buildTriangulation, buildStats, buildInputVertices), DelaunayTriangulation, InsertionResult(insertionTriangulation))
import Moonlight.Planar.BuildStats (BuildMetric (SpatialSeedPoints, LineExtensions), buildStat)
import Moonlight.Planar.Scalar (CoordinateError(CoordinateNaN, CoordinateInfinite), NonFiniteValue(ValuePositiveInfinity, ValueNaN))
import Support ( assertEqual, assertValid, requireQueryPoint, requireRight, randomPoints )
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testBuildStatsSchema
    , testConstructors
    , testCircleSweepBulkLoad
    , testDegenerateConstruction
    , testRandomizedConstruction
    , testErrors
    ]

testBuildStatsSchema :: IO ()
testBuildStatsSchema = do
  let metrics = [minBound .. maxBound] :: [Build.BuildMetric]
      diagnostics = [minBound .. maxBound] :: [Operation.DiagnosticMetric]
      expected :: [Int]
      expected = map ((+ 1) . fromEnum) metrics
      (observed, diagnosticValues, published) = runST $ do
        operation <- Operation.newOperationState 1
        traverse_ (\metric -> Operation.setCounter operation (Operation.Reported metric) (fromEnum metric + 1)) metrics
        traverse_ (\metric -> Operation.setCounter operation (Operation.Diagnostic metric) (100 + fromEnum metric)) diagnostics
        values <- traverse (Operation.readCounter operation . Operation.Reported) metrics
        diagnosticSnapshot <- traverse (Operation.readCounter operation . Operation.Diagnostic) diagnostics
        finalized <- Operation.finalizeBuildStats operation
        pure (values, diagnosticSnapshot, finalized)
      valuesOf :: Build.BuildStats -> [Int]
      valuesOf stats = map (`Build.buildStat` stats) metrics
  assertEqual "every reported metric owns one counter" expected observed
  assertEqual "diagnostics own a distinct suffix" (map ((+ 100) . fromEnum) diagnostics) diagnosticValues
  assertEqual "publication is exactly the reported section" expected (valuesOf published)
  assertEqual "zero initialization derives from the metric universe" (map (const 0) metrics) (valuesOf Build.emptyBuildStats)
  assertEqual "tabulation preserves metric identity" expected (valuesOf (Build.tabulateBuildStats ((+ 1) . fromEnum)))
  assertEqual "indexed updates preserve unselected metrics"
    (map (\metric -> if metric == minBound then 100 else fromEnum metric + 1) metrics)
    (valuesOf (Build.updateBuildStats [(minBound, 100)] published))

testConstructors :: IO ()
testConstructors = do
  let vacant = empty unitElementDefaults :: DelaunayTriangulation (Point)
  originQuery <- requireQueryPoint "empty location query" (Point 0 0)
  assertValid "empty" vacant
  assertEqual "empty vertex count" 0 (numVertices vacant)
  assertEqual "empty directed edge count" 0 (numDirectedEdges vacant)
  assertEqual "empty face count" 1 (numFaces vacant)
  assertEqual "empty locates nowhere" EmptyTriangulation (locatePoint vacant originQuery)
  built <- requirePointBuild "clear source" [Point 0 0, Point 1 0, Point 0 1]
  assertEqual "clear returns the origin" vacant (clear (buildTriangulation built))

testCircleSweepBulkLoad :: IO ()
testCircleSweepBulkLoad = do
  let points = V.fromList (randomPoints 0xc1ac1e 1500)
  swept <- requireRight "circle-sweep build" (delaunay unitElementDefaults points)
  (_, sessioned, _) <-
    requireRight "session build" $
      withSession (empty unitElementDefaults) (V.length points) $
        V.mapM_ insertVertex points
  assertEqual
    "circle-sweep/session topology"
    (edgeKeys sessioned)
    (edgeKeys (buildTriangulation swept))
  assertValid "circle-sweep build" (buildTriangulation swept)
  assertValid "session build" sessioned

testDegenerateConstruction :: IO ()
testDegenerateConstruction = do
  emptyBuild <- requirePointBuild "empty" []
  emptyQuery <- requireQueryPoint "empty location" (Point 0 0)
  assertEqual "empty vertices" 0 (numVertices (buildTriangulation emptyBuild))
  assertEqual "empty location" EmptyTriangulation (locatePoint (buildTriangulation emptyBuild) emptyQuery)
  assertValid "empty" (buildTriangulation emptyBuild)

  singleton <- requirePointBuild "singleton" [Point 2 3]
  singletonQuery <- requireQueryPoint "singleton lookup" (Point 2 3)
  assertEqual "singleton lookup" (OnVertex (VertexId 0)) (locatePoint (buildTriangulation singleton) singletonQuery)
  assertValid "singleton" (buildTriangulation singleton)

  duplicates <- requirePointBuild "duplicates" [Point 0 0, Point 1 0, Point (-0.0) 0, Point 1 0, Point 2 0]
  assertEqual "deduplicated count" 3 (numVertices (buildTriangulation duplicates))
  assertEqual
    "stable duplicate mapping"
    (primArrayFromList [0, 1, 0, 1, 2])
    (buildInputVertices duplicates)
  assertValid "duplicates" (buildTriangulation duplicates)

  -- Deduplicating a signed zero is not the same claim as storing it canonically:
  -- the first only needs '==', which already identifies the two, while anything
  -- reading the bit pattern — a radix ordering, a byte-for-byte cross-check —
  -- reads the sign bit and gets a coordinate below every other. Both ingest
  -- paths canonicalize, and both are held to it here rather than to the weaker
  -- statement that comparison happens to survive.
  let bits point = (castDoubleToWord64 (pointX point), castDoubleToWord64 (pointY point))
  signedZero <- requirePointBuild "signed zero" [Point (-0.0) (-0.0), Point 1 0, Point 0 1]
  assertEqual "bulk load stores a canonical zero"
    (0, 0) (bits (vertexPoint (buildTriangulation signedZero) (VertexId 0)))
  incremental <-
    requireRight "signed zero insert"
      (insert (empty unitElementDefaults :: DelaunayTriangulation (Point)) (Point (-0.0) (-0.0)))
  assertEqual "insertion stores a canonical zero"
    (0, 0) (bits (vertexPoint (insertionTriangulation incremental) (VertexId 0)))

  traverse_
    assertBulkLine
    [ ("vertical", [Point 0 4, Point 0 0, Point 0 3, Point 0 2, Point 0 1])
    , ("horizontal reversed", reverse [Point x 7 | x <- [0 .. 8]])
    , ("oblique scrambled", [Point 3 8, Point (-2) (-2), Point 1 4, Point (-1) 0, Point 2 6, Point 0 2])
    , ("duplicate-bearing", [Point 2 5, Point 0 1, Point 1 3, Point 2 5, Point (-0.0) 1, Point 3 7])
    ]
  areaBase <- requirePointBuild "resident area before collinear batch" [Point 0 0, Point 4 0, Point 0 4]
  areaExtended <-
    requireRight
      "collinear batch into resident area"
      (insertMany (buildTriangulation areaBase) (V.fromList [Point 1 1, Point 2 2, Point 3 3]))
  assertEqual "resident area collinear batch count" 6 (numVertices (buildTriangulation areaExtended))
  assertValid "resident area collinear batch" (buildTriangulation areaExtended)
 where
  assertBulkLine (label, points) = do
    annotated <- requirePointBuild (label <> " annotated line") points
    geometry <- requireRight (label <> " geometry line") (delaunayGeometry (V.fromList points))
    batched <-
      requireRight
        (label <> " empty-base batch line")
        (insertMany (empty unitElementDefaults) (V.fromList points))
    let line = buildTriangulation annotated
        batchLine = buildTriangulation batched
        ascending = Set.toAscList (Set.fromList (map pointKey points))
        expectedEdges = Set.fromList (zip ascending (drop 1 ascending))
        uniqueCount = length ascending
        stats = buildStats annotated
    assertEqual (label <> " annotated edge set") expectedEdges (edgeKeys line)
    assertEqual (label <> " geometry edge set") expectedEdges (edgeKeys geometry)
    assertEqual (label <> " empty-base batch edge set") expectedEdges (edgeKeys batchLine)
    assertEqual (label <> " edge count") (max 0 (uniqueCount - 1)) (numUndirectedEdges line)
    assertEqual (label <> " face count") 0 (numInnerFaces line)
    assertEqual (label <> " spatial seed count") uniqueCount ((buildStat SpatialSeedPoints) stats)
    assertEqual (label <> " aggregate extension count") (max 0 (uniqueCount - 2)) ((buildStat LineExtensions) stats)
    assertValid (label <> " annotated line") line
    assertValid (label <> " geometry line") geometry
    assertValid (label <> " empty-base batch line") batchLine

testRandomizedConstruction :: IO ()
testRandomizedConstruction = do
  forM_ [0 .. 31] $ \index -> do
    let points = randomPoints (0x9e37_79b9 + fromIntegral index) (40 + index * 7)
    built <- requirePointBuild ("random build " <> show index) points
    let triangulation = buildTriangulation built
    assertValid ("random build " <> show index) triangulation
    assertEqual "random input mapping" (length points) (sizeofPrimArray (buildInputVertices built))

testErrors :: IO ()
testErrors = do
  assertEqual
    "NaN query x refusal"
    (Left (InvalidPointX CoordinateNaN))
    (mkQueryPoint (Point (0 / 0) 0))
  assertEqual
    "infinite query y refusal"
    (Left (InvalidPointY CoordinateInfinite))
    (mkQueryPoint (Point 0 (1 / 0)))
  assertEqual
    "negative-infinite query x refusal"
    (Left (InvalidPointX CoordinateInfinite))
    (mkQueryPoint (Point ((-1) / 0) 0))
  case delaunay unitElementDefaults (V.singleton (Point (0 / 0) 0 :: Point)) of
    Left (InvalidCoordinate (Just 0) _ CoordinateNaN) -> pure ()
    Left other -> fail ("NaN insertion result: " <> show other)
    Right built ->
      fail
        ( "NaN insertion result: built a triangulation of "
            <> show (numVertices (buildTriangulation built))
            <> " vertices"
        )
  expectBuildFailure
    "NaN minimum angle"
    (RefinementMinimumAngleNotFinite ValueNaN)
    (withMinimumAngle (0 / 0 :: Double) defaultRefinementParameters)
  expectBuildFailure
    "infinite minimum angle"
    (RefinementMinimumAngleNotFinite ValuePositiveInfinity)
    (withMinimumAngle (1 / 0 :: Double) defaultRefinementParameters)
  expectBuildFailure
    "out-of-range minimum angle"
    (RefinementMinimumAngleOutOfRange 61)
    (withMinimumAngle (61 :: Double) defaultRefinementParameters)
  expectBuildFailure
    "minimum angle whose derived ratio overflows"
    (RefinementMinimumAngleDerivedRatioNotFinite ValuePositiveInfinity)
    (withMinimumAngle (encodeFloat 1 (-1074) :: Double) defaultRefinementParameters)
  square <- requirePointBuild "area validation square" [Point 0 0, Point 1 0, Point 1 1, Point 0 1]
  let refineWith parameters =
        refine id parameters (buildTriangulation square)
      refineWithArea area =
        refineWith defaultRefinementParameters{refineMaxArea = Just area}
  expectBuildFailure
    "negative refinement vertex budget"
    (RefinementMaximumAdditionalVerticesNegative (-1))
    (refineWith defaultRefinementParameters{refineMaxAdditionalVertices = Just (-1)})
  expectBuildFailure
    "infinite minimum area"
    (RefinementMinimumAreaNotFinite ValuePositiveInfinity)
    (refineWith defaultRefinementParameters{refineMinArea = Just (1 / 0)})
  expectBuildFailure
    "negative minimum area"
    (RefinementMinimumAreaNegative (-1))
    (refineWith defaultRefinementParameters{refineMinArea = Just (-1)})
  expectBuildFailure
    "NaN maximum area"
    (RefinementMaximumAreaNotFinite ValueNaN)
    (refineWithArea (0 / 0))
  expectBuildFailure
    "infinite maximum area"
    (RefinementMaximumAreaNotFinite ValuePositiveInfinity)
    (refineWithArea (1 / 0))
  expectBuildFailure
    "zero maximum area"
    (RefinementMaximumAreaNotPositive 0)
    (refineWithArea 0)
  expectBuildFailure
    "negative maximum area"
    (RefinementMaximumAreaNotPositive (-1))
    (refineWithArea (-1))
  expectBuildFailure
    "infinite maximum radius/edge ratio"
    (RefinementMaximumRadiusEdgeRatioNotFinite ValuePositiveInfinity)
    (refineWith defaultRefinementParameters{refineMaxRadiusEdgeRatio = Just (1 / 0)})
  expectBuildFailure
    "non-positive maximum radius/edge ratio"
    (RefinementMaximumRadiusEdgeRatioNotPositive 0)
    (refineWith defaultRefinementParameters{refineMaxRadiusEdgeRatio = Just 0})
  expectBuildFailure
    "infinite maximum edge length"
    (RefinementMaximumEdgeLengthNotFinite ValuePositiveInfinity)
    (refineWith defaultRefinementParameters{refineMaxEdgeLength = Just (1 / 0)})
  expectBuildFailure
    "non-positive maximum edge length"
    (RefinementMaximumEdgeLengthNotPositive 0)
    (refineWith defaultRefinementParameters{refineMaxEdgeLength = Just 0})
  expectBuildFailure
    "minimum area above maximum area"
    (RefinementMinimumAreaExceedsMaximum 2 1)
    ( refineWith
        defaultRefinementParameters
          { refineMinArea = Just 2
          , refineMaxArea = Just 1
          }
    )
  case refineWithArea 0.25 of
    Right _ -> pure ()
    Left failure -> fail ("positive maximum area rejected: " <> show failure)

expectBuildFailure :: String -> BuildError -> Either BuildError value -> IO ()
expectBuildFailure label expected outcome =
  case outcome of
    Left actual -> assertEqual label expected actual
    Right _ -> fail (label <> ": expected " <> show expected <> ", got success")
