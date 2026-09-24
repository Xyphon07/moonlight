-- | Exact affine-envelope and compositional layer laws through the public
-- facade. Every comparison is against an independently composed public view.
module Moonlight.Planar.LayerOperationsSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Planar.Exact (ExactPoint, exactPoint, exactPointCoordinates)
import Moonlight.Planar.Internal.ExactRational (ExactRational)
import Moonlight.Planar.Overlay (CoverGap, LayerCoverageError (..), OverlayResult, coverGapRegion, layerCovers, overlayAll, overlayConfusion, overlayLayers, overlayMass, overlaySelectedRegion)
import Moonlight.Planar.PowerDiagram (AffineForm (..), UpperEnvelopeError (UpperEnvelopeEmptyForms), upperEnvelope)
import Moonlight.Planar.Region (PlanarLayer, PlanarRegion, PolygonComponent, RegionPublicationError (RegionUnboundedSelection), exactLoop, exactLoopPoints, planarLayer, planarLayerLabelAt, planarLayerRegions, planarRegion, planarRegionComponents, polygonComponent, polygonHoleLoops, polygonOuterLoop)
import Moonlight.Planar.Valuation (exactAreaValue, regionValuations, valuationArea)
import Moonlight.Planar.CellSet
  ( ExactCellSet
  , exactCellSetVertexCount
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , foldExactCellVertices
  )
import Moonlight.Planar.Exact (exactDivide, exactPointToEmbeddingCandidate, exactRational)
import Moonlight.Planar.Overlay
  ( OverlaySourceId (..)
  , boundaryRefSource
  , foldBoundedOverlayCells
  , overlayArrangementEdges
  , overlayArrangementVertices
  , overlayCells
  , overlayClosedIntersection
  , overlayClosedUnion
  , overlayEdgeSources
  , overlayInputSources
  , overlayReceipt
  , overlayPlanarLayer
  , overlayRegularizedDifference
  )
import Moonlight.Planar.Point (queryPointValue)
import Moonlight.Planar.Valuation
  ( ExactPlanarMoments
  , cellValuations
  , eulerCharacteristicValue
  , exactLengthExpression
  , exactLengthTerms
  , exactPlanarMeasure
  , exactPlanarFirstX
  , exactPlanarFirstY
  , exactPlanarSecondXX
  , exactPlanarSecondXY
  , exactPlanarSecondYY
  , polygonComponentMoments
  , orientedBoundaryMoments
  , valuationEuler
  , valuationIntrinsic1
  )
import Moonlight.Planar.OverlayFixtures (singletonLayer)
import Support (rectangleRegion, rectangleComponent, assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = do
  testConvexUpperEnvelope
  testNonconvexAndHoledUpperEnvelope
  testEmptyUpperEnvelope
  testLayerCoverage
  testNaryOverlayLabels
  testFusedOverlayFamilyLaws
  testOverlayMassAndConfusion
  testExactOverlayCollapsedEmbedding
  testExactOverlayTinyCrossings
  testExactOverlayDisconnectedCells
  testExactOverlayNestedHolesAndIslands
  testExactOverlayReversedSupportAndTJunction
  testExactOverlayVertexContact
  putStrLn "layer operations: ok"

testConvexUpperEnvelope :: IO ()
testConvexUpperEnvelope = do
  window <- rectangleComponent 0 0 10 10
  let forms =
        Map.fromList
          [ (0 :: Int, AffineForm 0 (-1) 0)
          , (1, AffineForm 0 (-1) 0)
          , (2, AffineForm (-10) 1 0)
          , (3, AffineForm (-11) 1 0)
          ]
  layer <- requireRight "convex exact upper envelope" (upperEnvelope window forms)
  reordered <-
    requireRight
      "upper envelope is independent of map construction order"
      (upperEnvelope window (Map.fromList (reverse (Map.toList forms))))
  assertEqual "upper envelope map-order invariance" layer reordered
  assertEqual "identical and dominated forms own no cell" [Just 0, Just 2] (Map.keys (planarLayerRegions layer))
  assertEnvelopeDominance forms layer
  assertEqual "left affine winner" (Just 0) (planarLayerLabelAt layer (exactPoint 2 5))
  assertEqual "right affine winner" (Just 2) (planarLayerLabelAt layer (exactPoint 8 5))
  assertEqual "outside finite window" Nothing (planarLayerLabelAt layer (exactPoint (-1) 5))
  requireCoverage "upper envelope covers its convex window" layer window

testNonconvexAndHoledUpperEnvelope :: IO ()
testNonconvexAndHoledUpperEnvelope = do
  nonconvex <- lShapedComponent
  holed <- holedComponent
  let constantForm = Map.singleton (7 :: Int) (AffineForm 1 0 0)
  assertEnvelopeEqualsWindow "nonconvex envelope restriction" nonconvex constantForm
  assertEnvelopeEqualsWindow "holed envelope restriction" holed constantForm

testEmptyUpperEnvelope :: IO ()
testEmptyUpperEnvelope = do
  window <- rectangleComponent 0 0 1 1
  case upperEnvelope window (Map.empty :: Map.Map Int AffineForm) of
    Left UpperEnvelopeEmptyForms -> pure ()
    result -> fail ("empty upper envelope: unexpected result " <> show result)

testLayerCoverage :: IO ()
testLayerCoverage = do
  window <- rectangleComponent 0 0 10 10
  half <- rectangleRegion 0 0 5 10
  incomplete <- singletonLayer False True half
  case layerCovers incomplete window of
    Left (LayerCoverageGap gap) -> assertGapArea 50 gap
    result -> fail ("incomplete layer coverage: unexpected result " <> show result)
  completeRegion <- requireRight "complete coverage region" (planarRegion [window])
  complete <- requireRight "complete coverage layer" (planarLayer False (Map.singleton True completeRegion))
  requireCoverage "complete layer" complete window

testNaryOverlayLabels :: IO ()
testNaryOverlayLabels = do
  vertical <- rectangleRegion 0 0 6 10 >>= singletonLayer False True
  horizontal <- rectangleRegion 0 0 10 6 >>= singletonLayer False True
  central <- rectangleRegion 2 2 8 8 >>= singletonLayer False True
  let sourceLayers = vertical :| [horizontal, central]
  result <- requireRight "fused n-ary overlay" (overlayAll sourceLayers)
  refined <- requireRight "publish fused n-ary overlay" (overlayPlanarLayer result)
  assertNaryLabel sourceLayers refined (exactPoint 3 3)
  assertNaryLabel sourceLayers refined (exactPoint 7 3)
  assertNaryLabel sourceLayers refined (exactPoint 9 9)
  assertNaryLabel sourceLayers refined (exactPoint (-1) (-1))
  singletonResult <- requireRight "singleton n-ary overlay" (overlayAll (vertical :| []))
  singleton <- requireRight "publish singleton n-ary overlay" (overlayPlanarLayer singletonResult)
  assertNaryLabel (vertical :| []) singleton (exactPoint 3 3)

testFusedOverlayFamilyLaws :: IO ()
testFusedOverlayFamilyLaws = do
  vertical <- rectangleRegion 0 0 6 10 >>= singletonLayer False True
  horizontal <- rectangleRegion 0 0 10 6 >>= singletonLayer False True
  central <- rectangleRegion 2 2 8 8 >>= singletonLayer False True
  traverse_ checkPermutation (List.permutations [vertical, horizontal, central])
  duplicate <- requireRight "fused duplicate source boundaries" (overlayAll (vertical :| [vertical, vertical]))
  assertEqual "fused construction records every source" 3 (overlayInputSources (overlayReceipt duplicate))
  assertEqual "duplicate boundaries retain every source ordinal" True
    (all
      ((== Set.fromList [OverlaySourceId 0, OverlaySourceId 1, OverlaySourceId 2])
        . Set.fromList . fmap boundaryRefSource . NonEmpty.toList . overlayEdgeSources . snd)
      (overlayArrangementEdges duplicate))
  duplicateMasses <- requireRight "fused duplicate direct confusion" (overlayConfusion duplicate)
  assertEqual "duplicate provenance does not multiply area"
    (Map.singleton (True :| [True, True]) 60) (fmap exactAreaValue duplicateMasses)
 where
  checkPermutation [firstLayer, secondLayer, thirdLayer] = do
    let sources = firstLayer :| [secondLayer, thirdLayer]
    fused <- requireRight "fused source permutation" (overlayAll sources)
    firstPair <- requireRight "grouped first binary construction" (overlayLayers firstLayer secondLayer)
    pairLayer <- requireRight "publish grouped first binary result" (overlayPlanarLayer firstPair)
    grouped <- requireRight "grouped second binary construction" (overlayLayers pairLayer thirdLayer)
    fusedMasses <- requireRight "fused direct masses" (overlayConfusion fused)
    groupedMasses <- requireRight "grouped direct masses" (overlayConfusion grouped)
    assertEqual "fused and grouped binary exact masses agree"
      (Map.mapKeys flattenLabels groupedMasses) fusedMasses
    fusedMoments <- directOverlayMoments fused
    groupedMoments <- directOverlayMoments grouped
    assertEqual "fused and grouped binary direct moments agree"
      (Map.mapKeys flattenLabels groupedMoments) fusedMoments
    secondMoment <- requireRight "independent three-way rectangle second moment" (exactDivide 832 3)
    assertEqual "direct fused moments equal independent Cartesian integrals"
      (16, 64, 64, secondMoment, 256, secondMoment)
      (momentDigest (Map.findWithDefault mempty (True :| [True, True]) fusedMoments))
    fusedLayer <- requireRight "publish fused source permutation" (overlayPlanarLayer fused)
    let flattenedResult = fmap flattenLabels grouped
    assertOverlayFixedGeometry "n-ary flattening changes labels only" grouped flattenedResult
    flattened <- requireRight "publish grouped labels flattened without reconstruction" (overlayPlanarLayer flattenedResult)
    assertEqual "fused publication agrees with independently grouped binary publication" flattened fusedLayer
    traverse_ (assertNaryLabel sources fusedLayer)
      [exactPoint 3 3, exactPoint 7 3, exactPoint 3 7, exactPoint 9 9, exactPoint (-1) (-1)]
  checkPermutation _ = fail "source-permutation fixture lost its three-source arity"
  flattenLabels :: ((Bool, Bool), Bool) -> NonEmpty Bool
  flattenLabels ((firstLabel, secondLabel), thirdLabel) = firstLabel :| [secondLabel, thirdLabel]

directOverlayMoments :: Ord labels => OverlayResult labels -> IO (Map.Map labels ExactPlanarMoments)
directOverlayMoments result = requireRight "direct exact boundary moments" $
  foldBoundedOverlayCells
    (\moments labels boundary -> Map.insertWith (<>) labels (orientedBoundaryMoments boundary) moments)
    Map.empty result

testOverlayMassAndConfusion :: IO ()
testOverlayMassAndConfusion = do
  left <- rectangleRegion 0 0 6 10 >>= singletonLayer False True
  right <- rectangleRegion 4 0 10 10 >>= singletonLayer False True
  result <- requireRight "mass overlay" (overlayLayers left right)
  assertOverlayLabelAlgebra result
  direct <- requireRight "direct overlap mass" (overlayMass (== (True, True)) result)
  published <-
    requireRight
      "published overlap region"
      (overlaySelectedRegion (== (True, True)) result)
  publishedValues <- requireRight "published overlap valuation" (regionValuations published)
  assertEqual
    "direct mass equals published-region valuation"
    (exactAreaValue (valuationArea publishedValues))
    (exactAreaValue direct)
  assertEqual "overlap mass" 20 (exactAreaValue direct)
  case overlayMass (== (False, False)) result of
    Left RegionUnboundedSelection -> pure ()
    value -> fail ("unbounded overlay mass: unexpected result " <> show value)
  confusion <- requireRight "direct finite confusion" (overlayConfusion result)
  assertEqual
    "one-pass finite confusion masses"
    ( Map.fromList
        [ ((False, True), 40)
        , ((True, False), 40)
        , ((True, True), 20)
        ]
    )
    (fmap exactAreaValue confusion)
  traverse_
    (\(labels, expected) -> do
       measured <- requireRight "direct confusion-cell mass" (overlayMass (== labels) result)
       assertEqual "confusion entry equals direct mass" expected (exactAreaValue measured))
    (Map.toAscList (fmap exactAreaValue confusion))

-- The result's functor/traversal ranges over its one face-label vector. Exact
-- incidence, source provenance and construction work are fixed observations,
-- not reconstructed payloads or a second label authoring surface.
assertOverlayLabelAlgebra :: OverlayResult (Bool, Bool) -> IO ()
assertOverlayLabelAlgebra result = do
  assertOverlayObservationsEqual "overlay Functor identity" result (fmap id result)
  assertOverlayObservationsEqual "overlay Functor composition"
    (fmap (markLabels . relabel) result) (fmap markLabels (fmap relabel result))
  assertOverlayObservationsEqual "overlay Traversable identity" result
    (runIdentity (traverse Identity result))
  assertEqual "overlay Foldable follows the authoritative face-label order"
    (fmap snd (overlayCells result)) (toList result)

  let composed = getCompose (traverse (Compose . fmap branchLabels . admitLabels) result)
      nested = fmap (traverse branchLabels) (traverse admitLabels result)
  assertEqual "overlay Traversable composition preserves effects and label order"
    (fmap (fmap overlayCells) composed) (fmap (fmap overlayCells) nested)
  traverse_ (traverse_ (assertOverlayFixedGeometry "composed label traversal fixes geometry" result)) composed
  case traverse rejectOverlap result of
    Nothing -> pure ()
    Just _ -> fail "overlay label traversal swallowed an explicit decoder refusal"

  let relabelled = fmap relabel result
  assertOverlayFixedGeometry "bijective relabeling preserves exact construction" result relabelled
  sourceMoments <- directOverlayMoments result
  relabelledMoments <- directOverlayMoments relabelled
  assertEqual "direct exact moments commute with bijective relabeling"
    (Map.mapKeys relabel sourceMoments) relabelledMoments
  sourceMass <- requireRight "source selected mass" (overlayMass (== (True, True)) result)
  relabelledMass <- requireRight "relabelled selected mass" (overlayMass (== (GT, GT)) relabelled)
  assertEqual "exact selected mass is independent of label encoding" sourceMass relabelledMass
  sourceRegion <- requireRight "source selected region" (overlaySelectedRegion (== (True, True)) result)
  relabelledRegion <- requireRight "relabelled selected region" (overlaySelectedRegion (== (GT, GT)) relabelled)
  assertEqual "exact selected publication is independent of label encoding" sourceRegion relabelledRegion
  sourceCells <- requireRight "source closed selected cells" (overlayClosedIntersection id id result)
  relabelledCells <- requireRight "relabelled closed selected cells" (overlayClosedIntersection (== GT) (== GT) relabelled)
  assertEqual "closed selection incidence survives transported predicates" (cellCounts sourceCells) (cellCounts relabelledCells)
  assertEqual "closed selection exact coordinates survive transported predicates" (cellPointSet sourceCells) (cellPointSet relabelledCells)
  sourceValues <- requireRight "source closed valuations" (cellValuations sourceCells)
  relabelledValues <- requireRight "relabelled closed valuations" (cellValuations relabelledCells)
  -- Relabeling fixes these integer boundary segments, so their term
  -- presentations agree. This is not a general Eq law for radical expressions.
  assertEqual "closed selection exact valuations commute with relabeling"
    (valuationEuler sourceValues, valuationArea sourceValues, exactLengthTerms (exactLengthExpression (valuationIntrinsic1 sourceValues)))
    (valuationEuler relabelledValues, valuationArea relabelledValues, exactLengthTerms (exactLengthExpression (valuationIntrinsic1 relabelledValues)))
 where
  relabel :: (Bool, Bool) -> (Ordering, Ordering)
  relabel (left, right) = (if left then GT else LT, if right then GT else LT)
  markLabels :: (Ordering, Ordering) -> (Maybe Ordering, Maybe Ordering)
  markLabels (left, right) = (Just left, Just right)
  admitLabels :: (Bool, Bool) -> Maybe (Ordering, Ordering)
  admitLabels = Just . relabel
  branchLabels :: (Ordering, Ordering) -> [(Maybe Ordering, Maybe Ordering)]
  branchLabels labels@(_, right) = [markLabels labels, (Nothing, Just right)]
  rejectOverlap :: (Bool, Bool) -> Maybe (Ordering, Ordering)
  rejectOverlap labels
    | labels == (True, True) = Nothing
    | otherwise = admitLabels labels

assertOverlayObservationsEqual
  :: (Eq labels, Show labels)
  => String -> OverlayResult labels -> OverlayResult labels -> IO ()
assertOverlayObservationsEqual label expected actual = do
  assertEqual (label <> " face labels") (overlayCells expected) (overlayCells actual)
  assertOverlayFixedGeometry label expected actual

assertOverlayFixedGeometry
  :: String -> OverlayResult leftLabels -> OverlayResult rightLabels -> IO ()
assertOverlayFixedGeometry label expected actual = do
  assertEqual (label <> " exact vertices and source provenance")
    (overlayArrangementVertices expected) (overlayArrangementVertices actual)
  assertEqual (label <> " atomic edges and source provenance")
    (overlayArrangementEdges expected) (overlayArrangementEdges actual)
  assertEqual (label <> " construction receipt") (overlayReceipt expected) (overlayReceipt actual)
  assertEqual (label <> " face identity") (fmap fst (overlayCells expected)) (fmap fst (overlayCells actual))
  expectedBoundaries <- requireRight (label <> " source oriented face boundaries")
    (foldBoundedOverlayCells (\boundaries _ boundary -> boundary : boundaries) [] expected)
  actualBoundaries <- requireRight (label <> " mapped oriented face boundaries")
    (foldBoundedOverlayCells (\boundaries _ boundary -> boundary : boundaries) [] actual)
  assertEqual (label <> " exact oriented face geometry") expectedBoundaries actualBoundaries

-- These corners are distinct exact points but all four project to (1,1).
-- Exact semantics must not depend on whether a faithful native DCEL exists.
testExactOverlayCollapsedEmbedding :: IO ()
testExactOverlayCollapsedEmbedding = do
  epsilon <- requireRight "exact overlay epsilon" (exactRational 1 (2 ^ (80 :: Int)))
  tiny <- exactRectangleRegion 1 1 (1 + epsilon) (1 + epsilon)
  let corners = concatMap componentBoundaryPoints (planarRegionComponents tiny)
  projected <- traverse (requireRight "tiny optional embedding" . exactPointToEmbeddingCandidate) corners
  assertEqual "tiny exact square has four distinct corners" 4 (Set.size (Set.fromList corners))
  assertEqual "tiny exact square has only one binary64 candidate" 1
    (Set.size (Set.fromList (fmap queryPointValue projected)))
  layer <- singletonLayer False True tiny
  overlay <- requireRight "exact overlay survives collapsed embedding" (overlayLayers layer layer)
  selected <- requireRight "publish tiny exact intersection" (overlaySelectedRegion (== (True, True)) overlay)
  assertEqual "tiny exact support is not rounded away" tiny selected
  assertRectangleMoments "tiny exact intersection moments" 1 1 (1 + epsilon) (1 + epsilon) selected
  mass <- requireRight "tiny exact intersection mass" (overlayMass (== (True, True)) overlay)
  confusion <- requireRight "tiny exact confusion" (overlayConfusion overlay)
  assertEqual "tiny exact area is nonzero" (epsilon * epsilon) (exactAreaValue mass)
  assertEqual "coincident source supports do not double-count exact area"
    (Map.singleton (True, True) (epsilon * epsilon)) (fmap exactAreaValue confusion)
  closed <- requireRight "tiny exact closed intersection" (overlayClosedIntersection id id overlay)
  assertCellValuations "tiny closed intersection" 1 (epsilon * epsilon) closed
  assertEqual "tiny closed selection retains all exact corners" (Set.fromList corners) (cellPointSet closed)
  naryResult <- requireRight "n-ary overlay retains tiny exact cell" (overlayAll (layer :| [layer, layer]))
  nary <- requireRight "publish n-ary tiny exact cell" (overlayPlanarLayer naryResult)
  assertEqual "n-ary tiny exact region" (Just tiny)
    (Map.lookup (True :| [True, True]) (planarLayerRegions nary))

testExactOverlayTinyCrossings :: IO ()
testExactOverlayTinyCrossings = do
  epsilon <- requireRight "exact crossing epsilon" (exactRational 1 (2 ^ (80 :: Int)))
  horizontal <- exactRectangleRegion 0 1 3 (1 + epsilon) >>= singletonLayer False True
  vertical <- exactRectangleRegion 1 0 (1 + epsilon) 3 >>= singletonLayer False True
  overlay <- requireRight "tiny rational proper crossings" (overlayLayers horizontal vertical)
  intersection <- requireRight "publish tiny crossing cell" (overlaySelectedRegion (== (True, True)) overlay)
  expected <- exactRectangleRegion 1 1 (1 + epsilon) (1 + epsilon)
  assertEqual "crossing points retain their exact offsets" expected intersection
  assertRectangleMoments "tiny crossing moments" 1 1 (1 + epsilon) (1 + epsilon) intersection
  let overlapArea = epsilon * epsilon
      exclusiveArea = 3 * epsilon - overlapArea
  confusion <- requireRight "tiny crossing direct confusion" (overlayConfusion overlay)
  assertEqual "tiny crossing partitions both exact strips"
    (Map.fromList [((True, False), exclusiveArea), ((False, True), exclusiveArea), ((True, True), overlapArea)])
    (fmap exactAreaValue confusion)
  halfEpsilon <- requireRight "half crossing epsilon" (exactDivide epsilon 2)
  labels <- requireRight "publish tiny crossing labels" (overlayPlanarLayer overlay)
  assertEqual "tiny exact crossing interior label" (True, True)
    (planarLayerLabelAt labels (exactPoint (1 + halfEpsilon) (1 + halfEpsilon)))
  assertEqual "tiny exact strip-only label" (True, False)
    (planarLayerLabelAt labels (exactPoint 2 (1 + halfEpsilon)))

  -- A non-axis-aligned event constructs (1-epsilon/2,1+epsilon/2),
  -- rather than reusing a submitted corner or a rounded line intersection.
  descending <- exactPolygonRegion
    (exactPoint 0 0 :| [exactPoint 2 0, exactPoint 0 2]) >>= singletonLayer False True
  ascending <- exactPolygonRegion
    (exactPoint 0 epsilon :| [exactPoint 2 epsilon, exactPoint 2 (2 + epsilon)]) >>= singletonLayer False True
  slanted <- requireRight "tiny-offset slanted crossing" (overlayLayers descending ascending)
  slantedIntersection <- requireRight "publish tiny-offset slanted crossing"
    (overlaySelectedRegion (== (True, True)) slanted)
  slantedExpected <- exactPolygonRegion
    (exactPoint 0 epsilon :| [exactPoint (2 - epsilon) epsilon, exactPoint (1 - halfEpsilon) (1 + halfEpsilon)])
  assertEqual "slanted event has its algebraic exact coordinates" slantedExpected slantedIntersection
  slantedArea <- requireRight "slanted exact triangle area" (exactDivide ((2 - epsilon) * (2 - epsilon)) 4)
  observedArea <- requireRight "slanted direct area" (overlayMass (== (True, True)) slanted)
  assertEqual "slanted exact triangle area formula" slantedArea (exactAreaValue observedArea)

testExactOverlayDisconnectedCells :: IO ()
testExactOverlayDisconnectedCells = do
  firstIsland <- rectangleComponent 0 0 2 2
  secondIsland <- rectangleComponent 5 0 7 2
  islands <- requireRight "disconnected source region" (planarRegion [secondIsland, firstIsland])
  left <- singletonLayer False True islands
  right <- rectangleRegion 1 (-1) 6 1 >>= singletonLayer False True
  overlay <- requireRight "disconnected exact refinement" (overlayLayers left right)
  selected <- requireRight "publish disconnected intersections" (overlaySelectedRegion (== (True, True)) overlay)
  firstExpected <- rectangleComponent 1 0 2 1
  secondExpected <- rectangleComponent 5 0 6 1
  expected <- requireRight "disconnected exact expected region" (planarRegion [firstExpected, secondExpected])
  assertEqual "disconnected cells do not share an artificial bridge" expected selected
  assertEqual "disconnected intersection component count" 2 (length (planarRegionComponents selected))
  assertEqual "disconnected exact moments"
    (momentDigest (polygonComponentMoments firstExpected <> polygonComponentMoments secondExpected))
    (momentDigest (foldMap polygonComponentMoments (planarRegionComponents selected)))
  closed <- requireRight "disconnected closed intersection" (overlayClosedIntersection id id overlay)
  assertCellValuations "disconnected closed intersection" 2 2 closed
  labels <- requireRight "publish disconnected labels" (overlayPlanarLayer overlay)
  assertEqual "gap retains the right-only label" (False, True)
    (planarLayerLabelAt labels (exactPoint 3 0))

testExactOverlayNestedHolesAndIslands :: IO ()
testExactOverlayNestedHolesAndIslands = do
  outer <- requireRight "nested outer loop" (exactLoop (rectanglePoints 0 0 10 10))
  hole <- requireRight "nested clockwise hole" (exactLoop (NonEmpty.reverse (rectanglePoints 2 2 8 8)))
  annulus <- requireRight "nested annulus component" (polygonComponent outer [hole])
  island <- rectangleComponent 4 4 6 6
  annularRegion <- requireRight "nested annulus region" (planarRegion [annulus])
  islandRegion <- requireRight "nested island region" (planarRegion [island])
  left <- singletonLayer False True annularRegion
  right <- singletonLayer False True islandRegion
  overlay <- requireRight "nested disconnected exact refinement" (overlayLayers left right)
  published <- requireRight "publish annulus with nested island" (overlaySelectedRegion (uncurry (||)) overlay)
  expected <- requireRight "annulus and nested island expected region" (planarRegion [annulus, island])
  assertEqual "nested CCBs belong to their actual planar faces" expected published
  assertEqual "nested hole is not mistaken for an independent disk"
    (2, 1) (length (planarRegionComponents published), sum (fmap (length . polygonHoleLoops) (planarRegionComponents published)))
  confusion <- requireRight "nested direct confusion" (overlayConfusion overlay)
  assertEqual "nested finite label masses"
    (Map.fromList [((True, False), 64), ((False, True), 4)]) (fmap exactAreaValue confusion)
  finiteOutsideArea <- requireRight "direct bounded outside-label cavity area" $
    foldBoundedOverlayCells
      (\area labels boundary -> if labels == (False, False) then area + exactPlanarMeasure (orientedBoundaryMoments boundary) else area)
      0 overlay
  assertEqual "bounded outside-label cavity excludes the nested island" 32 finiteOutsideArea
  union <- requireRight "nested closed union" (overlayClosedUnion id id overlay)
  assertCellValuations "nested closed union has annulus plus disk topology" 1 68 union
  emptyIntersection <- requireRight "nested closed intersection is empty" (overlayClosedIntersection id id overlay)
  assertCellValuations "nested disjoint closed intersection" 0 0 emptyIntersection
  assertEqual "nested disjoint closed intersection has no cells" (0, 0, 0) (cellCounts emptyIntersection)

  -- The hole boundary is clockwise in one source and counter-clockwise in
  -- the filler. Its closed intersection is a cycle, not a disk or four gaps.
  filler <- rectangleRegion 2 2 8 8 >>= singletonLayer False True
  filled <- requireRight "oppositely wound shared hole support" (overlayLayers left filler)
  sharedCycle <- requireRight "closed shared hole boundary" (overlayClosedIntersection id id filled)
  assertCellValuations "shared hole boundary is one closed cycle" 0 0 sharedCycle
  assertEqual "shared hole boundary retains exactly its four sides" (4, 4, 0) (cellCounts sharedCycle)
  assertEqual "shared hole support retains its exact vertices"
    (Set.fromList (NonEmpty.toList (rectanglePoints 2 2 8 8))) (cellPointSet sharedCycle)
  filledRegion <- requireRight "publish filled hole" (overlaySelectedRegion (uncurry (||)) filled)
  fullWindow <- rectangleRegion 0 0 10 10
  assertEqual "shared boundary dissolves when both incident faces are selected" fullWindow filledRegion
  difference <- requireRight "regularized annulus difference" (overlayRegularizedDifference id id filled)
  assertCellValuations "regularized difference retains annular topology" 0 64 difference

testExactOverlayReversedSupportAndTJunction :: IO ()
testExactOverlayReversedSupportAndTJunction = do
  leftRegion <- rectangleRegion 0 0 2 2
  rightRegion <- rectangleRegion 2 0 4 1
  left <- singletonLayer False True leftRegion
  right <- singletonLayer False True rightRegion
  overlay <- requireRight "reversed shared support with T-junction" (overlayLayers left right)
  reversed <- requireRight "operand-reversed T-junction" (overlayLayers right left)
  traverse_
    (\result -> do
      shared <- requireRight "T-junction closed shared segment" (overlayClosedIntersection id id result)
      assertEqual "T-junction splits an edge at the exact contact endpoint" (2, 1, 0) (cellCounts shared)
      assertEqual "opposite source directions glue onto the same support"
        (Set.fromList [integerPoint 2 0, integerPoint 2 1]) (cellPointSet shared)
      assertCellValuations "T-junction shared edge" 1 0 shared)
    [overlay, reversed]
  regularized <- requireRight "T-junction regularized difference" (overlayRegularizedDifference id id overlay)
  assertCellValuations "subtracting an adjacent area does not puncture the closed left region" 1 4 regularized
  published <- requireRight "publish regularized T-junction difference"
    (overlaySelectedRegion (\(insideLeft, insideRight) -> insideLeft && not insideRight) overlay)
  assertEqual "regularized difference restores the shared boundary" leftRegion published
  assertEqual "regularized closure includes both contact endpoints" True
    (Set.fromList [integerPoint 2 0, integerPoint 2 1] `Set.isSubsetOf` cellPointSet regularized)

testExactOverlayVertexContact :: IO ()
testExactOverlayVertexContact = do
  leftRegion <- rectangleRegion 0 0 1 1
  rightRegion <- rectangleRegion 1 1 2 2
  left <- singletonLayer False True leftRegion
  right <- singletonLayer False True rightRegion
  overlay <- requireRight "exact vertex-only contact" (overlayLayers left right)
  contact <- requireRight "closed vertex-only intersection" (overlayClosedIntersection id id overlay)
  assertEqual "closed intersection retains only the shared vertex" (1, 0, 0) (cellCounts contact)
  assertEqual "shared vertex has exact source coordinates" (Set.singleton (integerPoint 1 1)) (cellPointSet contact)
  assertCellValuations "a point has Euler one and area zero" 1 0 contact
  areaIntersection <- requireRight "regularized vertex-only intersection" (overlaySelectedRegion (== (True, True)) overlay)
  assertEqual "publishing only full-dimensional intersection does not fabricate a point polygon" [] (planarRegionComponents areaIntersection)
  difference <- requireRight "regularized vertex-contact difference" (overlayRegularizedDifference id id overlay)
  assertCellValuations "regularized point-contact difference remains a disk" 1 1 difference
  assertEqual "regularized closure keeps the contact vertex" True
    (Set.member (integerPoint 1 1) (cellPointSet difference))
  union <- requireRight "closed vertex-contact union" (overlayClosedUnion id id overlay)
  assertCellValuations "two closed disks glued at a point are connected" 1 2 union

rectanglePoints :: ExactRational -> ExactRational -> ExactRational -> ExactRational -> NonEmpty ExactPoint
rectanglePoints minimumX minimumY maximumX maximumY =
  exactPoint minimumX minimumY :|
    [exactPoint maximumX minimumY, exactPoint maximumX maximumY, exactPoint minimumX maximumY]

exactPolygonRegion :: NonEmpty ExactPoint -> IO PlanarRegion
exactPolygonRegion points = do
  loop <- requireRight "exact test polygon loop" (exactLoop points)
  component <- requireRight "exact test polygon component" (polygonComponent loop [])
  requireRight "exact test polygon region" (planarRegion [component])

exactRectangleRegion :: ExactRational -> ExactRational -> ExactRational -> ExactRational -> IO PlanarRegion
exactRectangleRegion minimumX minimumY maximumX maximumY =
  exactPolygonRegion (rectanglePoints minimumX minimumY maximumX maximumY)

-- Independent Cartesian integrals, not the production boundary-moment sum.
assertRectangleMoments
  :: String -> ExactRational -> ExactRational -> ExactRational -> ExactRational -> PlanarRegion -> IO ()
assertRectangleMoments label minimumX minimumY maximumX maximumY region = do
  firstX <- requireRight label (exactDivide (maximumX * maximumX - minimumX * minimumX) 2)
  firstY <- requireRight label (exactDivide (maximumY * maximumY - minimumY * minimumY) 2)
  secondX <- requireRight label (exactDivide (maximumX ^ (3 :: Int) - minimumX ^ (3 :: Int)) 3)
  secondY <- requireRight label (exactDivide (maximumY ^ (3 :: Int) - minimumY ^ (3 :: Int)) 3)
  let width = maximumX - minimumX
      height = maximumY - minimumY
  assertEqual label
    (width * height, firstX * height, width * firstY, secondX * height, firstX * firstY, width * secondY)
    (momentDigest (foldMap polygonComponentMoments (planarRegionComponents region)))

momentDigest :: ExactPlanarMoments -> (ExactRational, ExactRational, ExactRational, ExactRational, ExactRational, ExactRational)
momentDigest moments =
  ( exactPlanarMeasure moments, exactPlanarFirstX moments, exactPlanarFirstY moments
  , exactPlanarSecondXX moments, exactPlanarSecondXY moments, exactPlanarSecondYY moments
  )

cellCounts :: ExactCellSet -> (Int, Int, Int)
cellCounts cells = (exactCellSetVertexCount cells, exactCellSetEdgeCount cells, exactCellSetFaceCount cells)

cellPointSet :: ExactCellSet -> Set.Set ExactPoint
cellPointSet = foldExactCellVertices (\points _ point -> Set.insert point points) Set.empty

assertCellValuations :: String -> Int -> ExactRational -> ExactCellSet -> IO ()
assertCellValuations label expectedEuler expectedArea cells = do
  values <- requireRight label (cellValuations cells)
  assertEqual (label <> " Euler and area") (expectedEuler, expectedArea)
    (eulerCharacteristicValue (valuationEuler values), exactAreaValue (valuationArea values))

assertEnvelopeDominance
  :: Map.Map Int AffineForm
  -> PlanarLayer (Maybe Int)
  -> IO ()
assertEnvelopeDominance forms layer =
  traverse_ checkRegion (Map.toAscList (planarLayerRegions layer))
 where
  checkRegion :: (Maybe Int, PlanarRegion) -> IO ()
  checkRegion (Nothing, _) = fail "bounded upper-envelope region has the outside label"
  checkRegion (Just winner, region) =
    case Map.lookup winner forms of
      Nothing -> fail ("upper-envelope winner is absent from its input map: " <> show winner)
      Just winnerForm ->
        traverse_
          (checkWinnerAtPoint winner winnerForm)
          (concatMap componentBoundaryPoints (planarRegionComponents region))

  checkWinnerAtPoint
    :: Int
    -> AffineForm
    -> ExactPoint
    -> IO ()
  checkWinnerAtPoint winner winnerForm point =
    let winnerValue = affineValue winnerForm point
     in traverse_
          (\(candidate, candidateForm) ->
             unless (winnerValue >= affineValue candidateForm point) $
               fail
                 ( "upper-envelope dominance failed: winner "
                     <> show winner
                     <> ", candidate "
                     <> show candidate
                 ))
          (Map.toAscList forms)

componentBoundaryPoints :: PolygonComponent -> [ExactPoint]
componentBoundaryPoints component =
  NonEmpty.toList (exactLoopPoints (polygonOuterLoop component))
    <> concatMap (NonEmpty.toList . exactLoopPoints) (polygonHoleLoops component)

affineValue
  :: AffineForm
  -> ExactPoint
  -> ExactRational
affineValue form point =
  let (coordinateX, coordinateY) = exactPointCoordinates point
   in affineFormConstant form
        + affineFormXCoefficient form * coordinateX
        + affineFormYCoefficient form * coordinateY

assertEnvelopeEqualsWindow
  :: String
  -> PolygonComponent
  -> Map.Map Int AffineForm
  -> IO ()
assertEnvelopeEqualsWindow label window forms = do
  expected <- requireRight (label <> " expected region") (planarRegion [window])
  layer <- requireRight label (upperEnvelope window forms)
  assertEqual label (Just expected) (Map.lookup (Just 7) (planarLayerRegions layer))
  requireCoverage (label <> " coverage") layer window

assertGapArea :: Integer -> CoverGap -> IO ()
assertGapArea expected gap = do
  values <- requireRight "gap valuation" (regionValuations (coverGapRegion gap))
  assertEqual "coverage obstruction retains exact gap" (fromInteger expected) (exactAreaValue (valuationArea values))

assertNaryLabel
  :: NonEmpty (PlanarLayer Bool)
  -> PlanarLayer (NonEmpty Bool)
  -> ExactPoint
  -> IO ()
assertNaryLabel sourceLayers refined point =
  assertEqual
    "n-ary label tuple follows source order"
    (fmap (`planarLayerLabelAt` point) sourceLayers)
    (planarLayerLabelAt refined point)

requireCoverage
  :: Ord label
  => String
  -> PlanarLayer label
  -> PolygonComponent
  -> IO ()
requireCoverage label layer window =
  case layerCovers layer window of
    Right () -> pure ()
    Left failure -> fail (label <> ": " <> show failure)

lShapedComponent :: IO PolygonComponent
lShapedComponent = do
  outer <-
    requireRight
      "L-shaped outer loop"
      ( exactLoop
          ( integerPoint 0 0
              :| [ integerPoint 4 0
                 , integerPoint 4 1
                 , integerPoint 1 1
                 , integerPoint 1 4
                 , integerPoint 0 4
                 ]
          )
      )
  requireRight "L-shaped component" (polygonComponent outer [])

holedComponent :: IO PolygonComponent
holedComponent = do
  outer <-
    requireRight
      "holed outer loop"
      (exactLoop (integerPoint 0 0 :| [integerPoint 6 0, integerPoint 6 6, integerPoint 0 6]))
  hole <-
    requireRight
      "clockwise hole loop"
      (exactLoop (integerPoint 2 2 :| [integerPoint 2 4, integerPoint 4 4, integerPoint 4 2]))
  requireRight "holed component" (polygonComponent outer [hole])
