-- | Exact bounded power-cell laws through the public owner.
module Moonlight.Triangulation.PowerDiagramSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Moonlight.Triangulation
  ( ExactPoint
  , ExactRational
  , ExactVector (..)
  , BoundedPowerDiagram
  , ConvexPolygon
  , Point (..)
  , PowerCellDisposition (..)
  , PowerDualEdge (..)
  , PowerDiagramError (..)
  , PowerSite
  , PowerWeight
  , PowerWeightError (..)
  , powerAlphaBirthExact
  , RegularEditError (..)
  , RegularEditResult (..)
  , RegularSiteDisposition (..)
  , RegularSiteTransition (..)
  , RegularEdge
  , RegularTriangulation
  , boundedPowerDiagram
  , boundedPowerDiagramFromRegular
  , convexPolygon
  , convexPolygonPoints
  , exactPointCoordinates
  , exactPointFromPoint
  , exactRayDirection
  , exactRayOrigin
  , exactSegmentEndpoints
  , planarLayerRegions
  , powerCellDisposition
  , powerCellDispositions
  , powerDiagramActiveBoundaries
  , powerDiagramCoincidentEquivalentCells
  , powerDiagramCoincidentDominatedCells
  , powerDiagramEmptyCells
  , powerDiagramInputSites
  , powerDiagramLowerDimensionalCells
  , powerDiagramPlanarLayer
  , powerDiagramPublishedCells
  , powerDiagramOracleCells
  , powerDiagramMaximumCellConstraints
  , powerDiagramRegularEdges
  , powerDiagramSubmittedSiteConstraints
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerSiteWeight
  , powerWeight
  , powerWeightExact
  , regularAlphaBirths
  , regularAlphaComplex
  , regularAlphaComplexAtBirth
  , regularAlphaFiltration
  , emptyRegularTriangulation
  , insertRegularSite
  , regularEdgeDual
  , regularEdgeLabels
  , regularEdges
  , regularFaceDualPoint
  , regularFaceLabels
  , regularFaces
  , regularNeighbours
  , regularSite
  , regularSiteCount
  , regularSiteDisposition
  , regularSites
  , regularTriangulation
  , regularTriangulationReceipt
  , regularTriangulationEdges
  , regularTriangulationFaces
  , regularTriangulationInputSites
  , regularTriangulationVisibleSites
  , removeRegularSite
  , reweightRegularSites
  , translateExactPoint
  )
import Moonlight.Triangulation.Exact
  ( ExactClipDisposition (..)
  , ExactClosedHalfPlane
  , exactAffineLine
  , exactClipRetainedPolygon
  , exactClosedHalfPlane
  , exactRetainedPolygon
  , exactRetainedPolygonPoints
  )
import Moonlight.Triangulation.Simplex
  ( planarComplexCells
  , planarEdge
  , planarFace
  , planarVertex
  )
import Support (assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = do
  testEqualWeightsAndCommonShift
  testThreeSiteExactPartitionCoverage
  testRegularTriangleDualRays
  testRegularAlphaBirths
  testRegularAlphaCommonShift
  testRegularBoundedDualSegment
  testRegularCoplanarUpperFacet
  testRegularCollinearClassification
  testRegularLowerDimensionalAndHiddenSites
  testRegularSiteOwnership
  testHiddenInsertionAndExposure
  testRegularInsertionAndRemovalTransitions
  testRegularReweightTransitions
  testTopologyPreservingEdits
  testRegularEditDifferentialLaws
  testRegularEditObstructionsAndIdempotence
  testBoundedPowerFromRegular
  testSparsePowerMatchesCompleteOracle
  testEqualCoincidentResolution
  testDominantCoincidentResolution
  testLowerDimensionalCell
  testDistinctEmptyCell
  testDyadicNearParallelBisectors
  testBinary64PrecisionSites
  testDuplicateLabels
  testNonFiniteWeight
  putStrLn "power diagram: ok"

testRegularAlphaBirths :: IO ()
testRegularAlphaBirths = do
  positive <- admittedWeight 3
  singleton <- admittedSite "only" (Point 0 0) positive
  (singleRegular, _) <-
    requireRight "singleton regular topology" (regularTriangulation (singleton :| []))
  singleAlpha <- requireRight "singleton weighted alpha" (regularAlphaFiltration singleRegular)
  assertEqual
    "positive weight gives signed vertex birth"
    (Just (-3))
    (fmap powerAlphaBirthExact (Map.lookup (planarVertex "only") (regularAlphaBirths singleAlpha)))

  zero <- admittedWeight 0
  firstSite <- admittedSite "first" (Point 0 0) zero
  secondSite <- admittedSite "second" (Point 2 0) zero
  thirdSite <- admittedSite "third" (Point 0 2) zero
  dominated <- admittedSite "dominated" (Point 0 0) =<< admittedWeight (-1)
  (regular, _) <-
    requireRight
      "triangle regular topology for weighted alpha"
      (regularTriangulation (firstSite :| [secondSite, thirdSite, dominated]))
  filtration <- requireRight "triangle weighted alpha" (regularAlphaFiltration regular)
  firstSecond <- requireRight "first-second simplex" (planarEdge "first" "second")
  firstThird <- requireRight "first-third simplex" (planarEdge "first" "third")
  secondThird <- requireRight "second-third simplex" (planarEdge "second" "third")
  face <- requireRight "triangle simplex" (planarFace "first" "second" "third")
  let birthAt simplex = fmap powerAlphaBirthExact (Map.lookup simplex (regularAlphaBirths filtration))
  assertEqual "first leg birth" (Just 1) (birthAt firstSecond)
  assertEqual "second leg birth" (Just 1) (birthAt firstThird)
  assertEqual "hypotenuse birth" (Just 2) (birthAt secondThird)
  assertEqual "face birth" (Just 2) (birthAt face)
  assertEqual
    "coincident subordinate has no alpha simplex"
    False
    (Set.member (planarVertex "dominated") (planarComplexCells (regularAlphaComplex filtration)))
  threshold <- requireSome "edge threshold" (Map.lookup firstSecond (regularAlphaBirths filtration))
  _ <- requireRight "weighted alpha sublevel is closed" (regularAlphaComplexAtBirth threshold filtration)
  pure ()

testRegularAlphaCommonShift :: IO ()
testRegularAlphaCommonShift = do
  zero <- admittedWeight 0
  shifted <- admittedWeight 3
  let labelledPoints =
        ("south-west", Point 0 0)
          :| [ ("south-east", Point 4 0)
             , ("north-east", Point 3 3)
             , ("north-west", Point 0 4)
             ]
  baseSites <- traverse (\(label, point) -> admittedSite label point zero) labelledPoints
  shiftedSites <- traverse (\(label, point) -> admittedSite label point shifted) labelledPoints
  (baseRegular, _) <- requireRight "base regular alpha topology" (regularTriangulation baseSites)
  (shiftedRegular, _) <- requireRight "shifted regular alpha topology" (regularTriangulation shiftedSites)
  shiftedEdit <-
    requireRight
      "common regular weight shift"
      ( reweightRegularSites
          (Map.fromList [(label, shifted) | (label, _) <- NonEmpty.toList labelledPoints])
          baseRegular
      )
  assertEqual
    "common shift reuses the regular topology"
    shiftedRegular
    (regularEditTriangulation shiftedEdit)
  assertEqual "common shift changes no disposition" [] (editTransitions shiftedEdit)
  baseAlpha <- requireRight "base regular alpha" (regularAlphaFiltration baseRegular)
  shiftedAlpha <- requireRight "shifted regular alpha" (regularAlphaFiltration shiftedRegular)
  assertEqual
    "common weight shift preserves the complex"
    (regularAlphaComplex baseAlpha)
    (regularAlphaComplex shiftedAlpha)
  assertEqual
    "common weight shift subtracts from every birth"
    (Map.map ((\birth -> birth - 3) . powerAlphaBirthExact) (regularAlphaBirths baseAlpha))
    (Map.map powerAlphaBirthExact (regularAlphaBirths shiftedAlpha))

testRegularTriangleDualRays :: IO ()
testRegularTriangleDualRays = do
  zero <- admittedWeight 0
  firstSite <- admittedSite "first" (Point 0 0) zero
  secondSite <- admittedSite "second" (Point 2 0) zero
  thirdSite <- admittedSite "third" (Point 0 2) zero
  let sites = firstSite :| [secondSite, thirdSite]
  (regular, receipt) <-
    requireRight "three-site regular topology" (regularTriangulation sites)
  assertEqual "three visible regular sites" 3 (regularTriangulationVisibleSites receipt)
  assertEqual "one regular face" 1 (length (regularFaces regular))
  assertEqual "three regular boundary edges" 3 (length (regularEdges regular))
  dualPoint <-
    case regularFaces regular of
      [face] -> pure (regularFaceDualPoint face)
      faces -> fail ("three-site topology expected one face, got " <> show (length faces))
  traverse_ (assertBoundaryDualRay sites dualPoint) (regularEdges regular)
  traverse_
    (\site ->
       let label = powerSiteLabel site
        in do
          assertEqual
            ("visible regular disposition for " <> label)
            (Just RegularSiteVisible)
            (regularSiteDisposition label regular)
          assertEqual
            ("two regular neighbours for " <> label)
            2
            (Set.size (regularNeighbours label regular)))
    sites

testRegularBoundedDualSegment :: IO ()
testRegularBoundedDualSegment = do
  zero <- admittedWeight 0
  sites <-
    traverse
      (\(label, point) -> admittedSite label point zero)
      ( ("south-west", Point 0 0)
          :| [ ("south-east", Point 4 0)
             , ("north-east", Point 3 3)
             , ("north-west", Point 0 4)
             ]
      )
  (regular, receipt) <-
    requireRight "four-site regular topology" (regularTriangulation sites)
  assertEqual
    "regular planar Euler equation"
    1
    ( regularTriangulationVisibleSites receipt
        - regularTriangulationEdges receipt
        + regularTriangulationFaces receipt
    )
  let faceDuals = Set.fromList (fmap regularFaceDualPoint (regularFaces regular))
      bounded =
        [ segment
        | edge <- regularEdges regular
        , BoundedPowerDual segment <- [regularEdgeDual edge]
        ]
  case bounded of
    [segment] ->
      let (firstEndpoint, secondEndpoint) = exactSegmentEndpoints segment
       in unless
            (Set.member firstEndpoint faceDuals && Set.member secondEndpoint faceDuals)
            (fail "bounded regular dual does not join its two incident face duals")
    segments ->
      fail ("four-site topology expected one bounded dual, got " <> show (length segments))

testRegularCoplanarUpperFacet :: IO ()
testRegularCoplanarUpperFacet = do
  sites <-
    traverse
      prepareOracleSite
      ( ("a-bottom", Point 0.25 0.25, -0.875)
          :| [ ("b-south-west", Point 0 0, 0)
             , ("c-south-east", Point 1 0, 1)
             , ("d-center", Point 0.5 0.5, 0.5)
             , ("e-north-east", Point 1 1, 2)
             , ("f-north-west", Point 0 1, 1)
             ]
      )
  (regular, receipt) <-
    requireRight "coplanar upper regular facet" (regularTriangulation sites)
  traverse_
    (\label ->
       assertEqual
         ("upper-facet vertex remains visible: " <> label)
         (Just RegularSiteVisible)
         (regularSiteDisposition label regular))
    ["b-south-west", "c-south-east", "e-north-east", "f-north-west"]
  assertEqual
    "upper-facet interior generator remains lower-dimensional"
    (Just RegularSiteLowerDimensional)
    (regularSiteDisposition "d-center" regular)
  assertEqual
    "strictly lower lifted generator remains hidden"
    (Just RegularSiteHidden)
    (regularSiteDisposition "a-bottom" regular)
  assertEqual "coplanar upper facet has four visible vertices" 4 (regularTriangulationVisibleSites receipt)
  assertEqual "coplanar upper facet receives one deterministic diagonal" 2 (regularTriangulationFaces receipt)

testRegularLowerDimensionalAndHiddenSites :: IO ()
testRegularLowerDimensionalAndHiddenSites = do
  domain <- squareDomain
  zero <- admittedWeight 0
  lowerWeight <- admittedWeight (-1.5)
  hiddenWeight <- admittedWeight (-2)
  firstSite <- admittedSite "first" (Point 0 0) zero
  secondSite <- admittedSite "second" (Point 2 0) zero
  thirdSite <- admittedSite "third" (Point 0 2) zero
  lowerSite <- admittedSite "center" (Point 0.5 0.5) lowerWeight
  hiddenSite <- admittedSite "center" (Point 0.5 0.5) hiddenWeight
  let lowerSites = firstSite :| [secondSite, thirdSite, lowerSite]
      hiddenSites = firstSite :| [secondSite, thirdSite, hiddenSite]
  (lowerRegular, _) <-
    requireRight "coplanar regular topology" (regularTriangulation lowerSites)
  assertEqual
    "coplanar interior generator remains lower-dimensional"
    (Just RegularSiteLowerDimensional)
    (regularSiteDisposition "center" lowerRegular)
  (lowerDiagram, lowerReceipt) <-
    requireRight "bounded lower-dimensional power cell" (boundedPowerDiagram domain lowerSites)
  case powerCellDisposition "center" lowerDiagram of
    Just (LowerDimensionalPowerCell _) -> pure ()
    other -> fail ("expected lower-dimensional center cell, got " <> dispositionTag other)
  assertEqual "one lower-dimensional oracle cell" 1 (powerDiagramOracleCells lowerReceipt)
  (hiddenRegular, _) <-
    requireRight "hidden regular topology" (regularTriangulation hiddenSites)
  assertEqual
    "strictly interior lifted generator is hidden"
    (Just RegularSiteHidden)
    (regularSiteDisposition "center" hiddenRegular)
  (hiddenDiagram, hiddenReceipt) <-
    requireRight "bounded hidden power cell" (boundedPowerDiagram domain hiddenSites)
  assertEqual "hidden generator has empty cell" (Just EmptyPowerCell) (powerCellDisposition "center" hiddenDiagram)
  assertEqual "hidden generator needs no HPI oracle" 0 (powerDiagramOracleCells hiddenReceipt)
  unless
    ( powerDiagramMaximumCellConstraints hiddenReceipt <= powerDiagramRegularEdges hiddenReceipt
    )
    (fail "power construction retained more axes than the regular graph")

testRegularSiteOwnership :: IO ()
testRegularSiteOwnership = do
  zero <- admittedWeight 0
  sites <-
    traverse
      (\(label, point) -> admittedSite label point zero)
      ( ("c", Point 0 2)
          :| [("a", Point 0 0), ("b", Point 2 0)]
      )
  (regular, receipt) <-
    requireRight "site-owning regular topology" (regularTriangulation sites)
  assertEqual "regular site count" 3 (regularSiteCount regular)
  assertEqual
    "regular sites are the canonical ascending section"
    ["a", "b", "c"]
    (fmap powerSiteLabel (regularSites regular))
  traverse_
    (\site ->
       assertEqual
         ("regular site lookup for " <> powerSiteLabel site)
         (Just site)
         (regularSite (powerSiteLabel site) regular))
    sites
  case NonEmpty.nonEmpty (regularSites regular) of
    Nothing -> fail "nonempty regular topology lost its canonical site section"
    Just retainedSites -> do
      (reconstructed, _) <-
        requireRight "regular reconstruction from owned sites" (regularTriangulation retainedSites)
      assertEqual "owned sites reconstruct the same semantic value" regular reconstructed
  assertEqual "receipt counts every owned site" 3 (regularTriangulationInputSites receipt)
  assertEqual "empty regular site count" 0 (regularSiteCount emptyRegularTriangulation)
  assertEqual
    "empty regular site section"
    ([] :: [PowerSite String])
    (regularSites emptyRegularTriangulation)

testHiddenInsertionAndExposure :: IO ()
testHiddenInsertionAndExposure = do
  zero <- admittedWeight 0
  hiddenWeight <- admittedWeight (-2)
  first <- admittedSite "first" (Point 0 0) zero
  second <- admittedSite "second" (Point 2 0) zero
  third <- admittedSite "third" (Point 0 2) zero
  hidden <- admittedSite "hidden" (Point 0.5 0.5) hiddenWeight
  (initial, _) <-
    requireRight
      "regular hidden-insertion base"
      (regularTriangulation (first :| [second, third]))
  inserted <-
    requireRight "insert hidden regular site" (insertRegularSite hidden initial)
  assertEqual
    "hidden insertion is successful resident publication"
    (Just hidden)
    (regularSite "hidden" (regularEditTriangulation inserted))
  assertEqual
    "hidden insertion disposition"
    [RegularSiteAppeared "hidden" RegularSiteHidden]
    (editTransitions inserted)
  removed <-
    requireRight
      "remove face-defining site"
      (removeRegularSite "first" (regularEditTriangulation inserted))
  assertEqual
    "removal re-exposes a hidden resident"
    (Just RegularSiteVisible)
    (regularSiteDisposition "hidden" (regularEditTriangulation removed))
  assertEqual
    "hidden exposure receipt"
    [ RegularSiteDisappeared "first" RegularSiteVisible
    , RegularSiteTransitioned "hidden" RegularSiteHidden RegularSiteVisible
    ]
    (editTransitions removed)

testRegularInsertionAndRemovalTransitions :: IO ()
testRegularInsertionAndRemovalTransitions = do
  zero <- admittedWeight 0
  dominantWeight <- admittedWeight 1
  resident <- admittedSite "a-resident" (Point 0 0) zero
  dominant <- admittedSite "z-dominant" (Point 0 0) dominantWeight
  (initial, _) <-
    requireRight "regular insertion base" (regularTriangulation (resident :| []))
  inserted <-
    requireRight "dominant regular insertion" (insertRegularSite dominant initial)
  assertEqual
    "insert changed-site support"
    (Set.singleton "z-dominant")
    (regularEditChangedSites inserted)
  assertEqual
    "dominant insertion transitions"
    [ RegularSiteTransitioned
        "a-resident"
        RegularSiteVisible
        (RegularSiteCoincidentDominatedBy "z-dominant")
    , RegularSiteAppeared "z-dominant" RegularSiteVisible
    ]
    (editTransitions inserted)
  removed <-
    requireRight
      "dominant regular removal"
      (removeRegularSite "z-dominant" (regularEditTriangulation inserted))
  assertEqual
    "remove changed-site support"
    (Set.singleton "z-dominant")
    (regularEditChangedSites removed)
  assertEqual
    "dominant removal re-exposes resident"
    [ RegularSiteTransitioned
        "a-resident"
        (RegularSiteCoincidentDominatedBy "z-dominant")
        RegularSiteVisible
    , RegularSiteDisappeared "z-dominant" RegularSiteVisible
    ]
    (editTransitions removed)
  assertEqual
    "insert then remove returns the canonical site value"
    initial
    (regularEditTriangulation removed)

testRegularReweightTransitions :: IO ()
testRegularReweightTransitions = do
  dominantWeight <- admittedWeight 1
  subordinateWeight <- admittedWeight 0
  promotedWeight <- admittedWeight 2
  first <- admittedSite "a" (Point 0 0) dominantWeight
  second <- admittedSite "b" (Point 0 0) subordinateWeight
  (initial, _) <-
    requireRight "regular reweight base" (regularTriangulation (first :| [second]))
  reweighted <-
    requireRight
      "regular batch reweight"
      (reweightRegularSites (Map.singleton "b" promotedWeight) initial)
  assertEqual
    "reweight changed-site support"
    (Set.singleton "b")
    (regularEditChangedSites reweighted)
  assertEqual
    "reweight transitions preserve identities"
    [ RegularSiteTransitioned
        "a"
        RegularSiteVisible
        (RegularSiteCoincidentDominatedBy "b")
    , RegularSiteTransitioned
        "b"
        (RegularSiteCoincidentDominatedBy "a")
        RegularSiteVisible
    ]
    (editTransitions reweighted)
  let revised = regularEditTriangulation reweighted
  assertEqual
    "reweight preserves the site position"
    (Just (Point 0 0))
    (powerSitePosition <$> regularSite "b" revised)
  assertEqual
    "reweight replaces only the requested weight"
    (Just promotedWeight)
    (powerSiteWeight <$> regularSite "b" revised)

testTopologyPreservingEdits :: IO ()
testTopologyPreservingEdits = do
  domain <- squareDomain
  zero <- admittedWeight 0
  one <- admittedWeight 1
  hiddenWeight <- admittedWeight (-2)
  lowerHiddenWeight <- admittedWeight (-3)

  representative <- admittedSite "a" (Point 0 0) one
  subordinate <- admittedSite "b" (Point 0 0) zero
  (coincidentBase, _) <-
    requireRight "coincident edit base" (regularTriangulation (representative :| []))
  inserted <-
    requireRight
      "canonical coincident insertion"
      (insertRegularSite subordinate coincidentBase)
  assertEqual
    "coincident subordinate insertion transition"
    [RegularSiteAppeared "b" (RegularSiteCoincidentDominatedBy "a")]
    (editTransitions inserted)
  assertRegularReconstructs domain "canonical coincident insertion" (regularEditTriangulation inserted)

  reweighted <-
    requireRight
      "topology-preserving coincident reweight"
      (reweightRegularSites (Map.singleton "b" one) (regularEditTriangulation inserted))
  assertEqual
    "coincident subordinate reweight transition"
    [ RegularSiteTransitioned
        "b"
        (RegularSiteCoincidentDominatedBy "a")
        (RegularSiteCoincidentEquivalentTo "a")
    ]
    (editTransitions reweighted)
  assertRegularReconstructs domain "coincident reweight" (regularEditTriangulation reweighted)

  removedSubordinate <-
    requireRight
      "topology-preserving coincident removal"
      (removeRegularSite "b" (regularEditTriangulation reweighted))
  assertEqual
    "coincident subordinate removal transition"
    [RegularSiteDisappeared "b" (RegularSiteCoincidentEquivalentTo "a")]
    (editTransitions removedSubordinate)
  assertRegularReconstructs domain "coincident removal" (regularEditTriangulation removedSubordinate)

  first <- admittedSite "first" (Point 0 0) zero
  second <- admittedSite "second" (Point 2 0) zero
  third <- admittedSite "third" (Point 0 2) zero
  hidden <- admittedSite "hidden" (Point 0.5 0.5) hiddenWeight
  secondHidden <- admittedSite "hidden-second" (Point 0.75 0.5) hiddenWeight
  (hiddenBase, _) <-
    requireRight
      "hidden edit base"
      (regularTriangulation (first :| [second, third, hidden, secondHidden]))
  lowered <-
    requireRight
      "topology-preserving hidden reweight"
      ( reweightRegularSites
          (Map.fromList [("hidden", lowerHiddenWeight), ("hidden-second", lowerHiddenWeight)])
          hiddenBase
      )
  assertEqual
    "hidden batch changed-site support"
    (Set.fromList ["hidden", "hidden-second"])
    (regularEditChangedSites lowered)
  assertEqual "hidden downward reweight has no visibility transition" [] (editTransitions lowered)
  assertRegularReconstructs domain "hidden downward reweight" (regularEditTriangulation lowered)

  removedHidden <-
    requireRight
      "topology-preserving hidden removal"
      (removeRegularSite "hidden" (regularEditTriangulation lowered))
  assertEqual
    "hidden removal transition"
    [RegularSiteDisappeared "hidden" RegularSiteHidden]
    (editTransitions removedHidden)
  assertRegularReconstructs domain "hidden removal" (regularEditTriangulation removedHidden)

assertRegularReconstructs
  :: (Ord label, Show label)
  => ConvexPolygon
  -> String
  -> RegularTriangulation label
  -> IO ()
assertRegularReconstructs domain name edited =
  case NonEmpty.nonEmpty (regularSites edited) of
    Nothing -> assertEqual (name <> " remains empty") 0 (regularSiteCount edited)
    Just sites -> do
      (rebuilt, _) <-
        requireRight (name <> " reconstruction") (regularTriangulation sites)
      let labels = fmap powerSiteLabel (NonEmpty.toList sites)
      assertEqual
        (name <> " receipt")
        (regularTriangulationReceipt rebuilt)
        (regularTriangulationReceipt edited)
      assertEqual
        (name <> " face labels")
        (fmap regularFaceLabels (regularFaces rebuilt))
        (fmap regularFaceLabels (regularFaces edited))
      assertEqual (name <> " faces") (regularFaces rebuilt) (regularFaces edited)
      assertEqual
        (name <> " edge labels")
        (fmap regularEdgeLabels (regularEdges rebuilt))
        (fmap regularEdgeLabels (regularEdges edited))
      assertEqual (name <> " edges") (regularEdges rebuilt) (regularEdges edited)
      traverse_
        (\label -> do
           assertEqual
             (name <> " disposition " <> show label)
             (regularSiteDisposition label rebuilt)
             (regularSiteDisposition label edited)
           assertEqual
             (name <> " neighbours " <> show label)
             (regularNeighbours label rebuilt)
             (regularNeighbours label edited))
        labels
      rebuiltDiagram <-
        requireRight (name <> " rebuilt clipping") (boundedPowerDiagramFromRegular domain rebuilt)
      editedDiagram <-
        requireRight (name <> " edited clipping") (boundedPowerDiagramFromRegular domain edited)
      assertEqual (name <> " bounded cells") rebuiltDiagram editedDiagram

testRegularEditDifferentialLaws :: IO ()
testRegularEditDifferentialLaws = do
  domain <- squareDomain
  sites <- traverse prepareDifferentialSite [0 .. 47]
  nonEmptySites <-
    maybe (fail "differential regular fixture is empty") pure (NonEmpty.nonEmpty sites)
  baseSites <-
    maybe (fail "differential insertion fixture is empty") pure
      (NonEmpty.nonEmpty (NonEmpty.init nonEmptySites))
  (base, _) <- requireRight "differential insertion base" (regularTriangulation baseSites)
  inserted <-
    requireRight
      "differential local insertion"
      (insertRegularSite (NonEmpty.last nonEmptySites) base)
  assertRegularAlphaClosed
    "differential weighted alpha"
    (regularEditTriangulation inserted)
  assertRegularReconstructs
    domain
    "differential insertion"
    (regularEditTriangulation inserted)
  removed <-
    requireRight
      "differential local removal"
      (removeRegularSite "s23" (regularEditTriangulation inserted))
  assertRegularReconstructs
    domain
    "differential removal"
    (regularEditTriangulation removed)
  raised <- admittedWeight 2
  reweighted <-
    requireRight
      "differential local reweight"
      (reweightRegularSites (Map.singleton "s17" raised) (regularEditTriangulation removed))
  assertRegularReconstructs
    domain
    "differential reweight"
    (regularEditTriangulation reweighted)

assertRegularAlphaClosed
  :: (Ord label, Show label)
  => String
  -> RegularTriangulation label
  -> IO ()
assertRegularAlphaClosed name regular = do
  filtration <- requireRight name (regularAlphaFiltration regular)
  traverse_
    (requireRight (name <> " sublevel") . (`regularAlphaComplexAtBirth` filtration))
    (Map.elems (regularAlphaBirths filtration))

prepareDifferentialSite :: Int -> IO (PowerSite String)
prepareDifferentialSite label = do
  let coordinateX = fromIntegral ((label * 37 + 11) `mod` 97) / 10
      coordinateY = fromIntegral ((label * 61 + 7) `mod` 89) / 10
      weightValue = fromIntegral ((label * 17) `mod` 13 - 6) / 64
  weight <- admittedWeight weightValue
  admittedSite ("s" <> show label) (Point coordinateX coordinateY) weight

testRegularEditObstructionsAndIdempotence :: IO ()
testRegularEditObstructionsAndIdempotence = do
  zero <- admittedWeight 0
  one <- admittedWeight 1
  site <- admittedSite "site" (Point 0 0) zero
  conflicting <- admittedSite "site" (Point 0 0) one
  inserted <-
    requireRight
      "insert into empty regular topology"
      (insertRegularSite site emptyRegularTriangulation)
  repeated <-
    requireRight
      "repeat identical regular insertion"
      (insertRegularSite site (regularEditTriangulation inserted))
  assertEqual
    "identical insertion changes no site"
    Set.empty
    (regularEditChangedSites repeated)
  assertEqual "identical insertion has no transitions" [] (editTransitions repeated)
  case insertRegularSite conflicting (regularEditTriangulation inserted) of
    Left (RegularEditSiteConflict "site" resident requested) -> do
      assertEqual "conflict retains resident" site resident
      assertEqual "conflict retains requested site" conflicting requested
    other -> fail ("expected regular edit site conflict, got " <> show other)
  case reweightRegularSites (Map.singleton "missing" zero) (regularEditTriangulation inserted) of
    Left (RegularEditUnknownSites ("missing" :| [])) -> pure ()
    other -> fail ("expected unknown reweight site, got " <> show other)
  absent <-
    requireRight
      "remove absent regular site"
      (removeRegularSite "missing" (regularEditTriangulation inserted))
  assertEqual
    "absent removal changes no site"
    Set.empty
    (regularEditChangedSites absent)
  assertEqual "absent removal has no transitions" [] (editTransitions absent)
  removed <-
    requireRight
      "remove final regular site"
      (removeRegularSite "site" (regularEditTriangulation inserted))
  assertEqual "final removal returns empty" 0 (regularSiteCount (regularEditTriangulation removed))
  assertEqual
    "final removal transition"
    [RegularSiteDisappeared "site" RegularSiteVisible]
    (editTransitions removed)

testBoundedPowerFromRegular :: IO ()
testBoundedPowerFromRegular = do
  domain <- squareDomain
  fixtures <-
    traverse
      (\(name, submitted) -> (,) name <$> traverse prepareOracleSite submitted)
      [ ( "ordinary"
        , ("a", Point 1 1, 0.25)
            :| [("b", Point 3 1, -0.125), ("c", Point 2 3, 0.5)]
        )
      , ( "lower-dimensional"
        , ("a", Point 0 0, 0)
            :| [("b", Point 2 0, 0), ("c", Point 0 2, 0), ("center", Point 0.5 0.5, -1.5)]
        )
      , ( "hidden"
        , ("a", Point 0 0, 0)
            :| [("b", Point 2 0, 0), ("c", Point 0 2, 0), ("center", Point 0.5 0.5, -2)]
        )
      , ( "coincident"
        , ("a", Point 1 1, 0)
            :| [("b", Point 1 1, 0), ("c", Point 3 1, 0)]
        )
      ]
  traverse_ (assertPreparedPowerMatches domain) fixtures
  (emptyDiagram, emptyReceipt) <-
    requireRight
      "bounded power diagram from empty regular source"
      (boundedPowerDiagramFromRegular domain (emptyRegularTriangulation :: RegularTriangulation String))
  assertEqual "empty regular source has no cell dispositions" [] (powerCellDispositions emptyDiagram)
  assertEqual "empty regular source has no input sites" 0 (powerDiagramInputSites emptyReceipt)

assertPreparedPowerMatches
  :: ConvexPolygon
  -> (String, NonEmpty (PowerSite String))
  -> IO ()
assertPreparedPowerMatches domain (name, sites) = do
  (regular, _) <-
    requireRight (name <> " regular source") (regularTriangulation sites)
  oneShot <-
    requireRight (name <> " one-shot power diagram") (boundedPowerDiagram domain sites)
  fromRegular <-
    requireRight
      (name <> " prepared power diagram")
      (boundedPowerDiagramFromRegular domain regular)
  assertEqual (name <> " prepared clipping") oneShot fromRegular

testRegularCollinearClassification :: IO ()
testRegularCollinearClassification = do
  sites <- traverse prepareCollinearSite (0 :| [1 .. 8])
  (regular, _) <-
    requireRight "collinear regular topology" (regularTriangulation sites)
  traverse_
    (\index ->
       assertEqual
         ("collinear disposition for " <> show index)
         (Just (if even index then RegularSiteVisible else RegularSiteHidden))
         (regularSiteDisposition (show index) regular))
    ([0 .. 8] :: [Int])
 where
  prepareCollinearSite :: Int -> IO (PowerSite String)
  prepareCollinearSite index = do
    weight <- admittedWeight (if odd index then -(2 / 256) else 0)
    admittedSite (show index) (Point (fromIntegral index / 16) 0) weight

testSparsePowerMatchesCompleteOracle :: IO ()
testSparsePowerMatchesCompleteOracle = do
  domain <- squareDomain
  sites <-
    traverse
      prepareOracleSite
      ( ("a", Point 1.0 1.0, 0.25)
          :| [ ("b", Point 3.0 0.8, -0.125)
             , ("c", Point 5.2 1.4, 0.375)
             , ("d", Point 8.5 1.0, 0.0)
             , ("e", Point 1.7 3.8, -0.25)
             , ("f", Point 4.1 4.4, 0.125)
             , ("g", Point 7.6 3.5, -0.375)
             , ("h", Point 9.1 5.4, 0.25)
             , ("i", Point 1.0 7.8, 0.0)
             , ("j", Point 3.6 8.9, 0.5)
             , ("k", Point 6.4 7.5, -0.125)
             , ("l", Point 8.8 9.0, 0.375)
             ]
      )
  (diagram, receipt) <-
    requireRight "sparse regular-neighbour power diagram" (boundedPowerDiagram domain sites)
  traverse_
    (\site -> do
       oracle <- completeOraclePowerCell domain sites site
       assertEqual
         ("regular-neighbour cell equals complete HPI oracle for " <> powerSiteLabel site)
         (Just oracle)
         (powerCellDisposition (powerSiteLabel site) diagram))
    sites
  let siteCount = NonEmpty.length sites
      completeConstraintCount = siteCount * (siteCount - 1)
  unless (powerDiagramSubmittedSiteConstraints receipt < completeConstraintCount) $
    fail "regular-neighbour construction did not eliminate the complete pairwise cell schedule"
  unless (powerDiagramMaximumCellConstraints receipt <= powerDiagramRegularEdges receipt) $
    fail "regular-neighbour construction retained a non-topological radical axis"

prepareOracleSite
  :: (String, Point, Double)
  -> IO (PowerSite String)
prepareOracleSite (label, point, weightValue) = do
  weight <- admittedWeight weightValue
  admittedSite label point weight

data OracleScore = OracleScore
  { oracleXCoefficient :: !ExactRational
  , oracleYCoefficient :: !ExactRational
  , oracleConstant :: !ExactRational
  }

oracleScore :: PowerSite label -> IO OracleScore
oracleScore site = do
  point <- requireRight "oracle source point" (exactPointFromPoint (powerSitePosition site))
  let (coordinateX, coordinateY) = exactPointCoordinates point
  pure
    OracleScore
      { oracleXCoefficient = 2 * coordinateX
      , oracleYCoefficient = 2 * coordinateY
      , oracleConstant =
          powerWeightExact (powerSiteWeight site)
            - coordinateX * coordinateX
            - coordinateY * coordinateY
      }

completeOraclePowerCell
  :: ConvexPolygon
  -> NonEmpty (PowerSite String)
  -> PowerSite String
  -> IO (PowerCellDisposition String)
completeOraclePowerCell domain sites owner = do
  retainedDomain <-
    requireRight "oracle retained domain" (exactRetainedPolygon (convexPolygonPoints domain))
  ownerScore <- oracleScore owner
  halfPlanes <-
    traverse
      (oracleWinningHalfPlane ownerScore)
      ( filter
          ((/= powerSiteLabel owner) . powerSiteLabel)
          (NonEmpty.toList sites)
      )
  (disposition, _) <-
    requireRight
      ("complete HPI oracle for " <> powerSiteLabel owner)
      (exactClipRetainedPolygon retainedDomain halfPlanes)
  case disposition of
    ExactClipFullDimensional retained ->
      PublishedPowerCell
        <$> requireRight
          "oracle convex publication"
          (convexPolygon (exactRetainedPolygonPoints retained))
    ExactClipLowerDimensional points ->
      pure (LowerDimensionalPowerCell points)
    ExactClipEmpty -> pure EmptyPowerCell

oracleWinningHalfPlane
  :: OracleScore
  -> PowerSite label
  -> IO ExactClosedHalfPlane
oracleWinningHalfPlane owner competitor = do
  competitorScore <- oracleScore competitor
  line <-
    requireRight
      "oracle radical axis"
      ( exactAffineLine
          (oracleXCoefficient owner - oracleXCoefficient competitorScore)
          (oracleYCoefficient owner - oracleYCoefficient competitorScore)
          (oracleConstant owner - oracleConstant competitorScore)
      )
  pure (exactClosedHalfPlane line)

assertBoundaryDualRay
  :: NonEmpty (PowerSite String)
  -> ExactPoint
  -> RegularEdge String
  -> IO ()
assertBoundaryDualRay sites expectedOrigin edge =
  case regularEdgeDual edge of
    UnboundedPowerDual ray -> do
      assertEqual "regular ray starts at incident face dual" expectedOrigin (exactRayOrigin ray)
      let ExactVector directionX directionY = exactRayDirection ray
      unless (directionX /= 0 || directionY /= 0) $
        fail ("regular edge has a zero dual-ray direction: " <> show (regularEdgeLabels edge))
      let sample = translateExactPoint expectedOrigin (exactRayDirection ray)
          (firstLabel, secondLabel) = regularEdgeLabels edge
      firstSite <- requireSite firstLabel sites
      secondSite <- requireSite secondLabel sites
      firstDistance <- powerDistance sample firstSite
      secondDistance <- powerDistance sample secondSite
      assertEqual "regular ray remains on its radical axis" firstDistance secondDistance
      traverse_
        (\competitor -> do
           competitorDistance <- powerDistance sample competitor
           unless (firstDistance <= competitorDistance) $
             fail ("regular ray points outside the common winning cone: " <> show (firstLabel, secondLabel)))
        sites
    other -> fail ("regular triangle boundary expected a ray, got " <> show other)

requireSite :: Eq label => label -> NonEmpty (PowerSite label) -> IO (PowerSite label)
requireSite label sites =
  case List.find ((== label) . powerSiteLabel) (NonEmpty.toList sites) of
    Nothing -> fail "regular topology references a missing source site"
    Just site -> pure site

testEqualWeightsAndCommonShift :: IO ()
testEqualWeightsAndCommonShift = do
  domain <- squareDomain
  zero <- admittedWeight 0
  shifted <- admittedWeight 7
  zeroSites <-
    ( :| )
      <$> admittedSite "left" (Point 2 5) zero
      <*> traverse (uncurry3 admittedSite) [("right", Point 8 5, zero)]
  shiftedSites <-
    ( :| )
      <$> admittedSite "left" (Point 2 5) shifted
      <*> traverse (uncurry3 admittedSite) [("right", Point 8 5, shifted)]
  (diagram, receipt) <- requireRight "equal-weight power diagram" (boundedPowerDiagram domain zeroSites)
  (shiftedDiagram, _) <- requireRight "common-shift power diagram" (boundedPowerDiagram domain shiftedSites)
  assertEqual "common additive power shift" (powerCellDispositions diagram) (powerCellDispositions shiftedDiagram)
  assertPermutationInvariant "equal-weight power diagram" domain zeroSites diagram
  assertPermutationInvariant "common-shift power diagram" domain shiftedSites shiftedDiagram
  expectedLeft <-
    requireRight
      "expected left power cell"
      (convexPolygon (integerPoint 0 0 :| [integerPoint 5 0, integerPoint 5 10, integerPoint 0 10]))
  expectedRight <-
    requireRight
      "expected right power cell"
      (convexPolygon (integerPoint 5 0 :| [integerPoint 10 0, integerPoint 10 10, integerPoint 5 10]))
  assertEqual "equal-weight left bisector cell" (Just (PublishedPowerCell expectedLeft)) (powerCellDisposition "left" diagram)
  assertEqual "equal-weight right bisector cell" (Just (PublishedPowerCell expectedRight)) (powerCellDisposition "right" diagram)
  assertEqual "receipt input sites" 2 (powerDiagramInputSites receipt)
  assertEqual
    "one submitted site constraint per cell"
    2
    (powerDiagramSubmittedSiteConstraints receipt)
  assertEqual
    "parallel domain and site boundaries coalesce"
    8
    (powerDiagramActiveBoundaries receipt)
  assertEqual "receipt published cells" 2 (powerDiagramPublishedCells receipt)
  assertEqual "receipt empty cells" 0 (powerDiagramEmptyCells receipt)
  traverse_ (assertPublishedVerticesWin zeroSites) (powerCellDispositions diagram)
  let layer = powerDiagramPlanarLayer diagram
  assertEqual "derived layer contains both cells" [Just "left", Just "right"] (Map.keys (planarLayerRegions layer))

testThreeSiteExactPartitionCoverage :: IO ()
testThreeSiteExactPartitionCoverage = do
  domain <- squareDomain
  weight <- admittedWeight 0
  left <- admittedSite "left" (Point 2 5) weight
  middle <- admittedSite "middle" (Point 5 5) weight
  right <- admittedSite "right" (Point 8 5) weight
  let sites = left :| [middle, right]
  (diagram, receipt) <-
    requireRight "three-site exact partition" (boundedPowerDiagram domain sites)
  assertEqual "three-site partition publishes every cell" 3 (powerDiagramPublishedCells receipt)
  assertEqual
    "three-site exact cell areas cover the square"
    200
    (publishedTwiceAreaSum diagram)
  traverse_
    (\permutation -> do
       (permuted, _) <-
         requireRight
           "permuted three-site exact partition"
           (boundedPowerDiagram domain permutation)
       assertEqual
         "three-site partition is permutation invariant"
         (powerCellDispositions diagram)
         (powerCellDispositions permuted)
       assertEqual
         "permuted exact cell areas cover the square"
         200
         (publishedTwiceAreaSum permuted))
    (nonEmptyPermutations sites)

nonEmptyPermutations :: NonEmpty value -> [NonEmpty value]
nonEmptyPermutations =
  foldMap (maybe [] pure . NonEmpty.nonEmpty)
    . List.permutations
    . NonEmpty.toList

assertPermutationInvariant
  :: String
  -> ConvexPolygon
  -> NonEmpty (PowerSite String)
  -> BoundedPowerDiagram String
  -> IO ()
assertPermutationInvariant label domain sites expected =
  traverse_
    (\permutation -> do
       (permuted, _) <-
         requireRight
           (label <> " permutation")
           (boundedPowerDiagram domain permutation)
       assertEqual
         (label <> " is permutation invariant")
         (powerCellDispositions expected)
         (powerCellDispositions permuted))
    (nonEmptyPermutations sites)

publishedTwiceAreaSum :: BoundedPowerDiagram label -> ExactRational
publishedTwiceAreaSum =
  List.foldl'
    (\total (_, disposition) -> case disposition of
        PublishedPowerCell polygon -> total + exactPolygonTwiceArea (convexPolygonPoints polygon)
        LowerDimensionalPowerCell _ -> total
        EmptyPowerCell -> total
        CoincidentEquivalentTo _ -> total
        CoincidentDominatedBy _ -> total)
    0
    . powerCellDispositions

exactPolygonTwiceArea :: NonEmpty ExactPoint -> ExactRational
exactPolygonTwiceArea points =
  abs
    ( List.foldl'
        (\twiceArea (firstPoint, secondPoint) ->
           let (firstX, firstY) = exactPointCoordinates firstPoint
               (secondX, secondY) = exactPointCoordinates secondPoint
            in twiceArea + firstX * secondY - firstY * secondX)
        0
        (cyclicPairs points)
    )

cyclicPairs :: NonEmpty value -> [(value, value)]
cyclicPairs (firstValue :| remainingValues) =
  zip
    (firstValue : remainingValues)
    (remainingValues <> [firstValue])

testEqualCoincidentResolution :: IO ()
testEqualCoincidentResolution = do
  domain <- squareDomain
  weight <- admittedWeight 3
  a <- admittedSite "a" (Point 4 4) weight
  b <- admittedSite "b" (Point 4 4) weight
  (diagram, receipt) <- requireRight "equivalent coincident power sites" (boundedPowerDiagram domain (b :| [a]))
  assertPublished "coincident canonical representative" (powerCellDisposition "a" diagram)
  assertEqual "equal-function coincident alias" (Just (CoincidentEquivalentTo "a")) (powerCellDisposition "b" diagram)
  assertEqual "equivalent coincidence receipt" 1 (powerDiagramCoincidentEquivalentCells receipt)
  assertEqual "equivalent coincidence is not dominance" 0 (powerDiagramCoincidentDominatedCells receipt)
  (permuted, _) <- requireRight "permuted equivalent sites" (boundedPowerDiagram domain (a :| [b]))
  assertEqual "equivalent resolution permutation invariance" (powerCellDispositions diagram) (powerCellDispositions permuted)

testDominantCoincidentResolution :: IO ()
testDominantCoincidentResolution = do
  domain <- squareDomain
  high <- admittedWeight 3
  low <- admittedWeight 2
  winner <- admittedSite "winner" (Point 4 4) high
  dominated <- admittedSite "dominated" (Point 4 4) low
  (diagram, receipt) <- requireRight "dominant coincident power sites" (boundedPowerDiagram domain (dominated :| [winner]))
  assertPublished "dominant coincident representative" (powerCellDisposition "winner" diagram)
  assertEqual "lower-weight coincident site" (Just (CoincidentDominatedBy "winner")) (powerCellDisposition "dominated" diagram)
  assertEqual "dominant coincidence receipt" 1 (powerDiagramCoincidentDominatedCells receipt)
  assertEqual "dominance is not equivalence" 0 (powerDiagramCoincidentEquivalentCells receipt)
  assertPermutationInvariant "dominant coincident power sites" domain (dominated :| [winner]) diagram

testLowerDimensionalCell :: IO ()
testLowerDimensionalCell = do
  domain <- squareDomain
  zero <- admittedWeight 0
  suppressed <- admittedWeight (-1)
  left <- admittedSite "left" (Point 0 5) zero
  right <- admittedSite "right" (Point 2 5) zero
  middle <- admittedSite "middle" (Point 1 5) suppressed
  let sites = left :| [right, middle]
  (diagram, receipt) <- requireRight "one-dimensional bounded power cell" (boundedPowerDiagram domain sites)
  case powerCellDisposition "middle" diagram of
    Just (LowerDimensionalPowerCell points) ->
      assertEqual
        "middle cell is the exact x=1 segment"
        (List.sort [integerPoint 1 0, integerPoint 1 10])
        (List.sort (NonEmpty.toList points))
    other -> fail ("one-dimensional power cell: expected retained segment, got " <> dispositionTag other)
  assertEqual "lower-dimensional receipt" 1 (powerDiagramLowerDimensionalCells receipt)
  assertEqual "lower-dimensional cell is not empty" 0 (powerDiagramEmptyCells receipt)
  assertEqual "two full-dimensional neighbours" 2 (powerDiagramPublishedCells receipt)
  traverse_ (assertPublishedVerticesWin sites) (powerCellDispositions diagram)
  let layer = powerDiagramPlanarLayer diagram
  assertEqual "derived layer omits the one-dimensional cell" [Just "left", Just "right"] (Map.keys (planarLayerRegions layer))
  assertPermutationInvariant "one-dimensional bounded power cell" domain sites diagram

testDistinctEmptyCell :: IO ()
testDistinctEmptyCell = do
  domain <- squareDomain
  ordinary <- admittedWeight 0
  suppressed <- admittedWeight (-1000)
  winner <- admittedSite "winner" (Point 0 0) ordinary
  hidden <- admittedSite "hidden" (Point 5 5) suppressed
  (diagram, receipt) <- requireRight "distinct empty power cell" (boundedPowerDiagram domain (winner :| [hidden]))
  assertPublished "dominant distinct site" (powerCellDisposition "winner" diagram)
  assertEqual "distinct site may have empty bounded cell" (Just EmptyPowerCell) (powerCellDisposition "hidden" diagram)
  assertEqual "empty disposition receipt" 1 (powerDiagramEmptyCells receipt)
  assertPermutationInvariant "distinct empty power cell" domain (winner :| [hidden]) diagram

testDyadicNearParallelBisectors :: IO ()
testDyadicNearParallelBisectors = do
  domain <- squareDomain
  weight <- admittedWeight 0
  let epsilon = 2 ** (-20) :: Double
  origin <- admittedSite "origin" (Point 0 0) weight
  horizontal <- admittedSite "horizontal" (Point 2 0) weight
  tilted <- admittedSite "tilted" (Point 2 epsilon) weight
  let sites = origin :| [horizontal, tilted]
  (diagram, receipt) <-
    requireRight
      "dyadic near-parallel power bisectors"
      (boundedPowerDiagram domain sites)
  assertEqual "near-parallel cells retain full dimension" 3 (powerDiagramPublishedCells receipt)
  traverse_ (assertPublishedVerticesWin sites) (powerCellDispositions diagram)
  assertPermutationInvariant "dyadic near-parallel power bisectors" domain sites diagram

testBinary64PrecisionSites :: IO ()
testBinary64PrecisionSites = do
  domain <- squareDomain
  firstWeight <- admittedWeight 0.2
  secondWeight <- admittedWeight (-0.3)
  thirdWeight <- admittedWeight 0.7
  firstSite <- admittedSite "first" (Point 0.1 0.3) firstWeight
  secondSite <- admittedSite "second" (Point 9.7 0.2) secondWeight
  thirdSite <- admittedSite "third" (Point 4.9 9.6) thirdWeight
  let sites = firstSite :| [secondSite, thirdSite]
  (diagram, receipt) <-
    requireRight
      "exact binary64 power sites"
      (boundedPowerDiagram domain sites)
  assertEqual "binary64 fixture publishes every cell" 3 (powerDiagramPublishedCells receipt)
  traverse_ (assertPublishedVerticesWin sites) (powerCellDispositions diagram)
  assertPermutationInvariant "exact binary64 power sites" domain sites diagram

testDuplicateLabels :: IO ()
testDuplicateLabels = do
  domain <- squareDomain
  weight <- admittedWeight 0
  firstSite <- admittedSite "duplicate" (Point 1 1) weight
  secondSite <- admittedSite "duplicate" (Point 9 9) weight
  case boundedPowerDiagram domain (firstSite :| [secondSite]) of
    Left (DuplicatePowerSiteLabel "duplicate") -> pure ()
    other -> fail ("duplicate power-site label: expected typed refusal, got " <> show other)

testNonFiniteWeight :: IO ()
testNonFiniteWeight =
  case powerWeight (0 / 0) of
    Left (PowerWeightNonFinite _) -> pure ()
    other -> fail ("non-finite power weight: expected typed refusal, got " <> show other)

editTransitions
  :: RegularEditResult label
  -> [RegularSiteTransition label]
editTransitions =
  Vector.toList . regularEditTransitions

squareDomain :: IO ConvexPolygon
squareDomain =
  requireRight
    "square clipping domain"
    (convexPolygon (integerPoint 0 0 :| [integerPoint 10 0, integerPoint 10 10, integerPoint 0 10]))

admittedWeight :: Double -> IO PowerWeight
admittedWeight value =
  requireRight
    "finite power weight"
    (powerWeight value)

admittedSite :: String -> Point -> PowerWeight -> IO (PowerSite String)
admittedSite label point weight = requireRight "admitted power site" (powerSite label point weight)

uncurry3 :: (a -> b -> c -> result) -> (a, b, c) -> result
uncurry3 function (firstValue, secondValue, thirdValue) = function firstValue secondValue thirdValue

assertPublished :: String -> Maybe (PowerCellDisposition label) -> IO ()
assertPublished _ (Just (PublishedPowerCell _)) = pure ()
assertPublished label other = fail (label <> ": expected published cell, got " <> dispositionTag other)

dispositionTag :: Maybe (PowerCellDisposition label) -> String
dispositionTag Nothing = "missing"
dispositionTag (Just (PublishedPowerCell _)) = "published"
dispositionTag (Just (LowerDimensionalPowerCell _)) = "lower-dimensional"
dispositionTag (Just EmptyPowerCell) = "empty"
dispositionTag (Just (CoincidentEquivalentTo _)) = "coincident-equivalent"
dispositionTag (Just (CoincidentDominatedBy _)) = "coincident-dominated"

requireSome :: String -> Maybe value -> IO value
requireSome label value =
  case value of
    Just present -> pure present
    Nothing -> fail (label <> ": missing value")

assertPublishedVerticesWin
  :: NonEmpty (PowerSite String)
  -> (String, PowerCellDisposition String)
  -> IO ()
assertPublishedVerticesWin sites (ownerLabel, disposition) =
  case disposition of
    PublishedPowerCell polygon ->
      assertCellPointsWin ownerLabel sites (convexPolygonPoints polygon)
    LowerDimensionalPowerCell points ->
      assertCellPointsWin ownerLabel sites points
    EmptyPowerCell -> pure ()
    CoincidentEquivalentTo _ -> pure ()
    CoincidentDominatedBy _ -> pure ()

assertCellPointsWin
  :: String
  -> NonEmpty (PowerSite String)
  -> NonEmpty ExactPoint
  -> IO ()
assertCellPointsWin ownerLabel sites points =
  case lookupOwner ownerLabel (NonEmpty.toList sites) of
    Nothing -> fail ("power-cell owner missing: " <> ownerLabel)
    Just owner ->
      traverse_
        (\point -> traverse_ (assertOwnerWinsAt point owner) sites)
        points

lookupOwner :: Eq label => label -> [PowerSite label] -> Maybe (PowerSite label)
lookupOwner label = List.find ((== label) . powerSiteLabel)

assertOwnerWinsAt
  :: ExactPoint
  -> PowerSite String
  -> PowerSite String
  -> IO ()
assertOwnerWinsAt point owner competitor = do
  ownerValue <- powerDistance point owner
  competitorValue <- powerDistance point competitor
  unless (ownerValue <= competitorValue) $
    fail
      ( "power-cell vertex violates source inequality: "
          <> show (powerSiteLabel owner, powerSiteLabel competitor, ownerValue, competitorValue)
      )

powerDistance :: ExactPoint -> PowerSite label -> IO ExactRational
powerDistance point site = do
  exactSite <- requireRight "site exact position" (exactPointFromPoint (powerSitePosition site))
  let (x, y) = exactPointCoordinates point
      (siteX, siteY) = exactPointCoordinates exactSite
      deltaX = x - siteX
      deltaY = y - siteY
  pure (deltaX * deltaX + deltaY * deltaY - powerWeightExact (powerSiteWeight site))
