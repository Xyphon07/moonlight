-- | Regular topology, dual rays, and visible/hidden site classification laws.
module Moonlight.Planar.RegularTopologySpec
  ( tests
  ) where

import Control.Monad ( unless )
import Data.Foldable ( traverse_ )
import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.Exact ( ExactPoint, ExactVector(..), exactRayDirection, exactRayOrigin,
  exactSegmentEndpoints, translateExactPoint )
import Moonlight.Planar.PowerDiagram ( regularEdgeDual, regularEdgeLabels, regularFaceDualPoint,
  regularFaceLabels, boundedPowerDiagram, powerCellDisposition, emptyRegularTriangulation,
  regularEdges, regularFaces, regularNeighbours, regularSite, regularSiteCount,
  regularSiteDisposition, regularSites, regularTriangulation, PowerDualEdge(UnboundedPowerDual,
  BoundedPowerDual, CollapsedPowerDual), PowerCellDisposition(EmptyPowerCell,
  LowerDimensionalPowerCell), PowerDiagramReceipt(powerDiagramRegularEdges, powerDiagramOracleCells,
  powerDiagramMaximumCellConstraints), PowerSite(powerSiteLabel), RegularEdge,
  RegularSiteDisposition(RegularSiteHidden, RegularSiteLowerDimensional, RegularSiteVisible),
  RegularTriangulationReceipt(regularTriangulationInputSites, regularTriangulationEdges,
  regularTriangulationVisibleSites, regularTriangulationFaces) )
import Moonlight.Planar.PowerFixtures ( admittedSite, admittedWeight, admittedWeightedSite,
  squareDomain, dispositionTag, powerDistance )
import Moonlight.Planar.Point (Point(..))
import Support ( assertEqual, requireRight )
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set


tests :: IO ()
tests =
  sequence_
    [ testRegularTriangleDualRays
    , testRegularBoundedDualSegment
    , testRegularCoplanarUpperFacet
    , testRegularCollinearClassification
    , testRegularLowerDimensionalAndHiddenSites
    , testRegularSiteOwnership
    ]

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
    [segment] -> do
      let (firstEndpoint, secondEndpoint) = exactSegmentEndpoints segment
      unless
        (Set.member firstEndpoint faceDuals && Set.member secondEndpoint faceDuals)
        (fail "bounded regular dual does not join its two incident face duals")
      assertEqual "bounded regular dual has canonical ascending endpoints" LT
        (compare firstEndpoint secondEndpoint)
    segments ->
      fail ("four-site topology expected one bounded dual, got " <> show (length segments))

testRegularCoplanarUpperFacet :: IO ()
testRegularCoplanarUpperFacet = do
  sites <-
    traverse
      admittedWeightedSite
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
  let collapsed =
        [ point
        | edge <- regularEdges regular
        , CollapsedPowerDual point <- [regularEdgeDual edge]
        ]
      faceDuals = Set.fromList (fmap regularFaceDualPoint (regularFaces regular))
  assertEqual "coplanar diagonal is published as one collapsed dual" 1 (length collapsed)
  assertEqual "collapsed dual coincides with every incident face dual" faceDuals (Set.fromList collapsed)
  traverse_
    (\face ->
       let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
        in assertEqual "regular face rotation begins at least label" firstLabel
             (min firstLabel (min secondLabel thirdLabel)))
    (regularFaces regular)

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
