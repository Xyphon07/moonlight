{-# LANGUAGE BangPatterns #-}

-- | Exact bounded power-cell laws against a complete half-plane oracle.
module Moonlight.Planar.PowerDiagramSpec
  ( tests
  ) where

import Control.Monad ( unless )
import Data.Foldable ( traverse_ )
import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.Convex ( ConvexPolygon, convexPolygon, convexPolygonPoints )
import Moonlight.Planar.Exact ( ExactPoint, exactPointCoordinates, exactPointFromPoint,
  ExactClipDisposition(..), ExactClosedHalfPlane, exactAffineLine, exactClipRetainedPolygon,
  exactClosedHalfPlane, exactRetainedPolygon, exactRetainedPolygonPoints )
import Moonlight.Planar.Exact (ExactRational)
import Moonlight.Planar.PowerDiagram ( powerDiagramInputSites, powerSitePosition, powerWeight,
  powerWeightExact, boundedPowerDiagram, boundedPowerDiagramFromRegular, powerCellDisposition,
  powerCellDispositions, powerDiagramPlanarLayer, emptyRegularTriangulation, regularTriangulation,
  BoundedPowerDiagram, PowerCellDisposition(..), PowerDiagramError(DuplicatePowerSiteLabel),
  PowerDiagramReceipt(powerDiagramPublishedCells, powerDiagramMaximumCellConstraints,
  powerDiagramRegularEdges, powerDiagramSubmittedSiteConstraints, powerDiagramActiveBoundaries,
  powerDiagramCoincidentDominatedCells, powerDiagramCoincidentEquivalentCells,
  powerDiagramLowerDimensionalCells, powerDiagramEmptyCells), PowerSite(..),
  PowerWeightError(PowerWeightNonFinite), RegularTriangulation )
import Moonlight.Planar.PowerFixtures ( admittedSite, admittedWeight, admittedWeightedSite,
  squareDomain, dispositionTag, powerDistance )
import Moonlight.Planar.Region ( planarLayerRegions )
import Moonlight.Planar.Point (Point(..))
import Support ( assertEqual, integerPoint, requireRight )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty


tests :: IO ()
tests =
  sequence_
    [ testEqualWeightsAndCommonShift
    , testThreeSiteExactPartitionCoverage
    , testBoundedPowerFromRegular
    , testSparsePowerMatchesCompleteOracle
    , testEqualCoincidentResolution
    , testDominantCoincidentResolution
    , testLowerDimensionalCell
    , testDistinctEmptyCell
    , testDyadicNearParallelBisectors
    , testBinary64PrecisionSites
    , testDuplicateLabels
    , testNonFiniteWeight
    ]

testBoundedPowerFromRegular :: IO ()
testBoundedPowerFromRegular = do
  domain <- squareDomain
  fixtures <-
    traverse
      (\(name, submitted) -> (,) name <$> traverse admittedWeightedSite submitted)
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

testSparsePowerMatchesCompleteOracle :: IO ()
testSparsePowerMatchesCompleteOracle = do
  domain <- squareDomain
  sites <-
    traverse
      admittedWeightedSite
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

uncurry3 :: (a -> b -> c -> result) -> (a, b, c) -> result
uncurry3 function (firstValue, secondValue, thirdValue) = function firstValue secondValue thirdValue

assertPublished :: String -> Maybe (PowerCellDisposition label) -> IO ()
assertPublished _ (Just (PublishedPowerCell _)) = pure ()
assertPublished label other = fail (label <> ": expected published cell, got " <> dispositionTag other)

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
