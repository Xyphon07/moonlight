-- | Preparation and repeated observation are separate measured workloads.
-- This inspectable three-site fixture is a microbenchmark, not a production
-- speed claim. Reconstruction shares the canonical power-site encoding.
module Moonlight.Planar.PowerMassBench (prepareBenchmarks) where

import BenchMeasure (requireRight)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Moonlight.Planar.Convex (convexPolygon, convexPolygonPoints, retainConvexPolygon)
import Moonlight.Planar.Exact (ExactPoint, ExactRational, exactPoint, exactPointCross, exactRational)
import Moonlight.Planar.PowerDiagram
  ( PowerCellDisposition (..)
  , PowerDiagramError
  , PowerMassError
  , PowerSite
  , boundedPowerDiagram
  , evaluatePowerMasses
  , powerCellDispositions
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerWeightFromExact
  , preparePowerMassSection
  )
import Moonlight.Planar.Point (Point (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

prepareBenchmarks :: IO [Benchmark]
prepareBenchmarks = do
  window <- requireRight
    (convexPolygon (exactPoint (-2) (-2) :| [exactPoint 2 (-2), exactPoint 2 2, exactPoint (-2) 2]))
  sites <- requireRight
    (traverse (\(label, position) -> powerSite label position (powerWeightFromExact 0))
      ((0 :: Int, Point (-1) (-1)) :| [(1, Point 1 (-1)), (2, Point 0 1)]))
  let trajectories = NonEmpty.zip sites (1 :| [0, 0])
      direction = Map.fromList (fmap (\(site, rate) -> (powerSiteLabel site, rate)) (NonEmpty.toList trajectories))
      prepare = preparePowerMassSection window sites
  section <- requireRight (prepare direction) >>= evaluate . force
  parameters <- requireRight (traverse (`exactRational` 16) [-112 .. 64])
  half <- requireRight (exactRational 1 2)
  expiryBenchmarks <- prepareExpiryBenchmarks
  retentionBenchmarks <- traverse prepareRetentionBenchmark [4, 32, 128]
  let observations :: [ExactRational] -> Either (PowerMassError Int) [Map.Map Int ExactRational]
      observations queryParameters = traverse (`evaluatePowerMasses` section) queryParameters
      prepareAndObserve
        :: (Map.Map Int ExactRational, [ExactRational])
        -> Either (PowerMassError Int) [Map.Map Int ExactRational]
      prepareAndObserve (queryDirection, queryParameters) = do
        prepared <- prepare queryDirection
        traverse (`evaluatePowerMasses` prepared) queryParameters
      reconstruction parameter = do
        reweighted <- traverse
          (\(site, rate) -> powerSite (powerSiteLabel site) (powerSitePosition site) (powerWeightFromExact (parameter * rate)))
          trajectories
        (diagram, _) <- boundedPowerDiagram window reweighted
        pure (Map.fromList (fmap (\(label, cell) -> (label, cellArea cell)) (powerCellDispositions diagram)))
      cellArea :: PowerCellDisposition Int -> ExactRational
      cellArea disposition = case disposition of
        PublishedPowerCell polygon ->
          let points@(initial :| remaining) = convexPolygonPoints polygon
           in half * sum (zipWith exactPointCross (NonEmpty.toList points) (remaining <> [initial]))
        LowerDimensionalPowerCell _ -> 0
        EmptyPowerCell -> 0
        CoincidentEquivalentTo _ -> 0
        CoincidentDominatedBy _ -> 0
      amortizedBenchmarks queryCount =
        let queryParameters = take queryCount parameters
         in bgroup ("queries-" <> show queryCount)
              [ bench "prepare-and-evaluate" (nf prepareAndObserve (direction, queryParameters))
              , bench "reconstruct" (nf (traverse reconstruction) queryParameters)
              ]
  pure
    ([ bgroup "certified power mass/three-site"
        ( [ bench "prepare" (nf prepare direction)
          , bench "reuse-only-177-exact-parameters" (nf observations parameters)
          ]
          <> fmap amortizedBenchmarks [1, 2, 8, 32, 128]
        )
    , expiryBenchmarks
    ] <> retentionBenchmarks)

-- The admitted window is fixture cost. This measures only its conversion into
-- retained affine boundaries, over the same exact convex family at each size.
prepareRetentionBenchmark :: Int -> IO Benchmark
prepareRetentionBenchmark count = do
  let pointAt :: Int -> ExactPoint
      pointAt index = exactPoint (fromIntegral index) (fromIntegral (index * index))
  window <- requireRight (convexPolygon (pointAt 0 :| fmap pointAt [1 .. count - 1])) >>= evaluate . force
  pure (bench ("convex-retention/" <> show count)
    (nf retainConvexPolygon window))

-- The next seed is an explicitly supplied exact interior value, not an
-- epsilon-derived event transition. The benchmark includes the refusal at
-- the old endpoint and complete preparation at the new seed.
prepareExpiryBenchmarks :: IO Benchmark
prepareExpiryBenchmarks = do
  window <- requireRight
    (convexPolygon (exactPoint (-1) (-1) :| [exactPoint 1 (-1), exactPoint 1 1, exactPoint (-1) 1]))
  let siteAt :: Int -> Point -> ExactRational -> Either (PowerDiagramError Int) (PowerSite Int)
      siteAt label position weight = powerSite label position (powerWeightFromExact weight)
      direction = Map.fromList [(0 :: Int, 0), (1, 4)]
  firstSite <- requireRight (siteAt 0 (Point 0 0) 0)
  hiddenSite <- requireRight (siteAt 1 (Point 2 0) (-4))
  visibleSite <- requireRight (siteAt 1 (Point 2 0) 2)
  initial <- requireRight (preparePowerMassSection window (firstSite :| [hiddenSite]) direction) >>= evaluate . force
  let expireAndRecertify (parameter, updatedSites) =
        ( evaluatePowerMasses parameter initial
        , preparePowerMassSection window updatedSites direction
        )
  pure (bench "certified power mass/window-contact/expiry-and-recertify-at-3-over-2"
    (nf expireAndRecertify (1, firstSite :| [visibleSite])))
