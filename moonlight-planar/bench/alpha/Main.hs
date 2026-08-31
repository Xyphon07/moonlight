-- | Measure the exact alpha shortcut against recomputing homology at every
-- critical radius. Delaunay construction is deliberately outside every timed
-- lane: both algorithms consume the same already-built geometry.
module Main (main) where

import BenchMeasure
  ( requireRight
  , timedProjection
  , timedValue
  )
import BenchSupport (randomPoints)
import Data.Map.Strict qualified as Map
import Data.List.NonEmpty qualified as NonEmpty
import Data.Foldable (traverse_)
import Data.Vector qualified as Vector
import Moonlight.Homology.Boundary
  ( degreeCardinality
  , maxHomologicalDegree
  , restrictComplex
  )
import Moonlight.Homology.Chain
  ( HomologicalDegree (..)
  , HomologyFailure
  )
import Moonlight.Homology.Persistence
  ( FilteredFiniteChainComplex
  , criticalBettiVectors
  , filteredBaseComplex
  , filteredCellBirths
  , filteredCriticalValues
  , mod2PersistentPairsWithCriticalBettiTable
  )
import Moonlight.Homology.Topology (freeBettiVector)
import Moonlight.Triangulation.Alpha
  ( AlphaBirth
  , alphaFiltration
  )
import Moonlight.Triangulation.BulkLoad (delaunayGeometry)
import Moonlight.Triangulation.CellComplex (filteredAlphaComplex)
import Moonlight.Triangulation.LabelledComplex (filteredPlanarComplex)
import Moonlight.Triangulation.PowerDiagram
  ( PowerSite
  , RegularTriangulation
  , powerSite
  , powerWeight
  , regularTriangulation
  , regularTriangulationReceipt
  )
import Moonlight.Triangulation.RegularAlpha
  ( PowerAlphaBirth
  , regularAlphaBirths
  , regularAlphaComplex
  , regularAlphaFiltration
  )
import Moonlight.Triangulation.Types (DelaunayTriangulation, Point)

type PreparedAlpha = FilteredFiniteChainComplex AlphaBirth Int

main :: IO ()
main = do
  benchmarkAlphaFiltration 64
  traverse_ benchmarkWeightedAlphaFiltration [64, 512, 2048]

benchmarkAlphaFiltration :: Int -> IO ()
benchmarkAlphaFiltration siteCount = do
  triangulation <-
    requireRight
      (delaunayGeometry (Vector.fromList (randomPoints 0x9e3779b97f4a7c15 siteCount)))
  filtered <-
    timedProjection
      (benchmarkLabel siteCount "alpha-filtered-complex")
      preparationReceipt
      (prepareAlpha triangulation)
  let thresholds = filteredCriticalValues filtered
      maximumDegree = maxHomologicalDegreeValue filtered
  persistentProfile <-
    timedValue
      (benchmarkLabel siteCount "persistence-critical-profile")
      ( do
          (_, bettiTable) <-
            requireRight (mod2PersistentPairsWithCriticalBettiTable filtered)
          pure (criticalBettiVectors bettiTable)
      )
  repeatedProfile <-
    timedValue
      (benchmarkLabel siteCount "repeated-threshold-homology")
      (requireRight (traverse (bettiVectorAtThreshold maximumDegree filtered) thresholds))
  if persistentProfile == repeatedProfile
    then
      putStrLn
        ( benchmarkLabel siteCount "agreement"
            <> ": critical-radii="
            <> show (length thresholds)
            <> " profiles="
            <> show (length persistentProfile)
            <> " prepared-cells="
            <> show (preparationReceipt filtered)
        )
    else fail (benchmarkLabel siteCount "profile-mismatch")

benchmarkLabel :: Int -> String -> String
benchmarkLabel siteCount suffix =
  "alpha/n" <> show siteCount <> "/" <> suffix

prepareAlpha
  :: DelaunayTriangulation ()
  -> IO PreparedAlpha
prepareAlpha triangulation = do
  filtration <- requireRight (alphaFiltration triangulation)
  requireRight (filteredAlphaComplex filtration)

preparationReceipt
  :: FilteredFiniteChainComplex filtration coefficient
  -> (Int, Int, [Int])
preparationReceipt filtered =
  let finite = filteredBaseComplex filtered
      HomologicalDegree maximumDegree = maxHomologicalDegree finite
   in ( length (filteredCriticalValues filtered)
      , Map.size (filteredCellBirths filtered)
      , fmap
          (degreeCardinality finite . HomologicalDegree)
          [0 .. maximumDegree]
      )

maxHomologicalDegreeValue
  :: FilteredFiniteChainComplex filtration coefficient
  -> Int
maxHomologicalDegreeValue filtered =
  case maxHomologicalDegree (filteredBaseComplex filtered) of
    HomologicalDegree degreeValue -> degreeValue

bettiVectorAtThreshold
  :: Int
  -> FilteredFiniteChainComplex AlphaBirth Int
  -> AlphaBirth
  -> Either HomologyFailure [Int]
bettiVectorAtThreshold maximumDegree filtered threshold = do
  restricted <-
    restrictComplex
      (Map.keysSet (Map.filter (<= threshold) (filteredCellBirths filtered)))
      (filteredBaseComplex filtered)
  pure (padBettiVector maximumDegree (freeBettiVector restricted))

padBettiVector :: Int -> [Int] -> [Int]
padBettiVector maximumDegree bettiVector =
  bettiVector
    <> replicate
      (max 0 (maximumDegree + 1 - length bettiVector))
      0

type PreparedWeightedAlpha = FilteredFiniteChainComplex PowerAlphaBirth Int

benchmarkWeightedAlphaFiltration :: Int -> IO ()
benchmarkWeightedAlphaFiltration siteCount = do
  sites <-
    traverse prepareWeightedSite
      (zip [0 ..] (randomPoints 0x517cc1b727220a95 siteCount))
  submitted <-
    maybe (fail "weighted alpha benchmark requires at least one site") pure
      (NonEmpty.nonEmpty sites)
  regular <-
    timedProjection
      (benchmarkLabel siteCount "regular-topology")
      regularTriangulationReceipt
      (fst <$> requireRight (regularTriangulation submitted))
  filtered <-
    timedProjection
      (benchmarkLabel siteCount "weighted-filtered-complex")
      preparationReceipt
      (prepareWeightedAlpha regular)
  profiles <-
    timedValue
      (benchmarkLabel siteCount "weighted-persistence-critical-profile")
      ( do
          (_, bettiTable) <-
            requireRight (mod2PersistentPairsWithCriticalBettiTable filtered)
          pure (criticalBettiVectors bettiTable)
      )
  putStrLn
    ( benchmarkLabel siteCount "weighted-profile"
        <> ": critical-births="
        <> show (length (filteredCriticalValues filtered))
        <> " profiles="
        <> show (length profiles)
        <> " prepared-cells="
        <> show (preparationReceipt filtered)
    )

prepareWeightedSite :: (Int, Point) -> IO (PowerSite Int)
prepareWeightedSite (label, point) = do
  weight <- requireRight (powerWeight (fromIntegral (label `mod` 11 - 5) / 256))
  requireRight (powerSite label point weight)

prepareWeightedAlpha
  :: RegularTriangulation Int
  -> IO PreparedWeightedAlpha
prepareWeightedAlpha regular = do
  filtration <- requireRight (regularAlphaFiltration regular)
  requireRight
    ( filteredPlanarComplex
        (regularAlphaComplex filtration)
        (regularAlphaBirths filtration)
    )
