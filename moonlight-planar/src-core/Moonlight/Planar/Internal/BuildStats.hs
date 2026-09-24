{-# LANGUAGE DerivingStrategies #-}

-- | The closed reported-metric schema and its dense immutable section.
-- Only the operation's final publication boundary may adopt a counter vector.
module Moonlight.Planar.Internal.BuildStats where

import Control.DeepSeq (NFData (..))
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U

data BuildMetric
  = InputPoints
  | UniquePoints
  | ExistingPoints
  | DuplicatePoints
  | SpatialSeedPoints
  | FaceSplits
  | InteriorEdgeSplits
  | BoundaryEdgeSplits
  | HullInsertions
  | LineSplits
  | LineExtensions
  | LineToAreaTransitions
  | EdgeFlips
  | LocationWalkSteps
  | LocationFallbacks
  | LocationMaxWalk
  | LegalizationMaxStack
  | SteinerPoints
  | RefinementFaceChecks
  | RefinementQueuePops
  | SweepFastPoints
  | SweepSkippedPoints
  deriving stock (Eq, Ord, Enum, Bounded, Show, Read)

buildMetricCount :: Int
buildMetricCount = fromEnum (maxBound :: BuildMetric) + 1
{-# INLINE buildMetricCount #-}

-- The vector has exactly buildMetricCount slots. No public constructor,
-- Generic or Read route can fabricate a shorter section.
newtype BuildStats = BuildStats (U.Vector Word64)
  deriving stock (Eq)

instance NFData BuildStats where
  rnf (BuildStats values) = rnf values

instance Show BuildStats where
  showsPrec precedence stats = showParen (precedence > 10) $
    showString "BuildStats " . showsPrec 11 [(metric, buildStat metric stats) | metric <- [minBound .. maxBound]]

buildStat :: BuildMetric -> BuildStats -> Int
buildStat metric (BuildStats values) = fromIntegral (U.unsafeIndex values (fromEnum metric))
{-# INLINE buildStat #-}

tabulateBuildStats :: (BuildMetric -> Int) -> BuildStats
tabulateBuildStats value = BuildStats (U.fromList [fromIntegral (value metric) | metric <- [minBound .. maxBound]])

emptyBuildStats :: BuildStats
emptyBuildStats = BuildStats (U.replicate buildMetricCount 0)

updateBuildStats :: [(BuildMetric, Int)] -> BuildStats -> BuildStats
updateBuildStats changes (BuildStats values) =
  BuildStats (values U.// [(fromEnum metric, fromIntegral value) | (metric, value) <- changes])
