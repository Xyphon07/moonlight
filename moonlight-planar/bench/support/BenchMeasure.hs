-- | Shared benchmark clocks, allocation accounting, forcing, and typed-result
-- admission. Domain fixtures live elsewhere so native components can reuse the
-- instrument without importing triangulation machinery.
module BenchMeasure
  ( timedValue
  , timedProjection
  , requireRight
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (getAllocationCounter)
import GHC.Stats
  ( RTSStats (max_live_bytes)
  , getRTSStats
  , getRTSStatsEnabled
  )
import System.CPUTime (getCPUTime)

timedValue :: NFData value => String -> IO value -> IO value
timedValue label = timedProjection label id

timedProjection :: NFData observation => String -> (value -> observation) -> IO value -> IO value
timedProjection label observe action = do
  statsEnabled <- getRTSStatsEnabled
  allocationStart <- getAllocationCounter
  wallStart <- getMonotonicTimeNSec
  cpuStart <- getCPUTime
  value <- action
  _ <- evaluate (force (observe value))
  cpuEnd <- getCPUTime
  wallEnd <- getMonotonicTimeNSec
  allocationEnd <- getAllocationCounter
  after <- if statsEnabled then Just <$> getRTSStats else pure Nothing
  putStrLn (label <> "-elapsed: " <> show (fromIntegral (wallEnd - wallStart) / 1.0e9 :: Double) <> "s")
  putStrLn (label <> "-cpu: " <> show (fromIntegral (cpuEnd - cpuStart) / 1.0e12 :: Double) <> "s")
  putStrLn (label <> "-allocated-bytes: " <> show (allocationStart - allocationEnd))
  case after of
    Just right -> putStrLn (label <> "-process-max-live-bytes: " <> show (max_live_bytes right))
    Nothing -> pure ()
  pure value

requireRight :: Show obstruction => Either obstruction value -> IO value
requireRight = either (fail . show) pure
