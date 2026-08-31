-- | The single fresh-process timing boundary shared by both Haskell runners.
module Moonlight.Triangulation.Bench.SpadeCompare.Timing
  ( measureLane
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import GHC.Clock (getMonotonicTimeNSec)
import Moonlight.Triangulation.Bench.SpadeCompare.Lane
  ( LaneRequest
  , laneRequestLabel
  )
import System.Mem (performGC)

-- | Time exactly one fully forced iteration. Input construction belongs to
-- each interpreter before this boundary; the driver supplies fresh processes.
measureLane :: NFData value => LaneRequest -> IO value -> IO ()
measureLane request work = do
  performGC
  start <- getMonotonicTimeNSec
  _ <- work >>= evaluate . force
  stop <- getMonotonicTimeNSec
  putStrLn (laneRequestLabel request <> "," <> show (stop - start))
