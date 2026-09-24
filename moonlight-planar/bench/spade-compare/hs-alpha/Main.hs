-- | Narrow Haskell interpreter for the opt-in alpha-persistence diagnostic.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Vector qualified as Vector
import Moonlight.Planar.Bench.SpadeCompare.Alpha
  ( alphaPersistenceReceipt
  , renderAlphaPersistenceReceipt
  )
import Moonlight.Planar.Bench.SpadeCompare.Input (randomPoints)
import Moonlight.Planar.Bench.SpadeCompare.Lane
  ( LaneKind (AlphaPersistenceLane)
  , LaneObstruction
  , LaneRequest (..)
  , parseLaneRequest
  , renderLaneObstruction
  )
import Moonlight.Planar.Bench.SpadeCompare.Timing (measureLane)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["alpha-receipt", rawCount] ->
      either refuseLane reportReceipt
        (parseLaneRequest "alpha-persistence" rawCount "0")
    ["bench-one", lane, first, second] ->
      either refuseLane runAlphaLane (parseLaneRequest lane first second)
    _ -> refuseUsage

runAlphaLane :: LaneRequest -> IO ()
runAlphaLane request@(LaneRequest AlphaPersistenceLane pointCount _) = do
  points <-
    evaluate
      (force (Vector.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount)))
  measureLane request (requireResult (alphaPersistenceReceipt points))
runAlphaLane _ = refuseUsage

reportReceipt :: LaneRequest -> IO ()
reportReceipt (LaneRequest AlphaPersistenceLane pointCount _) = do
  let points = Vector.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount)
  receipt <- requireResult (alphaPersistenceReceipt points)
  putStrLn (renderAlphaPersistenceReceipt receipt)
reportReceipt _ = refuseUsage

requireResult :: Show obstruction => Either obstruction value -> IO value
requireResult result =
  case result of
    Left obstruction -> do
      hPutStrLn stderr ("alpha persistence refused: " <> show obstruction)
      exitWith (ExitFailure 1)
    Right value -> pure value

refuseLane :: LaneObstruction -> IO value
refuseLane obstruction = do
  hPutStrLn stderr (renderLaneObstruction obstruction)
  exitWith (ExitFailure 2)

refuseUsage :: IO value
refuseUsage = do
  hPutStrLn stderr "usage: moonlight-planar-alpha-spade-referent (alpha-receipt COUNT | bench-one alpha-persistence COUNT 0)"
  exitWith (ExitFailure 2)
