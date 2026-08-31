{-# LANGUAGE BangPatterns #-}

module BenchSupport
  ( BenchmarkFixtureFailure (..),
    benchFailure,
    forceBenchmarkFixture,
    assertBenchmarkAgreement,
    keys,
    lastKey,
    middleKey,
    halfSize,
    quarterSize,
    boundedOverlap,
    patchDeltaSizes,
    readStateScale,
    defaultAllocationRepetitions,
    deltaSizes,
    frontierSizes,
    frontierDominatedSizes,
    repairSizes,
    repeatedDeltaKeys,
    repeatedDeltaSupportSize,
    caseLabel,
    mapIntWeight,
    maybeIntWeight,
    naturalWeight,
  )
where

import Control.DeepSeq
  ( NFData (rnf),
  )
import Control.Exception
  ( Exception,
    evaluate,
    throw,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Numeric.Natural
  ( Natural,
  )
import System.Environment
  ( lookupEnv,
  )
import Text.Read
  ( readMaybe,
  )

data BenchmarkFixtureFailure = BenchmarkFixtureFailure
  { benchmarkFixtureFailureLabel :: !String,
    benchmarkFixtureFailureDetail :: !String
  }
  deriving stock (Show)

instance Exception BenchmarkFixtureFailure

benchFailure :: Show err => String -> err -> result
benchFailure label err =
  throw (BenchmarkFixtureFailure label (show err))

forceBenchmarkFixture :: NFData fixture => fixture -> IO fixture
forceBenchmarkFixture fixture =
  evaluate (rnf fixture) *> pure fixture

assertBenchmarkAgreement ::
  (Eq result, Show result) =>
  String ->
  result ->
  result ->
  IO ()
assertBenchmarkAgreement label expected actual =
  if expected == actual
    then pure ()
    else
      throw
        ( BenchmarkFixtureFailure
            label
            ("reference: " <> show expected <> "; optimized: " <> show actual)
        )

keys :: Int -> [Int]
keys size = [0 .. size - 1]

lastKey :: Int -> Int
lastKey size =
  max 0 (size - 1)

middleKey :: Int -> Int
middleKey size =
  min (lastKey size) (max 0 (size `div` 2))

halfSize :: Int -> Int
halfSize size =
  max 1 (size `div` 2)

quarterSize :: Int -> Int
quarterSize size =
  max 1 (size `div` 4)

boundedOverlap :: Int -> Int -> Int
boundedOverlap size requestedOverlap =
  max 0 (min size requestedOverlap)

patchDeltaSizes :: [Int]
patchDeltaSizes = [1, 2, 4, 8, 16, 32, 63, 64, 65, 128, 512, 2048, 8192]

readStateScale :: IO Int
readStateScale = do
  raw <- lookupEnv "MOONLIGHT_DELTA_STATE_SCALE"
  pure (maybe 1 (max 1) (raw >>= readMaybe))

defaultAllocationRepetitions :: Int
defaultAllocationRepetitions = 16

deltaSizes :: [Int]
deltaSizes = [128, 512, 2048]

frontierSizes :: [Int]
frontierSizes = [64, 256, 1024]

frontierDominatedSizes :: [Int]
frontierDominatedSizes = [64, 256]

repairSizes :: [Int]
repairSizes = [128, 512, 2048]

repeatedDeltaKeys :: [Int]
repeatedDeltaKeys =
  keys repeatedDeltaSupportSize

repeatedDeltaSupportSize :: Int
repeatedDeltaSupportSize =
  32

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> " n=" <> show size

mapIntWeight :: Map Int Int -> Int
mapIntWeight =
  Map.foldlWithKey' weighEntry 0
  where
    weighEntry :: Int -> Int -> Int -> Int
    weighEntry !total key value =
      total + key + value

maybeIntWeight :: Maybe Int -> Int
maybeIntWeight maybeValue =
  case maybeValue of
    Nothing ->
      0
    Just value ->
      value

naturalWeight :: Natural -> Int
naturalWeight =
  fromIntegral
