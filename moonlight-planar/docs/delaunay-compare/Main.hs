-- | Haskell owner of the upstream-compatible construction comparison.
module Main (main) where

import Moonlight.Planar.Bench.DelaunayCompare.Domain (renderCompareObstruction)
import Moonlight.Planar.Bench.DelaunayCompare.Native (withNativeApi)
import Moonlight.Planar.Bench.DelaunayCompare.Suite
  ( preflightSuite
  , suiteAgreementMessage
  , suiteBenchmarks
  , withPreparedSuite
  )
import System.Exit (die)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  outcome <-
    withNativeApi $ \api ->
      withPreparedSuite api $ \suite -> do
        agreement <- preflightSuite api suite
        case agreement of
          Left obstruction -> pure (Left obstruction)
          Right () -> do
            putStrLn suiteAgreementMessage
            defaultMain (suiteBenchmarks api suite)
            pure (Right ())
  either (die . renderCompareObstruction) pure outcome
