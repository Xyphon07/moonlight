module Main (main) where

import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import Moonlight.Planar.LiftedSignCases
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

main :: IO ()
main = do
  cases <- requireRight liftedSignCases >>= evaluate . force
  traverse_ reportFamily cases
  defaultMain (map familyBenchmarks cases)

familyBenchmarks :: (LiftedSignFamily, [LiftedSignInput]) -> Benchmark
familyBenchmarks (family, inputs) =
  bgroup
    (show family)
    [ bench "normalized-rational-magnitude-sign" (nf magnitudeSigns inputs)
    , bench "positive-row-scaled-integer-sign" (nf candidateSigns inputs)
    ]

reportFamily :: (LiftedSignFamily, [LiftedSignInput]) -> IO ()
reportFamily (family, inputs) = do
  putStrLn (show family <> "-predicates: " <> show (length inputs))
  _ <- timedValue (show family <> "-magnitude-sign") (evaluate (magnitudeSigns inputs))
  _ <- timedValue (show family <> "-integer-sign") (evaluate (candidateSigns inputs))
  pure ()

magnitudeSigns :: [LiftedSignInput] -> [Ordering]
magnitudeSigns = map magnitudeSign
{-# NOINLINE magnitudeSigns #-}

candidateSigns :: [LiftedSignInput] -> [Ordering]
candidateSigns = map candidateSign
{-# NOINLINE candidateSigns #-}
