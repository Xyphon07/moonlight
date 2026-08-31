module Main
  ( main,
  )
where

import MorseSpectral
  ( morseSpectralBenchmarks,
  )
import SparseSpectral
  ( benchmarkNotice,
    shouldIncludeLarge,
    shouldInclude100k,
    sparseSpectralBenchmarks,
  )
import Test.Tasty.Bench
  ( defaultMain,
  )
import ZigzagPersistence
  ( zigzagPersistenceBenchmarks,
  )

main :: IO ()
main = do
  includeLarge <- shouldIncludeLarge
  include100k <- shouldInclude100k
  putStrLn (benchmarkNotice includeLarge include100k)
  defaultMain
    [ morseSpectralBenchmarks includeLarge,
      sparseSpectralBenchmarks includeLarge include100k,
      zigzagPersistenceBenchmarks includeLarge
    ]
