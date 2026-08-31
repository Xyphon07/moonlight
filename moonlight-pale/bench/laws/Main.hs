module Main
  ( main,
  )
where

import LawBench (lawBenchmarks)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  case lawBenchmarks of
    Left obstruction -> do
      hPutStrLn stderr ("moonlight-pale finite-law benchmark corpus rejected: " <> show obstruction)
      exitFailure
    Right benchmarks ->
      defaultMain [benchmarks]
