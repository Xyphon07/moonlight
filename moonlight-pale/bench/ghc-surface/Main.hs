module Main
  ( main,
  )
where

import GhcSurfaceBench (ghcSurfaceBenchmarks)
import HieBench (hieBenchmarks)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  case (ghcSurfaceBenchmarks, hieBenchmarks) of
    (Left obstruction, _) ->
      rejectBenchmarkCorpus "GHC surface" obstruction
    (_, Left obstruction) ->
      rejectBenchmarkCorpus "HIE type graph" obstruction
    (Right surfaceBenchmarks, Right hieTypeBenchmarks) ->
      defaultMain [surfaceBenchmarks, hieTypeBenchmarks]

rejectBenchmarkCorpus :: Show obstruction => String -> obstruction -> IO ()
rejectBenchmarkCorpus corpusLabel obstruction = do
  hPutStrLn stderr ("moonlight-pale " <> corpusLabel <> " benchmark corpus rejected: " <> show obstruction)
  exitFailure
