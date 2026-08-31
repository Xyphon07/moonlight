module Main
  ( main,
  )
where

import DiagnosticBench (diagnosticBenchmarks)
import GhcSurfaceBench (ghcSurfaceBenchmarks)
import HieBench (hieBenchmarks)
import LawBench (lawBenchmarks)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  case (ghcSurfaceBenchmarks, hieBenchmarks, lawBenchmarks) of
    (Left obstruction, _, _) ->
      rejectBenchmarkCorpus "GHC surface" obstruction
    (_, Left obstruction, _) ->
      rejectBenchmarkCorpus "HIE type graph" obstruction
    (_, _, Left obstruction) ->
      rejectBenchmarkCorpus "finite laws" obstruction
    ( Right surfaceBenchmarks,
      Right hieTypeBenchmarks,
      Right finiteLawBenchmarks
      ) ->
        defaultMain
          [ diagnosticBenchmarks,
            surfaceBenchmarks,
            hieTypeBenchmarks,
            finiteLawBenchmarks
          ]

rejectBenchmarkCorpus :: Show obstruction => String -> obstruction -> IO ()
rejectBenchmarkCorpus corpusLabel obstruction = do
  hPutStrLn stderr ("moonlight-pale " <> corpusLabel <> " benchmark corpus rejected: " <> show obstruction)
  exitFailure
