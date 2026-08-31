-- Shared benchmark scaffolding for @moonlight-pale@: size-parameterized groups
-- measured on prepared @Int@-indexed inputs, forced to normal form before timing.
module BenchSupport
  ( preparedBenchmarks,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Test.Tasty.Bench (Benchmark, bench, env, nf)

preparedBenchmarks ::
  (NFData input, NFData result) =>
  String ->
  [(Int, input)] ->
  (input -> result) ->
  [Benchmark]
preparedBenchmarks label preparedInputs workload =
  [ env (evaluate (force preparedInput)) $ \prepared ->
      bench (label <> "/" <> show size) (nf workload prepared)
    | (size, preparedInput) <- preparedInputs
  ]
