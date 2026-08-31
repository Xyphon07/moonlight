-- | Field-level dense operations: full-rank PLU, rank, and kernel bases.
module Moonlight.LinAlg.Dense.Field
  ( DenseRankBackend,
    PLU (..),
    KernelBasis (..),
    pluDecompFullRank,
    rank,
    kernel,
  )
where

import Moonlight.LinAlg.Pure.Dense.Field
  ( DenseRankBackend,
    KernelBasis (..),
    PLU (..),
    kernel,
    pluDecompFullRank,
    rank,
  )
