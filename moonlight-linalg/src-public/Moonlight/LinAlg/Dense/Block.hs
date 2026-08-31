-- | Block-matrix inversion over the rationals, GF(2), and the unimodular integers.
module Moonlight.LinAlg.Dense.Block
  ( BlockMatrixFailure (..),
    invertRationalBlock,
    invertGF2Block,
    invertUnimodularIntegerBlock,
  )
where

import Moonlight.LinAlg.Pure.Dense.Block
  ( BlockMatrixFailure (..),
    invertGF2Block,
    invertRationalBlock,
    invertUnimodularIntegerBlock,
  )
