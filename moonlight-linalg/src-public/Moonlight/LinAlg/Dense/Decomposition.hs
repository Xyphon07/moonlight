-- | Dense matrix factorizations: QR, Cholesky, symmetric eigendecomposition, and thin SVD.
module Moonlight.LinAlg.Dense.Decomposition
  ( qrDecompFullColumnRank,
    choleskyDecomp,
    symmetricEigen,
    symmetricEigenPairs,
    thinSvdFullColumnRank,
  )
where

import Moonlight.LinAlg.Pure.Dense.Decomposition
  ( choleskyDecomp,
    qrDecompFullColumnRank,
    symmetricEigen,
    symmetricEigenPairs,
    thinSvdFullColumnRank,
  )
