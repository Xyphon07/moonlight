-- | Validated rectangular row authoring surface.
--
-- This facade preserves nested-row shape failures as typed errors. It is not
-- the hot dense-storage owner; benchmark-sensitive code should use sealed
-- vector, sparse, tridiagonal, or native owners instead of pretending lists are
-- a BLAS implementation.
module Moonlight.LinAlg.Dense.Rows
  ( DenseRows,
    mkDenseRows,
    mkDenseRowsWithShape,
    mkDenseRowsFromFlat,
    denseRowsShape,
    denseRowsToLists,
    transposeRowsExact,
    zipRowsExactWith,
    matrixVectorProductRowsWith,
    matrixProductRowsWith,
    hcatRowsExact,
    vcatRowsExact,
  )
where

import Moonlight.LinAlg.Pure.Dense.Rows
  ( DenseRows,
    denseRowsShape,
    denseRowsToLists,
    hcatRowsExact,
    matrixProductRowsWith,
    matrixVectorProductRowsWith,
    mkDenseRows,
    mkDenseRowsFromFlat,
    mkDenseRowsWithShape,
    transposeRowsExact,
    vcatRowsExact,
    zipRowsExactWith,
  )
