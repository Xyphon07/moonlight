module Moonlight.Homology.Matrix
  ( ValidatedMatrix,
    matrixRowCount,
    matrixColumnCount,
    matrixRows,
    mkValidatedMatrix,
    validatedMatrixFromRows,
    zeroValidatedMatrix,
    validatedMatrixFromColumns,
    transposeValidatedMatrix,
    validatedDiagonal,
    validatedColumnAt,
    selectValidatedRows,
    selectValidatedColumns,
    applyValidatedMatrix
  )
where

import Moonlight.Homology.Pure.Matrix.Validated as X
