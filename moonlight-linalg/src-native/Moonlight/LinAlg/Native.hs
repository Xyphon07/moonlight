-- | Effectful native LAPACK backend boundary: on macOS it links Accelerate, elsewhere it expects BLAS\/LAPACK.
module Moonlight.LinAlg.Native
  ( denseDoubleLinearSolveLapack,
    denseDoubleMatrixProductBlas,
    denseDoubleSymmetricEigenpairsLapack,
    leastSquaresLapack,
    symmetricEigenRequestLapack,
    selectedSymmetricTridiagonalEigenRequestLapack,
    selectedSymmetricBlockTridiagonalEigenRequestLapack,
  )
where

import Moonlight.LinAlg.Effect.Native.Dispatch
  ( denseDoubleLinearSolveLapack,
    denseDoubleMatrixProductBlas,
    denseDoubleSymmetricEigenpairsLapack,
    leastSquaresLapack,
    selectedSymmetricBlockTridiagonalEigenRequestLapack,
    selectedSymmetricTridiagonalEigenRequestLapack,
    symmetricEigenRequestLapack,
  )
