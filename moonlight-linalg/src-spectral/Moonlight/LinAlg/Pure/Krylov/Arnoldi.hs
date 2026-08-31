{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Arnoldi
  ( arnoldi,
  )
where

import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg.Internal.VectorOps (normU, scaleU)
import Moonlight.LinAlg.Pure.Krylov.Config (ArnoldiConfig, arnoldiIterations, arnoldiReorthogonalize, arnoldiTolerance)
import Moonlight.LinAlg.Pure.Krylov.Decomposition (ArnoldiDecomposition, mkArnoldiDecomposition)
import Moonlight.LinAlg.Pure.Krylov.Internal
  ( normalizeSeed,
    orthogonalizeAgainst,
    requireBasisVector,
    sparseColumnsToDenseRowVectors,
    validateIterationCount,
    validateSquareOperator,
  )
import Moonlight.LinAlg.Pure.Operator (LinearOperator, operatorShape, runOperatorU)
import Prelude

arnoldi :: ArnoldiConfig -> LinearOperator symmetry -> U.Vector Double -> Either MoonlightError ArnoldiDecomposition
arnoldi config op seedVector = do
  validateSquareOperator "Arnoldi" op
  let (_, cols) = operatorShape op
  targetIterations <- validateIterationCount "Arnoldi" (arnoldiIterations config)
  firstBasis <- normalizeSeed "Arnoldi" cols (arnoldiTolerance config) seedVector
  let boundedIterations = min targetIterations cols
      go basisVectors hessenbergColumns iterationIndex = do
        currentBasis <- requireBasisVector iterationIndex basisVectors
        imageVector <- runOperatorU op currentBasis
        (reducedVector, coefficients) <-
          orthogonalizeAgainst (arnoldiReorthogonalize config) basisVectors imageVector
        let nextNorm = normU reducedVector
            nextColumn = U.snoc coefficients nextNorm
            nextHessenbergColumns = hessenbergColumns `Box.snoc` nextColumn
        if nextNorm <= arnoldiTolerance config || iterationIndex + 1 >= boundedIterations
          then finalize basisVectors nextHessenbergColumns
          else
            let nextBasis = scaleU (1.0 / nextNorm) reducedVector
             in go (basisVectors `Box.snoc` nextBasis) nextHessenbergColumns (iterationIndex + 1)
      finalize basisVectors hessenbergColumns =
        let hessenbergRows = sparseColumnsToDenseRowVectors (stepCount + 1) stepCount hessenbergColumns
            stepCount = Box.length hessenbergColumns
         in mkArnoldiDecomposition basisVectors hessenbergRows
   in go (Box.singleton firstBasis) Box.empty 0
