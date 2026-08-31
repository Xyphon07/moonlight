-- | Low-level vector and matrix primitives: dot products, norms, scaling, outer products, and linear combinations.
module Moonlight.LinAlg.Dense.Primitives
  ( dotProduct,
    vectorNorm,
    scaleVector,
    addVector,
    subVector,
    matrixVectorProduct,
    matrixSubtract,
    scaleMatrix,
    outerProduct,
    basisVector,
    linearCombination,
  )
where

import Moonlight.LinAlg.Internal.Primitives
  ( addVector,
    basisVector,
    dotProduct,
    linearCombination,
    matrixSubtract,
    matrixVectorProduct,
    outerProduct,
    scaleMatrix,
    scaleVector,
    subVector,
    vectorNorm,
  )
