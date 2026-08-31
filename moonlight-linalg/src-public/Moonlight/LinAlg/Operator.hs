{-# LANGUAGE DataKinds #-}

-- | Abstract affine-normalized linear operators with explicit self-adjoint construction boundaries.
module Moonlight.LinAlg.Operator
  ( OperatorSymmetry (..),
    LinearOperator,
    operatorShape,
    operatorDimension,
    mkVectorLinearOperator,
    declaredSelfAdjointVectorLinearOperator,
    runOperatorU,
    csrLinearOperator,
    selfAdjointCSRLinearOperator,
    graphLaplacianLinearOperator,
    diagonalLinearOperator,
    pathLaplacianLinearOperator,
    symmetricTridiagonalLinearOperator,
    packedSparseLinearOperator,
    scaleLinearOperator,
    addScaledIdentity,
    sigmaIdentityMinus,
  )
where

import Moonlight.LinAlg.Pure.Operator
  ( LinearOperator,
    OperatorSymmetry (..),
    addScaledIdentity,
    csrLinearOperator,
    declaredSelfAdjointVectorLinearOperator,
    diagonalLinearOperator,
    graphLaplacianLinearOperator,
    mkVectorLinearOperator,
    operatorDimension,
    operatorShape,
    packedSparseLinearOperator,
    pathLaplacianLinearOperator,
    runOperatorU,
    scaleLinearOperator,
    selfAdjointCSRLinearOperator,
    sigmaIdentityMinus,
    symmetricTridiagonalLinearOperator,
  )
