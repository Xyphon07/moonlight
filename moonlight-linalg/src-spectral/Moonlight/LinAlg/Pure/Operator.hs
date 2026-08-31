{-# LANGUAGE DataKinds #-}

module Moonlight.LinAlg.Pure.Operator
  ( OperatorSymmetry (..),
    LinearOperator,
    ApplyU,
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

import Moonlight.LinAlg.Pure.Operator.Internal
  ( ApplyU,
    LinearOperator,
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
