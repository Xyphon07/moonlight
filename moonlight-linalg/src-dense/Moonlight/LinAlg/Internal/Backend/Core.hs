{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Moonlight.LinAlg.Internal.Backend.Core
  ( DenseRankBackend (..),
    runPluDecomp,
    runKernel,
    runSmithNormalForm,
    runSmithDiagonalForm,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeNats (KnownNat)
import Moonlight.Algebra.Pure.Ring (EuclideanDomain)
import Moonlight.Core
  ( Field,
    MoonlightError (..),
  )
import Moonlight.LinAlg.Internal.Backend.PLU (PLU, pluDecompPure)
import Moonlight.LinAlg.Internal.Backend.RREF (KernelBasis, kernelPure, rankPure)
import Moonlight.LinAlg.Internal.Backend.Smith (SmithDiagonalForm, SmithNormalForm, smithDiagonalFormPure, smithNormalFormPure)
import Moonlight.LinAlg.Pure.Dense.GF2
  ( GF2
  , mkGF2PackedMatrixFromRowMajor
  , rankGF2PackedMatrix
  )
import Moonlight.LinAlg.Pure.Dense.Types (Matrix, matrixShape, toListMatrix)
import Prelude

type DenseRankBackend :: Type -> Constraint
class DenseRankBackend a where
  runRank ::
    forall r c.
    (KnownNat r, KnownNat c, Eq a, Field a) =>
    Matrix r c a ->
    Either MoonlightError Int
  default runRank ::
    forall r c.
    (KnownNat r, KnownNat c, Eq a, Field a) =>
    Matrix r c a ->
    Either MoonlightError Int
  runRank = rankPure

instance DenseRankBackend Double

instance DenseRankBackend GF2 where
  runRank matrixValue =
    let (rowCount, columnCount) =
          matrixShape matrixValue
     in either
          (Left . InvariantViolation . ("GF2 DenseRank: " <>) . show)
          (Right . rankGF2PackedMatrix)
          ( mkGF2PackedMatrixFromRowMajor
              (fromIntegral rowCount)
              (fromIntegral columnCount)
              (toListMatrix matrixValue)
          )

instance DenseRankBackend Integer

instance DenseRankBackend Rational

runPluDecomp ::
  forall r c a.
  (KnownNat r, KnownNat c, Field a) =>
  Matrix r c a ->
  Either MoonlightError (PLU r c a)
runPluDecomp = pluDecompPure

runKernel ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a) =>
  Matrix r c a ->
  Either MoonlightError (KernelBasis c a)
runKernel = kernelPure

runSmithNormalForm ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (SmithNormalForm r c a)
runSmithNormalForm = smithNormalFormPure

runSmithDiagonalForm ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (SmithDiagonalForm r c a)
runSmithDiagonalForm = smithDiagonalFormPure
