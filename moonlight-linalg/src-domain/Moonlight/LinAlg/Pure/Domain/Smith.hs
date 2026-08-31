module Moonlight.LinAlg.Pure.Domain.Smith
  ( SmithNormalForm (..),
    SmithDiagonalForm (..),
    smithNormalForm,
    smithDiagonalForm,
  )
where

import GHC.TypeNats (KnownNat)
import Moonlight.Algebra.Pure.Ring (EuclideanDomain)
import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg.Internal.Backend.Core (runSmithDiagonalForm, runSmithNormalForm)
import Moonlight.LinAlg.Internal.Backend.Smith (SmithDiagonalForm (..), SmithNormalForm (..))
import Moonlight.LinAlg.Pure.Domain.Smith.Multimodular (smithDiagonalFormMultimodular)
import Moonlight.LinAlg.Pure.Domain.Smith.Witnessed (smithNormalFormWitnessed)
import Moonlight.LinAlg.Pure.Dense.Types (Matrix)

smithNormalForm ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (SmithNormalForm r c a)
smithNormalForm = runSmithNormalForm

smithDiagonalForm ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (SmithDiagonalForm r c a)
smithDiagonalForm = runSmithDiagonalForm

{-# NOINLINE smithNormalForm #-}
{-# NOINLINE smithDiagonalForm #-}
{-# RULES "smithNormalForm/Integer" forall matrixValue. smithNormalForm matrixValue = smithNormalFormWitnessed matrixValue #-}
{-# RULES "smithDiagonalForm/Integer" forall matrixValue. smithDiagonalForm matrixValue = smithDiagonalFormMultimodular matrixValue #-}
