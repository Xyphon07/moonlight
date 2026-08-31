{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Algebra.Test.Generators
  ( AlgebraGeneratorConfig (..),
    defaultAlgebraGeneratorConfig,
    genBatch,
    genFreeAbelianGroup,
    genIntBasis,
    genIntegerCoefficient,
    genIntegerLawValue,
    genIntegerWeight,
    genLaneVector,
    genModulus,
    genOrientation,
    genPolynomial,
    genPowerSet,
    genSparseVec,
    genZn,
  )
where

import GHC.TypeNats (KnownNat, type (<=))
import Data.Vector.Unboxed qualified as UVector
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Moonlight.Algebra

data AlgebraGeneratorConfig = AlgebraGeneratorConfig
  { collectionLengthRange :: Range.Range Int,
    integerLawRange :: Range.Range Integer,
    integerCoefficientRange :: Range.Range Integer,
    integerWeightRange :: Range.Range Integer,
    intBasisRange :: Range.Range Int,
    modulusRange :: Range.Range Integer,
    znRepresentativeRange :: Range.Range Integer
  }

defaultAlgebraGeneratorConfig :: AlgebraGeneratorConfig
defaultAlgebraGeneratorConfig =
  AlgebraGeneratorConfig
    { collectionLengthRange = Range.linear 0 16,
      integerLawRange = Range.linear (-1000) 1000,
      integerCoefficientRange = Range.linear (-100) 100,
      integerWeightRange = Range.linear (-20) 20,
      intBasisRange = Range.linear (-8) 8,
      modulusRange = Range.linear 2 100,
      znRepresentativeRange = Range.linear (-1000) 1000
    }

genBatch :: AlgebraGeneratorConfig -> HH.Gen a -> HH.Gen (Batch a)
genBatch config genElement =
  Batch <$> Gen.list (collectionLengthRange config) genElement

genFreeAbelianGroup :: Ord g => AlgebraGeneratorConfig -> HH.Gen g -> HH.Gen (FreeAbelianGroup g)
genFreeAbelianGroup config genGenerator =
  fromTerms <$> Gen.list (collectionLengthRange config) ((,) <$> genGenerator <*> genIntegerWeight config)

genIntBasis :: AlgebraGeneratorConfig -> HH.Gen Int
genIntBasis =
  Gen.int . intBasisRange

genIntegerCoefficient :: AlgebraGeneratorConfig -> HH.Gen Integer
genIntegerCoefficient =
  Gen.integral . integerCoefficientRange

genIntegerLawValue :: AlgebraGeneratorConfig -> HH.Gen Integer
genIntegerLawValue =
  Gen.integral . integerLawRange

genIntegerWeight :: AlgebraGeneratorConfig -> HH.Gen Integer
genIntegerWeight =
  Gen.integral . integerWeightRange

genLaneVector :: HH.Gen LaneVector
genLaneVector =
  laneVectorFromLanes . UVector.fromList
    <$> Gen.list
      (Range.constant laneCount laneCount)
      (Gen.word64 Range.linearBounded)

genModulus :: AlgebraGeneratorConfig -> HH.Gen Integer
genModulus =
  Gen.integral . modulusRange

genOrientation :: HH.Gen Orientation
genOrientation =
  Gen.element [Positive, Negative]

genPolynomial :: (Eq r, AdditiveGroup r) => AlgebraGeneratorConfig -> HH.Gen r -> HH.Gen (Polynomial r)
genPolynomial config genScalar =
  fromCoefficients <$> Gen.list (collectionLengthRange config) genScalar

genPowerSet :: Ord a => AlgebraGeneratorConfig -> HH.Gen a -> HH.Gen (PowerSet a)
genPowerSet config genElement =
  fromList <$> Gen.list (collectionLengthRange config) genElement

genSparseVec ::
  (Eq r, AdditiveGroup r, Ord g) =>
  AlgebraGeneratorConfig ->
  HH.Gen g ->
  HH.Gen r ->
  HH.Gen (SparseVec r g)
genSparseVec config genGenerator genScalar =
  fromEntries <$> Gen.list (collectionLengthRange config) ((,) <$> genGenerator <*> genScalar)

genZn :: forall n. (KnownNat n, 1 <= n) => AlgebraGeneratorConfig -> HH.Gen (Zn n)
genZn config =
  mkZn <$> Gen.integral (znRepresentativeRange config)
