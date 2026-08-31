module Moonlight.LinAlg.Effect.Harness.Operator
  ( scaledOperatorActionLaw,
    shiftedIdentityActionLaw,
  )
where

import Data.Vector.Unboxed qualified as U
import Moonlight.LinAlg
  ( addScaledIdentity,
    diagonalLinearOperator,
    runOperatorU,
    scaleLinearOperator,
  )
import Moonlight.LinAlg.Effect.Harness.Core
  ( assertApproxList,
    assertRightProperty,
  )
import Test.Tasty.QuickCheck qualified as QC

newtype OperatorVector3 = OperatorVector3 [Double]
  deriving stock (Eq, Show)

newtype OperatorScale = OperatorScale Double
  deriving stock (Eq, Show)

newtype OperatorShift = OperatorShift Double
  deriving stock (Eq, Show)

instance QC.Arbitrary OperatorVector3 where
  arbitrary =
    OperatorVector3
      <$> QC.vectorOf 3 (fromIntegral <$> QC.chooseInt (-8, 8))

instance QC.Arbitrary OperatorScale where
  arbitrary =
    OperatorScale . fromIntegral <$> QC.chooseInt (-4, 4)

instance QC.Arbitrary OperatorShift where
  arbitrary =
    OperatorShift . fromIntegral <$> QC.chooseInt (-4, 4)

scaledOperatorActionLaw :: QC.Property
scaledOperatorActionLaw =
  QC.property scaledOperatorActionLawProperty

shiftedIdentityActionLaw :: QC.Property
shiftedIdentityActionLaw =
  QC.property shiftedIdentityActionLawProperty

scaledOperatorActionLawProperty :: OperatorScale -> OperatorVector3 -> QC.Property
scaledOperatorActionLawProperty (OperatorScale scaleValue) (OperatorVector3 vectorEntries) =
  assertRightProperty $ do
    operatorValue <- diagonalLinearOperator (U.fromList [2.0, -3.0, 5.0])
    baseImage <- runOperatorU operatorValue (U.fromList vectorEntries)
    scaledImage <- runOperatorU (scaleLinearOperator scaleValue operatorValue) (U.fromList vectorEntries)
    pure (assertApproxList (fmap (* scaleValue) (U.toList baseImage)) (U.toList scaledImage))

shiftedIdentityActionLawProperty :: OperatorShift -> OperatorVector3 -> QC.Property
shiftedIdentityActionLawProperty (OperatorShift shiftValue) (OperatorVector3 vectorEntries) =
  assertRightProperty $ do
    operatorValue <- diagonalLinearOperator (U.fromList [2.0, -3.0, 5.0])
    baseImage <- runOperatorU operatorValue (U.fromList vectorEntries)
    shiftedImage <- runOperatorU (addScaledIdentity shiftValue operatorValue) (U.fromList vectorEntries)
    pure (assertApproxList (zipWith (\baseValue inputValue -> baseValue + shiftValue * inputValue) (U.toList baseImage) vectorEntries) (U.toList shiftedImage))
