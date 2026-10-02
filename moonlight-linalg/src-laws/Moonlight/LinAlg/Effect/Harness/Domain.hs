module Moonlight.LinAlg.Effect.Harness.Domain
  ( smithDiagonalReconstructsInputLaw,
    smithDivisibilityChainLaw,
    smithWitnessesUnimodularLaw,
    smithDiagonalOnlyAgreesWithFullLaw,
    bareissRankAgreesWithFieldRankLaw,
    bareissDeterminantAgreesWithExteriorLaw,
  )
where

import Data.Bifunctor (first)
import Moonlight.LinAlg
  ( bareissDeterminant,
    bareissRank,
    exteriorPowerMatrix,
    fromListMatrix,
    mult,
    rank,
    smithDiagonal,
    smithDiagonalForm,
    smithDiagonalMatrix,
    smithLeft,
    smithLeftInverse,
    smithNormalForm,
    smithRight,
    smithRightInverse,
    toListMatrix,
  )
import Moonlight.LinAlg.Effect.Harness.Core (assertRightProperty, matrixRows3)
import Test.Tasty.QuickCheck qualified as QC

newtype IntegerMatrix3 = IntegerMatrix3 [Integer]
  deriving stock (Eq, Show)

instance QC.Arbitrary IntegerMatrix3 where
  arbitrary =
    IntegerMatrix3
      <$> QC.vectorOf 9 (fromIntegral <$> QC.chooseInt (-8, 8))

smithDiagonalReconstructsInputLaw :: QC.Property
smithDiagonalReconstructsInputLaw =
  QC.property smithDiagonalReconstructsInputLawProperty

smithDivisibilityChainLaw :: QC.Property
smithDivisibilityChainLaw =
  QC.property smithDivisibilityChainLawProperty

smithWitnessesUnimodularLaw :: QC.Property
smithWitnessesUnimodularLaw =
  QC.property smithWitnessesUnimodularLawProperty

smithDiagonalOnlyAgreesWithFullLaw :: QC.Property
smithDiagonalOnlyAgreesWithFullLaw =
  QC.property smithDiagonalOnlyAgreesWithFullLawProperty

bareissRankAgreesWithFieldRankLaw :: QC.Property
bareissRankAgreesWithFieldRankLaw =
  QC.property bareissRankAgreesWithFieldRankLawProperty

bareissDeterminantAgreesWithExteriorLaw :: QC.Property
bareissDeterminantAgreesWithExteriorLaw =
  QC.property bareissDeterminantAgreesWithExteriorLawProperty

smithDiagonalReconstructsInputLawProperty :: IntegerMatrix3 -> QC.Property
smithDiagonalReconstructsInputLawProperty (IntegerMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    smithValue <- smithNormalForm matrixValue
    leftTimesInput <- mult (smithLeft smithValue) matrixValue
    reconstructed <- mult leftTimesInput (smithRight smithValue)
    pure (toListMatrix reconstructed == toListMatrix (smithDiagonal smithValue))

smithDivisibilityChainLawProperty :: IntegerMatrix3 -> QC.Property
smithDivisibilityChainLawProperty (IntegerMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    smithValue <- smithNormalForm matrixValue
    pure (diagonalDivisibility (toListMatrix (smithDiagonal smithValue)))

smithWitnessesUnimodularLawProperty :: IntegerMatrix3 -> QC.Property
smithWitnessesUnimodularLawProperty (IntegerMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    smithValue <- smithNormalForm matrixValue
    leftInverseLeft <- mult (smithLeftInverse smithValue) (smithLeft smithValue)
    leftLeftInverse <- mult (smithLeft smithValue) (smithLeftInverse smithValue)
    rightInverseRight <- mult (smithRightInverse smithValue) (smithRight smithValue)
    rightRightInverse <- mult (smithRight smithValue) (smithRightInverse smithValue)
    let identityEntries = [1, 0, 0, 0, 1, 0, 0, 0, 1]
    pure
      ( toListMatrix leftInverseLeft == identityEntries
          && toListMatrix leftLeftInverse == identityEntries
          && toListMatrix rightInverseRight == identityEntries
          && toListMatrix rightRightInverse == identityEntries
      )

smithDiagonalOnlyAgreesWithFullLawProperty :: IntegerMatrix3 -> QC.Property
smithDiagonalOnlyAgreesWithFullLawProperty (IntegerMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    fullValue <- smithNormalForm matrixValue
    diagonalOnlyValue <- smithDiagonalForm matrixValue
    pure (toListMatrix (smithDiagonal fullValue) == toListMatrix (smithDiagonalMatrix diagonalOnlyValue))

bareissRankAgreesWithFieldRankLawProperty :: IntegerMatrix3 -> QC.Property
bareissRankAgreesWithFieldRankLawProperty (IntegerMatrix3 entries) =
  assertRightProperty $ do
    integerMatrix <- fromListMatrix @3 @3 entries
    rationalMatrix <- fromListMatrix @3 @3 (fromInteger <$> entries :: [Rational])
    integerRank <- bareissRank integerMatrix
    rationalRank <- rank rationalMatrix
    pure (integerRank == rationalRank)

bareissDeterminantAgreesWithExteriorLawProperty :: IntegerMatrix3 -> QC.Property
bareissDeterminantAgreesWithExteriorLawProperty (IntegerMatrix3 entries) =
  assertRightProperty $ do
    integerMatrix <- first show (fromListMatrix @3 @3 entries)
    integerDeterminant <- first show (bareissDeterminant integerMatrix)
    exteriorDeterminant <- first show (exteriorPowerMatrix 3 (matrixRows3 (fromInteger <$> entries :: [Rational])))
    pure (exteriorDeterminant == [[fromInteger integerDeterminant]])

diagonalDivisibility :: [Integer] -> Bool
diagonalDivisibility entries =
  case entries of
    [d0, z01, z02, z10, d1, z12, z20, z21, d2] ->
      all (== 0) [z01, z02, z10, z12, z20, z21]
        && divides d0 d1
        && divides d1 d2
    _ -> False

divides :: Integer -> Integer -> Bool
divides left right =
  left == 0 || right == 0 || right `rem` left == 0
