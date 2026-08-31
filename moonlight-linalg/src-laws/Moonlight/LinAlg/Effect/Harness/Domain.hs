module Moonlight.LinAlg.Effect.Harness.Domain
  ( smithDiagonalReconstructsInputLaw,
    smithDivisibilityChainLaw,
    smithWitnessesUnimodularLaw,
    smithDiagonalOnlyAgreesWithFullLaw,
    bareissRankAgreesWithFieldRankLaw,
    bareissDeterminantAgreesWithExteriorLaw,
    exactFeasibilityPrimalCertificateValidLaw,
    exactFeasibilityFarkasCertificateValidLaw,
    exactOptimizationOptimalCertificateValidLaw,
    exactOptimizationUnboundedCertificateValidLaw,
  )
where

import Data.Bifunctor (first)
import Moonlight.LinAlg
  ( FeasibilityDecision (..),
    LinearOptimizationDecision (..),
    bareissDeterminant,
    bareissRank,
    decideLinearFeasibility,
    exactLinearOptimumValue,
    exteriorPowerMatrix,
    fromListMatrix,
    mkDynMatrix,
    mkDynVector,
    mkStandardFeasibility,
    mkStandardLinearProgram,
    mult,
    optimizeLinearProgram,
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
    verifyExactLinearOptimum,
    verifyFarkasCertificate,
    verifyLinearProgramUnboundedWitness,
    verifyNonNegativeSolution,
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

exactFeasibilityPrimalCertificateValidLaw :: QC.Property
exactFeasibilityPrimalCertificateValidLaw =
  QC.property exactFeasibilityPrimalCertificateValidLawProperty

exactFeasibilityFarkasCertificateValidLaw :: QC.Property
exactFeasibilityFarkasCertificateValidLaw =
  QC.property exactFeasibilityFarkasCertificateValidLawProperty

exactOptimizationOptimalCertificateValidLaw :: QC.Property
exactOptimizationOptimalCertificateValidLaw =
  QC.property exactOptimizationOptimalCertificateValidLawProperty

exactOptimizationUnboundedCertificateValidLaw :: QC.Property
exactOptimizationUnboundedCertificateValidLaw =
  QC.property exactOptimizationUnboundedCertificateValidLawProperty

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

exactFeasibilityPrimalCertificateValidLawProperty :: Int -> Int -> QC.Property
exactFeasibilityPrimalCertificateValidLawProperty leftRaw rightRaw =
  assertRightProperty $ do
    let leftValue = fromInteger (abs (toInteger leftRaw) `rem` 17) :: Rational
        rightValue = fromInteger (abs (toInteger rightRaw) `rem` 17) :: Rational
    matrixValue <- first show (mkDynMatrix 2 2 [1, 0, 0, 1])
    rhsValue <- first show (mkDynVector 2 [leftValue, rightValue])
    problem <- first show (mkStandardFeasibility matrixValue rhsValue)
    decision <- first show (decideLinearFeasibility problem)
    pure
      ( case decision of
          Feasible solution -> verifyNonNegativeSolution problem solution
          Infeasible _ -> False
      )

exactFeasibilityFarkasCertificateValidLawProperty :: Int -> QC.Property
exactFeasibilityFarkasCertificateValidLawProperty rawValue =
  assertRightProperty $ do
    let positiveValue = fromInteger (1 + abs (toInteger rawValue) `rem` 17) :: Rational
    matrixValue <- first show (mkDynMatrix 2 2 [1, 0, 0, 1])
    rhsValue <- first show (mkDynVector 2 [negate positiveValue, positiveValue])
    problem <- first show (mkStandardFeasibility matrixValue rhsValue)
    decision <- first show (decideLinearFeasibility problem)
    pure
      ( case decision of
          Feasible _ -> False
          Infeasible certificate -> verifyFarkasCertificate problem certificate
      )

exactOptimizationOptimalCertificateValidLawProperty :: Int -> Int -> Int -> QC.Property
exactOptimizationOptimalCertificateValidLawProperty rhsRaw leftObjectiveRaw rightObjectiveRaw =
  assertRightProperty $ do
    let rhsValue = fromInteger (abs (toInteger rhsRaw) `rem` 17) :: Rational
        leftObjective = fromInteger (toInteger leftObjectiveRaw `rem` 17) :: Rational
        rightObjective = fromInteger (toInteger rightObjectiveRaw `rem` 17) :: Rational
    matrixValue <- first show (mkDynMatrix 1 2 [1, 1])
    rightHandSide <- first show (mkDynVector 1 [rhsValue])
    problem <- first show (mkStandardFeasibility matrixValue rightHandSide)
    objective <- first show (mkDynVector 2 [leftObjective, rightObjective])
    linearProgram <- first show (mkStandardLinearProgram problem objective)
    decision <- first show (optimizeLinearProgram linearProgram)
    pure
      ( case decision of
          LinearProgramOptimal optimum ->
            verifyExactLinearOptimum linearProgram optimum
              && exactLinearOptimumValue optimum == rhsValue * max leftObjective rightObjective
          LinearProgramInfeasible _ -> False
          LinearProgramUnbounded _ -> False
      )

exactOptimizationUnboundedCertificateValidLawProperty :: Int -> QC.Property
exactOptimizationUnboundedCertificateValidLawProperty objectiveRaw =
  assertRightProperty $ do
    let positiveObjective = fromInteger (1 + abs (toInteger objectiveRaw) `rem` 17) :: Rational
    matrixValue <- first show (mkDynMatrix 1 2 [1, -1])
    rightHandSide <- first show (mkDynVector 1 [0])
    problem <- first show (mkStandardFeasibility matrixValue rightHandSide)
    objective <- first show (mkDynVector 2 [positiveObjective, 0])
    linearProgram <- first show (mkStandardLinearProgram problem objective)
    decision <- first show (optimizeLinearProgram linearProgram)
    pure
      ( case decision of
          LinearProgramUnbounded witness -> verifyLinearProgramUnboundedWitness linearProgram witness
          LinearProgramInfeasible _ -> False
          LinearProgramOptimal _ -> False
      )

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
