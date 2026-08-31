module Moonlight.LinAlg.Effect.Harness.Preconditioner
  ( ic0FactorSolveRoundTripLaw,
    ic0RejectsNonpositivePivotLaw,
    preconditionedCgConvergesOnSpdLaw,
  )
where

import Data.Vector.Unboxed qualified as U
import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg
  ( IC0Config (..),
    SparseConjugateGradientConfig (..),
    SparseCSR,
    SparseIterativeFailure (..),
    SparsePreconditionerFamily (..),
    SparseIterativeResult (..),
    cooToCSR,
    csrMatVecVector,
    mkSparseCOO,
    solveSparseCG,
  )
import Moonlight.LinAlg.Effect.Harness.Core
  ( approxTolerance,
    assertApproxList,
    assertRightProperty,
    vectorNorm,
  )
import Test.Tasty.QuickCheck qualified as QC

newtype PositiveDiagonal3 = PositiveDiagonal3 [Double]
  deriving stock (Eq, Show)

newtype RightHandSide3 = RightHandSide3 [Double]
  deriving stock (Eq, Show)

instance QC.Arbitrary PositiveDiagonal3 where
  arbitrary =
    PositiveDiagonal3
      <$> QC.vectorOf 3 (fromIntegral <$> QC.chooseInt (1, 9))

instance QC.Arbitrary RightHandSide3 where
  arbitrary =
    RightHandSide3
      <$> QC.vectorOf 3 (fromIntegral <$> QC.chooseInt (-8, 8))

ic0FactorSolveRoundTripLaw :: QC.Property
ic0FactorSolveRoundTripLaw =
  QC.property ic0FactorSolveRoundTripLawProperty

ic0RejectsNonpositivePivotLaw :: QC.Property
ic0RejectsNonpositivePivotLaw =
  assertRightProperty $ do
    matrixValue <-
      mkSparseCOO
        2
        2
        [(0, 0, 1.0), (0, 1, 2.0), (1, 0, 2.0), (1, 1, 1.0)]
        >>= cooToCSR
    let resultValue =
          solveSparseCG
            (cgConfigWith 8 (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)))
            matrixValue
            (U.fromList [1.0, 1.0])
            (U.fromList [0.0, 0.0])
    pure
      ( case resultValue of
          Left (SparseNonpositivePivot _ _) -> True
          _ -> False
      )

preconditionedCgConvergesOnSpdLaw :: QC.Property
preconditionedCgConvergesOnSpdLaw =
  assertRightProperty $ do
    matrixValue <- anchoredPathLaplacian 16
    let rhsValues = anchoredPathRightHandSide 16
        resultValue =
          solveSparseCG
            (cgConfigWith 128 (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)))
            matrixValue
            rhsValues
            (U.replicate 16 0.0)
    pure
      ( case resultValue of
          Right sparseResult ->
            sparseResidualNorm sparseResult <= scgcTolerance (cgConfigWith 128 (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)))
              && trueResidualNorm matrixValue rhsValues (sparseSolution sparseResult) <= scgcTolerance (cgConfigWith 128 (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)))
          Left _ -> False
      )

ic0FactorSolveRoundTripLawProperty :: PositiveDiagonal3 -> RightHandSide3 -> QC.Property
ic0FactorSolveRoundTripLawProperty (PositiveDiagonal3 diagonalEntries) (RightHandSide3 rhsEntries) =
  assertRightProperty $ do
    matrixValue <- diagonalMatrix3 diagonalEntries
    let rhsValues = U.fromList rhsEntries
        expectedSolution = zipWith (/) rhsEntries diagonalEntries
        resultValue =
          solveSparseCG
            (cgConfigWith 8 (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)))
            matrixValue
            rhsValues
            (U.replicate 3 0.0)
    pure
      ( case resultValue of
          Right sparseResult ->
            assertApproxList expectedSolution (U.toList (sparseSolution sparseResult))
              && trueResidualNorm matrixValue rhsValues (sparseSolution sparseResult) <= scgcTolerance (cgConfigWith 8 (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)))
          Left _ -> False
      )

cgConfigWith :: Int -> SparsePreconditionerFamily -> SparseConjugateGradientConfig
cgConfigWith iterationLimit preconditionerFamily =
  SparseConjugateGradientConfig
    { scgcTolerance = approxTolerance,
      scgcIterationLimit = iterationLimit,
      scgcPreconditionerFamily = preconditionerFamily
    }

diagonalMatrix3 :: [Double] -> Either MoonlightError (SparseCSR Double)
diagonalMatrix3 diagonalEntries =
  mkSparseCOO
    3
    3
    (zipWith (\entryIndex entryValue -> (entryIndex, entryIndex, entryValue)) [0 ..] diagonalEntries)
    >>= cooToCSR

anchoredPathLaplacian :: Int -> Either MoonlightError (SparseCSR Double)
anchoredPathLaplacian dimension =
  mkSparseCOO dimension dimension ((0, 0, 1.0) : concatMap anchoredPathEdgeEntries [0 .. dimension - 2])
    >>= cooToCSR

anchoredPathEdgeEntries :: Int -> [(Int, Int, Double)]
anchoredPathEdgeEntries leftIndex =
  let rightIndex = leftIndex + 1
   in [ (leftIndex, leftIndex, 1.0),
        (leftIndex, rightIndex, -1.0),
        (rightIndex, leftIndex, -1.0),
        (rightIndex, rightIndex, 1.0)
      ]

anchoredPathRightHandSide :: Int -> U.Vector Double
anchoredPathRightHandSide dimension =
  U.generate
    dimension
    ( \indexValue ->
        let entryPhase = fromIntegral (indexValue + 1)
            entrySkew = fromIntegral ((indexValue * 7) `mod` 11)
         in 1.0 + sin entryPhase + 0.125 * entrySkew
    )

trueResidualNorm :: SparseCSR Double -> U.Vector Double -> U.Vector Double -> Double
trueResidualNorm matrixValue rhsValues solutionValues =
  either
    (const (1.0 / 0.0))
    (\productValues -> vectorNorm (U.toList (U.zipWith (-) productValues rhsValues)))
    (csrMatVecVector matrixValue solutionValues)
