module Moonlight.LinAlg.Effect.Harness.Sparse
  ( cooCsrRoundTripLaw,
    cooCscRoundTripLaw,
    csrCscTransposeAgreementLaw,
    csrMatVecAgreesWithDenseLaw,
    canonicalCsrCombinesDuplicatesLaw,
    graphLaplacianSymmetricRowSumsZeroLaw,
    selfAdjointCsrRejectsAsymmetryLaw,
  )
where

import Data.Vector.Unboxed qualified as U
import Moonlight.LinAlg
  ( GraphEdge (..),
    canonicalCSRFromEntries,
    cooToCSC,
    cooToCSR,
    cooToDense,
    cscToCSR,
    cscToDense,
    csrMatVecVector,
    csrToCSC,
    csrToDense,
    fromListMatrix,
    graphLaplacianCSR,
    mkSparseCOO,
    selfAdjointCSRLinearOperator,
    toListMatrix,
  )
import Moonlight.LinAlg.Effect.Harness.Core (assertApproxList, assertRightProperty, matrix3VectorProduct)
import Test.Tasty.QuickCheck qualified as QC

newtype SparseEntries3 = SparseEntries3 [(Int, Int, Double)]
  deriving stock (Eq, Show)

newtype DenseVector3 = DenseVector3 [Double]
  deriving stock (Eq, Show)

instance QC.Arbitrary SparseEntries3 where
  arbitrary =
    SparseEntries3
      <$> QC.listOf
        ( (,,)
            <$> QC.chooseInt (0, 2)
            <*> QC.chooseInt (0, 2)
            <*> (fromIntegral <$> QC.chooseInt (-5, 5))
        )

instance QC.Arbitrary DenseVector3 where
  arbitrary =
    DenseVector3
      <$> QC.vectorOf 3 (fromIntegral <$> QC.chooseInt (-5, 5))

cooCsrRoundTripLaw :: QC.Property
cooCsrRoundTripLaw =
  QC.property cooCsrRoundTripLawProperty

cooCscRoundTripLaw :: QC.Property
cooCscRoundTripLaw =
  QC.property cooCscRoundTripLawProperty

csrCscTransposeAgreementLaw :: QC.Property
csrCscTransposeAgreementLaw =
  QC.property csrCscTransposeAgreementLawProperty

csrMatVecAgreesWithDenseLaw :: QC.Property
csrMatVecAgreesWithDenseLaw =
  QC.property csrMatVecAgreesWithDenseLawProperty

cooCsrRoundTripLawProperty :: SparseEntries3 -> QC.Property
cooCsrRoundTripLawProperty (SparseEntries3 entries) =
  assertRightProperty $ do
    cooValue <- mkSparseCOO 3 3 entries
    csrValue <- cooToCSR cooValue
    originalDense <- cooToDense @3 @3 cooValue
    roundTripDense <- csrToDense @3 @3 csrValue
    pure (toListMatrix originalDense == toListMatrix roundTripDense)

cooCscRoundTripLawProperty :: SparseEntries3 -> QC.Property
cooCscRoundTripLawProperty (SparseEntries3 entries) =
  assertRightProperty $ do
    cooValue <- mkSparseCOO 3 3 entries
    cscValue <- cooToCSC cooValue
    originalDense <- cooToDense @3 @3 cooValue
    roundTripDense <- cscToDense @3 @3 cscValue
    pure (toListMatrix originalDense == toListMatrix roundTripDense)

csrCscTransposeAgreementLawProperty :: SparseEntries3 -> QC.Property
csrCscTransposeAgreementLawProperty (SparseEntries3 entries) =
  assertRightProperty $ do
    cooValue <- mkSparseCOO 3 3 entries
    csrValue <- cooToCSR cooValue
    cscValue <- csrToCSC csrValue
    csrRoundTrip <- cscToCSR cscValue
    originalDense <- csrToDense @3 @3 csrValue
    roundTripDense <- csrToDense @3 @3 csrRoundTrip
    pure (toListMatrix originalDense == toListMatrix roundTripDense)

csrMatVecAgreesWithDenseLawProperty :: SparseEntries3 -> DenseVector3 -> QC.Property
csrMatVecAgreesWithDenseLawProperty (SparseEntries3 entries) (DenseVector3 vectorEntries) =
  assertRightProperty $ do
    cooValue <- mkSparseCOO 3 3 entries
    csrValue <- cooToCSR cooValue
    denseMatrix <- csrToDense @3 @3 csrValue
    csrProduct <- csrMatVecVector csrValue (U.fromList vectorEntries)
    pure (assertApproxList (matrix3VectorProduct (rows3 (toListMatrix denseMatrix)) vectorEntries) (U.toList csrProduct))

canonicalCsrCombinesDuplicatesLaw :: QC.Property
canonicalCsrCombinesDuplicatesLaw =
  assertRightProperty $ do
    csrValue <-
      canonicalCSRFromEntries
        2
        3
        ([(0, 1, 2.0), (0, 1, 3.0), (0, 2, 0.0), (1, 0, 5.0), (1, 0, -5.0), (1, 2, 4.0)] :: [(Int, Int, Double)])
    denseMatrix <- csrToDense @2 @3 csrValue
    pure (toListMatrix denseMatrix == [0.0, 5.0, 0.0, 0.0, 0.0, 4.0])

graphLaplacianSymmetricRowSumsZeroLaw :: QC.Property
graphLaplacianSymmetricRowSumsZeroLaw =
  assertRightProperty $ do
    csrValue <-
      graphLaplacianCSR
        ["a", "b", "c"]
        [GraphEdge "a" "b" 1.0, GraphEdge "b" "c" 2.0, GraphEdge "a" "c" 3.0]
    denseMatrix <- csrToDense @3 @3 csrValue
    let rowsValue = rows3 (toListMatrix denseMatrix)
    pure (symmetricRows rowsValue && all (\rowValue -> assertApproxList [0.0] [sum rowValue]) rowsValue)

selfAdjointCsrRejectsAsymmetryLaw :: QC.Property
selfAdjointCsrRejectsAsymmetryLaw =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 ([0.0, 1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0] :: [Double])
    let csrValue = cooToCSR =<< mkSparseCOO 3 3 [(0, 1, 1.0), (1, 2, 2.0)]
        directValue = case csrValue of
          Left _ -> False
          Right value -> case selfAdjointCSRLinearOperator value of
            Left _ -> True
            Right _ -> False
    pure (directValue && toListMatrix matrixValue == [0.0, 1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0])

rows3 :: [a] -> [[a]]
rows3 values =
  case values of
    [a00, a01, a02, a10, a11, a12, a20, a21, a22] ->
      [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]]
    _ -> []

symmetricRows :: [[Double]] -> Bool
symmetricRows rowsValue =
  case rowsValue of
    [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]] ->
      and
        [ a00 == a00,
          assertApproxList [a01] [a10],
          assertApproxList [a02] [a20],
          a11 == a11,
          assertApproxList [a12] [a21],
          a22 == a22
        ]
    _ -> False
