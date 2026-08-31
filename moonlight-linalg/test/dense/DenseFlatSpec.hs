{-# LANGUAGE RecordWildCards #-}

module DenseFlatSpec (tests) where

import Data.Vector.Storable qualified as S
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Eigen.Residual (ResidualReport (..))
import Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( CertifiedSymmetricEigenResult (..),
    certifySymmetricEigenResult,
    symmetricEigenPairsDenseUnchecked,
  )
import Moonlight.LinAlg.Pure.Dense.Flat (trustedDenseDoubleMatrixRowMajor)
import Moonlight.LinAlg.Dense
  ( denseDoubleMatrixShape,
    denseDoubleMatrixToRows,
    denseDoubleMatrixVectorProduct,
    mkDenseDoubleMatrixRowMajor,
    mkDenseDoubleMatrixRows,
  )
import Moonlight.LinAlg.Native
  ( denseDoubleLinearSolveLapack,
    denseDoubleMatrixProductBlas,
    denseDoubleSymmetricEigenpairsLapack,
  )
import Moonlight.LinAlg.Spectral
  ( eigenpairCount,
    eigenpairResidualNorms,
    eigenpairValues,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Prelude

tests :: TestTree
tests =
  testGroup
    "Dense flat Double matrix"
    [ testCase "row-major constructor rejects invalid payload length" $
        assertEqual
          "shape error"
          (Left (InvariantViolation "dense Double row-major payload length mismatch: expected 6 values but received 5"))
          (mkDenseDoubleMatrixRowMajor 2 3 (S.fromList [1.0 .. 5.0])),
      testCase "row-major constructor rejects non-finite payloads" $
        assertEqual
          "finite payload"
          (Left (InvariantViolation "dense Double row-major payload requires finite entries"))
          (mkDenseDoubleMatrixRowMajor 1 1 (S.fromList [0 / 0])),
      testCase "row-major constructor rejects wrapped shape cardinality" $
        let wrappedDimension = 2 ^ (32 :: Int)
         in assertEqual
              "wrapped shape"
              (Left (InvariantViolation "dense Double matrix dimensions exceed Int cardinality"))
              (mkDenseDoubleMatrixRowMajor wrappedDimension wrappedDimension S.empty),
      testCase "row constructor preserves rectangular shape and projection" $
        assertEqual
          "rows"
          (Right ((2, 3), [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]))
          (fmap (\matrixValue -> (denseDoubleMatrixShape matrixValue, denseDoubleMatrixToRows matrixValue)) (mkDenseDoubleMatrixRows [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])),
      testCase "matrix/vector product uses validated flat storage" $
        assertEqual
          "matvec"
          (Right (S.fromList [140.0, 320.0]))
          ( do
              matrixValue <- mkDenseDoubleMatrixRowMajor 2 3 (S.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
              denseDoubleMatrixVectorProduct matrixValue (S.fromList [10.0, 20.0, 30.0])
          ),
      testCase "matrix/vector product rejects vector shape mismatch" $
        assertEqual
          "shape error"
          (Left (InvariantViolation "dense Double matrix/vector shape mismatch (matrix=(2,3), vector=2)"))
          ( do
              matrixValue <- mkDenseDoubleMatrixRowMajor 2 3 (S.fromList [1.0 .. 6.0])
              denseDoubleMatrixVectorProduct matrixValue (S.fromList [10.0, 20.0])
          ),
      testCase "BLAS matrix product matches reference rows" testDenseDoubleMatrixProductBlas,
      testCase "LAPACK dense solve matches reference solution" testDenseDoubleLinearSolveLapack,
      testCase "LAPACK dense solve rejects singular input" testDenseDoubleLinearSolveSingular,
      testCase "LAPACK dense symmetric eigenpairs are residual certified" testDenseDoubleSymmetricEigenpairsLapack,
      testCase "pure dense symmetric eigen certification is explicit" testPureDenseSymmetricEigenCertification
      , testCase "pure dense symmetric eigen rejects oversized workspace cardinality" testPureDenseSymmetricEigenWorkspaceOverflow
    ]

testDenseDoubleMatrixProductBlas :: Assertion
testDenseDoubleMatrixProductBlas = do
  productResult <-
    case (mkDenseDoubleMatrixRowMajor 2 3 (S.fromList [1.0 .. 6.0]), mkDenseDoubleMatrixRowMajor 3 2 (S.fromList [7.0 .. 12.0])) of
      (Right leftMatrix, Right rightMatrix) ->
        denseDoubleMatrixProductBlas leftMatrix rightMatrix
      (Left err, _) -> pure (Left err)
      (_, Left err) -> pure (Left err)
  assertEqual
    "matrix product rows"
    (Right [[58.0, 64.0], [139.0, 154.0]])
    (denseDoubleMatrixToRows <$> productResult)

testDenseDoubleLinearSolveLapack :: Assertion
testDenseDoubleLinearSolveLapack = do
  solveResult <-
    case mkDenseDoubleMatrixRowMajor 2 2 (S.fromList [3.0, 1.0, 1.0, 2.0]) of
      Left err -> pure (Left err)
      Right matrixValue ->
        denseDoubleLinearSolveLapack matrixValue (S.fromList [9.0, 8.0])
  assertApproxStorableVector "solution" 1.0e-10 (S.fromList [2.0, 3.0]) solveResult

testDenseDoubleLinearSolveSingular :: Assertion
testDenseDoubleLinearSolveSingular = do
  solveResult <-
    case mkDenseDoubleMatrixRowMajor 2 2 (S.fromList [1.0, 2.0, 2.0, 4.0]) of
      Left err -> pure (Left err)
      Right matrixValue ->
        denseDoubleLinearSolveLapack matrixValue (S.fromList [1.0, 2.0])
  assertEqual
    "singular solve"
    (Left (InvariantViolation "LAPACK DGESV detected exact singularity at U diagonal 2"))
    solveResult

testDenseDoubleSymmetricEigenpairsLapack :: Assertion
testDenseDoubleSymmetricEigenpairsLapack = do
  eigenResult <-
    case mkDenseDoubleMatrixRowMajor 2 2 (S.fromList [2.0, 0.0, 0.0, 3.0]) of
      Left err -> pure (Left err)
      Right matrixValue -> denseDoubleSymmetricEigenpairsLapack matrixValue
  case eigenResult of
    Left err -> assertEqual "eigen success" (Right ()) (Left err)
    Right pairs -> do
      assertEqual "eigenpair count" 2 (eigenpairCount pairs)
      assertApproxUnboxedVector "eigenvalues" 1.0e-10 (U.fromList [2.0, 3.0]) (Right (eigenpairValues pairs))
      assertBool
        "residuals stay certified"
        (U.all (<= 1.0e-10) (eigenpairResidualNorms pairs))

testPureDenseSymmetricEigenCertification :: Assertion
testPureDenseSymmetricEigenCertification = do
  let resultValue = do
        matrixValue <- mkDenseDoubleMatrixRowMajor 2 2 (S.fromList [2.0, 0.0, 0.0, 3.0])
        eigenResult <- symmetricEigenPairsDenseUnchecked 2 matrixValue
        case certifySymmetricEigenResult matrixValue eigenResult of
          Left err -> Left (InvariantViolation ("unexpected eigen certification failure: " <> show err))
          Right certified -> Right certified
  case resultValue of
    Left err -> assertEqual "certification success" (Right ()) (Left err)
    Right CertifiedSymmetricEigenResult {certifiedSymmetricEigenResidualReport = ResidualReport {..}} -> do
      assertBool "residual scale stays certified" (residualScaled <= 1.0e7)
      assertBool "orthogonality scale stays certified" (residualOrthogonalityScaled <= 1.0e7)

testPureDenseSymmetricEigenWorkspaceOverflow :: Assertion
testPureDenseSymmetricEigenWorkspaceOverflow =
  let wrappedDimension = 2 ^ (32 :: Int)
   in assertEqual
        "oversized eigensolver workspace"
        (Left (InvariantViolation "symmetric eigen workspace cardinality exceeds Int range"))
        ( symmetricEigenPairsDenseUnchecked
            wrappedDimension
            (trustedDenseDoubleMatrixRowMajor wrappedDimension wrappedDimension S.empty)
        )

assertApproxStorableVector :: String -> Double -> S.Vector Double -> Either MoonlightError (S.Vector Double) -> Assertion
assertApproxStorableVector label tolerance expected actualResult =
  case actualResult of
    Left err -> assertEqual label (Right expected) (Left err)
    Right actual ->
      assertBool
        label
        ( S.length expected == S.length actual
            && S.and (S.zipWith (\left right -> abs (left - right) <= tolerance) expected actual)
        )

assertApproxUnboxedVector :: String -> Double -> U.Vector Double -> Either MoonlightError (U.Vector Double) -> Assertion
assertApproxUnboxedVector label tolerance expected actualResult =
  case actualResult of
    Left err -> assertEqual label (Right expected) (Left err)
    Right actual ->
      assertBool
        label
        ( U.length expected == U.length actual
            && U.and (U.zipWith (\left right -> abs (left - right) <= tolerance) expected actual)
        )
