module SymmetricSpec
  ( tests,
  )
where

import Moonlight.Algebra.Pure.Module (BilinearSpace (..))
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg
  ( DiagonalizedSymmetric2 (..),
    DiagonalizedSymmetric3 (..),
    Symmetric2 (..),
    Symmetric3 (..),
    Vec2 (..),
    Vec3 (..),
    applySymmetric2,
    applySymmetric3,
    diagonalSymmetric2,
    diagonalSymmetric3,
    diagonalizedSymmetric2ToTensor,
    diagonalizedSymmetric2ToVec2,
    diagonalizedSymmetric3ToTensor,
    diagonalizedSymmetric3ToVec3,
    eigendecomposeSymmetric2,
    eigendecomposeSymmetric2With,
    eigendecomposeSymmetric3,
    eigendecomposeSymmetric3With,
    outerSymmetric2,
    outerSymmetric3,
    symmetric2Entries,
    symmetric2ToMatrix,
    symmetric3Entries,
    symmetric3ToMatrix,
    toListMatrix,
    toListVector,
    vec2FromList,
    vec3FromList,
  )
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Symmetric"
    [ testCase "Symmetric2 accumulates compact entries componentwise" testSymmetric2Monoid,
      testCase "symmetric2ToMatrix expands the upper triangle into a dense matrix" testSymmetric2DenseExpansion,
      testCase "Symmetric2 bilinearForm uses the Frobenius form of the represented matrix" testSymmetric2FrobeniusInnerProduct,
      testCase "eigendecomposeSymmetric2 diagonalizes a symmetric tensor" testSymmetric2EigenDecomposition,
      QC.testProperty "eigendecomposeSymmetric2 reconstructs generated near-degenerate tensors" propSymmetric2GeneratedReconstruction,
      testCase "eigendecomposeSymmetric2With lifts the spectrum into a reusable diagonalized form" testSymmetric2DiagonalizedDecomposition,
      testCase "outerSymmetric2 and applySymmetric2 agree on the induced linear map" testSymmetric2Apply,
      testCase "vec2FromList rejects non-exact payloads" testVec2FromListRejectsNonExactPayload,
      testCase "Semigroup and Monoid accumulate compact entries componentwise" testMonoid,
      testCase "symmetric3ToMatrix expands the upper triangle into a dense matrix" testDenseExpansion,
      testCase "bilinearForm uses the Frobenius form of the represented matrix" testFrobeniusInnerProduct,
      testCase "eigendecomposeSymmetric3 diagonalizes a symmetric tensor" testEigenDecomposition,
      QC.testProperty "eigendecomposeSymmetric3 reconstructs generated near-degenerate tensors" propSymmetric3GeneratedReconstruction,
      testCase "eigendecomposeSymmetric3With lifts the spectrum into a reusable diagonalized form" testDiagonalizedDecomposition,
      testCase "outerSymmetric3 and applySymmetric3 agree on the induced linear map" testApply,
      testCase "vec3FromList rejects non-exact payloads" testVec3FromListRejectsNonExactPayload
    ]

closeTo :: Double -> Double -> Double -> Bool
closeTo tolerance expected actual = abs (expected - actual) <= tolerance

data GeneratedSymmetric2Case = GeneratedSymmetric2Case
  { generatedSymmetric2Tensor :: !(Symmetric2 Double),
    generatedSymmetric2Scale :: !Double
  }
  deriving stock (Show)

instance QC.Arbitrary GeneratedSymmetric2Case where
  arbitrary = do
    scaleValue <- generatedScale
    baseValue <- QC.choose (-3.0, 3.0)
    gapValue <- generatedEigenvalueGap
    angleValue <- QC.choose (-pi, pi)
    let cosineValue = cos angleValue
        sineValue = sin angleValue
        firstAxis = Vec2 cosineValue sineValue
        secondAxis = Vec2 (-sineValue) cosineValue
        firstEigenvalue = scaleValue * (baseValue + gapValue)
        secondEigenvalue = scaleValue * (baseValue - gapValue)
    pure
      GeneratedSymmetric2Case
        { generatedSymmetric2Tensor =
            symmetric2FromEigenFrame firstEigenvalue secondEigenvalue firstAxis secondAxis,
          generatedSymmetric2Scale = scaleValue
        }

data GeneratedSymmetric3Case = GeneratedSymmetric3Case
  { generatedSymmetric3Tensor :: !(Symmetric3 Double),
    generatedSymmetric3Scale :: !Double
  }
  deriving stock (Show)

instance QC.Arbitrary GeneratedSymmetric3Case where
  arbitrary = do
    scaleValue <- generatedScale
    baseValue <- QC.choose (-3.0, 3.0)
    firstGap <- generatedEigenvalueGap
    secondGap <- generatedEigenvalueGap
    spectrumShape <- QC.elements [0 :: Int, 1, 2, 3]
    thetaValue <- QC.choose (-pi, pi)
    phiValue <- QC.choose (-pi, pi)
    let (firstAxis, secondAxis, thirdAxis) = generatedOrthonormalFrame thetaValue phiValue
        (firstEigenvalue, secondEigenvalue, thirdEigenvalue) =
          case spectrumShape of
            0 -> (baseValue, baseValue, baseValue)
            1 -> (baseValue + firstGap, baseValue, baseValue - firstGap)
            2 -> (baseValue + 1.0, baseValue + firstGap, baseValue)
            _ -> (baseValue + 1.0, baseValue + firstGap, baseValue - secondGap)
    pure
      GeneratedSymmetric3Case
        { generatedSymmetric3Tensor =
            symmetric3FromEigenFrame
              (scaleValue * firstEigenvalue)
              (scaleValue * secondEigenvalue)
              (scaleValue * thirdEigenvalue)
              firstAxis
              secondAxis
              thirdAxis,
          generatedSymmetric3Scale = scaleValue
        }

generatedScale :: QC.Gen Double
generatedScale =
  QC.elements [1.0e-12, 1.0e-6, 1.0, 1.0e6, 1.0e12]

generatedEigenvalueGap :: QC.Gen Double
generatedEigenvalueGap =
  QC.elements [0.0, 1.0e-12, 1.0e-10, 1.0e-8, 1.0e-4, 1.0]

generatedOrthonormalFrame :: Double -> Double -> (Vec3, Vec3, Vec3)
generatedOrthonormalFrame thetaValue phiValue =
  let cosineTheta = cos thetaValue
      sineTheta = sin thetaValue
      cosinePhi = cos phiValue
      sinePhi = sin phiValue
   in ( Vec3 cosineTheta sineTheta 0.0,
        Vec3 (-sineTheta * cosinePhi) (cosineTheta * cosinePhi) sinePhi,
        Vec3 (sineTheta * sinePhi) (-cosineTheta * sinePhi) cosinePhi
      )

symmetric2FromEigenFrame :: Double -> Double -> Vec2 -> Vec2 -> Symmetric2 Double
symmetric2FromEigenFrame firstEigenvalue secondEigenvalue firstAxis secondAxis =
  outerSymmetric2 firstEigenvalue firstAxis <> outerSymmetric2 secondEigenvalue secondAxis

symmetric3FromEigenFrame :: Double -> Double -> Double -> Vec3 -> Vec3 -> Vec3 -> Symmetric3 Double
symmetric3FromEigenFrame firstEigenvalue secondEigenvalue thirdEigenvalue firstAxis secondAxis thirdAxis =
  outerSymmetric3 firstEigenvalue firstAxis
    <> outerSymmetric3 secondEigenvalue secondAxis
    <> outerSymmetric3 thirdEigenvalue thirdAxis

propSymmetric2GeneratedReconstruction :: GeneratedSymmetric2Case -> QC.Property
propSymmetric2GeneratedReconstruction generatedCase =
  case eigendecomposeSymmetric2 (generatedSymmetric2Tensor generatedCase) of
    Left err ->
      QC.counterexample ("unexpected symmetric2 decomposition failure: " <> show err) False
    Right (eigenvalues, eigenvectors) ->
      case reconstructSymmetric2 (toListVector eigenvalues) (toListMatrix eigenvectors) of
        Nothing ->
          QC.counterexample "symmetric2 decomposition returned malformed carriers" False
        Just reconstructedTensor ->
          QC.counterexample
            ( "symmetric2 reconstruction="
                <> show reconstructedTensor
                <> ", expected="
                <> show (generatedSymmetric2Tensor generatedCase)
                <> ", scale="
                <> show (generatedSymmetric2Scale generatedCase)
            )
            ( symmetric2Close (generatedSymmetric2Tensor generatedCase) reconstructedTensor
                && orthonormal2 (toListMatrix eigenvectors)
            )

propSymmetric3GeneratedReconstruction :: GeneratedSymmetric3Case -> QC.Property
propSymmetric3GeneratedReconstruction generatedCase =
  case eigendecomposeSymmetric3 (generatedSymmetric3Tensor generatedCase) of
    Left err ->
      QC.counterexample ("unexpected symmetric3 decomposition failure: " <> show err) False
    Right (eigenvalues, eigenvectors) ->
      case reconstructSymmetric3 (toListVector eigenvalues) (toListMatrix eigenvectors) of
        Nothing ->
          QC.counterexample "symmetric3 decomposition returned malformed carriers" False
        Just reconstructedTensor ->
          QC.counterexample
            ( "symmetric3 reconstruction="
                <> show reconstructedTensor
                <> ", expected="
                <> show (generatedSymmetric3Tensor generatedCase)
                <> ", scale="
                <> show (generatedSymmetric3Scale generatedCase)
            )
            ( symmetric3Close (generatedSymmetric3Tensor generatedCase) reconstructedTensor
                && orthonormal3 (toListMatrix eigenvectors)
            )

reconstructSymmetric2 :: [Double] -> [Double] -> Maybe (Symmetric2 Double)
reconstructSymmetric2 eigenvalues eigenvectors =
  case (eigenvalues, eigenvectors) of
    ([firstEigenvalue, secondEigenvalue], [x1, x2, y1, y2]) ->
      Just
        ( symmetric2FromEigenFrame
            firstEigenvalue
            secondEigenvalue
            (Vec2 x1 y1)
            (Vec2 x2 y2)
        )
    _ -> Nothing

reconstructSymmetric3 :: [Double] -> [Double] -> Maybe (Symmetric3 Double)
reconstructSymmetric3 eigenvalues eigenvectors =
  case (eigenvalues, eigenvectors) of
    ([firstEigenvalue, secondEigenvalue, thirdEigenvalue], [x1, x2, x3, y1, y2, y3, z1, z2, z3]) ->
      Just
        ( symmetric3FromEigenFrame
            firstEigenvalue
            secondEigenvalue
            thirdEigenvalue
            (Vec3 x1 y1 z1)
            (Vec3 x2 y2 z2)
            (Vec3 x3 y3 z3)
        )
    _ -> Nothing

symmetric2Close :: Symmetric2 Double -> Symmetric2 Double -> Bool
symmetric2Close expectedTensor actualTensor =
  symmetric2MaxAbsDifference expectedTensor actualTensor <= 1.0e-7 * max 1.0 (symmetric2MaxAbs expectedTensor)

symmetric3Close :: Symmetric3 Double -> Symmetric3 Double -> Bool
symmetric3Close expectedTensor actualTensor =
  symmetric3MaxAbsDifference expectedTensor actualTensor <= 1.0e-7 * max 1.0 (symmetric3MaxAbs expectedTensor)

symmetric2MaxAbs :: Symmetric2 Double -> Double
symmetric2MaxAbs tensorValue =
  maximum (abs <$> [sym2XX tensorValue, sym2XY tensorValue, sym2YY tensorValue])

symmetric3MaxAbs :: Symmetric3 Double -> Double
symmetric3MaxAbs tensorValue =
  maximum (abs <$> [sym3XX tensorValue, sym3XY tensorValue, sym3XZ tensorValue, sym3YY tensorValue, sym3YZ tensorValue, sym3ZZ tensorValue])

symmetric2MaxAbsDifference :: Symmetric2 Double -> Symmetric2 Double -> Double
symmetric2MaxAbsDifference expectedTensor actualTensor =
  maximum
    ( abs
        <$> [ sym2XX expectedTensor - sym2XX actualTensor,
              sym2XY expectedTensor - sym2XY actualTensor,
              sym2YY expectedTensor - sym2YY actualTensor
            ]
    )

symmetric3MaxAbsDifference :: Symmetric3 Double -> Symmetric3 Double -> Double
symmetric3MaxAbsDifference expectedTensor actualTensor =
  maximum
    ( abs
        <$> [ sym3XX expectedTensor - sym3XX actualTensor,
              sym3XY expectedTensor - sym3XY actualTensor,
              sym3XZ expectedTensor - sym3XZ actualTensor,
              sym3YY expectedTensor - sym3YY actualTensor,
              sym3YZ expectedTensor - sym3YZ actualTensor,
              sym3ZZ expectedTensor - sym3ZZ actualTensor
            ]
    )

orthonormal2 :: [Double] -> Bool
orthonormal2 eigenvectors =
  case eigenvectors of
    [x1, x2, y1, y2] ->
      closeTo 1.0e-8 1.0 (x1 * x1 + y1 * y1)
        && closeTo 1.0e-8 1.0 (x2 * x2 + y2 * y2)
        && closeTo 1.0e-8 0.0 (x1 * x2 + y1 * y2)
    _ -> False

orthonormal3 :: [Double] -> Bool
orthonormal3 eigenvectors =
  case eigenvectors of
    [x1, x2, x3, y1, y2, y3, z1, z2, z3] ->
      closeTo 1.0e-8 1.0 (x1 * x1 + y1 * y1 + z1 * z1)
        && closeTo 1.0e-8 1.0 (x2 * x2 + y2 * y2 + z2 * z2)
        && closeTo 1.0e-8 1.0 (x3 * x3 + y3 * y3 + z3 * z3)
        && closeTo 1.0e-8 0.0 (x1 * x2 + y1 * y2 + z1 * z2)
        && closeTo 1.0e-8 0.0 (x1 * x3 + y1 * y3 + z1 * z3)
        && closeTo 1.0e-8 0.0 (x2 * x3 + y2 * y3 + z2 * z3)
    _ -> False

testSymmetric2Monoid :: IO ()
testSymmetric2Monoid =
  let leftValue =
        ( Symmetric2
            { sym2XX = 1.0,
              sym2XY = 2.0,
              sym2YY = 3.0
            } ::
            Symmetric2 Double
        )
      rightValue =
        ( Symmetric2
            { sym2XX = 0.5,
              sym2XY = -1.5,
              sym2YY = 0.25
            } ::
            Symmetric2 Double
        )
   in assertEqual
        "compact storage should add componentwise"
        [1.5, 0.5, 0.5, 3.25]
        (symmetric2Entries (leftValue <> rightValue <> mempty))

testSymmetric2DenseExpansion :: IO ()
testSymmetric2DenseExpansion =
  let tensorValue =
        ( Symmetric2
            { sym2XX = 1.0,
              sym2XY = 2.0,
              sym2YY = 4.0
            } ::
            Symmetric2 Double
        )
   in extractRight
        (symmetric2ToMatrix tensorValue)
        (\matrixValue ->
            assertEqual
              "dense expansion should mirror the upper triangle"
              [1.0, 2.0, 2.0, 4.0]
              (toListMatrix matrixValue)
        )

testSymmetric2FrobeniusInnerProduct :: IO ()
testSymmetric2FrobeniusInnerProduct =
  let leftValue =
        ( Symmetric2
            { sym2XX = 1.0,
              sym2XY = 2.0,
              sym2YY = 3.0
            } ::
            Symmetric2 Double
        )
      rightValue =
        ( Symmetric2
            { sym2XX = 2.0,
              sym2XY = 1.5,
              sym2YY = 1.0
            } ::
            Symmetric2 Double
        )
      expectedValue = 2.0 + 3.0 + 2.0 * 3.0
   in assertBool
        "off-diagonal components should count twice under the Frobenius inner product"
        (closeTo 1.0e-9 expectedValue (bilinearForm leftValue rightValue))

testSymmetric2EigenDecomposition :: IO ()
testSymmetric2EigenDecomposition =
  extractRight
    (eigendecomposeSymmetric2 (diagonalSymmetric2 2.0 5.0))
    (\(eigenvalues, eigenvectors) -> do
        assertEqual "eigenvalues should be sorted descending" [5.0, 2.0] (toListVector eigenvalues)
        assertEqual
          "eigenvectors should be the canonical basis for a diagonal tensor"
          [0.0, 1.0, 1.0, 0.0]
          (toListMatrix eigenvectors)
    )

testSymmetric2DiagonalizedDecomposition :: IO ()
testSymmetric2DiagonalizedDecomposition =
  extractRight
    (eigendecomposeSymmetric2With (Just . length) 0 (diagonalSymmetric2 2.0 5.0))
    (\diagonalizedValue -> do
        assertEqual
          "the lifted decomposition should expose sorted eigenvalues"
          (DiagonalizedSymmetric2 5.0 2.0 4)
          diagonalizedValue
        assertEqual
          "the lifted decomposition should reconstruct the diagonal tensor"
          (diagonalSymmetric2 5.0 2.0 :: Symmetric2 Double)
          (diagonalizedSymmetric2ToTensor diagonalizedValue)
        assertEqual
          "the lifted decomposition should expose the diagonal as a Vec2"
          (Vec2 5.0 2.0)
          (diagonalizedSymmetric2ToVec2 diagonalizedValue)
    )

testSymmetric2Apply :: IO ()
testSymmetric2Apply =
  let tensorValue = outerSymmetric2 2.0 (Vec2 1.0 (-1.0))
      actualValue = applySymmetric2 tensorValue (Vec2 3.0 1.0)
   in assertBool
        "outerSymmetric2 should produce the expected rank-one linear map"
        (actualValue == Vec2 4.0 (-4.0))

testVec2FromListRejectsNonExactPayload :: IO ()
testVec2FromListRejectsNonExactPayload =
  case vec2FromList [1.0, 2.0, 3.0] of
    Left (InvariantViolation _) -> pure ()
    Left err -> assertFailure ("expected Vec2 shape error, got " <> show err)
    Right value -> assertFailure ("expected Vec2 constructor failure, got " <> show value)

testMonoid :: IO ()
testMonoid =
  let leftValue =
        ( Symmetric3
            { sym3XX = 1.0,
              sym3XY = 2.0,
              sym3XZ = 3.0,
              sym3YY = 4.0,
              sym3YZ = 5.0,
              sym3ZZ = 6.0
            } ::
            Symmetric3 Double
        )
      rightValue =
        ( Symmetric3
            { sym3XX = 0.5,
              sym3XY = 1.5,
              sym3XZ = -2.0,
              sym3YY = 0.25,
              sym3YZ = 0.75,
              sym3ZZ = 1.25
            } ::
            Symmetric3 Double
        )
   in assertEqual
        "compact storage should add componentwise"
        [1.5, 3.5, 1.0, 3.5, 4.25, 5.75, 1.0, 5.75, 7.25]
        (symmetric3Entries (leftValue <> rightValue <> mempty))

testDenseExpansion :: IO ()
testDenseExpansion =
  let tensorValue =
        ( Symmetric3
            { sym3XX = 1.0,
              sym3XY = 2.0,
              sym3XZ = 3.0,
              sym3YY = 4.0,
              sym3YZ = 5.0,
              sym3ZZ = 6.0
            } ::
            Symmetric3 Double
        )
   in extractRight
        (symmetric3ToMatrix tensorValue)
        (\matrixValue ->
            assertEqual
              "dense expansion should mirror the upper triangle"
              [1.0, 2.0, 3.0, 2.0, 4.0, 5.0, 3.0, 5.0, 6.0]
              (toListMatrix matrixValue)
        )

testFrobeniusInnerProduct :: IO ()
testFrobeniusInnerProduct =
  let leftValue =
        ( Symmetric3
            { sym3XX = 1.0,
              sym3XY = 2.0,
              sym3XZ = 0.0,
              sym3YY = 3.0,
              sym3YZ = 4.0,
              sym3ZZ = 5.0
            } ::
            Symmetric3 Double
        )
      rightValue =
        ( Symmetric3
            { sym3XX = 2.0,
              sym3XY = 1.5,
              sym3XZ = 0.0,
              sym3YY = 1.0,
              sym3YZ = 0.5,
              sym3ZZ = 4.0
            } ::
            Symmetric3 Double
        )
      expectedValue = 2.0 + 3.0 + 20.0 + 2.0 * (3.0 + 0.0 + 2.0)
   in assertBool
        "off-diagonal components should count twice under the Frobenius inner product"
        (closeTo 1.0e-9 expectedValue (bilinearForm leftValue rightValue))

testEigenDecomposition :: IO ()
testEigenDecomposition =
  extractRight
    (eigendecomposeSymmetric3 (diagonalSymmetric3 2.0 3.0 5.0))
    (\(eigenvalues, eigenvectors) -> do
        assertEqual "eigenvalues should be sorted descending" [5.0, 3.0, 2.0] (toListVector eigenvalues)
        assertEqual
          "eigenvectors should be the canonical basis for a diagonal tensor"
          [0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0]
          (toListMatrix eigenvectors)
    )

testDiagonalizedDecomposition :: IO ()
testDiagonalizedDecomposition =
  extractRight
    (eigendecomposeSymmetric3With (const Nothing) "fallback" (diagonalSymmetric3 2.0 3.0 5.0))
    (\diagonalizedValue -> do
        assertEqual
          "the lifted decomposition should expose sorted eigenvalues and fallback axes"
          (DiagonalizedSymmetric3 5.0 3.0 2.0 "fallback")
          diagonalizedValue
        assertEqual
          "the lifted decomposition should reconstruct the diagonal tensor"
          (diagonalSymmetric3 5.0 3.0 2.0 :: Symmetric3 Double)
          (diagonalizedSymmetric3ToTensor diagonalizedValue)
        assertEqual
          "the lifted decomposition should expose the diagonal as a Vec3"
          (Vec3 5.0 3.0 2.0)
          (diagonalizedSymmetric3ToVec3 diagonalizedValue)
    )

testApply :: IO ()
testApply =
  let tensorValue = outerSymmetric3 2.0 (Vec3 1.0 2.0 (-1.0))
      actualValue = applySymmetric3 tensorValue (Vec3 3.0 0.0 1.0)
   in assertBool
        "outerSymmetric3 should produce the expected rank-one linear map"
        (actualValue == Vec3 4.0 8.0 (-4.0))

testVec3FromListRejectsNonExactPayload :: IO ()
testVec3FromListRejectsNonExactPayload =
  case vec3FromList [1.0, 2.0] of
    Left (InvariantViolation _) -> pure ()
    Left err -> assertFailure ("expected Vec3 shape error, got " <> show err)
    Right value -> assertFailure ("expected Vec3 constructor failure, got " <> show value)
