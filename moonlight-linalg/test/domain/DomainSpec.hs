
module DomainSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Foldable (traverse_)
import Data.List (mapAccumL)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, natVal)
import Moonlight.Core (MoonlightError)
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
import Moonlight.LinAlg.Pure.Domain.Smith.Multimodular (smithDiagonalFormMultimodular)
import Moonlight.LinAlg.Pure.Domain.Smith.Witnessed (smithNormalFormWitnessed)
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Domain"
    [ testCase "smithNormalForm returns diagonal matrix for diagonal input" testSmithDiagonal,
      testCase "smithNormalForm clears off-diagonal entries on simple integer matrix" testSmithClearsOffDiagonal,
      testCase "smithNormalForm enforces divisibility chain" testSmithDivisibilityChain,
      testCase "smithNormalForm witness reconstruction" testSmithWitness,
      testCase "smithNormalForm inverse witnesses survive row and column reductions" testSmithWitnessInversesRowColumn,
      testCase "smithNormalForm inverse witnesses survive divisibility repair" testSmithWitnessInversesDivisibilityRepair,
      testCase "smithDiagonalForm agrees with full Smith invariant factors" testSmithDiagonalOnlyAgreesWithFull,
      testCase "smithDiagonalForm rectangular multimodular fixture agrees with full Smith" testSmithDiagonalRectangularMultimodular,
      testCase "smithDiagonalForm rank-deficient multimodular fixture agrees with full Smith" testSmithDiagonalRankDeficientMultimodular,
      testCase "smithDiagonalForm torsion-rich multimodular fixture agrees with full Smith" testSmithDiagonalTorsionRichMultimodular,
      testCase "smithDiagonalForm adversarial large-determinant fixture agrees with full Smith" testSmithDiagonalLargeDeterminantMultimodular,
      testCase "smithNormalForm invariant factors match the determinantal divisors of the 3x3 minors" testSmithDeterminantalDivisorsThreeByThree,
      testCase "smithNormalForm invariant factors match the determinantal divisors of the 4x4 minors" testSmithDeterminantalDivisorsFourByFour,
      testCase "smithNormalForm degenerate shapes yield an empty invariant-factor list" testSmithZeroDimensionalShapes,
      testCase "smithNormalFormWitnessed rectangular fixture reconstructs and agrees with multimodular diagonal" testSmithWitnessedRectangular,
      testCase "smithNormalFormWitnessed rank-deficient fixture reconstructs and agrees with multimodular diagonal" testSmithWitnessedRankDeficient,
      testCase "smithNormalFormWitnessed torsion-rich fixture reconstructs and agrees with multimodular diagonal" testSmithWitnessedTorsionRich,
      testCase "smithNormalFormWitnessed adversarial large-entry fixture reconstructs and agrees with multimodular diagonal" testSmithWitnessedLargeEntry,
      testCase "smithNormalFormWitnessed nonsingular fast-path fixture reconstructs and agrees with multimodular diagonal" testSmithWitnessedFastPath,
      testCase "Bareiss rank agrees with Rational field rank" testBareissRankAgreesWithRationalRank,
      testCase "Bareiss determinant agrees with Rational exterior determinant" testBareissDeterminantAgreesWithRationalDeterminant,
      testCase "smithNormalForm identity matrix" testSmithIdentity,
      testCase "smithNormalForm zero matrix" testSmithZero
    ]

testSmithDiagonal :: Assertion
testSmithDiagonal =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [2 :: Integer, 0, 0, 4]
        fmap (toListMatrix . smithDiagonal) (smithNormalForm matrixValue)
   in extractRight result (\values -> assertEqual "smith diagonal" [2, 0, 0, 4] values)

testSmithClearsOffDiagonal :: Assertion
testSmithClearsOffDiagonal =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [2 :: Integer, 4, 0, 2]
        fmap (toListMatrix . smithDiagonal) (smithNormalForm matrixValue)
   in extractRight result assertTwoByTwoOffDiagonalZero

testSmithDivisibilityChain :: Assertion
testSmithDivisibilityChain =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [6 :: Integer, 0, 0, 4]
        smithValue <- smithNormalForm matrixValue
        pure (toListMatrix (smithDiagonal smithValue))
   in extractRight result assertTwoByTwoDiagonalDivisibility

testSmithWitness :: Assertion
testSmithWitness =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [2 :: Integer, 4, 0, 2]
        smithValue <- smithNormalForm matrixValue
        let leftMatrix = smithLeft smithValue
            diagonal = smithDiagonal smithValue
            rightMatrix = smithRight smithValue
        la <- mult leftMatrix matrixValue
        lar <- mult la rightMatrix
        let diagValues = toListMatrix diagonal
        pure (diagValues, toListMatrix lar)
   in extractRight result (\(diagValues, reconstructed) ->
        assertEqual "L * A * R must equal diagonal" diagValues reconstructed)

testSmithWitnessInversesRowColumn :: Assertion
testSmithWitnessInversesRowColumn =
  assertSmithWitnessInverses "row and column reductions" [2, 4, 6, 8]

testSmithWitnessInversesDivisibilityRepair :: Assertion
testSmithWitnessInversesDivisibilityRepair =
  assertSmithWitnessInverses "divisibility repair" [6, 0, 0, 4]

testSmithDiagonalOnlyAgreesWithFull :: Assertion
testSmithDiagonalOnlyAgreesWithFull =
  traverse_
    assertDiagonalAgreement
    [ generatedIntegerEntries 3 3 11,
      generatedIntegerEntries 3 3 29,
      [2, 0, 0, 0, 6, 0, 0, 0, 0],
      [2, 4, 6, 1, 2, 3, 0, 0, 0]
    ]
  where
    assertDiagonalAgreement entries =
      let result = do
            matrixValue <- fromListMatrix @3 @3 @Integer entries
            fullValue <- smithNormalForm matrixValue
            diagonalOnly <- smithDiagonalForm matrixValue
            pure (toListMatrix (smithDiagonal fullValue), toListMatrix (smithDiagonalMatrix diagonalOnly))
       in extractRight result $
            \(fullDiagonal, diagonalOnly) ->
              assertEqual "diagonal-only Smith factors" fullDiagonal diagonalOnly

testSmithDiagonalRectangularMultimodular :: Assertion
testSmithDiagonalRectangularMultimodular =
  assertSmithDiagonalAgreement (Proxy @3) (Proxy @4) "rectangular multimodular Smith diagonal" [6, 10, 14, 22, 9, 15, 21, 33, 3, 5, 7, 11]

testSmithDiagonalRankDeficientMultimodular :: Assertion
testSmithDiagonalRankDeficientMultimodular =
  assertSmithDiagonalAgreement (Proxy @4) (Proxy @4) "rank-deficient multimodular Smith diagonal" [4, 8, 12, 16, 6, 12, 18, 24, 10, 20, 30, 40, 0, 0, 0, 0]

testSmithDiagonalTorsionRichMultimodular :: Assertion
testSmithDiagonalTorsionRichMultimodular =
  assertSmithDiagonalAgreement (Proxy @4) (Proxy @4) "torsion-rich multimodular Smith diagonal" [12, 18, 30, 42, 0, 36, 54, 78, 0, 0, 90, 126, 6, 0, 0, 210]

testSmithDiagonalLargeDeterminantMultimodular :: Assertion
testSmithDiagonalLargeDeterminantMultimodular =
  assertSmithDiagonalAgreement (Proxy @3) (Proxy @3) "large-determinant multimodular Smith diagonal" [4294967296, 0, 0, 0, 4294967296, 0, 0, 0, 4294967296]

-- The determinantal divisors are an oracle independent of every Smith route:
-- Delta_k is the gcd of all k x k minors, and d_k = Delta_k / Delta_(k-1).
testSmithDeterminantalDivisorsThreeByThree :: Assertion
testSmithDeterminantalDivisorsThreeByThree =
  traverse_
    assertThreeByThreeCertificate
    [ ("3x3 generated seed 11", generatedIntegerEntries 3 3 11),
      ("3x3 generated seed 29", generatedIntegerEntries 3 3 29),
      ("3x3 torsion diagonal", [2, 0, 0, 0, 6, 0, 0, 0, 0]),
      ("3x3 rank-deficient", [2, 4, 6, 1, 2, 3, 0, 0, 0]),
      ("3x3 large determinant", [4294967296, 0, 0, 0, 4294967296, 0, 0, 0, 4294967296])
    ]
  where
    assertThreeByThreeCertificate (label, entries) =
      let result = do
            matrixValue <- fromListMatrix @3 @3 @Integer entries
            fullValue <- smithNormalForm matrixValue
            firstDivisor <- minorDeterminantGcd (Proxy @1) 3 3 entries
            secondDivisor <- minorDeterminantGcd (Proxy @2) 3 3 entries
            thirdDivisor <- minorDeterminantGcd (Proxy @3) 3 3 entries
            pure
              ( invariantFactorsFromDivisors [firstDivisor, secondDivisor, thirdDivisor],
                diagonalEntriesOf 3 3 (toListMatrix (smithDiagonal fullValue))
              )
       in extractRight result $
            \(determinantalFactors, smithFactors) ->
              assertEqual (label <> ": d_k = Delta_k / Delta_(k-1)") determinantalFactors smithFactors

testSmithDeterminantalDivisorsFourByFour :: Assertion
testSmithDeterminantalDivisorsFourByFour =
  traverse_
    assertFourByFourCertificate
    [ ("4x4 rank-deficient", [4, 8, 12, 16, 6, 12, 18, 24, 10, 20, 30, 40, 0, 0, 0, 0]),
      ("4x4 torsion-rich", [12, 18, 30, 42, 0, 36, 54, 78, 0, 0, 90, 126, 6, 0, 0, 210]),
      ("4x4 generated seed 7", generatedIntegerEntries 4 4 7)
    ]
  where
    assertFourByFourCertificate (label, entries) =
      let result = do
            matrixValue <- fromListMatrix @4 @4 @Integer entries
            fullValue <- smithNormalForm matrixValue
            firstDivisor <- minorDeterminantGcd (Proxy @1) 4 4 entries
            secondDivisor <- minorDeterminantGcd (Proxy @2) 4 4 entries
            thirdDivisor <- minorDeterminantGcd (Proxy @3) 4 4 entries
            fourthDivisor <- minorDeterminantGcd (Proxy @4) 4 4 entries
            pure
              ( invariantFactorsFromDivisors [firstDivisor, secondDivisor, thirdDivisor, fourthDivisor],
                diagonalEntriesOf 4 4 (toListMatrix (smithDiagonal fullValue))
              )
       in extractRight result $
            \(determinantalFactors, smithFactors) ->
              assertEqual (label <> ": d_k = Delta_k / Delta_(k-1)") determinantalFactors smithFactors

testSmithZeroDimensionalShapes :: Assertion
testSmithZeroDimensionalShapes = do
  extractRight (fromListMatrix @0 @3 @Integer [] >>= smithNormalForm) $
    \value -> assertEqual "0x3 Smith diagonal" [] (toListMatrix (smithDiagonal value))
  extractRight (fromListMatrix @3 @0 @Integer [] >>= smithNormalForm) $
    \value -> assertEqual "3x0 Smith diagonal" [] (toListMatrix (smithDiagonal value))
  extractRight (fromListMatrix @0 @0 @Integer [] >>= smithNormalForm) $
    \value -> assertEqual "0x0 Smith diagonal" [] (toListMatrix (smithDiagonal value))

minorDeterminantGcd ::
  forall k.
  KnownNat k =>
  Proxy k ->
  Int ->
  Int ->
  [Integer] ->
  Either MoonlightError Integer
minorDeterminantGcd _ rowCount columnCount entries =
  foldM accumulateDivisor 0 minorSelections
  where
    minorSize = matrixNat (Proxy @k)
    rows = chunkRowsOf columnCount entries
    minorSelections =
      [ (rowSelection, columnSelection)
      | rowSelection <- combinationsOf minorSize [0 .. rowCount - 1],
        columnSelection <- combinationsOf minorSize [0 .. columnCount - 1]
      ]
    accumulateDivisor divisorSoFar (rowSelection, columnSelection) = do
      minorMatrix <-
        fromListMatrix @k @k @Integer
          (concatMap (selectIndexed columnSelection) (selectIndexed rowSelection rows))
      minorDeterminant <- bareissDeterminant minorMatrix
      pure (gcd divisorSoFar (abs minorDeterminant))

invariantFactorsFromDivisors :: [Integer] -> [Integer]
invariantFactorsFromDivisors =
  snd . mapAccumL nextFactor 1
  where
    nextFactor :: Integer -> Integer -> (Integer, Integer)
    nextFactor previousDivisor divisor
      | previousDivisor == 0 || divisor == 0 = (0, 0)
      | otherwise = (divisor, divisor `div` previousDivisor)

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf size values
  | size <= 0 = [[]]
  | otherwise =
      case values of
        [] -> []
        value : rest ->
          fmap (value :) (combinationsOf (size - 1) rest) <> combinationsOf size rest

selectIndexed :: [Int] -> [a] -> [a]
selectIndexed selection =
  fmap snd . filter (\(index, _) -> index `elem` selection) . zip [0 :: Int ..]

diagonalEntriesOf :: Int -> Int -> [Integer] -> [Integer]
diagonalEntriesOf rowCount columnCount entries
  | columnCount <= 0 = []
  | otherwise =
      fmap snd (filter (onDiagonal . fst) (zip [0 :: Int ..] entries))
  where
    onDiagonal index =
      case index `divMod` columnCount of
        (rowIndex, columnIndex) ->
          rowIndex == columnIndex && rowIndex < min rowCount columnCount

testSmithWitnessedRectangular :: Assertion
testSmithWitnessedRectangular =
  assertSmithWitnessedFixture (Proxy @3) (Proxy @4) "rectangular witnessed Smith" [6, 10, 14, 22, 9, 15, 21, 33, 3, 5, 7, 11]

testSmithWitnessedRankDeficient :: Assertion
testSmithWitnessedRankDeficient =
  assertSmithWitnessedFixture (Proxy @4) (Proxy @4) "rank-deficient witnessed Smith" [4, 8, 12, 16, 6, 12, 18, 24, 10, 20, 30, 40, 0, 0, 0, 0]

testSmithWitnessedTorsionRich :: Assertion
testSmithWitnessedTorsionRich =
  assertSmithWitnessedFixture (Proxy @4) (Proxy @4) "torsion-rich witnessed Smith" [12, 18, 30, 42, 0, 36, 54, 78, 0, 0, 90, 126, 6, 0, 0, 210]

testSmithWitnessedLargeEntry :: Assertion
testSmithWitnessedLargeEntry =
  assertSmithWitnessedFixture (Proxy @3) (Proxy @3) "large-entry witnessed Smith" [4294967291, 4294967279, 4294967231, 4294967197, 4294967189, 4294967161, 4294967143, 4294967111, 4294967087]

testSmithWitnessedFastPath :: Assertion
testSmithWitnessedFastPath =
  assertSmithWitnessedFixture (Proxy @26) (Proxy @26) "nonsingular fast-path witnessed Smith" diagonallyDominantEntries
  where
    diagonallyDominantEntries :: [Integer]
    diagonallyDominantEntries =
      [ if rowIndex == columnIndex
          then 26 + fromIntegral (rowIndex `mod` 9)
          else fromIntegral ((rowIndex * 31 + columnIndex * 17) `mod` 3) - 1
        | rowIndex <- [0 .. 25 :: Int],
          columnIndex <- [0 .. 25 :: Int]
      ]

assertSmithDiagonalAgreement ::
  forall r c.
  (KnownNat r, KnownNat c) =>
  Proxy r ->
  Proxy c ->
  String ->
  [Integer] ->
  Assertion
assertSmithDiagonalAgreement _ _ label entries =
  let result = do
        matrixValue <- fromListMatrix @r @c @Integer entries
        fullValue <- smithNormalForm matrixValue
        diagonalOnly <- smithDiagonalForm matrixValue
        multimodular <- smithDiagonalFormMultimodular matrixValue
        pure
          ( toListMatrix (smithDiagonal fullValue),
            toListMatrix (smithDiagonalMatrix diagonalOnly),
            toListMatrix (smithDiagonalMatrix multimodular)
          )
   in extractRight result $
        \(fullDiagonal, diagonalOnly, multimodular) -> do
          assertEqual label fullDiagonal diagonalOnly
          assertEqual (label <> " engine by name") fullDiagonal multimodular

assertSmithWitnessedFixture ::
  forall r c.
  (KnownNat r, KnownNat c) =>
  Proxy r ->
  Proxy c ->
  String ->
  [Integer] ->
  Assertion
assertSmithWitnessedFixture _ _ label entries =
  let rowCount = matrixNat (Proxy @r)
      columnCount = matrixNat (Proxy @c)
      result = do
        matrixValue <- fromListMatrix @r @c @Integer entries
        smithValue <- smithNormalFormWitnessed matrixValue
        multimodular <- smithDiagonalFormMultimodular matrixValue
        leftApplied <- mult (smithLeft smithValue) matrixValue
        reconstructed <- mult leftApplied (smithRight smithValue)
        leftInverseLeft <- mult (smithLeftInverse smithValue) (smithLeft smithValue)
        leftLeftInverse <- mult (smithLeft smithValue) (smithLeftInverse smithValue)
        rightInverseRight <- mult (smithRightInverse smithValue) (smithRight smithValue)
        rightRightInverse <- mult (smithRight smithValue) (smithRightInverse smithValue)
        pure
          ( toListMatrix (smithDiagonal smithValue),
            toListMatrix (smithDiagonalMatrix multimodular),
            toListMatrix reconstructed,
            toListMatrix leftInverseLeft,
            toListMatrix leftLeftInverse,
            toListMatrix rightInverseRight,
            toListMatrix rightRightInverse
          )
   in extractRight result $
        \(witnessDiagonal, multimodularDiagonal, reconstructed, leftInverseLeftEntries, leftLeftInverseEntries, rightInverseRightEntries, rightRightInverseEntries) -> do
          assertEqual (label <> ": L * A * R") witnessDiagonal reconstructed
          assertEqual (label <> ": multimodular diagonal") multimodularDiagonal witnessDiagonal
          assertEqual (label <> ": L^-1 * L") (identityEntries rowCount) leftInverseLeftEntries
          assertEqual (label <> ": L * L^-1") (identityEntries rowCount) leftLeftInverseEntries
          assertEqual (label <> ": R^-1 * R") (identityEntries columnCount) rightInverseRightEntries
          assertEqual (label <> ": R * R^-1") (identityEntries columnCount) rightRightInverseEntries

matrixNat :: forall n. KnownNat n => Proxy n -> Int
matrixNat _ =
  fromIntegral (natVal (Proxy @n))

testBareissRankAgreesWithRationalRank :: Assertion
testBareissRankAgreesWithRationalRank =
  traverse_
    assertRankAgreement
    [ generatedIntegerEntries 3 4 41,
      generatedIntegerEntries 3 4 53,
      [1, 2, 3, 4, 2, 4, 6, 8, 0, 0, 0, 0]
    ]
  where
    assertRankAgreement entries =
      let result = do
            integerMatrix <- fromListMatrix @3 @4 @Integer entries
            rationalMatrix <- fromListMatrix @3 @4 @Rational (fmap fromInteger entries)
            integerRank <- bareissRank integerMatrix
            rationalRank <- rank rationalMatrix
            pure (integerRank, rationalRank)
       in extractRight result $
            \(integerRank, rationalRank) ->
              assertEqual "Bareiss rank must match Rational field rank" rationalRank integerRank

testBareissDeterminantAgreesWithRationalDeterminant :: Assertion
testBareissDeterminantAgreesWithRationalDeterminant =
  traverse_
    assertDeterminantAgreement
    [ generatedIntegerEntries 4 4 67,
      generatedIntegerEntries 4 4 79,
      [1, 2, 3, 4, 2, 4, 6, 8, 3, 6, 9, 12, 0, 0, 0, 0]
    ]
  where
    assertDeterminantAgreement entries =
      let integerResult = do
            integerMatrix <- fromListMatrix @4 @4 @Integer entries
            bareissDeterminant integerMatrix
          rationalRows = chunkRowsOf 4 (fmap fromInteger entries :: [Rational])
          rationalResult = exteriorPowerMatrix 4 rationalRows
       in case (integerResult, rationalResult) of
            (Right integerDeterminant, Right [[rationalDeterminant]]) ->
              assertEqual "Bareiss determinant must match Rational determinant" rationalDeterminant (fromInteger integerDeterminant)
            (Left failure, _) ->
              assertFailure ("Bareiss determinant failed: " <> show failure)
            (_, Left failure) ->
              assertFailure ("Rational exterior determinant failed: " <> show failure)
            (_, Right unexpected) ->
              assertFailure ("Rational exterior determinant was not 1x1: " <> show unexpected)

assertSmithWitnessInverses :: String -> [Integer] -> Assertion
assertSmithWitnessInverses label matrixEntries =
  let result = do
        matrixValue <- fromListMatrix @2 @2 matrixEntries
        smithValue <- smithNormalForm matrixValue
        let leftMatrix = smithLeft smithValue
            diagonalMatrix = smithDiagonal smithValue
            rightMatrix = smithRight smithValue
            leftInverseMatrix = smithLeftInverse smithValue
            rightInverseMatrix = smithRightInverse smithValue
        la <- mult leftMatrix matrixValue
        lar <- mult la rightMatrix
        leftInverseLeft <- mult leftInverseMatrix leftMatrix
        leftLeftInverse <- mult leftMatrix leftInverseMatrix
        rightInverseRight <- mult rightInverseMatrix rightMatrix
        rightRightInverse <- mult rightMatrix rightInverseMatrix
        pure
          ( toListMatrix diagonalMatrix,
            toListMatrix lar,
            toListMatrix leftInverseLeft,
            toListMatrix leftLeftInverse,
            toListMatrix rightInverseRight,
            toListMatrix rightRightInverse
          )
   in extractRight result $
        \(diagonalEntries, reconstructedEntries, leftInverseLeftEntries, leftLeftInverseEntries, rightInverseRightEntries, rightRightInverseEntries) -> do
          assertEqual (label <> ": L * A * R") diagonalEntries reconstructedEntries
          assertEqual (label <> ": L^-1 * L") identityEntries2 leftInverseLeftEntries
          assertEqual (label <> ": L * L^-1") identityEntries2 leftLeftInverseEntries
          assertEqual (label <> ": R^-1 * R") identityEntries2 rightInverseRightEntries
          assertEqual (label <> ": R * R^-1") identityEntries2 rightRightInverseEntries
          assertTwoByTwoDiagonalDivisibility diagonalEntries

identityEntries2 :: [Integer]
identityEntries2 = [1, 0, 0, 1]

identityEntries :: Int -> [Integer]
identityEntries sizeValue =
  [ if rowIndex == columnIndex then 1 else 0
    | rowIndex <- [0 .. sizeValue - 1],
      columnIndex <- [0 .. sizeValue - 1]
  ]

assertTwoByTwoOffDiagonalZero :: [Integer] -> Assertion
assertTwoByTwoOffDiagonalZero values =
  case values of
    [_, offDiagonal01, offDiagonal10, _] ->
      assertBool "off-diagonal entries must be zero" (offDiagonal01 == 0 && offDiagonal10 == 0)
    _ ->
      assertFailure ("expected a 2x2 matrix payload, got " <> show values)

assertTwoByTwoDiagonalDivisibility :: [Integer] -> Assertion
assertTwoByTwoDiagonalDivisibility values =
  case values of
    [d0, offDiagonal01, offDiagonal10, d1] -> do
      assertBool "off-diagonal entries must be zero" (offDiagonal01 == 0 && offDiagonal10 == 0)
      assertBool "d0 must divide d1" (d1 == 0 || d0 == 0 || d1 `mod` d0 == 0)
    _ ->
      assertFailure ("expected a 2x2 diagonal matrix payload, got " <> show values)

testSmithIdentity :: Assertion
testSmithIdentity =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [1 :: Integer, 0, 0, 1]
        fmap (toListMatrix . smithDiagonal) (smithNormalForm matrixValue)
   in extractRight result (\values -> assertEqual "smith identity" [1, 0, 0, 1] values)

testSmithZero :: Assertion
testSmithZero =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [0 :: Integer, 0, 0, 0]
        fmap (toListMatrix . smithDiagonal) (smithNormalForm matrixValue)
   in extractRight result (\values -> assertEqual "smith zero" [0, 0, 0, 0] values)

generatedIntegerEntries :: Int -> Int -> Int -> [Integer]
generatedIntegerEntries rowCount columnCount seedValue =
  [ generatedIntegerEntry seedValue rowIndex columnIndex
    | rowIndex <- [0 .. rowCount - 1],
      columnIndex <- [0 .. columnCount - 1]
  ]

generatedIntegerEntry :: Int -> Int -> Int -> Integer
generatedIntegerEntry seedValue rowIndex columnIndex =
  fromIntegral ((((seedValue + 13 * rowIndex + 23 * columnIndex + 5 * rowIndex * columnIndex) `mod` 17) - 8) :: Int)

chunkRowsOf :: Int -> [a] -> [[a]]
chunkRowsOf columnCount values =
  case values of
    [] -> []
    _ ->
      let (rowValues, restValues) = splitAt columnCount values
       in rowValues : chunkRowsOf columnCount restValues
