module ExteriorSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Moonlight.LinAlg
  ( ExteriorBasis (..),
    choose,
    exteriorBasis,
    exteriorPowerMatrix,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "exterior algebra"
    [ testCase "rank of exterior basis is choose n p" testExteriorBasisRank,
      testCase "induced exterior maps compose on a diagonal fixture" testExteriorMapComposition,
      testCase "integer exterior determinant keeps minor signs" testExteriorMinorSigns,
      testCase "degree zero exterior map is rank-one constant" testDegreeZeroExteriorMap,
      testCase "direct exterior kernels agree with recursive minors" testExteriorDirectKernels
    ]

testExteriorBasisRank :: Assertion
testExteriorBasisRank = do
  basis <- expectRight (exteriorBasis 2 4)
  assertEqual "Λ^2(Q^4) has choose 4 2 generators" (fromIntegral (choose 4 2)) (length (ebBasisVectors basis))

testExteriorMapComposition :: Assertion
testExteriorMapComposition = do
  lambdaF <- expectRight (exteriorPowerMatrix 2 ([[2, 0], [0, 3]] :: [[Integer]]))
  lambdaG <- expectRight (exteriorPowerMatrix 2 ([[5, 0], [0, 7]] :: [[Integer]]))
  lambdaGF <- expectRight (exteriorPowerMatrix 2 ([[10, 0], [0, 21]] :: [[Integer]]))
  assertEqual "Λ²(g ∘ f) multiplies the determinant scalars" (multiply1x1 lambdaG lambdaF) lambdaGF

testExteriorMinorSigns :: Assertion
testExteriorMinorSigns = do
  lambdaTwo <- expectRight (exteriorPowerMatrix 2 ([[1, 2], [3, 4]] :: [[Integer]]))
  assertEqual "Λ² records the signed determinant" [[-2]] lambdaTwo

testDegreeZeroExteriorMap :: Assertion
testDegreeZeroExteriorMap = do
  lambdaZero <- expectRight (exteriorPowerMatrix 0 ([[2, 4], [6, 8], [10, 12]] :: [[Integer]]))
  assertEqual "Λ⁰ is the constant rank-one map" [[1]] lambdaZero

testExteriorDirectKernels :: Assertion
testExteriorDirectKernels =
  traverse_
    assertDirectKernel
    [ (2, 4, 11),
      (2, 5, 23),
      (3, 3, 37),
      (3, 5, 41)
    ]
  where
    assertDirectKernel (degree, rankValue, seedValue) = do
      let rows = generatedIntegerRows seedValue rankValue rankValue
      direct <- expectRight (exteriorPowerMatrix degree rows)
      recursive <- expectRight (referenceExteriorPowerMatrix degree rows)
      assertEqual ("Λ^" <> show degree <> " direct minors at rank " <> show rankValue) recursive direct

multiply1x1 :: Num coefficient => [[coefficient]] -> [[coefficient]] -> [[coefficient]]
multiply1x1 left right =
  case (left, right) of
    ([[leftValue]], [[rightValue]]) -> [[leftValue * rightValue]]
    _ -> []

expectRight :: Show failure => Either failure value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left failure -> assertFailure ("unexpected failure: " <> show failure)

generatedIntegerRows :: Int -> Int -> Int -> [[Integer]]
generatedIntegerRows seedValue rowCount columnCount =
  [ [ generatedIntegerEntry seedValue rowIndex columnIndex
      | columnIndex <- [0 .. columnCount - 1]
    ]
    | rowIndex <- [0 .. rowCount - 1]
  ]

generatedIntegerEntry :: Int -> Int -> Int -> Integer
generatedIntegerEntry seedValue rowIndex columnIndex =
  fromIntegral ((((seedValue + 17 * rowIndex + 31 * columnIndex + 7 * rowIndex * columnIndex) `mod` 19) - 9) :: Int)

referenceExteriorPowerMatrix :: Int -> [[Integer]] -> Either String [[Integer]]
referenceExteriorPowerMatrix degree rows =
  case rows of
    [] -> Right []
    firstRow : _ -> do
      targetBasis <- either (Left . show) Right (exteriorBasis degree (length rows))
      sourceBasis <- either (Left . show) Right (exteriorBasis degree (length firstRow))
      traverse
        ( \targetVector ->
            traverse
              (\sourceVector -> referenceMinorDeterminant rows targetVector sourceVector)
              (ebBasisVectors sourceBasis)
        )
        (ebBasisVectors targetBasis)

referenceMinorDeterminant :: [[Integer]] -> [Int] -> [Int] -> Either String Integer
referenceMinorDeterminant rows targetVector sourceVector =
  fmap
    referenceDeterminant
    ( traverse
        ( \targetIndex ->
            traverse
              ( \sourceIndex ->
                  safeIndex targetIndex rows
                    >>= safeIndex sourceIndex
              )
              sourceVector
        )
        targetVector
    )

referenceDeterminant :: [[Integer]] -> Integer
referenceDeterminant matrix =
  case matrix of
    [] -> 1
    [singleRow] ->
      case singleRow of
        [value] -> value
        _ -> 0
    firstRow : remainingRows ->
      sum
        ( fmap
            (\(columnIndex, value) -> referenceSignFor columnIndex * value * referenceDeterminant (referenceRemoveColumn columnIndex remainingRows))
            (zip [0 ..] firstRow)
        )

referenceRemoveColumn :: Int -> [[Integer]] -> [[Integer]]
referenceRemoveColumn columnIndex =
  fmap (fmap snd . filter ((/= columnIndex) . fst) . zip [0 :: Int ..])

referenceSignFor :: Int -> Integer
referenceSignFor columnIndex =
  if even columnIndex then 1 else (-1)

safeIndex :: Int -> [a] -> Either String a
safeIndex targetIndex values =
  case drop targetIndex values of
    value : _ -> Right value
    [] -> Left ("index out of bounds: " <> show targetIndex)
