module Moonlight.LinAlg.Effect.Harness.Core
  ( approxTolerance,
    orthonormalTolerance,
    residualTolerance,
    matrix3Product,
    assertApproxEqual,
    assertApproxEqualWith,
    assertApproxList,
    assertApproxListWith,
    assertRightBool,
    assertRightProperty,
    exactRightProperty,
    matrixRows3,
    matrix3VectorProduct,
    maxAbsDifference,
    vectorDot,
    vectorNorm,
  )
where

import Test.Tasty.QuickCheck qualified as QC

approxTolerance :: Double
approxTolerance =
  1.0e-8

residualTolerance :: Double
residualTolerance =
  1.0e-5

orthonormalTolerance :: Double
orthonormalTolerance =
  1.0e-6

assertApproxEqual :: Double -> Double -> Bool
assertApproxEqual expected actual =
  assertApproxEqualWith approxTolerance expected actual

assertApproxEqualWith :: Double -> Double -> Double -> Bool
assertApproxEqualWith tolerance expected actual =
  abs (expected - actual) <= tolerance

assertApproxList :: [Double] -> [Double] -> Bool
assertApproxList expected actual =
  assertApproxListWith approxTolerance expected actual

assertApproxListWith :: Double -> [Double] -> [Double] -> Bool
assertApproxListWith tolerance expected actual =
  length expected == length actual
    && and (zipWith (assertApproxEqualWith tolerance) expected actual)

assertRightBool :: Either failure Bool -> Bool
assertRightBool =
  either (const False) id

exactRightProperty :: (Eq value, Show failure, Show value) => Either failure value -> Either failure value -> QC.Property
exactRightProperty left right =
  case (left, right) of
    (Right leftValue, Right rightValue) ->
      QC.counterexample (show (leftValue, rightValue)) (leftValue == rightValue)
    (Left leftFailure, _) ->
      QC.counterexample (show leftFailure) False
    (_, Left rightFailure) ->
      QC.counterexample (show rightFailure) False

assertRightProperty :: Show failure => Either failure Bool -> QC.Property
assertRightProperty result =
  case result of
    Left failure ->
      QC.counterexample (show failure) False
    Right accepted ->
      QC.property accepted

matrixRows3 :: [a] -> [[a]]
matrixRows3 values =
  case values of
    [a00, a01, a02, a10, a11, a12, a20, a21, a22] ->
      [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]]
    _ -> []

matrix3VectorProduct :: [[Double]] -> [Double] -> [Double]
matrix3VectorProduct rows vectorValue =
  fmap (`vectorDot` vectorValue) rows

matrix3Product :: [[Double]] -> [[Double]] -> [[Double]]
matrix3Product leftRows rightRows =
  let rightColumns = transpose3 rightRows
   in fmap (\leftRow -> fmap (vectorDot leftRow) rightColumns) leftRows

vectorDot :: [Double] -> [Double] -> Double
vectorDot left right =
  sum (zipWith (*) left right)

vectorNorm :: [Double] -> Double
vectorNorm values =
  sqrt (sum ((\entryValue -> entryValue * entryValue) <$> values))

maxAbsDifference :: [Double] -> [Double] -> Double
maxAbsDifference expected actual =
  maximum (0.0 : zipWith (\leftValue rightValue -> abs (leftValue - rightValue)) expected actual)

transpose3 :: [[a]] -> [[a]]
transpose3 rows =
  case rows of
    [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]] ->
      [[a00, a10, a20], [a01, a11, a21], [a02, a12, a22]]
    _ -> []
