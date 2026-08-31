{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.LinAlg.Pure.Dense.Exterior
  ( ExteriorBasis (..),
    ExteriorPowerFailure (..),
    choose,
    exteriorBasis,
    exteriorBasisCardinality,
    exteriorPowerMatrix,
    exteriorPowerMatrixWithShape,
  )
where

import Data.Kind (Type)
import Data.Vector qualified as Box

-- | Ordered basis of Λ^p(k^n), represented by increasing source-coordinate
-- subsets. The order is lexicographic and therefore stable across coefficient
-- rings.
type ExteriorBasis :: Type
data ExteriorBasis = ExteriorBasis
  { ebDegree :: !Int,
    ebRank :: !Int,
    ebBasisVectors :: ![[Int]]
  }
  deriving stock (Eq, Ord, Show)

type ExteriorPowerFailure :: Type
data ExteriorPowerFailure
  = ExteriorNegativeDegree !Int
  | ExteriorNegativeSourceRank !Int
  | ExteriorNegativeTargetRank !Int
  | ExteriorMatrixShapeMismatch
      !Int
      !Int
      !Int
      ![Int]
  deriving stock (Eq, Ord, Show)

choose :: Int -> Int -> Integer
choose n k
  | n < 0 || k < 0 || k > n = 0
  | otherwise = choosePositive n (min k (n - k))
  where
    choosePositive :: Int -> Int -> Integer
    choosePositive _ 0 = 1
    choosePositive nValue kValue =
      product [fromIntegral (nValue - kValue + 1) .. fromIntegral nValue]
        `div` product [1 .. fromIntegral kValue]
{-# INLINEABLE choose #-}

exteriorBasis :: Int -> Int -> Either ExteriorPowerFailure ExteriorBasis
exteriorBasis degree rankValue
  | degree < 0 = Left (ExteriorNegativeDegree degree)
  | rankValue < 0 = Left (ExteriorNegativeSourceRank rankValue)
  | otherwise =
      Right
        ExteriorBasis
          { ebDegree = degree,
            ebRank = rankValue,
            ebBasisVectors = combinationsOf degree [0 .. rankValue - 1]
          }
{-# INLINEABLE exteriorBasis #-}

exteriorBasisCardinality :: ExteriorBasis -> Int
exteriorBasisCardinality =
  length . ebBasisVectors
{-# INLINE exteriorBasisCardinality #-}

-- | Infer source/target ranks from a rectangular dense matrix and compute the
-- induced Λ^p matrix. Use 'exteriorPowerMatrixWithShape' when a zero-row matrix
-- must still remember its source rank.
exteriorPowerMatrix :: Num coefficient => Int -> [[coefficient]] -> Either ExteriorPowerFailure [[coefficient]]
exteriorPowerMatrix degree matrix =
  case inferredShape matrix of
    Left failure -> Left failure
    Right (targetRank, sourceRank) ->
      exteriorPowerMatrixWithShape degree targetRank sourceRank matrix
{-# INLINEABLE exteriorPowerMatrix #-}

-- | Compute the induced matrix Λ^p(f) for an explicitly shaped matrix
-- f : k^sourceRank -> k^targetRank. Rows are target coordinates; columns are
-- source coordinates. Entry (I,J) is the determinant of the I×J minor.
exteriorPowerMatrixWithShape ::
  Num coefficient =>
  Int ->
  Int ->
  Int ->
  [[coefficient]] ->
  Either ExteriorPowerFailure [[coefficient]]
exteriorPowerMatrixWithShape degree targetRank sourceRank matrix
  | degree < 0 = Left (ExteriorNegativeDegree degree)
  | sourceRank < 0 = Left (ExteriorNegativeSourceRank sourceRank)
  | targetRank < 0 = Left (ExteriorNegativeTargetRank targetRank)
  | actualShapeRows matrix /= targetRank || any (/= sourceRank) (actualShapeColumns matrix) =
      Left
        ( ExteriorMatrixShapeMismatch
            targetRank
            sourceRank
            (actualShapeRows matrix)
            (actualShapeColumns matrix)
        )
  | otherwise = do
      targetBasis <- exteriorBasisWithRole targetRank
      sourceBasis <- exteriorBasisWithRole sourceRank
      let entries = denseEntries matrix
          shapeFailure =
            ExteriorMatrixShapeMismatch
              targetRank
              sourceRank
              (actualShapeRows matrix)
              (actualShapeColumns matrix)
      traverse
        (\targetVector -> traverse (minorDeterminant shapeFailure sourceRank entries targetVector) (ebBasisVectors sourceBasis))
        (ebBasisVectors targetBasis)
  where
    exteriorBasisWithRole rankValue =
      case exteriorBasis degree rankValue of
        Left (ExteriorNegativeSourceRank badRank) -> Left (ExteriorNegativeTargetRank badRank)
        Left failure -> Left failure
        Right basis -> Right basis
{-# INLINEABLE exteriorPowerMatrixWithShape #-}

inferredShape :: [[coefficient]] -> Either ExteriorPowerFailure (Int, Int)
inferredShape matrix =
  case actualShapeColumns matrix of
    [] -> Right (0, 0)
    firstWidth : widths ->
      if all (== firstWidth) widths
        then Right (length matrix, firstWidth)
        else Left (ExteriorMatrixShapeMismatch (length matrix) firstWidth (length matrix) (firstWidth : widths))

actualShapeRows :: [[coefficient]] -> Int
actualShapeRows =
  length
{-# INLINE actualShapeRows #-}

actualShapeColumns :: [[coefficient]] -> [Int]
actualShapeColumns =
  fmap length
{-# INLINE actualShapeColumns #-}

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf degree values
  | degree < 0 = []
  | otherwise =
      case (degree, values) of
        (0, _) -> [[]]
        (_, []) -> []
        (remaining, value : rest) ->
          fmap (value :) (combinationsOf (remaining - 1) rest)
            <> combinationsOf remaining rest
{-# INLINEABLE combinationsOf #-}

denseEntries :: [[coefficient]] -> Box.Vector coefficient
denseEntries matrix =
  Box.fromList (concat matrix)
{-# INLINE denseEntries #-}

entryAt ::
  ExteriorPowerFailure ->
  Int ->
  Box.Vector coefficient ->
  Int ->
  Int ->
  Either ExteriorPowerFailure coefficient
entryAt failure sourceRank entries targetIndex sourceIndex =
  maybe (Left failure) Right (entries Box.!? (targetIndex * sourceRank + sourceIndex))
{-# INLINE entryAt #-}

minorDeterminant ::
  Num coefficient =>
  ExteriorPowerFailure ->
  Int ->
  Box.Vector coefficient ->
  [Int] ->
  [Int] ->
  Either ExteriorPowerFailure coefficient
minorDeterminant failure sourceRank entries targetVector sourceVector =
  case (targetVector, sourceVector) of
    ([], []) -> Right 1
    ([row0], [column0]) ->
      entryAt failure sourceRank entries row0 column0
    ([row0, row1], [column0, column1]) -> do
      a <- entryAt failure sourceRank entries row0 column0
      b <- entryAt failure sourceRank entries row0 column1
      c <- entryAt failure sourceRank entries row1 column0
      d <- entryAt failure sourceRank entries row1 column1
      Right ((a * d) - (b * c))
    ([row0, row1, row2], [column0, column1, column2]) -> do
      a <- entryAt failure sourceRank entries row0 column0
      b <- entryAt failure sourceRank entries row0 column1
      c <- entryAt failure sourceRank entries row0 column2
      d <- entryAt failure sourceRank entries row1 column0
      e <- entryAt failure sourceRank entries row1 column1
      f <- entryAt failure sourceRank entries row1 column2
      g <- entryAt failure sourceRank entries row2 column0
      h <- entryAt failure sourceRank entries row2 column1
      i <- entryAt failure sourceRank entries row2 column2
      Right ((a * e * i) + (b * f * g) + (c * d * h) - (c * e * g) - (b * d * i) - (a * f * h))
    _ ->
      fmap determinant
        ( traverse
            (\targetIndex -> traverse (entryAt failure sourceRank entries targetIndex) sourceVector)
            targetVector
        )
{-# INLINE minorDeterminant #-}

determinant :: Num coefficient => [[coefficient]] -> coefficient
determinant matrix =
  case matrix of
    [] -> 1
    [singleRow] ->
      case singleRow of
        [value] -> value
        _ -> 0
    firstRow : remainingRows ->
      sum
        ( fmap
            (\(columnIndex, value) -> signFor columnIndex * value * determinant (removeColumn columnIndex remainingRows))
            (zip [0 ..] firstRow)
        )
{-# INLINEABLE determinant #-}

removeColumn :: Int -> [[coefficient]] -> [[coefficient]]
removeColumn columnIndex =
  fmap (fmap snd . filter ((/= columnIndex) . fst) . zip [0 :: Int ..])
{-# INLINE removeColumn #-}

signFor :: Num coefficient => Int -> coefficient
signFor columnIndex =
  if even columnIndex then 1 else (-1)
{-# INLINE signFor #-}
