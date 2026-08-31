{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.LinAlg.Pure.Dense.Block
  ( BlockMatrixFailure (..),
    invertRationalBlock,
    invertGF2Block,
    invertUnimodularIntegerBlock,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.List (transpose)
import Data.Ratio (denominator, numerator)
import Moonlight.LinAlg.Internal.Discrete (GF2 (..))

-- | Failures for finite dense block inversion. These are obstruction values,
-- not runtime accidents: Schur contraction is only valid when the pivot block is
-- square and invertible in the requested coefficient domain.
type BlockMatrixFailure :: Type
data BlockMatrixFailure
  = BlockMatrixNotSquare !Int ![Int]
  | BlockMatrixSingular !Int
  | BlockMatrixNonUnimodular ![[Rational]]
  | BlockMatrixInverseLawFailed !Int
  deriving stock (Eq, Show)

type FieldBlockOps :: Type -> Type
data FieldBlockOps coefficient = FieldBlockOps
  { fboZero :: !coefficient,
    fboOne :: !coefficient,
    fboAdd :: coefficient -> coefficient -> coefficient,
    fboNegate :: coefficient -> coefficient,
    fboMultiply :: coefficient -> coefficient -> coefficient,
    fboInverse :: coefficient -> Maybe coefficient
  }

invertRationalBlock :: [[Rational]] -> Either BlockMatrixFailure [[Rational]]
invertRationalBlock = invertFieldBlock rationalBlockOps
{-# INLINEABLE invertRationalBlock #-}

invertGF2Block :: [[GF2]] -> Either BlockMatrixFailure [[GF2]]
invertGF2Block = invertFieldBlock gf2BlockOps
{-# INLINEABLE invertGF2Block #-}

invertUnimodularIntegerBlock :: [[Integer]] -> Either BlockMatrixFailure [[Integer]]
invertUnimodularIntegerBlock matrix = do
  dimension <- squareDimension matrix
  rationalInverse <- invertRationalBlock (fmap (fmap toRational) matrix)
  integerInverse <-
    maybe
      (Left (BlockMatrixNonUnimodular rationalInverse))
      Right
      (traverse (traverse rationalToIntegerExact) rationalInverse)
  if matrixProductInteger matrix integerInverse == identityMatrix dimension
      && matrixProductInteger integerInverse matrix == identityMatrix dimension
    then Right integerInverse
    else Left (BlockMatrixInverseLawFailed dimension)
{-# INLINEABLE invertUnimodularIntegerBlock #-}

invertFieldBlock :: Eq coefficient => FieldBlockOps coefficient -> [[coefficient]] -> Either BlockMatrixFailure [[coefficient]]
invertFieldBlock ops matrix = do
  dimension <- squareDimension matrix
  let augmented = zipWith (<>) matrix (identityMatrixWith ops dimension)
  reduced <- foldM (rrefPivotStep ops dimension) augmented [0 .. dimension - 1]
  let leftBlock = fmap (take dimension) reduced
      rightBlock = fmap (drop dimension) reduced
      identity = identityMatrixWith ops dimension
  if leftBlock == identity
      && matrixProductWith ops matrix rightBlock == identity
      && matrixProductWith ops rightBlock matrix == identity
    then Right rightBlock
    else Left (BlockMatrixInverseLawFailed dimension)
{-# INLINEABLE invertFieldBlock #-}

rationalBlockOps :: FieldBlockOps Rational
rationalBlockOps =
  FieldBlockOps
    { fboZero = 0,
      fboOne = 1,
      fboAdd = (+),
      fboNegate = negate,
      fboMultiply = (*),
      fboInverse = \value -> if value == 0 then Nothing else Just (recip value)
    }

gf2BlockOps :: FieldBlockOps GF2
gf2BlockOps =
  FieldBlockOps
    { fboZero = GF2Zero,
      fboOne = GF2One,
      fboAdd = (+),
      fboNegate = id,
      fboMultiply = (*),
      fboInverse = \value -> case value of
        GF2Zero -> Nothing
        GF2One -> Just GF2One
    }

squareDimension :: [[coefficient]] -> Either BlockMatrixFailure Int
squareDimension matrix =
  let rowCount = length matrix
      widths = fmap length matrix
   in if all (== rowCount) widths
        then Right rowCount
        else Left (BlockMatrixNotSquare rowCount widths)
{-# INLINEABLE squareDimension #-}

rrefPivotStep :: Eq coefficient => FieldBlockOps coefficient -> Int -> [[coefficient]] -> Int -> Either BlockMatrixFailure [[coefficient]]
rrefPivotStep ops dimension rows pivotIndex = do
  pivotRowIndex <-
    requireBlockValue
      (BlockMatrixSingular dimension)
      (findPivotRow ops pivotIndex rows)
  swappedRows <- swapRows pivotIndex pivotRowIndex rows
  pivotRow <-
    requireBlockValue
      (BlockMatrixSingular dimension)
      (rowAt pivotIndex swappedRows)
  pivotValue <-
    requireBlockValue
      (BlockMatrixSingular dimension)
      (entryAt pivotIndex pivotRow)
  pivotInverse <-
    requireBlockValue
      (BlockMatrixSingular dimension)
      (fboInverse ops pivotValue)
  let normalizedPivot = fmap (fboMultiply ops pivotInverse) pivotRow
      normalizedRows = replaceRow pivotIndex normalizedPivot swappedRows
  pure (eliminatePivotColumn ops pivotIndex normalizedPivot normalizedRows)
{-# INLINEABLE rrefPivotStep #-}

findPivotRow :: Eq coefficient => FieldBlockOps coefficient -> Int -> [[coefficient]] -> Maybe Int
findPivotRow ops pivotIndex =
  fmap fst
    . findFirst
      ( \(rowIndex, rowValues) ->
          rowIndex >= pivotIndex
            && maybe False (/= fboZero ops) (entryAt pivotIndex rowValues)
      )
    . zip [0 ..]
{-# INLINEABLE findPivotRow #-}

eliminatePivotColumn :: Eq coefficient => FieldBlockOps coefficient -> Int -> [coefficient] -> [[coefficient]] -> [[coefficient]]
eliminatePivotColumn ops pivotIndex normalizedPivot =
  fmap
    ( \(rowIndex, rowValues) ->
        if rowIndex == pivotIndex
          then normalizedPivot
          else
            case entryAt pivotIndex rowValues of
              Nothing -> rowValues
              Just factor
                | factor == fboZero ops -> rowValues
                | otherwise -> subtractMultiple ops factor normalizedPivot rowValues
    )
    . zip [0 ..]
{-# INLINEABLE eliminatePivotColumn #-}

subtractMultiple :: FieldBlockOps coefficient -> coefficient -> [coefficient] -> [coefficient] -> [coefficient]
subtractMultiple ops factor pivotRow targetRow =
  zipWith
    (\targetEntry pivotEntry -> fboAdd ops targetEntry (fboNegate ops (fboMultiply ops factor pivotEntry)))
    targetRow
    pivotRow
{-# INLINEABLE subtractMultiple #-}

swapRows :: Int -> Int -> [[coefficient]] -> Either BlockMatrixFailure [[coefficient]]
swapRows leftIndex rightIndex rows = do
  leftRow <- requireBlockValue (BlockMatrixSingular (length rows)) (rowAt leftIndex rows)
  rightRow <- requireBlockValue (BlockMatrixSingular (length rows)) (rowAt rightIndex rows)
  pure
    ( fmap
        ( \(rowIndex, rowValues) ->
            if rowIndex == leftIndex
              then rightRow
              else
                if rowIndex == rightIndex
                  then leftRow
                  else rowValues
        )
        (zip [0 ..] rows)
    )
{-# INLINEABLE swapRows #-}

replaceRow :: Int -> [coefficient] -> [[coefficient]] -> [[coefficient]]
replaceRow targetIndex replacement =
  fmap
    (\(rowIndex, rowValues) -> if rowIndex == targetIndex then replacement else rowValues)
    . zip [0 ..]
{-# INLINEABLE replaceRow #-}

requireBlockValue :: BlockMatrixFailure -> Maybe value -> Either BlockMatrixFailure value
requireBlockValue failureValue =
  maybe (Left failureValue) Right
{-# INLINEABLE requireBlockValue #-}

rowAt :: Int -> [row] -> Maybe row
rowAt indexValue rows
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue rows of
        rowValue : _ -> Just rowValue
        [] -> Nothing
{-# INLINE rowAt #-}

entryAt :: Int -> [entry] -> Maybe entry
entryAt indexValue entries
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue entries of
        entryValue : _ -> Just entryValue
        [] -> Nothing
{-# INLINE entryAt #-}

identityMatrix :: Num coefficient => Int -> [[coefficient]]
identityMatrix dimension =
  [ [ if rowIndex == columnIndex then 1 else 0
      | columnIndex <- [0 .. dimension - 1]
    ]
    | rowIndex <- [0 .. dimension - 1]
  ]
{-# INLINEABLE identityMatrix #-}

identityMatrixWith :: FieldBlockOps coefficient -> Int -> [[coefficient]]
identityMatrixWith ops dimension =
  [ [ if rowIndex == columnIndex then fboOne ops else fboZero ops
      | columnIndex <- [0 .. dimension - 1]
    ]
    | rowIndex <- [0 .. dimension - 1]
  ]
{-# INLINEABLE identityMatrixWith #-}

matrixProductInteger :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixProductInteger = matrixProductWith integerBlockOps
{-# INLINEABLE matrixProductInteger #-}

integerBlockOps :: FieldBlockOps Integer
integerBlockOps =
  FieldBlockOps
    { fboZero = 0,
      fboOne = 1,
      fboAdd = (+),
      fboNegate = negate,
      fboMultiply = (*),
      fboInverse = \value -> case value of
        1 -> Just 1
        -1 -> Just (-1)
        _ -> Nothing
    }

matrixProductWith :: FieldBlockOps coefficient -> [[coefficient]] -> [[coefficient]] -> [[coefficient]]
matrixProductWith ops left right =
  let rightColumns = transpose right
   in fmap
        ( \leftRow ->
            fmap
              (dotWith ops leftRow)
              rightColumns
        )
        left
{-# INLINEABLE matrixProductWith #-}

dotWith :: FieldBlockOps coefficient -> [coefficient] -> [coefficient] -> coefficient
dotWith ops left right =
  foldl'
    (fboAdd ops)
    (fboZero ops)
    (zipWith (fboMultiply ops) left right)
{-# INLINEABLE dotWith #-}

rationalToIntegerExact :: Rational -> Maybe Integer
rationalToIntegerExact value =
  if denominator value == 1
    then Just (numerator value)
    else Nothing
{-# INLINEABLE rationalToIntegerExact #-}

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate =
  foldr
    (\value rest -> if predicate value then Just value else rest)
    Nothing
{-# INLINE findFirst #-}
