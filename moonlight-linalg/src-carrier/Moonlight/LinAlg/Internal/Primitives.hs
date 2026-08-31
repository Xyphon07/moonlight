{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Internal.Primitives
  ( MatrixIndex,
    RowIndex,
    ColumnIndex,
    mkRowIndex,
    mkColumnIndex,
    rowIndexInt,
    columnIndexInt,
    rowIndices,
    columnIndices,
    natInt,
    epsilon,
    selectAt,
    selectAtIndex,
    requireAt,
    requireAtIndex,
    requireRow,
    requireColumnEntry,
    requireMatrixEntry,
    requireMatrixEntryAt,
    updateAt,
    replaceAt,
    replaceAtIndexChecked,
    replaceAtChecked,
    replaceRowChecked,
    replaceColumnEntryChecked,
    swapAtIndexChecked,
    swapAtChecked,
    swapRowsChecked,
    swapColumnsChecked,
    dotProduct,
    vectorNorm,
    scaleVector,
    addVector,
    subVector,
    matrixVectorProduct,
    matrixSubtract,
    scaleMatrix,
    outerProduct,
    basisVector,
    linearCombination,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, natVal)
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.DenseList (matrixVectorProductWith, outerProductWith, scaleMatrixWith)
import Prelude

type RowAxis :: Type
data RowAxis
type ColumnAxis :: Type
data ColumnAxis

type MatrixIndex :: Type -> Type
newtype MatrixIndex axis = MatrixIndex Int
  deriving stock (Eq, Ord)

instance Show (MatrixIndex axis) where
  show = show . matrixIndexInt

type RowIndex :: Type
type RowIndex = MatrixIndex RowAxis
type ColumnIndex :: Type
type ColumnIndex = MatrixIndex ColumnAxis

natInt :: forall n. KnownNat n => Int
natInt = fromIntegral (natVal (Proxy @n))

epsilon :: Double
epsilon = 1.0e-12

mkIndex :: MoonlightError -> Int -> Int -> Either MoonlightError (MatrixIndex axis)
mkIndex indexError upperBound candidateIndex
  | candidateIndex < 0 = Left indexError
  | candidateIndex >= upperBound = Left indexError
  | otherwise = Right (MatrixIndex candidateIndex)

mkRowIndex :: MoonlightError -> Int -> Int -> Either MoonlightError RowIndex
mkRowIndex = mkIndex

mkColumnIndex :: MoonlightError -> Int -> Int -> Either MoonlightError ColumnIndex
mkColumnIndex = mkIndex

matrixIndexInt :: MatrixIndex axis -> Int
matrixIndexInt (MatrixIndex indexValue) = indexValue

rowIndexInt :: RowIndex -> Int
rowIndexInt = matrixIndexInt

columnIndexInt :: ColumnIndex -> Int
columnIndexInt = matrixIndexInt

rowIndices :: Int -> [RowIndex]
rowIndices rowCount = map MatrixIndex [0 .. rowCount - 1]

columnIndices :: Int -> [ColumnIndex]
columnIndices columnCount = map MatrixIndex [0 .. columnCount - 1]

selectAt :: Int -> [a] -> Maybe a
selectAt targetIndex values
  | targetIndex < 0 = Nothing
  | otherwise =
      case drop targetIndex values of
        value : _ -> Just value
        [] -> Nothing

selectAtIndex :: MatrixIndex axis -> [a] -> Maybe a
selectAtIndex targetIndex = selectAt (matrixIndexInt targetIndex)

requireAt :: MoonlightError -> Int -> [a] -> Either MoonlightError a
requireAt lookupError targetIndex values =
  maybe
    (Left lookupError)
    Right
    (selectAt targetIndex values)

requireAtIndex :: MoonlightError -> MatrixIndex axis -> [a] -> Either MoonlightError a
requireAtIndex lookupError targetIndex values =
  maybe
    (Left lookupError)
    Right
    (selectAtIndex targetIndex values)

requireRow :: MoonlightError -> RowIndex -> [[a]] -> Either MoonlightError [a]
requireRow = requireAtIndex

requireColumnEntry :: MoonlightError -> ColumnIndex -> [a] -> Either MoonlightError a
requireColumnEntry = requireAtIndex

requireMatrixEntry :: MoonlightError -> Int -> Int -> [[a]] -> Either MoonlightError a
requireMatrixEntry lookupError rowIndex columnIndex matrixRows =
  requireAt lookupError rowIndex matrixRows
    >>= requireAt lookupError columnIndex

requireMatrixEntryAt :: MoonlightError -> RowIndex -> ColumnIndex -> [[a]] -> Either MoonlightError a
requireMatrixEntryAt lookupError rowIndex columnIndex matrixRows =
  requireRow lookupError rowIndex matrixRows
    >>= requireColumnEntry lookupError columnIndex

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt targetIndex fn =
  map
    (\(indexValue, value) -> if indexValue == targetIndex then fn value else value)
    . zip [0 :: Int ..]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt targetIndex replacement = updateAt targetIndex (const replacement)

replaceAtIndexChecked :: MoonlightError -> MatrixIndex axis -> a -> [a] -> Either MoonlightError [a]
replaceAtIndexChecked updateError targetIndex replacement values =
  requireAtIndex updateError targetIndex values
    >>= const (Right (replaceAt (matrixIndexInt targetIndex) replacement values))

replaceAtChecked :: MoonlightError -> Int -> a -> [a] -> Either MoonlightError [a]
replaceAtChecked updateError targetIndex replacement values =
  requireAt updateError targetIndex values
    >>= const (Right (replaceAt targetIndex replacement values))

replaceRowChecked :: MoonlightError -> RowIndex -> [a] -> [[a]] -> Either MoonlightError [[a]]
replaceRowChecked = replaceAtIndexChecked

replaceColumnEntryChecked :: MoonlightError -> ColumnIndex -> a -> [a] -> Either MoonlightError [a]
replaceColumnEntryChecked = replaceAtIndexChecked

swapAtIndexChecked :: MoonlightError -> MatrixIndex axis -> MatrixIndex axis -> [a] -> Either MoonlightError [a]
swapAtIndexChecked updateError leftIndex rightIndex values = do
  leftValue <- requireAtIndex updateError leftIndex values
  rightValue <- requireAtIndex updateError rightIndex values
  replaceAtIndexChecked updateError leftIndex rightValue values
    >>= replaceAtIndexChecked updateError rightIndex leftValue

swapAtChecked :: MoonlightError -> Int -> Int -> [a] -> Either MoonlightError [a]
swapAtChecked updateError leftIndex rightIndex values = do
  leftValue <- requireAt updateError leftIndex values
  rightValue <- requireAt updateError rightIndex values
  replaceAtChecked updateError leftIndex rightValue values
    >>= replaceAtChecked updateError rightIndex leftValue

swapRowsChecked :: MoonlightError -> RowIndex -> RowIndex -> [[a]] -> Either MoonlightError [[a]]
swapRowsChecked = swapAtIndexChecked

swapColumnsChecked :: MoonlightError -> ColumnIndex -> ColumnIndex -> [a] -> Either MoonlightError [a]
swapColumnsChecked = swapAtIndexChecked

dotProduct :: [Double] -> [Double] -> Either MoonlightError Double
dotProduct left right =
  go 0.0 left right
  where
    go !accumulator leftValues rightValues =
      case (leftValues, rightValues) of
        ([], []) -> Right accumulator
        (leftValue : leftRest, rightValue : rightRest) ->
          go
            (accumulator + leftValue * rightValue)
            leftRest
            rightRest
        _ ->
          Left
            ( InvariantViolation
                ( "dotProduct: length mismatch (left="
                    <> show (length left)
                    <> ", right="
                    <> show (length right)
                    <> ")"
                )
            )
{-# INLINE dotProduct #-}

vectorNorm :: [Double] -> Either MoonlightError Double
vectorNorm v = fmap sqrt (dotProduct v v)

scaleVector :: Double -> [Double] -> [Double]
scaleVector scalarValue = map (\value -> scalarValue * value)

addVector :: [Double] -> [Double] -> Either MoonlightError [Double]
addVector left right
  | length left /= length right =
      Left (InvariantViolation ("addVector: length mismatch (left=" <> show (length left) <> ", right=" <> show (length right) <> ")"))
  | otherwise = Right (zipWith (+) left right)

subVector :: [Double] -> [Double] -> Either MoonlightError [Double]
subVector left right
  | length left /= length right =
      Left (InvariantViolation ("subVector: length mismatch (left=" <> show (length left) <> ", right=" <> show (length right) <> ")"))
  | otherwise = Right (zipWith (-) left right)

matrixVectorProduct :: [[Double]] -> [Double] -> Either MoonlightError [Double]
matrixVectorProduct matrixRows vectorValue =
  first (\msg -> InvariantViolation ("matrixVectorProduct: " <> msg)) (matrixVectorProductWith (*) (+) 0.0 matrixRows vectorValue)

matrixSubtract :: [[Double]] -> [[Double]] -> Either MoonlightError [[Double]]
matrixSubtract left right
  | length left /= length right =
      Left (InvariantViolation ("matrixSubtract: row count mismatch (left=" <> show (length left) <> ", right=" <> show (length right) <> ")"))
  | otherwise = traverse (\(l, r) -> subVector l r) (zip left right)

scaleMatrix :: Double -> [[Double]] -> [[Double]]
scaleMatrix = scaleMatrixWith (*)

outerProduct :: [Double] -> [Double] -> [[Double]]
outerProduct = outerProductWith (*)

basisVector :: Int -> Int -> [Double]
basisVector size indexValue =
  map (\position -> if position == indexValue then 1.0 else 0.0) [0 .. size - 1]

linearCombination :: [(Double, [Double])] -> Either MoonlightError [Double]
linearCombination [] = Right []
linearCombination ((firstCoefficient, firstVector) : rest) =
  foldM
    (\accumulator (coefficient, vectorValue) -> addVector accumulator (scaleVector coefficient vectorValue))
    (scaleVector firstCoefficient firstVector)
    rest
