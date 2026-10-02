{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynVector,
    DynMatrix,
    mkDynVector,
    mkDynMatrix,
    dynMatrixFromRows,
    dynMatrixToRows,
    toDynVector,
    toDynMatrix,
    fromDynVector,
    fromDynMatrix,
    withDynVector,
    withDynMatrix,
    dynVectorLength,
    dynMatrixShape,
    dynMatrixDenseRows,
    dynVectorToList,
    dynMatrixToList,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Moonlight.Core
  ( MoonlightError (..),
    checkedNaturalToInt,
  )
import Moonlight.LinAlg.Internal.Storage (checkFlatLength, chunkRows)
import Moonlight.LinAlg.Pure.Dense.Rows
  ( DenseRows,
    denseRowsShape,
    denseRowsToLists,
    mkDenseRows,
    mkDenseRowsFromFlat,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    Vector,
    fromListMatrix,
    fromListVector,
    matrixShape,
    toListVector,
    toListMatrix,
    vectorLength,
  )
import Prelude

type DynVector :: Type -> Type
data DynVector a = DynVector
  { dynVectorLength :: Int,
    dynVectorValues :: [a]
  }

type DynMatrix :: Type -> Type
data DynMatrix a = DynMatrix
  { dynRows :: Int,
    dynCols :: Int,
    dynMatrixValues :: [a]
  }

mkDynVector :: Int -> [a] -> Either MoonlightError (DynVector a)
mkDynVector expected values
  | expected < 0 = Left (InvariantViolation "dynamic vector length must be non-negative")
  | expected /= length values = Left (InvariantViolation "dynamic vector payload length mismatch")
  | otherwise = Right (DynVector expected values)

mkDynMatrix :: Int -> Int -> [a] -> Either MoonlightError (DynMatrix a)
mkDynMatrix rowCount columnCount values = do
  checkFlatLength rowCount columnCount values
  Right (DynMatrix rowCount columnCount values)

dynMatrixFromRows :: [[a]] -> Either MoonlightError (DynMatrix a)
dynMatrixFromRows rowValues = do
  denseRowsValue <- mkDenseRows rowValues
  let (rowCount, columnCount) = denseRowsShape denseRowsValue
  Right
    DynMatrix
      { dynRows = rowCount,
        dynCols = columnCount,
        dynMatrixValues = concat (denseRowsToLists denseRowsValue)
      }

dynMatrixToRows :: DynMatrix a -> Either MoonlightError [[a]]
dynMatrixToRows dynValue = do
  let rowCount = dynRows dynValue
      columnCount = dynCols dynValue
      values = dynMatrixValues dynValue
  checkFlatLength rowCount columnCount values
  if columnCount == 0
    then Right (replicate rowCount [])
    else chunkRows columnCount values

toDynVector :: KnownNat n => Vector n a -> DynVector a
toDynVector vectorValue =
  DynVector
    { dynVectorLength = vectorLength vectorValue,
      dynVectorValues = toListVector vectorValue
    }

toDynMatrix :: (KnownNat r, KnownNat c) => Matrix r c a -> DynMatrix a
toDynMatrix matrixValue =
  let (rowCount, columnCount) = matrixShape matrixValue
   in DynMatrix
        { dynRows = rowCount,
          dynCols = columnCount,
          dynMatrixValues = toListMatrix matrixValue
        }

fromDynVector :: forall n a. KnownNat n => DynVector a -> Either MoonlightError (Vector n a)
fromDynVector dynValue = do
  expected <- checkedStaticDimension @n
  let actual = dynVectorLength dynValue
  if actual /= expected
        then
          Left
            ( InvariantViolation
                ( "dynamic vector shape does not match static dimension: expected "
                    <> show expected
                    <> " but received "
                    <> show actual
                )
            )
        else fromListVector @n (dynVectorValues dynValue)

fromDynMatrix :: forall r c a. (KnownNat r, KnownNat c) => DynMatrix a -> Either MoonlightError (Matrix r c a)
fromDynMatrix dynValue = do
  expectedRows <- checkedStaticDimension @r
  expectedColumns <- checkedStaticDimension @c
  let expected = (expectedRows, expectedColumns)
      actual = dynMatrixShape dynValue
  if actual /= expected
        then
          Left
            ( InvariantViolation
                ( "dynamic matrix shape does not match static dimensions: expected "
                    <> show expected
                    <> " but received "
                    <> show actual
                )
            )
        else fromListMatrix @r @c (dynMatrixValues dynValue)

checkedStaticDimension :: forall n. KnownNat n => Either MoonlightError Int
checkedStaticDimension =
  either
    (const (Left (InvariantViolation "static dimension exceeds Int cardinality")))
    Right
    (checkedNaturalToInt (natVal (Proxy @n)))

withDynVector ::
  forall a b.
  DynVector a ->
  (forall n. KnownNat n => Vector n a -> b) ->
  Either MoonlightError b
withDynVector dynValue callback
  | dynVectorLength dynValue < 0 = Left (InvariantViolation "dynamic vector length must be non-negative")
  | otherwise =
      case someNatVal (fromIntegral (dynVectorLength dynValue)) of
        SomeNat (_proxyN :: Proxy n) ->
          callback <$> (fromListVector (dynVectorValues dynValue) :: Either MoonlightError (Vector n a))

withDynMatrix ::
  forall a b.
  DynMatrix a ->
  (forall r c. (KnownNat r, KnownNat c) => Matrix r c a -> b) ->
  Either MoonlightError b
withDynMatrix dynValue callback
  | dynRows dynValue < 0 || dynCols dynValue < 0 = Left (InvariantViolation "dynamic matrix dimensions must be non-negative")
  | otherwise =
      case someNatVal (fromIntegral (dynRows dynValue)) of
        SomeNat (_proxyR :: Proxy r) ->
          case someNatVal (fromIntegral (dynCols dynValue)) of
            SomeNat (_proxyC :: Proxy c) ->
              callback <$> (fromListMatrix (dynMatrixValues dynValue) :: Either MoonlightError (Matrix r c a))

dynMatrixShape :: DynMatrix a -> (Int, Int)
dynMatrixShape dynValue = (dynRows dynValue, dynCols dynValue)

dynMatrixDenseRows :: DynMatrix a -> Either MoonlightError (DenseRows a)
dynMatrixDenseRows dynValue =
  mkDenseRowsFromFlat
    (dynRows dynValue)
    (dynCols dynValue)
    (dynMatrixValues dynValue)

dynVectorToList :: DynVector a -> [a]
dynVectorToList = dynVectorValues

dynMatrixToList :: DynMatrix a -> [a]
dynMatrixToList = dynMatrixValues
