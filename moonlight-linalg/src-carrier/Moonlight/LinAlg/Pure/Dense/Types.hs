{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Pure.Dense.Types
  ( Vector,
    Matrix,
    fromListVector,
    fromListMatrix,
    matrixRows,
    toListVector,
    toListMatrix,
    vectorLength,
    matrixShape,
    matrixDenseRows,
    matrixToRows,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Moonlight.Core
  ( MoonlightError (..),
    checkedNaturalToInt,
  )
import Moonlight.LinAlg.Internal.Discrete ()
import Moonlight.LinAlg.Internal.Storage (checkFlatLength)
import Moonlight.LinAlg.Pure.Dense.Rows
  ( DenseRows,
    denseRowsToLists,
    mkDenseRowsFromFlat,
    mkDenseRowsWithShape,
  )
import Prelude

type Vector :: Nat -> Type -> Type
data Vector (n :: Nat) a = Vector
  { vectorDimension :: !Int,
    vectorPayload :: [a]
  }

type Matrix :: Nat -> Nat -> Type -> Type
data Matrix (r :: Nat) (c :: Nat) a = Matrix
  { matrixRowCount :: !Int,
    matrixColumnCount :: !Int,
    matrixPayload :: [a]
  }

checkedTypeLevelDimension :: forall n. KnownNat n => Either MoonlightError Int
checkedTypeLevelDimension =
  either
    (const (Left (InvariantViolation "type-level dimension exceeds Int cardinality")))
    Right
    (checkedNaturalToInt (natVal (Proxy @n)))

fromListVector :: forall n a. KnownNat n => [a] -> Either MoonlightError (Vector n a)
fromListVector values = do
  expectedLength <- checkedTypeLevelDimension @n
  if length values /= expectedLength
    then
      Left
        ( InvariantViolation
            ( "vector length mismatch: expected "
                <> show expectedLength
                <> " values but received "
                <> show (length values)
            )
        )
    else Right (Vector expectedLength values)

fromListMatrix :: forall r c a. (KnownNat r, KnownNat c) => [a] -> Either MoonlightError (Matrix r c a)
fromListMatrix values = do
  rowCount <- checkedTypeLevelDimension @r
  columnCount <- checkedTypeLevelDimension @c
  checkFlatLength rowCount columnCount values
  Right (Matrix rowCount columnCount values)

matrixRows :: forall r c a. (KnownNat r, KnownNat c) => [[a]] -> Either MoonlightError (Matrix r c a)
matrixRows rowValues = do
  rowCount <- checkedTypeLevelDimension @r
  columnCount <- checkedTypeLevelDimension @c
  denseRowsValue <-
    mkDenseRowsWithShape
      rowCount
      columnCount
      rowValues
  Right (Matrix rowCount columnCount (concat (denseRowsToLists denseRowsValue)))

toListVector :: Vector n a -> [a]
toListVector = vectorPayload

toListMatrix :: Matrix r c a -> [a]
toListMatrix = matrixPayload

vectorLength :: forall n a. KnownNat n => Vector n a -> Int
vectorLength vectorValue =
  natVal (Proxy @n) `seq` vectorDimension vectorValue

matrixShape :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> (Int, Int)
matrixShape matrixValue =
  natVal (Proxy @r)
    `seq` natVal (Proxy @c)
    `seq` (matrixRowCount matrixValue, matrixColumnCount matrixValue)

matrixDenseRows :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> Either MoonlightError (DenseRows a)
matrixDenseRows matrixValue =
  let (rowCount, columnCount) = matrixShape matrixValue
   in mkDenseRowsFromFlat
        rowCount
        columnCount
        (toListMatrix matrixValue)

matrixToRows :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> Either MoonlightError [[a]]
matrixToRows =
  fmap denseRowsToLists . matrixDenseRows
