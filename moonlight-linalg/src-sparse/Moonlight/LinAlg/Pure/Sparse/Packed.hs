{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.LinAlg.Pure.Sparse.Packed
  ( PackedSparseEntry,
    packedSparseEntry,
    packedSparseEntrySourceOffset,
    packedSparseEntryTargetOffset,
    packedSparseEntryCoefficient,
    PackedSparseOperator,
    packedSparseOperatorSourceCardinality,
    packedSparseOperatorTargetCardinality,
    packedSparseOperatorEntryCount,
    packedSparseOperatorEntries,
    PackedSparseOperatorShapeError (..),
    PackedSparseApplyError (..),
    mkPackedSparseOperator,
    applyPackedSparseOperatorDense,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Core (checkedNaturalToInt)
import Numeric.Natural (Natural)

-- | Canonical packed COO-style entry for a sparse operator.
--
-- The entry is source/target oriented because boundary and coboundary
-- operators name their domain and codomain that way. The operator constructor
-- validates every entry against the declared shape before sealing the unboxed
-- vectors.
type PackedSparseEntry :: Type -> Type
data PackedSparseEntry coefficient = PackedSparseEntry
  { pseSourceOffset :: !Int,
    pseTargetOffset :: !Int,
    pseCoefficient :: !coefficient
  }
  deriving stock (Eq, Ord, Show)

packedSparseEntry :: Int -> Int -> coefficient -> PackedSparseEntry coefficient
packedSparseEntry sourceOffsetValue targetOffsetValue coefficientValue =
  PackedSparseEntry
    { pseSourceOffset = sourceOffsetValue,
      pseTargetOffset = targetOffsetValue,
      pseCoefficient = coefficientValue
    }

packedSparseEntrySourceOffset :: PackedSparseEntry coefficient -> Int
packedSparseEntrySourceOffset =
  pseSourceOffset

packedSparseEntryTargetOffset :: PackedSparseEntry coefficient -> Int
packedSparseEntryTargetOffset =
  pseTargetOffset

packedSparseEntryCoefficient :: PackedSparseEntry coefficient -> coefficient
packedSparseEntryCoefficient =
  pseCoefficient

-- | Shape-validated packed sparse operator.
--
-- The constructor is hidden: once built, offset vectors are known in-bounds and
-- zero coefficients have been pruned, so dense apply can stay a pure unboxed
-- vector kernel.
type PackedSparseOperator :: Type -> Type
data PackedSparseOperator coefficient = PackedSparseOperator
  { psoSourceCardinality :: !Int,
    psoTargetCardinality :: !Int,
    psoSourceOffsets :: !(Unboxed.Vector Int),
    psoTargetOffsets :: !(Unboxed.Vector Int),
    psoCoefficients :: !(Unboxed.Vector coefficient)
  }
  deriving stock (Eq, Show)

packedSparseOperatorSourceCardinality :: PackedSparseOperator coefficient -> Int
packedSparseOperatorSourceCardinality =
  psoSourceCardinality

packedSparseOperatorTargetCardinality :: PackedSparseOperator coefficient -> Int
packedSparseOperatorTargetCardinality =
  psoTargetCardinality

packedSparseOperatorEntryCount ::
  Unboxed.Unbox coefficient =>
  PackedSparseOperator coefficient ->
  Int
packedSparseOperatorEntryCount =
  Unboxed.length . psoCoefficients

packedSparseOperatorEntries ::
  Unboxed.Unbox coefficient =>
  PackedSparseOperator coefficient ->
  [PackedSparseEntry coefficient]
packedSparseOperatorEntries packedOperator =
  zipWith3
    packedSparseEntry
    (Unboxed.toList (psoSourceOffsets packedOperator))
    (Unboxed.toList (psoTargetOffsets packedOperator))
    (Unboxed.toList (psoCoefficients packedOperator))

type PackedSparseOperatorShapeError :: Type
data PackedSparseOperatorShapeError
  = PackedSparseCardinalityOutOfBounds !Natural
  | PackedSparseEntryOutOfBounds !Int !Int !Int !Int
  deriving stock (Eq, Show)

type PackedSparseApplyError :: Type
data PackedSparseApplyError
  = PackedSparseInputLengthMismatch !Int !Int
  deriving stock (Eq, Show)

mkPackedSparseOperator ::
  (Eq coefficient, Num coefficient, Unboxed.Unbox coefficient) =>
  Natural ->
  Natural ->
  [PackedSparseEntry coefficient] ->
  Either PackedSparseOperatorShapeError (PackedSparseOperator coefficient)
mkPackedSparseOperator sourceCardinalityValue targetCardinalityValue entries = do
  sourceDimension <- packedSparseCardinalityToInt sourceCardinalityValue
  targetDimension <- packedSparseCardinalityToInt targetCardinalityValue
  case firstOutOfBoundsEntry sourceDimension targetDimension entries of
    Just entryValue -> Left (entryOutOfBoundsError sourceDimension targetDimension entryValue)
    Nothing ->
      let nonzeroEntries = filter ((/= 0) . pseCoefficient) entries
       in Right
            PackedSparseOperator
              { psoSourceCardinality = sourceDimension,
                psoTargetCardinality = targetDimension,
                psoSourceOffsets = Unboxed.fromList (fmap pseSourceOffset nonzeroEntries),
                psoTargetOffsets = Unboxed.fromList (fmap pseTargetOffset nonzeroEntries),
                psoCoefficients = Unboxed.fromList (fmap pseCoefficient nonzeroEntries)
              }

packedSparseCardinalityToInt :: Natural -> Either PackedSparseOperatorShapeError Int
packedSparseCardinalityToInt cardinalityValue =
  first
    (const (PackedSparseCardinalityOutOfBounds cardinalityValue))
    (checkedNaturalToInt cardinalityValue)

applyPackedSparseOperatorDense ::
  (Num coefficient, Unboxed.Unbox coefficient) =>
  PackedSparseOperator coefficient ->
  Unboxed.Vector coefficient ->
  Either PackedSparseApplyError (Unboxed.Vector coefficient)
applyPackedSparseOperatorDense packedOperator sourceVector =
  if Unboxed.length sourceVector == packedSparseOperatorSourceCardinality packedOperator
    then
      Right
        ( Unboxed.accumulate_
            (+)
            (Unboxed.replicate (packedSparseOperatorTargetCardinality packedOperator) 0)
            (psoTargetOffsets packedOperator)
            (Unboxed.zipWith (*) (psoCoefficients packedOperator) (Unboxed.backpermute sourceVector (psoSourceOffsets packedOperator)))
        )
    else
      Left
        ( PackedSparseInputLengthMismatch
            (packedSparseOperatorSourceCardinality packedOperator)
            (Unboxed.length sourceVector)
        )

firstOutOfBoundsEntry :: Int -> Int -> [PackedSparseEntry coefficient] -> Maybe (PackedSparseEntry coefficient)
firstOutOfBoundsEntry sourceDimension targetDimension =
  listToMaybe . filter (not . entryWithinBounds sourceDimension targetDimension)

entryWithinBounds :: Int -> Int -> PackedSparseEntry coefficient -> Bool
entryWithinBounds sourceDimension targetDimension entry =
  pseSourceOffset entry >= 0
    && pseSourceOffset entry < sourceDimension
    && pseTargetOffset entry >= 0
    && pseTargetOffset entry < targetDimension

entryOutOfBoundsError ::
  Int ->
  Int ->
  PackedSparseEntry coefficient ->
  PackedSparseOperatorShapeError
entryOutOfBoundsError sourceDimension targetDimension entry =
  PackedSparseEntryOutOfBounds
    (pseSourceOffset entry)
    (pseTargetOffset entry)
    sourceDimension
    targetDimension
