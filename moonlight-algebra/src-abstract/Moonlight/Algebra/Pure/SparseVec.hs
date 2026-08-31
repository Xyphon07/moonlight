-- | Finitely-supported sparse vectors keyed by a basis type, plus compiled
-- integer-coordinate sparse kernels for repeated linear substitutions.
--
-- Laws: addition forms an abelian group with the empty vector as identity and
-- scaling is the ring action; the normal form drops all zero entries.
module Moonlight.Algebra.Pure.SparseVec
  ( SparseVec,
    SparseIxVec,
    SparseLinearMap,
    SparseLinearMapCompileError (..),
    SparseLinearMapError (..),
    fromEntries,
    toEntries,
    sparseIxVecFromEntries,
    sparseIxVecToEntries,
    compileSparseLinearMap,
    sparseLinearMapToEntries,
    applySparseLinearMap,
    normalize,
    lookupEntry,
    extendLinear,
  )
where

import Control.Exception (assert)
import Data.List qualified as List
import Data.Kind (Type)
import Data.Maybe (isJust, mapMaybe)
import Data.Vector qualified as BoxedVector
import Data.Vector.Unboxed qualified as UVector
import qualified Data.Map.Strict as Map
import Moonlight.Algebra.Pure.Module (FreeModule (..), Module (..))
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    IsoNorm (..),
    MultiplicativeMonoid (..),
    Ring,
    isoNormalize,
  )

type SparseVec :: Type -> Type -> Type
newtype SparseVec r g = SparseVec (Map.Map g r)
  deriving stock (Eq, Show)

type SparseIxVec :: Type -> Type
data SparseIxVec r = SparseIxVec
  { sivIndices :: !(UVector.Vector Int),
    sivValues :: !(BoxedVector.Vector r)
  }
  deriving stock (Eq, Show)

type SparseLinearMap :: Type -> Type
data SparseLinearMap r = SparseLinearMap
  { slmOffsets :: !(UVector.Vector Int),
    slmTargets :: !(UVector.Vector Int),
    slmValues :: !(BoxedVector.Vector r),
    slmTargetsMonotoneBySource :: !Bool
  }
  deriving stock (Eq, Show)

type SparseLinearMapCompileError :: Type
data SparseLinearMapCompileError
  = SparseLinearMapNegativeSourceCount !Int
  | SparseLinearMapOffsetCountOverflow !Integer
  | SparseLinearMapEntryCountOverflow !Integer
  deriving stock (Eq, Ord, Show)

type SparseLinearMapError :: Type
data SparseLinearMapError
  = SparseLinearMapSourceOutOfBounds !Int
  deriving stock (Eq, Ord, Show)

fromEntries :: (Eq r, AdditiveMonoid r, Ord g) => [(g, r)] -> SparseVec r g
fromEntries =
  SparseVec . Map.mapMaybe normalizeEntry . Map.fromListWith add

toEntries :: SparseVec r g -> [(g, r)]
toEntries (SparseVec entries) = Map.toAscList entries

sparseIxVecFromEntries :: (Eq r, AdditiveMonoid r) => [(Int, r)] -> SparseIxVec r
sparseIxVecFromEntries =
  sparseIxVecFromConsolidatedEntries . consolidateSortedIxEntries . List.sortOn fst
{-# INLINE sparseIxVecFromEntries #-}

sparseIxVecToEntries :: SparseIxVec r -> [(Int, r)]
sparseIxVecToEntries vectorValue =
  zip (UVector.toList (sivIndices vectorValue)) (BoxedVector.toList (sivValues vectorValue))
{-# INLINE sparseIxVecToEntries #-}

sparseLinearMapToEntries :: SparseLinearMap r -> [(Int, Int, r)]
sparseLinearMapToEntries linearMap =
  [ ( sourceIndex,
      unboxedIndexInvariant (slmTargets linearMap) entryOffset,
      boxedIndexInvariant (slmValues linearMap) entryOffset
    )
  | sourceIndex <- [0 .. sparseLinearMapSourceCount linearMap - 1],
    let startOffset = unboxedIndexInvariant (slmOffsets linearMap) sourceIndex,
    let endOffset = unboxedIndexInvariant (slmOffsets linearMap) (sourceIndex + 1),
    entryOffset <- [startOffset .. endOffset - 1]
  ]
{-# INLINE sparseLinearMapToEntries #-}

compileSparseLinearMap ::
  (Eq r, AdditiveMonoid r) =>
  Int ->
  (Int -> [(Int, r)]) ->
  Either SparseLinearMapCompileError (SparseLinearMap r)
compileSparseLinearMap sourceCount substitutionEntries
  | sourceCount < 0 =
      Left (SparseLinearMapNegativeSourceCount sourceCount)
  | requestedOffsetCount > toInteger (maxBound :: Int) =
      Left (SparseLinearMapOffsetCountOverflow requestedOffsetCount)
  | totalEntryCount > toInteger (maxBound :: Int) =
      Left (SparseLinearMapEntryCountOverflow totalEntryCount)
  | otherwise =
      Right
        SparseLinearMap
          { slmOffsets = UVector.fromList (fmap fromInteger offsetIntegers),
            slmTargets = UVector.concat (fmap sivIndices sourceSegments),
            slmValues = BoxedVector.concat (fmap sivValues sourceSegments),
            slmTargetsMonotoneBySource = sparseIxSegmentsAreMonotone sourceSegments
          }
  where
    requestedOffsetCount =
      toInteger sourceCount + 1

    sourceSegments =
      [ sparseIxVecFromEntries (substitutionEntries sourceIndex)
      | sourceIndex <- [0 .. sourceCount - 1]
      ]

    segmentLengths =
      fmap (toInteger . UVector.length . sivIndices) sourceSegments

    offsetIntegers =
      scanl (+) 0 segmentLengths

    totalEntryCount =
      List.foldl' (+) 0 segmentLengths
{-# INLINE compileSparseLinearMap #-}

applySparseLinearMap ::
  (Eq r, Ring r) =>
  SparseLinearMap r ->
  SparseIxVec r ->
  Either SparseLinearMapError (SparseIxVec r)
applySparseLinearMap linearMap vectorValue =
  fmap
    (sparseIxVecFromEmittedEntries linearMap . foldMap emittedEntriesForSource)
    (traverse checkSourceEntry (sparseIxVecToEntries vectorValue))
  where
    sourceCount =
      sparseLinearMapSourceCount linearMap

    checkSourceEntry sourceEntry@(sourceIndex, _)
      | sourceIndex >= 0 && sourceIndex < sourceCount =
          Right sourceEntry
      | otherwise =
          Left (SparseLinearMapSourceOutOfBounds sourceIndex)

    emittedEntriesForSource (sourceIndex, scalar) =
      [ ( unboxedIndexInvariant (slmTargets linearMap) entryOffset,
          mul scalar (boxedIndexInvariant (slmValues linearMap) entryOffset)
        )
      | entryOffset <- [startOffset .. endOffset - 1]
      ]
      where
        startOffset =
          unboxedIndexInvariant (slmOffsets linearMap) sourceIndex

        endOffset =
          unboxedIndexInvariant (slmOffsets linearMap) (sourceIndex + 1)
{-# INLINE applySparseLinearMap #-}

sparseIxVecFromEmittedEntries :: (Eq r, AdditiveMonoid r) => SparseLinearMap r -> [(Int, r)] -> SparseIxVec r
sparseIxVecFromEmittedEntries linearMap
  | slmTargetsMonotoneBySource linearMap =
      sparseIxVecFromConsolidatedEntries . consolidateSortedIxEntries
  | otherwise =
      sparseIxVecFromEntries
{-# INLINE sparseIxVecFromEmittedEntries #-}

sparseIxSegmentsAreMonotone :: [SparseIxVec r] -> Bool
sparseIxSegmentsAreMonotone =
  isJust . List.foldl' boundaryStep (Just Nothing) . mapMaybe sparseIxVecBounds
  where
    boundaryStep :: Maybe (Maybe Int) -> (Int, Int) -> Maybe (Maybe Int)
    boundaryStep Nothing _ = Nothing
    boundaryStep (Just previousLast) (firstTarget, lastTarget)
      | firstTarget <= lastTarget && maybe True (<= firstTarget) previousLast = Just (Just lastTarget)
      | otherwise = Nothing
{-# INLINE sparseIxSegmentsAreMonotone #-}

sparseIxVecBounds :: SparseIxVec r -> Maybe (Int, Int)
sparseIxVecBounds vectorValue =
  (,)
    <$> sivIndices vectorValue UVector.!? 0
    <*> sivIndices vectorValue UVector.!? (UVector.length (sivIndices vectorValue) - 1)
{-# INLINE sparseIxVecBounds #-}

sparseLinearMapSourceCount :: SparseLinearMap r -> Int
sparseLinearMapSourceCount linearMap =
  max 0 (UVector.length (slmOffsets linearMap) - 1)
{-# INLINE sparseLinearMapSourceCount #-}

boxedIndexInvariant :: BoxedVector.Vector a -> Int -> a
boxedIndexInvariant vector index =
  assert (index >= 0 && index < BoxedVector.length vector) $
    BoxedVector.unsafeIndex vector index
{-# INLINE boxedIndexInvariant #-}

unboxedIndexInvariant :: UVector.Unbox a => UVector.Vector a -> Int -> a
unboxedIndexInvariant vector index =
  assert (index >= 0 && index < UVector.length vector) $
    UVector.unsafeIndex vector index
{-# INLINE unboxedIndexInvariant #-}

normalize :: (Eq r, AdditiveMonoid r, Ord g) => SparseVec r g -> SparseVec r g
normalize = isoNormalize

lookupEntry :: (AdditiveMonoid r, Ord g) => g -> SparseVec r g -> r
lookupEntry basisElement (SparseVec entries) = Map.findWithDefault zero basisElement entries

extendLinear ::
  (Eq r, Ring r, Ord targetBasis) =>
  (sourceBasis -> SparseVec r targetBasis) ->
  SparseVec r sourceBasis ->
  SparseVec r targetBasis
extendLinear substituteBasis (SparseVec entries) =
  fromEntries
    ( foldMap
        scaledSubstitutionEntries
        (Map.toAscList entries)
    )
  where
    scaledSubstitutionEntries (basisElement, scalar) =
      fmap
        (\(targetBasis, targetCoefficient) -> (targetBasis, mul scalar targetCoefficient))
        (toEntries (substituteBasis basisElement))

normalizeEntry :: (Eq r, AdditiveMonoid r) => r -> Maybe r
normalizeEntry entryValue
  | entryValue == zero = Nothing
  | otherwise = Just entryValue

combineEntries ::
  (Eq r, AdditiveMonoid r) =>
  key ->
  r ->
  r ->
  Maybe r
combineEntries _key leftValue rightValue =
  normalizeEntry (add leftValue rightValue)

consolidateSortedIxEntries :: (Eq r, AdditiveMonoid r) => [(Int, r)] -> [(Int, r)]
consolidateSortedIxEntries =
  foldr combineEntry []
  where
    combineEntry ::
      (Eq r, AdditiveMonoid r) =>
      (Int, r) ->
      [(Int, r)] ->
      [(Int, r)]
    combineEntry (indexValue, coefficientValue) consolidatedEntries =
      case consolidatedEntries of
        (nextIndex, nextCoefficient) : rest
          | indexValue == nextIndex ->
              maybe rest (\combined -> (indexValue, combined) : rest) (normalizeEntry (add coefficientValue nextCoefficient))
        _ ->
          maybe consolidatedEntries (\nonZero -> (indexValue, nonZero) : consolidatedEntries) (normalizeEntry coefficientValue)
{-# INLINE consolidateSortedIxEntries #-}

sparseIxVecFromConsolidatedEntries :: [(Int, r)] -> SparseIxVec r
sparseIxVecFromConsolidatedEntries entries =
  SparseIxVec
    { sivIndices = UVector.fromList (fmap fst entries),
      sivValues = BoxedVector.fromList (fmap snd entries)
    }
{-# INLINE sparseIxVecFromConsolidatedEntries #-}

instance (Eq r, AdditiveMonoid r, Ord g) => IsoNorm (SparseVec r g) [(g, r)] where
  isoFrom = fromEntries
  isoTo = toEntries

instance (Eq r, AdditiveMonoid r, Ord g) => AdditiveMonoid (SparseVec r g) where
  zero = SparseVec Map.empty
  add (SparseVec left) (SparseVec right) =
    SparseVec (Map.mergeWithKey combineEntries id id left right)

instance (Eq r, AdditiveGroup r, Ord g) => AdditiveGroup (SparseVec r g) where
  neg (SparseVec entries) =
    SparseVec (Map.map neg entries)
  sub left right = add left (neg right)

instance (Eq r, Ring r, Ord g) => Module r (SparseVec r g) where
  scale scalar (SparseVec entries) =
    SparseVec (Map.mapMaybe (normalizeEntry . mul scalar) entries)

instance (Eq r, Ring r, Ord g) => FreeModule r (SparseVec r g) where
  type Basis r (SparseVec r g) = g
  support (SparseVec entries) = Map.keys entries
  coefficient = lookupEntry
  generator basisElement = fromEntries [(basisElement, one)]
