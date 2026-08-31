{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Plan
  ( ContextPlan (..),
    ContextDenseTablePlan (..),
    ContextMaskPlan (..),
    ContextDenseRowsPlan (..),
    ContextDistributivePlan (..),
    ContextTotalOrderPlan (..),
    ContextBoundedFanPlan (..),
    ContextBooleanPlan (..),
    contextPlanLeq,
    contextPlanJoinKey,
    contextPlanMeetKey,
    contextPlanJoinMeetKeys,
    contextPlanUpperKeys,
    contextPlanLowerKeys,
    contextPlanUpperCoverKeys,
    contextPlanLowerCoverKeys,
    contextPlanMonotonicityTargets,
    ordinalBoundedFanKeyLeq,
    totalOrderKeyRank,
    boundedFanKeyLeq,
    booleanMaskForKey,
    booleanKeyForMask,
  )
where

import Data.Bits
  ( (.&.),
    (.|.),
    bit,
    complement,
    testBit,
    xor,
  )
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.FiniteLattice.Internal.Distributive
  ( ContextDistributivePlan (..),
    distributiveJoinKey,
    distributiveJoinMeetKeys,
    distributiveKeyLeq,
    distributiveLowerCoverKeys,
    distributiveLowerKeys,
    distributiveMeetKey,
    distributiveUpperCoverKeys,
    distributiveUpperKeys,
  )
import Moonlight.FiniteLattice.Internal.Invariant
  ( ContextPlanInvariantError (..),
    invariantLookup,
    unboxedIndexInvariant,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
    ContextKeyTable,
    contextKeySetAll,
    contextKeySetChunkCount,
    contextKeySetDelete,
    contextKeySetEmpty,
    contextKeySetFilter,
    contextKeySetFromKeys,
    contextKeySetIntersectsExcept,
    contextKeySetSingleton,
    contextKeyTableLookup,
  )
import Moonlight.FiniteLattice.Internal.Relation
  ( ContextRowIndex,
    ContextRows,
    contextKeyRelated,
    rowForKey,
    rowJoinKeyMaybe,
    rowMeetKeyMaybe,
  )

type ContextPlan :: Type
data ContextPlan
  = DensePlan !ContextDenseTablePlan
  | MaskPlan !ContextMaskPlan
  | OrdinalTotalOrderPlan !Int
  | TotalOrderPlan !ContextTotalOrderPlan
  | OrdinalBoundedFanPlan !Int
  | BoundedFanPlan !ContextBoundedFanPlan

type ContextDenseTablePlan :: Type
data ContextDenseTablePlan = ContextDenseTablePlan
  { cdtpSize :: !Int,
    cdtpUpperRows :: !ContextRows,
    cdtpLowerRows :: !ContextRows,
    cdtpJoinTable :: !ContextKeyTable,
    cdtpMeetTable :: !ContextKeyTable
  }

type ContextMaskPlan :: Type
data ContextMaskPlan
  = BooleanPlan !ContextBooleanPlan
  | DistributivePlan !ContextDistributivePlan
  | DenseRowsPlan !ContextDenseRowsPlan

type ContextDenseRowsPlan :: Type
data ContextDenseRowsPlan = ContextDenseRowsPlan
  { cdrpUpperRows :: !ContextRows,
    cdrpLowerRows :: !ContextRows,
    cdrpUpperRowIndex :: !ContextRowIndex,
    cdrpLowerRowIndex :: !ContextRowIndex
  }

type ContextTotalOrderPlan :: Type
data ContextTotalOrderPlan = ContextTotalOrderPlan
  { ctoTopKey :: !ContextKey,
    ctoRankByKey :: !(UVector.Vector Int),
    ctoKeyByRank :: !(UVector.Vector Int)
  }

type ContextBoundedFanPlan :: Type
data ContextBoundedFanPlan = ContextBoundedFanPlan
  { cbfSize :: !Int,
    cbfTopKey :: !ContextKey,
    cbfBottomKey :: !ContextKey,
    cbfAtomKeys :: !ContextKeySet,
    cbfAllKeys :: !ContextKeySet
  }

type ContextBooleanPlan :: Type
data ContextBooleanPlan = ContextBooleanPlan
  { cboAtomCount :: !Int,
    cboFullMask :: !Word64,
    cboMaskByKey :: !(UVector.Vector Word64),
    cboKeyByMask :: !(UVector.Vector Int)
  }

contextPlanLeq :: ContextPlan -> ContextKey -> ContextKey -> Bool
contextPlanLeq plan leftKey rightKey =
  case plan of
    DensePlan densePlan ->
      contextKeyRelated (cdtpUpperRows densePlan) leftKey rightKey
    MaskPlan maskPlan ->
      contextMaskPlanLeq maskPlan leftKey rightKey
    OrdinalTotalOrderPlan _ ->
      contextKeyOrdinal leftKey <= contextKeyOrdinal rightKey
    TotalOrderPlan totalOrderPlan ->
      totalOrderKeyRank totalOrderPlan leftKey
        <= totalOrderKeyRank totalOrderPlan rightKey
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanKeyLeq size leftKey rightKey
    BoundedFanPlan fanPlan ->
      boundedFanKeyLeq fanPlan leftKey rightKey
{-# INLINE contextPlanLeq #-}

contextPlanJoinKey :: ContextPlan -> ContextKey -> ContextKey -> ContextKey
contextPlanJoinKey plan leftKey rightKey =
  case plan of
    DensePlan tablePlan ->
      denseTableJoinKey tablePlan leftKey rightKey
    MaskPlan maskPlan ->
      contextMaskPlanJoinKey maskPlan leftKey rightKey
    OrdinalTotalOrderPlan _ ->
      if contextKeyOrdinal leftKey >= contextKeyOrdinal rightKey
            then leftKey
            else rightKey
    TotalOrderPlan totalOrderPlan ->
      if totalOrderKeyRank totalOrderPlan leftKey
            >= totalOrderKeyRank totalOrderPlan rightKey
            then leftKey
            else rightKey
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanJoinKey size leftKey rightKey
    BoundedFanPlan fanPlan ->
      boundedFanJoinKey fanPlan leftKey rightKey
{-# INLINE contextPlanJoinKey #-}

contextPlanMeetKey :: ContextPlan -> ContextKey -> ContextKey -> ContextKey
contextPlanMeetKey plan leftKey rightKey =
  case plan of
    DensePlan tablePlan ->
      denseTableMeetKey tablePlan leftKey rightKey
    MaskPlan maskPlan ->
      contextMaskPlanMeetKey maskPlan leftKey rightKey
    OrdinalTotalOrderPlan _ ->
      if contextKeyOrdinal leftKey <= contextKeyOrdinal rightKey
            then leftKey
            else rightKey
    TotalOrderPlan totalOrderPlan ->
      if totalOrderKeyRank totalOrderPlan leftKey
            <= totalOrderKeyRank totalOrderPlan rightKey
            then leftKey
            else rightKey
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanMeetKey size leftKey rightKey
    BoundedFanPlan fanPlan ->
      boundedFanMeetKey fanPlan leftKey rightKey
{-# INLINE contextPlanMeetKey #-}

contextPlanJoinMeetKeys :: ContextPlan -> ContextKey -> ContextKey -> (ContextKey, ContextKey)
contextPlanJoinMeetKeys plan leftKey rightKey =
  case plan of
    DensePlan tablePlan ->
      denseTableJoinMeetKeys tablePlan leftKey rightKey
    MaskPlan maskPlan ->
      contextMaskPlanJoinMeetKeys maskPlan leftKey rightKey
    OrdinalTotalOrderPlan _ ->
      if contextKeyOrdinal leftKey >= contextKeyOrdinal rightKey
            then (leftKey, rightKey)
            else (rightKey, leftKey)
    TotalOrderPlan totalOrderPlan ->
      let leftRank = totalOrderKeyRank totalOrderPlan leftKey
          rightRank = totalOrderKeyRank totalOrderPlan rightKey
       in if leftRank >= rightRank
                then (leftKey, rightKey)
                else (rightKey, leftKey)
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanJoinMeetKeys size leftKey rightKey
    BoundedFanPlan fanPlan ->
      boundedFanJoinMeetKeys fanPlan leftKey rightKey
{-# INLINE contextPlanJoinMeetKeys #-}

contextPlanUpperKeys :: ContextPlan -> ContextKey -> ContextKeySet
contextPlanUpperKeys plan key =
  case plan of
    DensePlan densePlan ->
      rowForKey (cdtpUpperRows densePlan) key
    MaskPlan maskPlan ->
      contextMaskPlanUpperKeys maskPlan key
    OrdinalTotalOrderPlan size ->
      ordinalTotalOrderKeysFromRank size (contextKeyOrdinal key) (size - 1)
    TotalOrderPlan totalOrderPlan ->
      totalOrderKeysFromRank
        totalOrderPlan
        (totalOrderKeyRank totalOrderPlan key)
        (UVector.length (ctoRankByKey totalOrderPlan) - 1)
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanUpperKeys size key
    BoundedFanPlan fanPlan ->
      boundedFanUpperKeys fanPlan key

contextPlanLowerKeys :: ContextPlan -> ContextKey -> ContextKeySet
contextPlanLowerKeys plan key =
  case plan of
    DensePlan densePlan ->
      rowForKey (cdtpLowerRows densePlan) key
    MaskPlan maskPlan ->
      contextMaskPlanLowerKeys maskPlan key
    OrdinalTotalOrderPlan size ->
      ordinalTotalOrderKeysFromRank size 0 (contextKeyOrdinal key)
    TotalOrderPlan totalOrderPlan ->
      totalOrderKeysFromRank
        totalOrderPlan
        0
        (totalOrderKeyRank totalOrderPlan key)
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanLowerKeys size key
    BoundedFanPlan fanPlan ->
      boundedFanLowerKeys fanPlan key

contextPlanUpperCoverKeys :: ContextPlan -> ContextKey -> ContextKeySet
contextPlanUpperCoverKeys plan key =
  case plan of
    DensePlan densePlan ->
      denseTableUpperCoverKeys densePlan key
    MaskPlan maskPlan ->
      contextMaskPlanUpperCoverKeys maskPlan key
    OrdinalTotalOrderPlan size ->
      ordinalTotalOrderUpperCoverKeys size key
    TotalOrderPlan totalOrderPlan ->
      let nextRank = totalOrderKeyRank totalOrderPlan key + 1
          size = UVector.length (ctoKeyByRank totalOrderPlan)
          chunkCount = contextKeySetChunkCount size
       in if nextRank < size
                then
                  contextKeySetSingleton
                    chunkCount
                    (totalOrderKeyAtRank totalOrderPlan nextRank)
                else contextKeySetEmpty chunkCount
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanUpperCoverKeys size key
    BoundedFanPlan fanPlan
      | key == cbfBottomKey fanPlan -> cbfAtomKeys fanPlan
      | key == cbfTopKey fanPlan ->
          contextKeySetEmpty (contextKeySetChunkCount (cbfSize fanPlan))
      | otherwise ->
          contextKeySetSingleton
                (contextKeySetChunkCount (cbfSize fanPlan))
                (cbfTopKey fanPlan)

contextPlanLowerCoverKeys :: ContextPlan -> ContextKey -> ContextKeySet
contextPlanLowerCoverKeys plan key =
  case plan of
    DensePlan densePlan ->
      denseTableLowerCoverKeys densePlan key
    MaskPlan maskPlan ->
      contextMaskPlanLowerCoverKeys maskPlan key
    OrdinalTotalOrderPlan size ->
      ordinalTotalOrderLowerCoverKeys size key
    TotalOrderPlan totalOrderPlan ->
      let previousRank = totalOrderKeyRank totalOrderPlan key - 1
          size = UVector.length (ctoKeyByRank totalOrderPlan)
          chunkCount = contextKeySetChunkCount size
       in if previousRank >= 0
                then
                  contextKeySetSingleton
                    chunkCount
                    (totalOrderKeyAtRank totalOrderPlan previousRank)
                else contextKeySetEmpty chunkCount
    OrdinalBoundedFanPlan size ->
      ordinalBoundedFanLowerCoverKeys size key
    BoundedFanPlan fanPlan
      | key == cbfTopKey fanPlan -> cbfAtomKeys fanPlan
      | key == cbfBottomKey fanPlan ->
          contextKeySetEmpty (contextKeySetChunkCount (cbfSize fanPlan))
      | otherwise ->
          contextKeySetSingleton
                (contextKeySetChunkCount (cbfSize fanPlan))
                (cbfBottomKey fanPlan)

-- | Dense plans already contain the transitive relation, so scanning all
-- successors is cheaper than reconstructing the Hasse diagram. The reflexive
-- edge is harmless for monotonicity and avoids allocating a copied row merely
-- to delete one bit.
-- Specialized plans enumerate covers. Either set generates the order and is
-- sufficient for an exact monotonicity check.
contextPlanMonotonicityTargets :: ContextPlan -> ContextKey -> ContextKeySet
contextPlanMonotonicityTargets plan key =
  case plan of
    DensePlan _ ->
      contextPlanUpperKeys plan key
    MaskPlan maskPlan ->
      contextMaskPlanMonotonicityTargets maskPlan key
    _ -> contextPlanUpperCoverKeys plan key

contextMaskPlanLeq :: ContextMaskPlan -> ContextKey -> ContextKey -> Bool
contextMaskPlanLeq plan leftKey rightKey =
  case plan of
    BooleanPlan booleanPlan ->
      let leftMask = booleanMaskForKey booleanPlan leftKey
          rightMask = booleanMaskForKey booleanPlan rightKey
       in leftMask .&. rightMask == leftMask
    DistributivePlan distributivePlan ->
      distributiveKeyLeq distributivePlan leftKey rightKey
    DenseRowsPlan rowsPlan ->
      contextKeyRelated (cdrpUpperRows rowsPlan) leftKey rightKey
{-# INLINE contextMaskPlanLeq #-}

contextMaskPlanJoinKey :: ContextMaskPlan -> ContextKey -> ContextKey -> ContextKey
contextMaskPlanJoinKey plan leftKey rightKey =
  case plan of
    BooleanPlan booleanPlan ->
      booleanKeyForMask
            booleanPlan
            ( booleanMaskForKey booleanPlan leftKey
                .|. booleanMaskForKey booleanPlan rightKey
            )
    DistributivePlan distributivePlan ->
      distributiveJoinKey distributivePlan leftKey rightKey
    DenseRowsPlan rowsPlan ->
      denseRowsJoinKey rowsPlan leftKey rightKey
{-# INLINE contextMaskPlanJoinKey #-}

contextMaskPlanMeetKey :: ContextMaskPlan -> ContextKey -> ContextKey -> ContextKey
contextMaskPlanMeetKey plan leftKey rightKey =
  case plan of
    BooleanPlan booleanPlan ->
      booleanKeyForMask
            booleanPlan
            ( booleanMaskForKey booleanPlan leftKey
                .&. booleanMaskForKey booleanPlan rightKey
            )
    DistributivePlan distributivePlan ->
      distributiveMeetKey distributivePlan leftKey rightKey
    DenseRowsPlan rowsPlan ->
      denseRowsMeetKey rowsPlan leftKey rightKey
{-# INLINE contextMaskPlanMeetKey #-}

contextMaskPlanJoinMeetKeys :: ContextMaskPlan -> ContextKey -> ContextKey -> (ContextKey, ContextKey)
contextMaskPlanJoinMeetKeys plan leftKey rightKey =
  case plan of
    BooleanPlan booleanPlan ->
      let leftMask = booleanMaskForKey booleanPlan leftKey
          rightMask = booleanMaskForKey booleanPlan rightKey
       in ( booleanKeyForMask booleanPlan (leftMask .|. rightMask),
              booleanKeyForMask booleanPlan (leftMask .&. rightMask)
            )
    DistributivePlan distributivePlan ->
      distributiveJoinMeetKeys distributivePlan leftKey rightKey
    DenseRowsPlan rowsPlan ->
      ( denseRowsJoinKey rowsPlan leftKey rightKey,
        denseRowsMeetKey rowsPlan leftKey rightKey
      )
{-# NOINLINE contextMaskPlanJoinMeetKeys #-}

contextMaskPlanUpperKeys :: ContextMaskPlan -> ContextKey -> ContextKeySet
contextMaskPlanUpperKeys plan key =
  case plan of
    BooleanPlan booleanPlan ->
      booleanUpperKeys booleanPlan key
    DistributivePlan distributivePlan ->
      distributiveUpperKeys distributivePlan key
    DenseRowsPlan rowsPlan ->
      rowForKey (cdrpUpperRows rowsPlan) key

contextMaskPlanLowerKeys :: ContextMaskPlan -> ContextKey -> ContextKeySet
contextMaskPlanLowerKeys plan key =
  case plan of
    BooleanPlan booleanPlan ->
      booleanLowerKeys booleanPlan key
    DistributivePlan distributivePlan ->
      distributiveLowerKeys distributivePlan key
    DenseRowsPlan rowsPlan ->
      rowForKey (cdrpLowerRows rowsPlan) key

contextMaskPlanUpperCoverKeys :: ContextMaskPlan -> ContextKey -> ContextKeySet
contextMaskPlanUpperCoverKeys plan key =
  case plan of
    BooleanPlan booleanPlan ->
      booleanUpperCoverKeys booleanPlan key
    DistributivePlan distributivePlan ->
      distributiveUpperCoverKeys distributivePlan key
    DenseRowsPlan rowsPlan ->
      denseRowsUpperCoverKeys rowsPlan key

contextMaskPlanLowerCoverKeys :: ContextMaskPlan -> ContextKey -> ContextKeySet
contextMaskPlanLowerCoverKeys plan key =
  case plan of
    BooleanPlan booleanPlan ->
      booleanLowerCoverKeys booleanPlan key
    DistributivePlan distributivePlan ->
      distributiveLowerCoverKeys distributivePlan key
    DenseRowsPlan rowsPlan ->
      denseRowsLowerCoverKeys rowsPlan key

contextMaskPlanMonotonicityTargets :: ContextMaskPlan -> ContextKey -> ContextKeySet
contextMaskPlanMonotonicityTargets plan key =
  case plan of
    BooleanPlan booleanPlan -> booleanUpperCoverKeys booleanPlan key
    DistributivePlan _ -> contextMaskPlanUpperKeys plan key
    DenseRowsPlan _ -> contextMaskPlanUpperKeys plan key

denseTableJoinKey :: ContextDenseTablePlan -> ContextKey -> ContextKey -> ContextKey
denseTableJoinKey plan =
  contextKeyTableLookup (cdtpJoinTable plan)
{-# INLINE denseTableJoinKey #-}

denseTableMeetKey :: ContextDenseTablePlan -> ContextKey -> ContextKey -> ContextKey
denseTableMeetKey plan =
  contextKeyTableLookup (cdtpMeetTable plan)
{-# INLINE denseTableMeetKey #-}

denseTableJoinMeetKeys :: ContextDenseTablePlan -> ContextKey -> ContextKey -> (ContextKey, ContextKey)
denseTableJoinMeetKeys plan leftKey rightKey =
  ( contextKeyTableLookup (cdtpJoinTable plan) leftKey rightKey,
    contextKeyTableLookup (cdtpMeetTable plan) leftKey rightKey
  )
{-# INLINE denseTableJoinMeetKeys #-}

denseRowsJoinKey :: ContextDenseRowsPlan -> ContextKey -> ContextKey -> ContextKey
denseRowsJoinKey plan leftKey rightKey =
  invariantLookup
    (ContextPlanJoinMissing (contextKeyOrdinal leftKey) (contextKeyOrdinal rightKey))
    (rowJoinKeyMaybe (cdrpUpperRows plan) (cdrpLowerRows plan) (cdrpUpperRowIndex plan) leftKey rightKey)
{-# INLINE denseRowsJoinKey #-}

denseRowsMeetKey :: ContextDenseRowsPlan -> ContextKey -> ContextKey -> ContextKey
denseRowsMeetKey plan leftKey rightKey =
  invariantLookup
    (ContextPlanMeetMissing (contextKeyOrdinal leftKey) (contextKeyOrdinal rightKey))
    (rowMeetKeyMaybe (cdrpUpperRows plan) (cdrpLowerRows plan) (cdrpLowerRowIndex plan) leftKey rightKey)
{-# INLINE denseRowsMeetKey #-}


-- A candidate u is an upper cover of l iff there is no member of
-- (up(l) \\ {l}) ∩ down(u) other than u.
denseTableUpperCoverKeys :: ContextDenseTablePlan -> ContextKey -> ContextKeySet
denseTableUpperCoverKeys plan lowerKey =
  contextKeySetFilter isCover candidates
  where
    candidates =
      contextKeySetDelete
        (contextKeyOrdinal lowerKey)
        (rowForKey (cdtpUpperRows plan) lowerKey)

    isCover upperOrdinal =
      not
        ( contextKeySetIntersectsExcept
            upperOrdinal
            candidates
            (rowForKey (cdtpLowerRows plan) (ContextKey upperOrdinal))
        )

-- Dual of 'denseTableUpperCoverKeys'.
denseTableLowerCoverKeys :: ContextDenseTablePlan -> ContextKey -> ContextKeySet
denseTableLowerCoverKeys plan upperKey =
  contextKeySetFilter isCover candidates
  where
    candidates =
      contextKeySetDelete
        (contextKeyOrdinal upperKey)
        (rowForKey (cdtpLowerRows plan) upperKey)

    isCover lowerOrdinal =
      not
        ( contextKeySetIntersectsExcept
            lowerOrdinal
            candidates
            (rowForKey (cdtpUpperRows plan) (ContextKey lowerOrdinal))
        )

denseRowsUpperCoverKeys :: ContextDenseRowsPlan -> ContextKey -> ContextKeySet
denseRowsUpperCoverKeys plan lowerKey =
  contextKeySetFilter isCover candidates
  where
    candidates =
      contextKeySetDelete
        (contextKeyOrdinal lowerKey)
        (rowForKey (cdrpUpperRows plan) lowerKey)

    isCover upperOrdinal =
      not
        ( contextKeySetIntersectsExcept
            upperOrdinal
            candidates
            (rowForKey (cdrpLowerRows plan) (ContextKey upperOrdinal))
        )

denseRowsLowerCoverKeys :: ContextDenseRowsPlan -> ContextKey -> ContextKeySet
denseRowsLowerCoverKeys plan upperKey =
  contextKeySetFilter isCover candidates
  where
    candidates =
      contextKeySetDelete
        (contextKeyOrdinal upperKey)
        (rowForKey (cdrpLowerRows plan) upperKey)

    isCover lowerOrdinal =
      not
        ( contextKeySetIntersectsExcept
            lowerOrdinal
            candidates
            (rowForKey (cdrpUpperRows plan) (ContextKey lowerOrdinal))
        )


totalOrderKeyRank :: ContextTotalOrderPlan -> ContextKey -> Int
totalOrderKeyRank plan (ContextKey keyOrdinal) =
  unboxedIndexInvariant (ctoRankByKey plan) keyOrdinal
{-# INLINE totalOrderKeyRank #-}

totalOrderKeyAtRank :: ContextTotalOrderPlan -> Int -> ContextKey
totalOrderKeyAtRank plan rank =
  ContextKey (unboxedIndexInvariant (ctoKeyByRank plan) rank)
{-# INLINE totalOrderKeyAtRank #-}

totalOrderKeysFromRank :: ContextTotalOrderPlan -> Int -> Int -> ContextKeySet
totalOrderKeysFromRank plan firstRank lastRank =
  contextKeySetFromKeys
    (contextKeySetChunkCount size)
    [ contextKeyOrdinal (totalOrderKeyAtRank plan rank)
    | rank <- [firstRank .. lastRank]
    ]
  where
    size = UVector.length (ctoKeyByRank plan)

ordinalTotalOrderKeysFromRank :: Int -> Int -> Int -> ContextKeySet
ordinalTotalOrderKeysFromRank size firstRank lastRank =
  contextKeySetFromKeys (contextKeySetChunkCount size) [firstRank .. lastRank]

ordinalTotalOrderUpperCoverKeys :: Int -> ContextKey -> ContextKeySet
ordinalTotalOrderUpperCoverKeys size (ContextKey keyOrdinal)
  | nextOrdinal < size =
      contextKeySetSingleton
        (contextKeySetChunkCount size)
        (ContextKey nextOrdinal)
  | otherwise = contextKeySetEmpty (contextKeySetChunkCount size)
  where
    nextOrdinal = keyOrdinal + 1

ordinalTotalOrderLowerCoverKeys :: Int -> ContextKey -> ContextKeySet
ordinalTotalOrderLowerCoverKeys size (ContextKey keyOrdinal)
  | previousOrdinal >= 0 =
      contextKeySetSingleton
        (contextKeySetChunkCount size)
        (ContextKey previousOrdinal)
  | otherwise = contextKeySetEmpty (contextKeySetChunkCount size)
  where
    previousOrdinal = keyOrdinal - 1

ordinalBoundedFanKeyLeq :: Int -> ContextKey -> ContextKey -> Bool
ordinalBoundedFanKeyLeq size (ContextKey leftOrdinal) (ContextKey rightOrdinal) =
  leftOrdinal == rightOrdinal
    || leftOrdinal == ordinalBottomOrdinal
    || rightOrdinal == ordinalTopOrdinal size
{-# INLINE ordinalBoundedFanKeyLeq #-}

ordinalBoundedFanJoinKey :: Int -> ContextKey -> ContextKey -> ContextKey
ordinalBoundedFanJoinKey size leftKey@(ContextKey leftOrdinal) rightKey@(ContextKey rightOrdinal)
  | leftOrdinal == ordinalBottomOrdinal = rightKey
  | rightOrdinal == ordinalBottomOrdinal = leftKey
  | leftOrdinal == rightOrdinal = leftKey
  | otherwise = ContextKey (ordinalTopOrdinal size)
{-# INLINE ordinalBoundedFanJoinKey #-}

ordinalBoundedFanMeetKey :: Int -> ContextKey -> ContextKey -> ContextKey
ordinalBoundedFanMeetKey size leftKey@(ContextKey leftOrdinal) rightKey@(ContextKey rightOrdinal)
  | leftOrdinal == ordinalTopOrdinal size = rightKey
  | rightOrdinal == ordinalTopOrdinal size = leftKey
  | leftOrdinal == rightOrdinal = leftKey
  | otherwise = ContextKey ordinalBottomOrdinal
{-# INLINE ordinalBoundedFanMeetKey #-}

ordinalBoundedFanJoinMeetKeys :: Int -> ContextKey -> ContextKey -> (ContextKey, ContextKey)
ordinalBoundedFanJoinMeetKeys size leftKey rightKey =
  (ordinalBoundedFanJoinKey size leftKey rightKey, ordinalBoundedFanMeetKey size leftKey rightKey)
{-# INLINE ordinalBoundedFanJoinMeetKeys #-}

ordinalBoundedFanUpperKeys :: Int -> ContextKey -> ContextKeySet
ordinalBoundedFanUpperKeys size key@(ContextKey keyOrdinal)
  | keyOrdinal == ordinalBottomOrdinal = contextKeySetAll size
  | keyOrdinal == ordinalTopOrdinal size =
      contextKeySetSingleton chunkCount (ContextKey (ordinalTopOrdinal size))
  | otherwise =
      contextKeySetFromKeys chunkCount [contextKeyOrdinal key, ordinalTopOrdinal size]
  where
    chunkCount = contextKeySetChunkCount size

ordinalBoundedFanLowerKeys :: Int -> ContextKey -> ContextKeySet
ordinalBoundedFanLowerKeys size key@(ContextKey keyOrdinal)
  | keyOrdinal == ordinalBottomOrdinal =
      contextKeySetSingleton chunkCount (ContextKey ordinalBottomOrdinal)
  | keyOrdinal == ordinalTopOrdinal size = contextKeySetAll size
  | otherwise =
      contextKeySetFromKeys chunkCount [ordinalBottomOrdinal, contextKeyOrdinal key]
  where
    chunkCount = contextKeySetChunkCount size

ordinalBoundedFanUpperCoverKeys :: Int -> ContextKey -> ContextKeySet
ordinalBoundedFanUpperCoverKeys size (ContextKey keyOrdinal)
  | keyOrdinal == ordinalBottomOrdinal =
      contextKeySetFromKeys (contextKeySetChunkCount size) [1 .. ordinalTopOrdinal size - 1]
  | keyOrdinal == ordinalTopOrdinal size =
      contextKeySetEmpty (contextKeySetChunkCount size)
  | otherwise =
      contextKeySetSingleton (contextKeySetChunkCount size) (ContextKey (ordinalTopOrdinal size))

ordinalBoundedFanLowerCoverKeys :: Int -> ContextKey -> ContextKeySet
ordinalBoundedFanLowerCoverKeys size (ContextKey keyOrdinal)
  | keyOrdinal == ordinalTopOrdinal size =
      contextKeySetFromKeys (contextKeySetChunkCount size) [1 .. ordinalTopOrdinal size - 1]
  | keyOrdinal == ordinalBottomOrdinal =
      contextKeySetEmpty (contextKeySetChunkCount size)
  | otherwise =
      contextKeySetSingleton (contextKeySetChunkCount size) (ContextKey ordinalBottomOrdinal)

ordinalTopOrdinal :: Int -> Int
ordinalTopOrdinal size =
  size - 1
{-# INLINE ordinalTopOrdinal #-}

ordinalBottomOrdinal :: Int
ordinalBottomOrdinal =
  0
{-# INLINE ordinalBottomOrdinal #-}

boundedFanKeyLeq :: ContextBoundedFanPlan -> ContextKey -> ContextKey -> Bool
boundedFanKeyLeq plan leftKey rightKey =
  leftKey == rightKey
    || leftKey == cbfBottomKey plan
    || rightKey == cbfTopKey plan
{-# INLINE boundedFanKeyLeq #-}

boundedFanJoinKey :: ContextBoundedFanPlan -> ContextKey -> ContextKey -> ContextKey
boundedFanJoinKey plan leftKey rightKey
  | leftKey == cbfBottomKey plan = rightKey
  | rightKey == cbfBottomKey plan = leftKey
  | leftKey == rightKey = leftKey
  | otherwise = cbfTopKey plan
{-# INLINE boundedFanJoinKey #-}

boundedFanMeetKey :: ContextBoundedFanPlan -> ContextKey -> ContextKey -> ContextKey
boundedFanMeetKey plan leftKey rightKey
  | leftKey == cbfTopKey plan = rightKey
  | rightKey == cbfTopKey plan = leftKey
  | leftKey == rightKey = leftKey
  | otherwise = cbfBottomKey plan
{-# INLINE boundedFanMeetKey #-}

boundedFanJoinMeetKeys :: ContextBoundedFanPlan -> ContextKey -> ContextKey -> (ContextKey, ContextKey)
boundedFanJoinMeetKeys plan leftKey rightKey =
  (boundedFanJoinKey plan leftKey rightKey, boundedFanMeetKey plan leftKey rightKey)
{-# INLINE boundedFanJoinMeetKeys #-}

boundedFanUpperKeys :: ContextBoundedFanPlan -> ContextKey -> ContextKeySet
boundedFanUpperKeys plan key
  | key == cbfBottomKey plan = cbfAllKeys plan
  | key == cbfTopKey plan =
      contextKeySetSingleton chunkCount (cbfTopKey plan)
  | otherwise =
      contextKeySetFromKeys
        chunkCount
        [contextKeyOrdinal key, contextKeyOrdinal (cbfTopKey plan)]
  where
    chunkCount = contextKeySetChunkCount (cbfSize plan)

boundedFanLowerKeys :: ContextBoundedFanPlan -> ContextKey -> ContextKeySet
boundedFanLowerKeys plan key
  | key == cbfBottomKey plan =
      contextKeySetSingleton chunkCount (cbfBottomKey plan)
  | key == cbfTopKey plan = cbfAllKeys plan
  | otherwise =
      contextKeySetFromKeys
        chunkCount
        [contextKeyOrdinal (cbfBottomKey plan), contextKeyOrdinal key]
  where
    chunkCount = contextKeySetChunkCount (cbfSize plan)

booleanMaskForKey :: ContextBooleanPlan -> ContextKey -> Word64
booleanMaskForKey plan (ContextKey keyOrdinal) =
  unboxedIndexInvariant (cboMaskByKey plan) keyOrdinal
{-# INLINE booleanMaskForKey #-}

booleanKeyForMask :: ContextBooleanPlan -> Word64 -> ContextKey
booleanKeyForMask plan mask =
  ContextKey (unboxedIndexInvariant (cboKeyByMask plan) (fromIntegral mask))
{-# INLINE booleanKeyForMask #-}

booleanUpperKeys :: ContextBooleanPlan -> ContextKey -> ContextKeySet
booleanUpperKeys plan key =
  contextKeySetFromKeys
    (contextKeySetChunkCount size)
    [ contextKeyOrdinal (booleanKeyForMask plan (keyMask .|. freeSubmask))
    | freeSubmask <- submasks freeMask
    ]
  where
    size = UVector.length (cboMaskByKey plan)
    keyMask = booleanMaskForKey plan key
    freeMask = cboFullMask plan `xor` keyMask

booleanLowerKeys :: ContextBooleanPlan -> ContextKey -> ContextKeySet
booleanLowerKeys plan key =
  contextKeySetFromKeys
    (contextKeySetChunkCount size)
    [ contextKeyOrdinal (booleanKeyForMask plan lowerMask)
    | lowerMask <- submasks keyMask
    ]
  where
    size = UVector.length (cboMaskByKey plan)
    keyMask = booleanMaskForKey plan key

booleanUpperCoverKeys :: ContextBooleanPlan -> ContextKey -> ContextKeySet
booleanUpperCoverKeys plan key =
  contextKeySetFromKeys
    (contextKeySetChunkCount size)
    [ contextKeyOrdinal (booleanKeyForMask plan (keyMask .|. bit atomIndex))
    | atomIndex <- [0 .. cboAtomCount plan - 1],
      not (testBit keyMask atomIndex)
    ]
  where
    size = UVector.length (cboMaskByKey plan)
    keyMask = booleanMaskForKey plan key

booleanLowerCoverKeys :: ContextBooleanPlan -> ContextKey -> ContextKeySet
booleanLowerCoverKeys plan key =
  contextKeySetFromKeys
    (contextKeySetChunkCount size)
    [ contextKeyOrdinal
        ( booleanKeyForMask
            plan
            (keyMask .&. (cboFullMask plan .&. complement (bit atomIndex)))
        )
    | atomIndex <- [0 .. cboAtomCount plan - 1],
      testBit keyMask atomIndex
    ]
  where
    size = UVector.length (cboMaskByKey plan)
    keyMask = booleanMaskForKey plan key

submasks :: Word64 -> [Word64]
submasks mask =
  go mask
  where
    go submask
      | submask == 0 = [0]
      | otherwise = submask : go ((submask - 1) .&. mask)

