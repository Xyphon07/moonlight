{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Distributive
  ( ContextDistributivePlan (..),
    ContextDistributiveRowsResult (..),
    distributivePlanFromRows,
    distributivePlanFromDenseComponents,
    distributiveKeyLeq,
    distributiveJoinKey,
    distributiveMeetKey,
    distributiveJoinMeetKeys,
    distributiveUpperKeys,
    distributiveLowerKeys,
    distributiveUpperCoverKeys,
    distributiveLowerCoverKeys,
    distributiveResidualKey,
  )
where

import Data.Bits
  ( (.&.),
    (.|.),
    bit,
    complement,
    countTrailingZeros,
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.FiniteLattice.Internal.Invariant
  ( ContextPlanInvariantError (..),
    invariantLookup,
    unboxedIndexInvariant,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
    ContextKeyTable,
    contextKeySetCardinality,
    contextKeySetDelete,
    contextKeySetFilter,
    contextKeySetFromKeys,
    contextKeySetIntersectsExcept,
    contextKeyTableLookup,
  )
import Moonlight.FiniteLattice.Internal.Relation
  ( ContextRowIndex,
    ContextRows,
    contextKeyRelated,
    rowForRawKey,
    rowJoinCandidateKeys,
    rowJoinKeyMaybe,
    rowMeetCandidateKeys,
    rowMeetKeyMaybe,
  )

-- | A Birkhoff representation of a finite distributive lattice. Each element
-- is encoded as the downset of join-irreducible elements below it. Masks are
-- indexed by join-irreducible position, not by context-key ordinal.
data ContextDistributivePlan = ContextDistributivePlan
  { cdipSize :: !Int,
    cdipTopKey :: !ContextKey,
    cdipJoinIrreducibleKeyByIndex :: !(UVector.Vector Int),
    cdipMaskByKey :: !JoinIrreducibleMaskRows,
    cdipKeyByMask :: !(Map JoinIrreducibleMaskSignature ContextKey),
    cdipUpClosureByJoinIrreducible :: !JoinIrreducibleMaskRows,
    cdipFullMask :: !JoinIrreducibleMask
  }

data JoinIrreducibleMask = JoinIrreducibleMask !(UVector.Vector Word64)
  deriving stock (Eq, Show)

data JoinIrreducibleMaskRows = JoinIrreducibleMaskRows
  { jimrChunkCount :: !Int,
    jimrChunks :: !(UVector.Vector Word64)
  }
  deriving stock (Eq, Show)

data JoinIrreducibleMaskSignature
  = JoinIrreducibleMaskSignature0
  | JoinIrreducibleMaskSignature1 !Word64
  | JoinIrreducibleMaskSignature2 !Word64 !Word64
  | JoinIrreducibleMaskSignatureN ![Word64]
  deriving stock (Eq, Ord, Show)

data ContextDistributiveRowsResult
  = ContextDistributiveRowsPlan !ContextDistributivePlan
  | ContextDenseRowsValidated
  | ContextRowJoinAbsent !ContextKey !ContextKey !ContextKeySet
  | ContextRowMeetAbsent !ContextKey !ContextKey !ContextKeySet

distributivePlanFromRows ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextRowIndex ->
  ContextDistributiveRowsResult
distributivePlanFromRows size topKey bottomKey upperRows lowerRows upperRowIndex lowerRowIndex =
  validateRowOperationsAgainstMasks
    size
    upperRows
    lowerRows
    upperRowIndex
    lowerRowIndex
    candidatePlan
  where
    candidatePlan =
      distributivePlanCandidate
        size
        topKey
        bottomKey
        upperRows
        lowerRows

distributivePlanFromDenseComponents ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  ContextKeyTable ->
  ContextKeyTable ->
  Maybe ContextDistributivePlan
distributivePlanFromDenseComponents size topKey bottomKey upperRows lowerRows joinTable meetTable =
  candidatePlan >>= \plan ->
    if
      tableOperationsMatchMasks
        size
        joinTable
        meetTable
        (cdipMaskByKey plan)
        (cdipKeyByMask plan)
      then Just plan
      else Nothing
  where
    candidatePlan =
      distributivePlanCandidate
        size
        topKey
        bottomKey
        upperRows
        lowerRows

distributivePlanCandidate ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Maybe ContextDistributivePlan
distributivePlanCandidate size topKey bottomKey upperRows lowerRows = do
  guardMaybe (size > 0)
  let joinIrreducibleKeys = joinIrreducibleKeyOrdinals bottomKey size upperRows lowerRows
      joinIrreducibleCount = UVector.length joinIrreducibleKeys
      maskByKey = maskRowsForKeys size joinIrreducibleKeys upperRows
      fullMask = maskFull joinIrreducibleCount
      maskEntries =
        [ ( maskSignature (maskRowsRow maskByKey keyOrdinal),
            ContextKey keyOrdinal
          )
        | keyOrdinal <- [0 .. size - 1]
        ]
      keyByMask = Map.fromList maskEntries
  guardMaybe (Map.size keyByMask == size)
  guardMaybe (maskRowsRow maskByKey (contextKeyOrdinal bottomKey) == maskEmpty joinIrreducibleCount)
  guardMaybe (maskRowsRow maskByKey (contextKeyOrdinal topKey) == fullMask)
  let upClosureByJoinIrreducible =
        upClosureRowsForJoinIrreducibles joinIrreducibleKeys upperRows
  pure
    ContextDistributivePlan
      { cdipSize = size,
        cdipTopKey = topKey,
        cdipJoinIrreducibleKeyByIndex = joinIrreducibleKeys,
        cdipMaskByKey = maskByKey,
        cdipKeyByMask = keyByMask,
        cdipUpClosureByJoinIrreducible = upClosureByJoinIrreducible,
        cdipFullMask = fullMask
      }

distributiveKeyLeq :: ContextDistributivePlan -> ContextKey -> ContextKey -> Bool
distributiveKeyLeq plan leftKey rightKey =
  maskIsSubsetOf
    (distributiveMaskForKey plan leftKey)
    (distributiveMaskForKey plan rightKey)
{-# INLINE distributiveKeyLeq #-}

distributiveJoinKey :: ContextDistributivePlan -> ContextKey -> ContextKey -> ContextKey
distributiveJoinKey plan leftKey rightKey =
  distributiveKeyForMaskInvariant
    (ContextPlanJoinMissing (contextKeyOrdinal leftKey) (contextKeyOrdinal rightKey))
    plan
    (maskUnion leftMask rightMask)
  where
    leftMask = distributiveMaskForKey plan leftKey
    rightMask = distributiveMaskForKey plan rightKey
{-# INLINE distributiveJoinKey #-}

distributiveMeetKey :: ContextDistributivePlan -> ContextKey -> ContextKey -> ContextKey
distributiveMeetKey plan leftKey rightKey =
  distributiveKeyForMaskInvariant
    (ContextPlanMeetMissing (contextKeyOrdinal leftKey) (contextKeyOrdinal rightKey))
    plan
    (maskIntersection leftMask rightMask)
  where
    leftMask = distributiveMaskForKey plan leftKey
    rightMask = distributiveMaskForKey plan rightKey
{-# INLINE distributiveMeetKey #-}

distributiveJoinMeetKeys :: ContextDistributivePlan -> ContextKey -> ContextKey -> (ContextKey, ContextKey)
distributiveJoinMeetKeys plan leftKey rightKey =
  ( distributiveKeyForMaskInvariant
      (ContextPlanJoinMissing (contextKeyOrdinal leftKey) (contextKeyOrdinal rightKey))
      plan
      (maskUnion leftMask rightMask),
    distributiveKeyForMaskInvariant
      (ContextPlanMeetMissing (contextKeyOrdinal leftKey) (contextKeyOrdinal rightKey))
      plan
      (maskIntersection leftMask rightMask)
  )
  where
    leftMask = distributiveMaskForKey plan leftKey
    rightMask = distributiveMaskForKey plan rightKey
{-# INLINE distributiveJoinMeetKeys #-}

distributiveUpperKeys :: ContextDistributivePlan -> ContextKey -> ContextKeySet
distributiveUpperKeys plan key =
  contextKeySetFromKeys
    (maskUniverseChunkCount plan)
    [ candidateOrdinal
    | candidateOrdinal <- [0 .. cdipSize plan - 1],
      maskIsSubsetOf keyMask (maskRowsRow (cdipMaskByKey plan) candidateOrdinal)
    ]
  where
    keyMask = distributiveMaskForKey plan key

distributiveLowerKeys :: ContextDistributivePlan -> ContextKey -> ContextKeySet
distributiveLowerKeys plan key =
  contextKeySetFromKeys
    (maskUniverseChunkCount plan)
    [ candidateOrdinal
    | candidateOrdinal <- [0 .. cdipSize plan - 1],
      maskIsSubsetOf (maskRowsRow (cdipMaskByKey plan) candidateOrdinal) keyMask
    ]
  where
    keyMask = distributiveMaskForKey plan key

distributiveUpperCoverKeys :: ContextDistributivePlan -> ContextKey -> ContextKeySet
distributiveUpperCoverKeys plan key@(ContextKey keyOrdinal) =
  contextKeySetFromKeys
    (maskUniverseChunkCount plan)
    (foldMap upperCoverKeyOrdinals [0 .. joinIrreducibleCount - 1])
  where
    keyMask = distributiveMaskForKey plan key
    joinIrreducibleCount = UVector.length (cdipJoinIrreducibleKeyByIndex plan)

    upperCoverKeyOrdinals joinIrreducibleIndex
      | maskMember joinIrreducibleIndex keyMask = []
      | not (maskIsSubsetOf requiredLowerMask keyMask) = []
      | otherwise =
          let ContextKey upperOrdinal =
                distributiveKeyForMaskInvariant
                (ContextPlanUpperCoverMissing keyOrdinal joinIrreducibleIndex)
                plan
                (maskUnion keyMask (maskSingleton joinIrreducibleCount joinIrreducibleIndex))
           in [upperOrdinal]
      where
        joinIrreducibleKey = ContextKey (unboxedIndexInvariant (cdipJoinIrreducibleKeyByIndex plan) joinIrreducibleIndex)
        joinIrreducibleMask = distributiveMaskForKey plan joinIrreducibleKey
        requiredLowerMask =
          maskDifference
            joinIrreducibleMask
            (maskSingleton joinIrreducibleCount joinIrreducibleIndex)

distributiveLowerCoverKeys :: ContextDistributivePlan -> ContextKey -> ContextKeySet
distributiveLowerCoverKeys plan key@(ContextKey keyOrdinal) =
  contextKeySetFromKeys
    (maskUniverseChunkCount plan)
    (foldMap lowerCoverKeyOrdinals [0 .. joinIrreducibleCount - 1])
  where
    keyMask = distributiveMaskForKey plan key
    joinIrreducibleCount = UVector.length (cdipJoinIrreducibleKeyByIndex plan)

    lowerCoverKeyOrdinals joinIrreducibleIndex
      | not (maskMember joinIrreducibleIndex keyMask) = []
      | not (maskNull strictUpperContainedMask) = []
      | otherwise =
          let ContextKey lowerOrdinal =
                distributiveKeyForMaskInvariant
                (ContextPlanLowerCoverMissing keyOrdinal joinIrreducibleIndex)
                plan
                (maskDifference keyMask singletonMask)
           in [lowerOrdinal]
      where
        singletonMask = maskSingleton joinIrreducibleCount joinIrreducibleIndex
        upperClosureMask = maskRowsRow (cdipUpClosureByJoinIrreducible plan) joinIrreducibleIndex
        strictUpperContainedMask =
          maskDifference
            (maskIntersection keyMask upperClosureMask)
            singletonMask

distributiveResidualKey ::
  ContextDistributivePlan ->
  ContextKey ->
  ContextKey ->
  ContextKey
distributiveResidualKey plan antecedentKey consequentKey =
  if maskNull forbiddenMask
    then cdipTopKey plan
    else
      distributiveKeyForMaskInvariant
        (ContextPlanResidualMissing (contextKeyOrdinal antecedentKey) (contextKeyOrdinal consequentKey))
        plan
        residualMask
  where
    antecedentMask = distributiveMaskForKey plan antecedentKey
    consequentMask = distributiveMaskForKey plan consequentKey
    forbiddenMask = maskDifference antecedentMask consequentMask
    rejectedMask =
      maskUnionImages
        (jimrChunkCount (cdipUpClosureByJoinIrreducible plan))
        (maskRowsRow (cdipUpClosureByJoinIrreducible plan))
        forbiddenMask
    residualMask = maskDifference (cdipFullMask plan) rejectedMask
{-# INLINE distributiveResidualKey #-}

distributiveMaskForKey :: ContextDistributivePlan -> ContextKey -> JoinIrreducibleMask
distributiveMaskForKey plan (ContextKey keyOrdinal) =
  maskRowsRow (cdipMaskByKey plan) keyOrdinal
{-# INLINE distributiveMaskForKey #-}

distributiveKeyForMask :: ContextDistributivePlan -> JoinIrreducibleMask -> Maybe ContextKey
distributiveKeyForMask plan mask =
  Map.lookup (maskSignature mask) (cdipKeyByMask plan)
{-# INLINE distributiveKeyForMask #-}

distributiveKeyForMaskInvariant ::
  ContextPlanInvariantError ->
  ContextDistributivePlan ->
  JoinIrreducibleMask ->
  ContextKey
distributiveKeyForMaskInvariant invariantError plan mask =
  invariantLookup invariantError (distributiveKeyForMask plan mask)
{-# INLINE distributiveKeyForMaskInvariant #-}

joinIrreducibleKeyOrdinals :: ContextKey -> Int -> ContextRows -> ContextRows -> UVector.Vector Int
joinIrreducibleKeyOrdinals bottomKey size upperRows lowerRows =
  UVector.fromList
    [ keyOrdinal
    | keyOrdinal <- [0 .. size - 1],
      ContextKey keyOrdinal /= bottomKey,
      contextKeySetCardinality
        (lowerCoverOrdinals upperRows lowerRows (ContextKey keyOrdinal))
        == 1
    ]

lowerCoverOrdinals :: ContextRows -> ContextRows -> ContextKey -> ContextKeySet
lowerCoverOrdinals upperRows lowerRows (ContextKey keyOrdinal) =
  maximalWithin
    upperRows
    (contextKeySetDelete keyOrdinal (rowForRawKey lowerRows keyOrdinal))

maximalWithin :: ContextRows -> ContextKeySet -> ContextKeySet
maximalWithin upperRows candidates =
  contextKeySetFilter isMaximal candidates
  where
    isMaximal candidateOrdinal =
      not
        ( contextKeySetIntersectsExcept
            candidateOrdinal
            candidates
            (rowForRawKey upperRows candidateOrdinal)
        )

maskRowsForKeys :: Int -> UVector.Vector Int -> ContextRows -> JoinIrreducibleMaskRows
maskRowsForKeys size joinIrreducibleKeys upperRows =
  maskRowsGenerate size (UVector.length joinIrreducibleKeys) $ \keyOrdinal joinIrreducibleIndex ->
    contextKeyRelated
      upperRows
      (ContextKey (unboxedIndexInvariant joinIrreducibleKeys joinIrreducibleIndex))
      (ContextKey keyOrdinal)

upClosureRowsForJoinIrreducibles :: UVector.Vector Int -> ContextRows -> JoinIrreducibleMaskRows
upClosureRowsForJoinIrreducibles joinIrreducibleKeys upperRows =
  maskRowsGenerate joinIrreducibleCount joinIrreducibleCount $ \sourceIndex targetIndex ->
    contextKeyRelated
      upperRows
      (ContextKey (unboxedIndexInvariant joinIrreducibleKeys sourceIndex))
      (ContextKey (unboxedIndexInvariant joinIrreducibleKeys targetIndex))
  where
    joinIrreducibleCount = UVector.length joinIrreducibleKeys

validateRowOperationsAgainstMasks ::
  Int ->
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextRowIndex ->
  Maybe ContextDistributivePlan ->
  ContextDistributiveRowsResult
validateRowOperationsAgainstMasks size upperRows lowerRows upperRowIndex lowerRowIndex candidatePlan =
  foldr
    validatePair
    initialResult
    [ (ContextKey leftOrdinal, ContextKey rightOrdinal)
    | leftOrdinal <- [0 .. size - 1],
      rightOrdinal <- [0 .. size - 1]
    ]
  where
    initialResult =
      maybe ContextDenseRowsValidated ContextDistributiveRowsPlan candidatePlan

    validatePair (leftKey, rightKey) remainingPairs =
      case rowJoinKeyMaybe upperRows lowerRows upperRowIndex leftKey rightKey of
        Nothing ->
          ContextRowJoinAbsent
            leftKey
            rightKey
            (rowJoinCandidateKeys upperRows lowerRows leftKey rightKey)
        Just joinKey ->
          case rowMeetKeyMaybe upperRows lowerRows lowerRowIndex leftKey rightKey of
            Nothing ->
              ContextRowMeetAbsent
                leftKey
                rightKey
                (rowMeetCandidateKeys upperRows lowerRows leftKey rightKey)
            Just meetKey ->
              case remainingPairs of
                ContextDistributiveRowsPlan plan
                  | rowOperationMatchesMasks plan leftKey rightKey joinKey meetKey ->
                      remainingPairs
                  | otherwise -> ContextDenseRowsValidated
                ContextDenseRowsValidated -> ContextDenseRowsValidated
                obstruction@(ContextRowJoinAbsent _ _ _) -> obstruction
                obstruction@(ContextRowMeetAbsent _ _ _) -> obstruction

rowOperationMatchesMasks ::
  ContextDistributivePlan ->
  ContextKey ->
  ContextKey ->
  ContextKey ->
  ContextKey ->
  Bool
rowOperationMatchesMasks candidatePlan (ContextKey leftOrdinal) (ContextKey rightOrdinal) joinKey meetKey =
  let leftMask = maskRowsRow (cdipMaskByKey candidatePlan) leftOrdinal
      rightMask = maskRowsRow (cdipMaskByKey candidatePlan) rightOrdinal
   in Map.lookup (maskSignature (maskUnion leftMask rightMask)) (cdipKeyByMask candidatePlan) == Just joinKey
        && Map.lookup (maskSignature (maskIntersection leftMask rightMask)) (cdipKeyByMask candidatePlan) == Just meetKey

tableOperationsMatchMasks ::
  Int ->
  ContextKeyTable ->
  ContextKeyTable ->
  JoinIrreducibleMaskRows ->
  Map JoinIrreducibleMaskSignature ContextKey ->
  Bool
tableOperationsMatchMasks size joinTable meetTable maskByKey keyByMask =
  all pairMatches
    [ (ContextKey leftOrdinal, ContextKey rightOrdinal)
    | leftOrdinal <- [0 .. size - 1],
      rightOrdinal <- [0 .. size - 1]
    ]
  where
    pairMatches (leftKey@(ContextKey leftOrdinal), rightKey@(ContextKey rightOrdinal)) =
      let leftMask = maskRowsRow maskByKey leftOrdinal
          rightMask = maskRowsRow maskByKey rightOrdinal
          joinMask = maskUnion leftMask rightMask
          meetMask = maskIntersection leftMask rightMask
          joinKey = contextKeyTableLookup joinTable leftKey rightKey
          meetKey = contextKeyTableLookup meetTable leftKey rightKey
       in Map.lookup (maskSignature joinMask) keyByMask == Just joinKey
            && Map.lookup (maskSignature meetMask) keyByMask == Just meetKey

maskRowsGenerate ::
  Int ->
  Int ->
  (Int -> Int -> Bool) ->
  JoinIrreducibleMaskRows
maskRowsGenerate rowCount bitCount member =
  JoinIrreducibleMaskRows
    { jimrChunkCount = chunkCount,
      jimrChunks =
        UVector.generate (rowCount * chunkCount) $ \flatIndex ->
          let (rowIndex, chunkIndex) = flatIndex `quotRem` chunkCount
              bitBase = chunkIndex * bitsPerChunk
           in generateChunk rowIndex bitBase 0 0
    }
  where
    chunkCount = maskChunkCount bitCount

    generateChunk !rowIndex !bitBase !bitIndex !chunkValue
      | bitIndex >= bitsPerChunk = chunkValue
      | maskBitIndex >= bitCount = chunkValue
      | member rowIndex maskBitIndex =
          generateChunk rowIndex bitBase (bitIndex + 1) (chunkValue .|. bit bitIndex)
      | otherwise =
          generateChunk rowIndex bitBase (bitIndex + 1) chunkValue
      where
        maskBitIndex = bitBase + bitIndex

maskRowsRow :: JoinIrreducibleMaskRows -> Int -> JoinIrreducibleMask
maskRowsRow rows rowIndex =
  JoinIrreducibleMask
    ( UVector.slice
        (rowIndex * jimrChunkCount rows)
        (jimrChunkCount rows)
        (jimrChunks rows)
    )
{-# INLINE maskRowsRow #-}

maskEmpty :: Int -> JoinIrreducibleMask
maskEmpty bitCount =
  JoinIrreducibleMask (UVector.replicate (maskChunkCount bitCount) 0)

maskFull :: Int -> JoinIrreducibleMask
maskFull bitCount
  | bitCount <= 0 = JoinIrreducibleMask UVector.empty
  | otherwise =
      JoinIrreducibleMask
        ( UVector.generate chunkCount $ \chunkIndex ->
            if chunkIndex == chunkCount - 1
              then finalChunk
              else maxBound
        )
  where
    chunkCount = maskChunkCount bitCount
    finalBitCount = bitCount .&. (bitsPerChunk - 1)
    finalChunk
      | finalBitCount == 0 = maxBound
      | otherwise = bit finalBitCount - 1

maskSingleton :: Int -> Int -> JoinIrreducibleMask
maskSingleton bitCount bitIndex =
  JoinIrreducibleMask
    ( UVector.generate (maskChunkCount bitCount) $ \chunkIndex ->
        if chunkIndex == bitIndex `quot` bitsPerChunk
          then bit (bitIndex .&. (bitsPerChunk - 1))
          else 0
    )

maskUnion :: JoinIrreducibleMask -> JoinIrreducibleMask -> JoinIrreducibleMask
maskUnion = maskZipWith (.|.)
{-# INLINE maskUnion #-}

maskIntersection :: JoinIrreducibleMask -> JoinIrreducibleMask -> JoinIrreducibleMask
maskIntersection = maskZipWith (.&.)
{-# INLINE maskIntersection #-}

maskDifference :: JoinIrreducibleMask -> JoinIrreducibleMask -> JoinIrreducibleMask
maskDifference = maskZipWith (\leftChunk rightChunk -> leftChunk .&. complement rightChunk)
{-# INLINE maskDifference #-}

maskIsSubsetOf :: JoinIrreducibleMask -> JoinIrreducibleMask -> Bool
maskIsSubsetOf (JoinIrreducibleMask leftChunks) (JoinIrreducibleMask rightChunks) =
  UVector.and
    ( UVector.zipWith
        (\leftChunk rightChunk -> leftChunk .&. complement rightChunk == 0)
        leftChunks
        rightChunks
    )
{-# INLINE maskIsSubsetOf #-}

maskNull :: JoinIrreducibleMask -> Bool
maskNull (JoinIrreducibleMask chunks) =
  UVector.all (== 0) chunks
{-# INLINE maskNull #-}

maskMember :: Int -> JoinIrreducibleMask -> Bool
maskMember bitIndex (JoinIrreducibleMask chunks)
  | chunkIndex >= UVector.length chunks = False
  | otherwise = unboxedIndexInvariant chunks chunkIndex .&. bit (bitIndex .&. (bitsPerChunk - 1)) /= 0
  where
    chunkIndex = bitIndex `quot` bitsPerChunk
{-# INLINE maskMember #-}

maskUnionImages ::
  Int ->
  (Int -> JoinIrreducibleMask) ->
  JoinIrreducibleMask ->
  JoinIrreducibleMask
maskUnionImages chunkCount image =
  maskFoldr
    (maskUnion . image)
    (JoinIrreducibleMask (UVector.replicate chunkCount 0))

maskFoldr :: (Int -> result -> result) -> result -> JoinIrreducibleMask -> result
maskFoldr step initial (JoinIrreducibleMask chunks) =
  UVector.ifoldr foldChunk initial chunks
  where
    foldChunk chunkIndex =
      foldWord (chunkIndex * bitsPerChunk)

    foldWord !baseIndex !remainingBits rest
      | remainingBits == 0 = rest
      | otherwise =
          let bitIndex = countTrailingZeros remainingBits
              nextBits = remainingBits .&. (remainingBits - 1)
           in step
                (baseIndex + bitIndex)
                (foldWord baseIndex nextBits rest)

maskZipWith ::
  (Word64 -> Word64 -> Word64) ->
  JoinIrreducibleMask ->
  JoinIrreducibleMask ->
  JoinIrreducibleMask
maskZipWith combine (JoinIrreducibleMask leftChunks) (JoinIrreducibleMask rightChunks) =
  JoinIrreducibleMask
    ( UVector.generate
        (UVector.length leftChunks)
        ( \chunkIndex ->
            combine
              (unboxedIndexInvariant leftChunks chunkIndex)
              (unboxedIndexInvariant rightChunks chunkIndex)
        )
    )
{-# INLINE maskZipWith #-}

maskSignature :: JoinIrreducibleMask -> JoinIrreducibleMaskSignature
maskSignature (JoinIrreducibleMask chunks) =
  case UVector.length chunks of
    0 -> JoinIrreducibleMaskSignature0
    1 -> JoinIrreducibleMaskSignature1 (unboxedIndexInvariant chunks 0)
    2 -> JoinIrreducibleMaskSignature2 (unboxedIndexInvariant chunks 0) (unboxedIndexInvariant chunks 1)
    _ -> JoinIrreducibleMaskSignatureN (UVector.toList chunks)

maskChunkCount :: Int -> Int
maskChunkCount bitCount
  | bitCount <= 0 = 0
  | otherwise = 1 + (bitCount - 1) `quot` bitsPerChunk
{-# INLINE maskChunkCount #-}

maskUniverseChunkCount :: ContextDistributivePlan -> Int
maskUniverseChunkCount plan =
  1 + (cdipSize plan - 1) `quot` bitsPerChunk
{-# INLINE maskUniverseChunkCount #-}

bitsPerChunk :: Int
bitsPerChunk = 64

guardMaybe :: Bool -> Maybe ()
guardMaybe condition =
  if condition then Just () else Nothing
