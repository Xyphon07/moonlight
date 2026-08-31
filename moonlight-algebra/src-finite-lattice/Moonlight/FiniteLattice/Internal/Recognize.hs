{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TupleSections #-}

module Moonlight.FiniteLattice.Internal.Recognize
  ( specializedContextPlanFromDeclaredPairs,
    specializedContextPlanFromRows,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Bits
  ( (.&.),
    (.|.),
    bit,
    countTrailingZeros,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    contextKeySetAll,
    contextKeySetCardinality,
    contextKeySetChunkCount,
    contextKeySetFromKeys,
    contextKeySetMember,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( ContextBooleanPlan (..),
    ContextBoundedFanPlan (..),
    ContextMaskPlan (..),
    ContextPlan (..),
    ContextTotalOrderPlan (..),
  )
import Moonlight.FiniteLattice.Internal.Relation
  ( ContextRows,
    contextKeyRelated,
    rowForRawKey,
  )
import Moonlight.FiniteLattice.Internal.Topological
  ( topologicalOrder,
  )
specializedContextPlanFromDeclaredPairs ::
  Int ->
  ContextKey ->
  ContextKey ->
  [(ContextKey, ContextKey)] ->
  Maybe ContextPlan
specializedContextPlanFromDeclaredPairs size topKey bottomKey declaredPairs =
  totalOrderPlanFromDeclaredPairs size topKey bottomKey declaredPairs
    <|> booleanPlanFromDeclaredPairs size topKey bottomKey declaredPairs
    <|> boundedFanPlanFromDeclaredPairs size topKey bottomKey declaredPairs

specializedContextPlanFromRows ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Maybe ContextPlan
specializedContextPlanFromRows size topKey bottomKey upperRows lowerRows =
  totalOrderPlanFromRows size topKey bottomKey upperRows lowerRows
    <|> booleanPlanFromRows size topKey bottomKey upperRows lowerRows
    <|> boundedFanPlanFromRows size topKey bottomKey upperRows lowerRows

totalOrderPlanFromDeclaredPairs ::
  Int ->
  ContextKey ->
  ContextKey ->
  [(ContextKey, ContextKey)] ->
  Maybe ContextPlan
totalOrderPlanFromDeclaredPairs size topKey@(ContextKey topOrdinal) bottomKey@(ContextKey bottomOrdinal) declaredPairs
  | size <= 0 = Nothing
  | not (contextKeysInBounds size topKey bottomKey) = Nothing
  | size == 1 =
      if topKey == bottomKey && Set.null strictPairs
        then
          Just
            ( TotalOrderPlan
                ContextTotalOrderPlan
                  { ctoTopKey = topKey,
                    ctoRankByKey = UVector.singleton 0,
                    ctoKeyByRank = UVector.singleton bottomOrdinal
                  }
            )
        else Nothing
  | topKey == bottomKey = Nothing
  | Set.size strictPairs /= size - 1 = Nothing
  | IntMap.size successorBySource /= size - 1 = Nothing
  | IntMap.size predecessorByTarget /= size - 1 = Nothing
  | IntMap.member bottomOrdinal predecessorByTarget = Nothing
  | IntMap.member topOrdinal successorBySource = Nothing
  | otherwise = do
      let path = successorPath size successorBySource bottomOrdinal
      guardMaybe (length path == size)
      guardMaybe (IntSet.fromList path == allContextKeyOrdinals size)
      let rankByKey = totalOrderRankByKey size path
      guardMaybe (totalOrderKeyRankValue rankByKey bottomOrdinal == Just 0)
      guardMaybe (totalOrderKeyRankValue rankByKey topOrdinal == Just (size - 1))
      pure
        ( if path == [0 .. size - 1]
            then OrdinalTotalOrderPlan size
            else
              TotalOrderPlan
              ContextTotalOrderPlan
                { ctoTopKey = topKey,
                  ctoRankByKey = rankByKey,
                    ctoKeyByRank = UVector.fromList path
                  }
        )
  where
    strictPairs = declaredStrictPairOrdinals declaredPairs
    successorBySource = IntMap.fromList (Set.toAscList strictPairs)
    predecessorByTarget =
      IntMap.fromList
        [ (targetOrdinal, sourceOrdinal)
        | (sourceOrdinal, targetOrdinal) <- Set.toAscList strictPairs
        ]

totalOrderPlanFromRows ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Maybe ContextPlan
totalOrderPlanFromRows size topKey bottomKey upperRows lowerRows = do
  guardMaybe (size > 0)
  let rankEntries =
        [ (keyOrdinal, contextKeySetCardinality (rowForRawKey lowerRows keyOrdinal) - 1)
        | keyOrdinal <- [0 .. size - 1]
        ]
      ranks = IntSet.fromList (fmap snd rankEntries)
  guardMaybe (ranks == IntSet.fromDistinctAscList [0 .. size - 1])
  guardMaybe
    ( all
        (\keyOrdinal ->
           contextKeySetCardinality (rowForRawKey upperRows keyOrdinal)
             + contextKeySetCardinality (rowForRawKey lowerRows keyOrdinal)
             == size + 1
        )
        [0 .. size - 1]
    )
  let rankByKey =
        UVector.accum
          (\_ rank -> rank)
          (UVector.replicate size (-1))
          rankEntries
      keyByRank =
        UVector.accum
          (\_ keyOrdinal -> keyOrdinal)
          (UVector.replicate size (-1))
          [ (rank, keyOrdinal)
          | (keyOrdinal, rank) <- rankEntries
          ]
  guardMaybe (UVector.all (>= 0) rankByKey)
  guardMaybe (UVector.all (>= 0) keyByRank)
  guardMaybe (totalOrderKeyRankValue rankByKey (contextKeyOrdinal bottomKey) == Just 0)
  guardMaybe (totalOrderKeyRankValue rankByKey (contextKeyOrdinal topKey) == Just (size - 1))
  pure
    ( if all (uncurry (==)) rankEntries
        then OrdinalTotalOrderPlan size
        else
          TotalOrderPlan
            ContextTotalOrderPlan
              { ctoTopKey = topKey,
                ctoRankByKey = rankByKey,
                ctoKeyByRank = keyByRank
              }
    )

boundedFanPlanFromDeclaredPairs ::
  Int ->
  ContextKey ->
  ContextKey ->
  [(ContextKey, ContextKey)] ->
  Maybe ContextPlan
boundedFanPlanFromDeclaredPairs size topKey@(ContextKey topOrdinal) bottomKey@(ContextKey bottomOrdinal) declaredPairs
  | size < 3 = Nothing
  | topKey == bottomKey = Nothing
  | not (contextKeysInBounds size topKey bottomKey) = Nothing
  | declaredStrictPairOrdinals declaredPairs /= expectedPairs = Nothing
  | otherwise =
      Just
        ( if bottomOrdinal == 0 && topOrdinal == size - 1
            then OrdinalBoundedFanPlan size
            else
              BoundedFanPlan
                ContextBoundedFanPlan
                  { cbfSize = size,
                    cbfTopKey = topKey,
                    cbfBottomKey = bottomKey,
                    cbfAtomKeys = contextKeySetFromKeys chunkCount atomOrdinals,
                    cbfAllKeys = contextKeySetAll size
                  }
        )
  where
    chunkCount = contextKeySetChunkCount size
    atomOrdinals =
      [ keyOrdinal
      | keyOrdinal <- [0 .. size - 1],
        keyOrdinal /= topOrdinal,
        keyOrdinal /= bottomOrdinal
      ]
    expectedPairs =
      Set.fromList
        ( fmap (bottomOrdinal,) atomOrdinals
            <> fmap (,topOrdinal) atomOrdinals
        )

boundedFanPlanFromRows ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Maybe ContextPlan
boundedFanPlanFromRows size topKey bottomKey upperRows lowerRows = do
  guardMaybe (size >= 3 && topKey /= bottomKey)
  guardMaybe
    ( all
        (boundedFanKeyRowsMatch size topKey bottomKey upperRows lowerRows)
        [0 .. size - 1]
    )
  pure
    ( if contextKeyOrdinal bottomKey == 0 && contextKeyOrdinal topKey == size - 1
        then OrdinalBoundedFanPlan size
        else
          BoundedFanPlan
            ContextBoundedFanPlan
              { cbfSize = size,
                cbfTopKey = topKey,
                cbfBottomKey = bottomKey,
                cbfAtomKeys =
                  contextKeySetFromKeys
                    (contextKeySetChunkCount size)
                    [ keyOrdinal
                    | keyOrdinal <- [0 .. size - 1],
                      ContextKey keyOrdinal /= topKey,
                      ContextKey keyOrdinal /= bottomKey
                    ],
                cbfAllKeys = contextKeySetAll size
              }
    )

boundedFanKeyRowsMatch ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Int ->
  Bool
boundedFanKeyRowsMatch size topKey bottomKey upperRows lowerRows keyOrdinal
  | key == bottomKey =
      upperCardinality == size
        && lowerCardinality == 1
        && contextKeySetMember keyOrdinal lowerRow
  | key == topKey =
      upperCardinality == 1
        && lowerCardinality == size
        && contextKeySetMember keyOrdinal upperRow
  | otherwise =
      upperCardinality == 2
        && lowerCardinality == 2
        && contextKeySetMember keyOrdinal upperRow
        && contextKeySetMember (contextKeyOrdinal topKey) upperRow
        && contextKeySetMember keyOrdinal lowerRow
        && contextKeySetMember (contextKeyOrdinal bottomKey) lowerRow
  where
    key = ContextKey keyOrdinal
    upperRow = rowForRawKey upperRows keyOrdinal
    lowerRow = rowForRawKey lowerRows keyOrdinal
    upperCardinality = contextKeySetCardinality upperRow
    lowerCardinality = contextKeySetCardinality lowerRow

booleanPlanFromDeclaredPairs ::
  Int ->
  ContextKey ->
  ContextKey ->
  [(ContextKey, ContextKey)] ->
  Maybe ContextPlan
booleanPlanFromDeclaredPairs size topKey bottomKey@(ContextKey bottomOrdinal) declaredPairs = do
  atomCount <- booleanAtomCountFromSize size
  guardMaybe (atomCount >= 2)
  guardMaybe (contextKeysInBounds size topKey bottomKey && topKey /= bottomKey)
  topologicalOrder' <- coverTopologicalOrder size strictPairs
  let successorSets = coverSuccessorSets strictPairs
      predecessorSets = coverPredecessorSets strictPairs
      atomOrdinals =
        IntSet.toAscList
          (IntMap.findWithDefault IntSet.empty bottomOrdinal successorSets)
  guardMaybe (length atomOrdinals == atomCount)
  let atomBitByKey =
        IntMap.fromDistinctAscList
          [ (atomOrdinal, bit atomIndex)
          | (atomOrdinal, atomIndex) <- zip atomOrdinals [0 .. atomCount - 1]
          ]
      fullMask = bit atomCount - 1
  maskByKeyMap <-
    booleanMaskMapFromCover
      size
      bottomOrdinal
      atomBitByKey
      predecessorSets
      topologicalOrder'
  let keyByMaskMap =
        Map.fromList
          [ (mask, keyOrdinal)
          | (keyOrdinal, mask) <- IntMap.toAscList maskByKeyMap
          ]
  guardMaybe (IntMap.size maskByKeyMap == size)
  guardMaybe (Map.size keyByMaskMap == size)
  guardMaybe (IntMap.lookup bottomOrdinal maskByKeyMap == Just 0)
  guardMaybe (IntMap.lookup (contextKeyOrdinal topKey) maskByKeyMap == Just fullMask)
  let maskByKey =
        UVector.generate
          size
          (\keyOrdinal -> IntMap.findWithDefault maxBound keyOrdinal maskByKeyMap)
      keyByMask =
        UVector.generate
          size
          (\maskOrdinal -> Map.findWithDefault (-1) (fromIntegral maskOrdinal) keyByMaskMap)
  guardMaybe (UVector.all (/= maxBound) maskByKey)
  guardMaybe (UVector.all (>= 0) keyByMask)
  guardMaybe
    ( strictPairs
        == booleanExpectedCoverPairs atomCount fullMask keyByMask
    )
  pure
    ( MaskPlan
        ( BooleanPlan
            ContextBooleanPlan
              { cboAtomCount = atomCount,
                cboFullMask = fullMask,
                cboMaskByKey = maskByKey,
                cboKeyByMask = keyByMask
              }
        )
    )
  where
    strictPairs = declaredStrictPairOrdinals declaredPairs

booleanPlanFromRows ::
  Int ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Maybe ContextPlan
booleanPlanFromRows size topKey bottomKey upperRows lowerRows = do
  atomCount <- booleanAtomCountFromSize size
  guardMaybe (atomCount >= 2)
  guardMaybe (contextKeysInBounds size topKey bottomKey)
  let atomOrdinals =
        [ keyOrdinal
        | keyOrdinal <- [0 .. size - 1],
          let lowerSet = rowForRawKey lowerRows keyOrdinal,
          ContextKey keyOrdinal /= bottomKey,
          contextKeySetCardinality lowerSet == 2,
          contextKeySetMember (contextKeyOrdinal bottomKey) lowerSet,
          contextKeySetMember keyOrdinal lowerSet
        ]
  guardMaybe (length atomOrdinals == atomCount)
  let atomBitByKey =
        IntMap.fromDistinctAscList
          [ (atomOrdinal, bit atomIndex)
          | (atomOrdinal, atomIndex) <- zip atomOrdinals [0 .. atomCount - 1]
          ]
      fullMask = bit atomCount - 1
      maskByKeyMap =
        IntMap.fromDistinctAscList
          [ (keyOrdinal, booleanMaskForRows upperRows atomBitByKey keyOrdinal)
          | keyOrdinal <- [0 .. size - 1]
          ]
      keyByMaskMap =
        Map.fromList
          [ (mask, keyOrdinal)
          | (keyOrdinal, mask) <- IntMap.toAscList maskByKeyMap
          ]
  guardMaybe (Map.size keyByMaskMap == size)
  guardMaybe (IntMap.lookup (contextKeyOrdinal bottomKey) maskByKeyMap == Just 0)
  guardMaybe (IntMap.lookup (contextKeyOrdinal topKey) maskByKeyMap == Just fullMask)
  guardMaybe (booleanOrderMatchesMasks size upperRows maskByKeyMap)
  let maskByKey =
        UVector.generate
          size
          (\keyOrdinal -> IntMap.findWithDefault maxBound keyOrdinal maskByKeyMap)
      keyByMask =
        UVector.generate
          size
          (\maskOrdinal -> Map.findWithDefault (-1) (fromIntegral maskOrdinal) keyByMaskMap)
  guardMaybe (UVector.all (/= maxBound) maskByKey)
  guardMaybe (UVector.all (>= 0) keyByMask)
  pure
    ( MaskPlan
        ( BooleanPlan
            ContextBooleanPlan
              { cboAtomCount = atomCount,
                cboFullMask = fullMask,
                cboMaskByKey = maskByKey,
                cboKeyByMask = keyByMask
              }
        )
    )

booleanMaskForRows ::
  ContextRows ->
  IntMap Word64 ->
  Int ->
  Word64
booleanMaskForRows upperRows atomBitByKey keyOrdinal =
  IntMap.foldlWithKey'
    (\mask atomOrdinal atomBit ->
       if
         contextKeyRelated
           upperRows
           (ContextKey atomOrdinal)
           (ContextKey keyOrdinal)
         then mask .|. atomBit
         else mask
    )
    0
    atomBitByKey

booleanOrderMatchesMasks ::
  Int ->
  ContextRows ->
  IntMap Word64 ->
  Bool
booleanOrderMatchesMasks size upperRows maskByKeyMap =
  all relationMatches
    [ (leftOrdinal, rightOrdinal)
    | leftOrdinal <- [0 .. size - 1],
      rightOrdinal <- [0 .. size - 1]
    ]
  where
    relationMatches (leftOrdinal, rightOrdinal) =
      case
        ( IntMap.lookup leftOrdinal maskByKeyMap,
          IntMap.lookup rightOrdinal maskByKeyMap
        )
        of
        (Just leftMask, Just rightMask) ->
          contextKeyRelated
            upperRows
            (ContextKey leftOrdinal)
            (ContextKey rightOrdinal)
            == (leftMask .&. rightMask == leftMask)
        _ -> False

booleanMaskMapFromCover ::
  Int ->
  Int ->
  IntMap Word64 ->
  IntMap IntSet ->
  [Int] ->
  Maybe (IntMap Word64)
booleanMaskMapFromCover size bottomOrdinal atomBitByKey predecessorSets =
  foldM includeKey IntMap.empty
  where
    includeKey masksByKey keyOrdinal
      | keyOrdinal < 0 || keyOrdinal >= size = Nothing
      | keyOrdinal == bottomOrdinal =
          Just (IntMap.insert keyOrdinal 0 masksByKey)
      | Just atomMask <- IntMap.lookup keyOrdinal atomBitByKey =
          Just (IntMap.insert keyOrdinal atomMask masksByKey)
      | otherwise = do
          let predecessorOrdinals =
                IntSet.toAscList
                  (IntMap.findWithDefault IntSet.empty keyOrdinal predecessorSets)
          guardMaybe (not (null predecessorOrdinals))
          predecessorMasks <-
            traverse (`IntMap.lookup` masksByKey) predecessorOrdinals
          let mask = Foldable.foldl' (.|.) 0 predecessorMasks
          guardMaybe (mask /= 0)
          Just (IntMap.insert keyOrdinal mask masksByKey)

booleanExpectedCoverPairs ::
  Int ->
  Word64 ->
  UVector.Vector Int ->
  Set (Int, Int)
booleanExpectedCoverPairs atomCount fullMask keyByMask =
  Set.fromList
    [ (lowerOrdinal, upperOrdinal)
    | lowerMask <- [0 .. fullMask],
      atomIndex <- [0 .. atomCount - 1],
      lowerMask .&. bit atomIndex == 0,
      let upperMask = lowerMask .|. bit atomIndex,
      Just lowerOrdinal <- [keyOrdinalForMask keyByMask lowerMask],
      Just upperOrdinal <- [keyOrdinalForMask keyByMask upperMask]
    ]

keyOrdinalForMask :: UVector.Vector Int -> Word64 -> Maybe Int
keyOrdinalForMask keyByMask mask
  | mask > fromIntegral (maxBound :: Int) = Nothing
  | otherwise =
      case keyByMask UVector.!? fromIntegral mask of
        Just keyOrdinal
          | keyOrdinal >= 0 -> Just keyOrdinal
        _ -> Nothing

booleanAtomCountFromSize :: Int -> Maybe Int
booleanAtomCountFromSize size
  | size <= 0 = Nothing
  | size .&. (size - 1) /= 0 = Nothing
  | otherwise = Just (countTrailingZeros size)

coverTopologicalOrder :: Int -> Set (Int, Int) -> Maybe [Int]
coverTopologicalOrder size strictPairs =
  topologicalOrder size $ \sourceOrdinal step initial ->
    IntSet.foldr
      step
      initial
      (IntMap.findWithDefault IntSet.empty sourceOrdinal successorSets)
  where
    successorSets = coverSuccessorSets strictPairs

coverSuccessorSets :: Set (Int, Int) -> IntMap IntSet
coverSuccessorSets =
  Foldable.foldl'
    (\successors (sourceOrdinal, targetOrdinal) ->
       IntMap.insertWith
         IntSet.union
         sourceOrdinal
         (IntSet.singleton targetOrdinal)
         successors
    )
    IntMap.empty
    . Set.toAscList

coverPredecessorSets :: Set (Int, Int) -> IntMap IntSet
coverPredecessorSets =
  Foldable.foldl'
    (\predecessors (sourceOrdinal, targetOrdinal) ->
       IntMap.insertWith
         IntSet.union
         targetOrdinal
         (IntSet.singleton sourceOrdinal)
         predecessors
    )
    IntMap.empty
    . Set.toAscList

declaredStrictPairOrdinals ::
  [(ContextKey, ContextKey)] ->
  Set (Int, Int)
declaredStrictPairOrdinals =
  Set.fromList
    . foldMap
      (\(ContextKey sourceOrdinal, ContextKey targetOrdinal) ->
         if sourceOrdinal == targetOrdinal
           then []
           else [(sourceOrdinal, targetOrdinal)]
      )

successorPath :: Int -> IntMap Int -> Int -> [Int]
successorPath size successorBySource bottomOrdinal =
  take size (unfoldr next (Just bottomOrdinal))
  where
    next Nothing = Nothing
    next (Just sourceOrdinal) =
      Just (sourceOrdinal, IntMap.lookup sourceOrdinal successorBySource)

totalOrderRankByKey :: Int -> [Int] -> UVector.Vector Int
totalOrderRankByKey size path =
  UVector.accum
    (\_ rank -> rank)
    (UVector.replicate size (-1))
    [ (keyOrdinal, rank)
    | (keyOrdinal, rank) <- zip path [0 ..],
      keyOrdinal >= 0,
      keyOrdinal < size
    ]

totalOrderKeyRankValue :: UVector.Vector Int -> Int -> Maybe Int
totalOrderKeyRankValue rankByKey keyOrdinal =
  case rankByKey UVector.!? keyOrdinal of
    Just rank
      | rank >= 0 -> Just rank
    _ -> Nothing

allContextKeyOrdinals :: Int -> IntSet
allContextKeyOrdinals size =
  IntSet.fromDistinctAscList [0 .. size - 1]

contextKeysInBounds :: Int -> ContextKey -> ContextKey -> Bool
contextKeysInBounds size leftKey rightKey =
  contextKeyInBounds size leftKey && contextKeyInBounds size rightKey

contextKeyInBounds :: Int -> ContextKey -> Bool
contextKeyInBounds size (ContextKey keyOrdinal) =
  keyOrdinal >= 0 && keyOrdinal < size

guardMaybe :: Bool -> Maybe ()
guardMaybe condition =
  if condition then Just () else Nothing
