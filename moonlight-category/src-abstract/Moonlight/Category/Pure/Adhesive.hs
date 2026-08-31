{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adhesive and PBPO categories with pushout-complement and PBPO complement
-- witnesses for double-pushout rewriting, plus monic-match and square-commutativity
-- checks.
module Moonlight.Category.Pure.Adhesive
  ( DenseIntSet,
    denseIntSetUniverseSize,
    denseIntSetEmpty,
    denseIntSetFull,
    denseIntSetInterval,
    denseIntSetFromAscList,
    denseIntSetMember,
    denseIntSetIsSubsetOf,
    denseIntSetUnion,
    denseIntSetUnions,
    denseIntSetIntersection,
    denseIntSetDifference,
    denseIntSetIntersects,
    denseIntSetSize,
    denseIntSetWeight,
    denseIntSetFoldl',
    AdhesiveCategory (..),
    PBPOAdhesiveCategory (..),
    MonicMatchComponents (..),
    PushoutComplementComponents (..),
    PBPOComplementComponents (..),
    MonicMatchWitness,
    monicMatchArrow,
    PushoutComplementWitness,
    pushoutComplementRuleLeg,
    pushoutComplementMonicMatch,
    pushoutComplementObject,
    pushoutComplementBorrowedLeg,
    pushoutComplementResidualLeg,
    PBPOComplementWitness,
    pbpoComplementRuleLeg,
    pbpoComplementMonicMatch,
    pbpoComplementPullbackObject,
    pbpoComplementPullbackToBorrowed,
    pbpoComplementPullbackToMatch,
    pbpoComplementPushoutObject,
    pbpoComplementPushoutFromComplement,
    pbpoComplementPushoutFromMatch,
    pbpoComplementBorrowedLeg,
    pbpoComplementResidualLeg,
    witnessMonic,
    pushoutComplement,
    pbpoComplement,
    pushoutComplementSquareCommutes,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
  )
where

import Control.Monad (foldM, guard)
import Data.Bits (Bits (complement, popCount, shiftL, shiftR, testBit, (.&.), (.|.)), countTrailingZeros)
import Data.Kind (Constraint, Type)
import Data.Word (Word64)
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.Limits (HasPullbacks (..), HasPushouts (..))

type DenseIntSet :: Type
-- | An immutable integer set over a fixed universe of at most 512 elements.
data DenseIntSet = DenseIntSet !Int !Int !Int !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64
  deriving stock (Eq, Ord, Show)

-- | The exclusive upper bound of admissible members.
denseIntSetUniverseSize :: DenseIntSet -> Int
denseIntSetUniverseSize (DenseIntSet universeSize _ _ _ _ _ _ _ _ _ _) =
  universeSize
{-# INLINE denseIntSetUniverseSize #-}

-- | Construct the empty set when the universe bound is supported.
denseIntSetEmpty :: Int -> Maybe DenseIntSet
denseIntSetEmpty universeSize = do
  guard (denseUniverseSizeValid universeSize)
  pure (DenseIntSet universeSize 0 0 0 0 0 0 0 0 0 0)
{-# INLINE denseIntSetEmpty #-}

-- | Construct the set containing its entire supported universe.
denseIntSetFull :: Int -> Maybe DenseIntSet
denseIntSetFull universeSize = do
  guard (denseUniverseSizeValid universeSize)
  pure
    ( denseIntSetFromWords
        universeSize
        (denseMaskForWord universeSize 0)
        (denseMaskForWord universeSize 1)
        (denseMaskForWord universeSize 2)
        (denseMaskForWord universeSize 3)
        (denseMaskForWord universeSize 4)
        (denseMaskForWord universeSize 5)
        (denseMaskForWord universeSize 6)
        (denseMaskForWord universeSize 7)
    )
{-# INLINE denseIntSetFull #-}

-- | Construct a bounded contiguous interval.
denseIntSetInterval :: Int -> Int -> Int -> Maybe DenseIntSet
denseIntSetInterval universeSize start count = do
  guard (denseUniverseSizeValid universeSize)
  guard (count >= 0 && count <= universeSize)
  guard (start >= 0 && start <= universeSize - count)
  denseIntSetFromAscList universeSize [start .. start + count - 1]
{-# INLINE denseIntSetInterval #-}

-- | Construct from strictly ascending, in-bounds members.
denseIntSetFromAscList :: Int -> [Int] -> Maybe DenseIntSet
denseIntSetFromAscList universeSize values = do
  guard (denseUniverseSizeValid universeSize)
  guard (denseAscValuesInBounds universeSize values)
  pure (foldl' denseIntSetInsertTrusted (DenseIntSet universeSize 0 0 0 0 0 0 0 0 0 0) values)
{-# INLINE denseIntSetFromAscList #-}

-- | Test membership; out-of-universe values are absent.
denseIntSetMember :: Int -> DenseIntSet -> Bool
denseIntSetMember value set@(DenseIntSet universeSize _ _ _ _ _ _ _ _ _ _) =
  value >= 0
    && value < universeSize
    && testBit (denseWordAt (denseWordIndex value) set) (denseBitOffset value)
{-# INLINE denseIntSetMember #-}

-- | Test inclusion when both sets share a universe.
denseIntSetIsSubsetOf :: DenseIntSet -> DenseIntSet -> Maybe Bool
denseIntSetIsSubsetOf =
  denseIntSetCompareWords
    (\leftWord rightWord -> leftWord .&. complement rightWord == 0)
{-# INLINE denseIntSetIsSubsetOf #-}

-- | Union sets with the same universe.
denseIntSetUnion :: DenseIntSet -> DenseIntSet -> Maybe DenseIntSet
denseIntSetUnion =
  denseIntSetZipWords (.|.)
{-# INLINE denseIntSetUnion #-}

-- | Union a family of sets in one declared universe.
denseIntSetUnions :: Int -> [DenseIntSet] -> Maybe DenseIntSet
denseIntSetUnions universeSize sets = do
  emptySet <- denseIntSetEmpty universeSize
  foldM denseIntSetUnion emptySet sets
{-# INLINE denseIntSetUnions #-}

-- | Intersect sets with the same universe.
denseIntSetIntersection :: DenseIntSet -> DenseIntSet -> Maybe DenseIntSet
denseIntSetIntersection =
  denseIntSetZipWords (.&.)
{-# INLINE denseIntSetIntersection #-}

-- | Subtract the right set when universes agree.
denseIntSetDifference :: DenseIntSet -> DenseIntSet -> Maybe DenseIntSet
denseIntSetDifference =
  denseIntSetZipWords (\leftWord rightWord -> leftWord .&. complement rightWord)
{-# INLINE denseIntSetDifference #-}

-- | Test whether same-universe sets overlap.
denseIntSetIntersects :: DenseIntSet -> DenseIntSet -> Maybe Bool
denseIntSetIntersects left right =
  denseIntSetCompareWords (\leftWord rightWord -> leftWord .&. rightWord == 0) left right
    >>= pure . not
{-# INLINE denseIntSetIntersects #-}

-- | The set cardinality.
denseIntSetSize :: DenseIntSet -> Int
denseIntSetSize (DenseIntSet _ size _ _ _ _ _ _ _ _ _) =
  size
{-# INLINE denseIntSetSize #-}

-- | The sum of member indices.
denseIntSetWeight :: DenseIntSet -> Int
denseIntSetWeight (DenseIntSet _ _ weight _ _ _ _ _ _ _ _) =
  weight
{-# INLINE denseIntSetWeight #-}

-- | Strictly fold members in ascending order.
denseIntSetFoldl' :: (value -> Int -> value) -> value -> DenseIntSet -> value
denseIntSetFoldl' step initialValue (DenseIntSet _ _ _ word0 word1 word2 word3 word4 word5 word6 word7) =
  denseWordFoldl' step (7 * denseWordBits) word7
    ( denseWordFoldl' step (6 * denseWordBits) word6
        ( denseWordFoldl' step (5 * denseWordBits) word5
            ( denseWordFoldl' step (4 * denseWordBits) word4
                ( denseWordFoldl' step (3 * denseWordBits) word3
                    ( denseWordFoldl' step (2 * denseWordBits) word2
                        (denseWordFoldl' step denseWordBits word1 (denseWordFoldl' step 0 word0 initialValue))
                    )
                )
            )
        )
    )
{-# INLINE denseIntSetFoldl' #-}

denseIntSetZipWords :: (Word64 -> Word64 -> Word64) -> DenseIntSet -> DenseIntSet -> Maybe DenseIntSet
denseIntSetZipWords combine (DenseIntSet leftUniverse _ _ left0 left1 left2 left3 left4 left5 left6 left7) (DenseIntSet rightUniverse _ _ right0 right1 right2 right3 right4 right5 right6 right7) = do
  guard (leftUniverse == rightUniverse)
  pure
    ( denseIntSetFromWords
        leftUniverse
        (combine left0 right0)
        (combine left1 right1)
        (combine left2 right2)
        (combine left3 right3)
        (combine left4 right4)
        (combine left5 right5)
        (combine left6 right6)
        (combine left7 right7)
    )
{-# INLINE denseIntSetZipWords #-}

denseIntSetCompareWords :: (Word64 -> Word64 -> Bool) -> DenseIntSet -> DenseIntSet -> Maybe Bool
denseIntSetCompareWords compareWords (DenseIntSet leftUniverse _ _ left0 left1 left2 left3 left4 left5 left6 left7) (DenseIntSet rightUniverse _ _ right0 right1 right2 right3 right4 right5 right6 right7) = do
  guard (leftUniverse == rightUniverse)
  pure
    ( compareWords left0 right0
        && compareWords left1 right1
        && compareWords left2 right2
        && compareWords left3 right3
        && compareWords left4 right4
        && compareWords left5 right5
        && compareWords left6 right6
        && compareWords left7 right7
    )
{-# INLINE denseIntSetCompareWords #-}

denseIntSetInsertTrusted :: DenseIntSet -> Int -> DenseIntSet
denseIntSetInsertTrusted (DenseIntSet universeSize size weight word0 word1 word2 word3 word4 word5 word6 word7) value =
  case denseWordIndex value of
    0 -> DenseIntSet universeSize nextSize nextWeight (inserted word0) word1 word2 word3 word4 word5 word6 word7
    1 -> DenseIntSet universeSize nextSize nextWeight word0 (inserted word1) word2 word3 word4 word5 word6 word7
    2 -> DenseIntSet universeSize nextSize nextWeight word0 word1 (inserted word2) word3 word4 word5 word6 word7
    3 -> DenseIntSet universeSize nextSize nextWeight word0 word1 word2 (inserted word3) word4 word5 word6 word7
    4 -> DenseIntSet universeSize nextSize nextWeight word0 word1 word2 word3 (inserted word4) word5 word6 word7
    5 -> DenseIntSet universeSize nextSize nextWeight word0 word1 word2 word3 word4 (inserted word5) word6 word7
    6 -> DenseIntSet universeSize nextSize nextWeight word0 word1 word2 word3 word4 word5 (inserted word6) word7
    _ -> DenseIntSet universeSize nextSize nextWeight word0 word1 word2 word3 word4 word5 word6 (inserted word7)
  where
    nextSize =
      size + 1
    nextWeight =
      weight + value
    inserted word =
      word .|. shiftL 1 (denseBitOffset value)
{-# INLINE denseIntSetInsertTrusted #-}

denseWordAt :: Int -> DenseIntSet -> Word64
denseWordAt wordIndex (DenseIntSet _ _ _ word0 word1 word2 word3 word4 word5 word6 word7) =
  case wordIndex of
    0 -> word0
    1 -> word1
    2 -> word2
    3 -> word3
    4 -> word4
    5 -> word5
    6 -> word6
    _ -> word7
{-# INLINE denseWordAt #-}

denseIntSetFromWords :: Int -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> DenseIntSet
denseIntSetFromWords universeSize word0 word1 word2 word3 word4 word5 word6 word7 =
  DenseIntSet
    universeSize
    ( denseWordSize word0
        + denseWordSize word1
        + denseWordSize word2
        + denseWordSize word3
        + denseWordSize word4
        + denseWordSize word5
        + denseWordSize word6
        + denseWordSize word7
    )
    ( denseWordWeight 0 word0
        + denseWordWeight denseWordBits word1
        + denseWordWeight (2 * denseWordBits) word2
        + denseWordWeight (3 * denseWordBits) word3
        + denseWordWeight (4 * denseWordBits) word4
        + denseWordWeight (5 * denseWordBits) word5
        + denseWordWeight (6 * denseWordBits) word6
        + denseWordWeight (7 * denseWordBits) word7
    )
    word0
    word1
    word2
    word3
    word4
    word5
    word6
    word7
{-# INLINE denseIntSetFromWords #-}

denseWordSize :: Word64 -> Int
denseWordSize =
  popCount
{-# INLINE denseWordSize #-}

denseWordWeight :: Int -> Word64 -> Int
denseWordWeight base word =
  denseWordFoldl' (\total value -> total + value) base word 0
{-# INLINE denseWordWeight #-}

denseWordFoldl' :: (value -> Int -> value) -> Int -> Word64 -> value -> value
denseWordFoldl' step base word initialValue =
  foldBits initialValue word
  where
    foldBits !current currentWord
      | currentWord == 0 =
          current
      | otherwise =
          let bitOffset = countTrailingZeros currentWord
              nextWord = currentWord .&. (currentWord - 1)
           in foldBits (step current (base + bitOffset)) nextWord
{-# INLINE denseWordFoldl' #-}

denseAscValuesInBounds :: Int -> [Int] -> Bool
denseAscValuesInBounds universeSize values =
  case values of
    [] ->
      True
    firstValue : remainingValues ->
      firstValue >= 0
        && firstValue < universeSize
        && snd
          ( foldl'
              ( \(previousValue, valid) value ->
                  (value, valid && value > previousValue && value < universeSize)
              )
              (firstValue, True)
              remainingValues
          )
{-# INLINE denseAscValuesInBounds #-}


denseMaskForWord :: Int -> Int -> Word64
denseMaskForWord universeSize wordIndex
  | remainingBits >= denseWordBits =
      complement 0
  | remainingBits <= 0 =
      0
  | otherwise =
      shiftR (complement 0) (denseWordBits - remainingBits)
  where
    remainingBits =
      universeSize - wordIndex * denseWordBits
{-# INLINE denseMaskForWord #-}

denseUniverseSizeValid :: Int -> Bool
denseUniverseSizeValid universeSize =
  universeSize >= 0 && universeSize <= denseMaxUniverseSize
{-# INLINE denseUniverseSizeValid #-}

denseWordIndex :: Int -> Int
denseWordIndex value =
  value `div` denseWordBits
{-# INLINE denseWordIndex #-}

denseBitOffset :: Int -> Int
denseBitOffset value =
  value `mod` denseWordBits
{-# INLINE denseBitOffset #-}

denseWordBits :: Int
denseWordBits =
  64
{-# INLINE denseWordBits #-}

denseMaxUniverseSize :: Int
denseMaxUniverseSize =
  512
{-# INLINE denseMaxUniverseSize #-}

type MonicMatchComponents :: Type -> Type
-- | Raw evidence supplied by an adhesive-category interpreter for a monic match.
data MonicMatchComponents c = MonicMatchComponents
  { monicMatchComponentArrow :: Mor c
  }

type PushoutComplementComponents :: Type -> Type
-- | The object and legs produced by a pushout-complement construction.
data PushoutComplementComponents c = PushoutComplementComponents
  { pushoutComplementComponentObject :: Ob c,
    pushoutComplementComponentBorrowedLeg :: Mor c,
    pushoutComplementComponentResidualLeg :: Mor c
  }

type PBPOComplementComponents :: Type -> Type
-- | The pullback and pushout sections produced by a PBPO complement.
data PBPOComplementComponents c = PBPOComplementComponents
  { pbpoComplementComponentPullbackObject :: Ob c,
    pbpoComplementComponentPullbackToBorrowed :: Mor c,
    pbpoComplementComponentPullbackToMatch :: Mor c,
    pbpoComplementComponentPushoutObject :: Ob c,
    pbpoComplementComponentPushoutFromComplement :: Mor c,
    pbpoComplementComponentPushoutFromMatch :: Mor c,
    pbpoComplementComponentBorrowedLeg :: Mor c,
    pbpoComplementComponentResidualLeg :: Mor c
  }

type MonicMatchWitness :: Type -> Type
-- | Opaque, admitted evidence that a match is monic.
data MonicMatchWitness c = MonicMatchWitness !(MonicMatchComponents c)

type PushoutComplementWitness :: Type -> Type
-- | Opaque pushout-complement evidence bound to its rule leg and monic match.
data PushoutComplementWitness c = PushoutComplementWitness !(Mor c) !(MonicMatchWitness c) !(PushoutComplementComponents c)

type PBPOComplementWitness :: Type -> Type
-- | Opaque PBPO-complement evidence bound to its rule leg and monic match.
data PBPOComplementWitness c = PBPOComplementWitness !(Mor c) !(MonicMatchWitness c) !(PBPOComplementComponents c)

type AdhesiveCategory :: Type -> Constraint
-- | A category that can admit monic matches and construct pushout complements.
class (HasPushouts c, HasPullbacks c) => AdhesiveCategory c where
  monicMatchComponents :: c -> Mor c -> Maybe (MonicMatchComponents c)

  pushoutComplementComponents ::
    c ->
    Mor c ->
    MonicMatchWitness c ->
    Maybe (PushoutComplementComponents c)

type PBPOAdhesiveCategory :: Type -> Constraint
-- | An adhesive category that can construct PBPO complements.
class AdhesiveCategory c => PBPOAdhesiveCategory c where
  pbpoComplementComponents ::
    c ->
    Mor c ->
    MonicMatchWitness c ->
    Maybe (PBPOComplementComponents c)
  pbpoComplementComponents categoryValue ruleLeg monicMatch = do
    pushoutComplementComponentsValue <- pushoutComplementComponents categoryValue ruleLeg monicMatch
    (pullbackObject, pullbackToBorrowed, pullbackToMatch) <-
      pullback
        categoryValue
        (pushoutComplementComponentBorrowedLeg pushoutComplementComponentsValue)
        (monicMatchArrow monicMatch)
    (pushoutObject, pushoutFromComplement, pushoutFromMatch) <-
      pushout
        categoryValue
        (pushoutComplementComponentResidualLeg pushoutComplementComponentsValue)
        ruleLeg
    pure
      PBPOComplementComponents
        { pbpoComplementComponentPullbackObject = pullbackObject,
          pbpoComplementComponentPullbackToBorrowed = pullbackToBorrowed,
          pbpoComplementComponentPullbackToMatch = pullbackToMatch,
          pbpoComplementComponentPushoutObject = pushoutObject,
          pbpoComplementComponentPushoutFromComplement = pushoutFromComplement,
          pbpoComplementComponentPushoutFromMatch = pushoutFromMatch,
          pbpoComplementComponentBorrowedLeg = pushoutComplementComponentBorrowedLeg pushoutComplementComponentsValue,
          pbpoComplementComponentResidualLeg = pushoutComplementComponentResidualLeg pushoutComplementComponentsValue
        }

-- | Admit an opaque monic-match witness.
witnessMonic :: AdhesiveCategory c => c -> Mor c -> Maybe (MonicMatchWitness c)
witnessMonic categoryValue morphism =
  MonicMatchWitness <$> monicMatchComponents categoryValue morphism

-- | Bind admitted complement components into an opaque witness.
pushoutComplement ::
  AdhesiveCategory c =>
  c ->
  Mor c ->
  MonicMatchWitness c ->
  Maybe (PushoutComplementWitness c)
pushoutComplement categoryValue ruleLeg monicMatch =
  PushoutComplementWitness ruleLeg monicMatch <$> pushoutComplementComponents categoryValue ruleLeg monicMatch

-- | Bind admitted PBPO components into an opaque witness.
pbpoComplement ::
  PBPOAdhesiveCategory c =>
  c ->
  Mor c ->
  MonicMatchWitness c ->
  Maybe (PBPOComplementWitness c)
pbpoComplement categoryValue ruleLeg monicMatch =
  PBPOComplementWitness ruleLeg monicMatch <$> pbpoComplementComponents categoryValue ruleLeg monicMatch

monicMatchArrow :: MonicMatchWitness c -> Mor c
monicMatchArrow (MonicMatchWitness components) =
  monicMatchComponentArrow components

pushoutComplementRuleLeg :: PushoutComplementWitness c -> Mor c
pushoutComplementRuleLeg (PushoutComplementWitness ruleLeg _ _) =
  ruleLeg

pushoutComplementMonicMatch :: PushoutComplementWitness c -> MonicMatchWitness c
pushoutComplementMonicMatch (PushoutComplementWitness _ monicMatch _) =
  monicMatch

pushoutComplementObject :: PushoutComplementWitness c -> Ob c
pushoutComplementObject (PushoutComplementWitness _ _ components) =
  pushoutComplementComponentObject components

pushoutComplementBorrowedLeg :: PushoutComplementWitness c -> Mor c
pushoutComplementBorrowedLeg (PushoutComplementWitness _ _ components) =
  pushoutComplementComponentBorrowedLeg components

pushoutComplementResidualLeg :: PushoutComplementWitness c -> Mor c
pushoutComplementResidualLeg (PushoutComplementWitness _ _ components) =
  pushoutComplementComponentResidualLeg components

pbpoComplementRuleLeg :: PBPOComplementWitness c -> Mor c
pbpoComplementRuleLeg (PBPOComplementWitness ruleLeg _ _) =
  ruleLeg

pbpoComplementMonicMatch :: PBPOComplementWitness c -> MonicMatchWitness c
pbpoComplementMonicMatch (PBPOComplementWitness _ monicMatch _) =
  monicMatch

pbpoComplementPullbackObject :: PBPOComplementWitness c -> Ob c
pbpoComplementPullbackObject (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentPullbackObject components

pbpoComplementPullbackToBorrowed :: PBPOComplementWitness c -> Mor c
pbpoComplementPullbackToBorrowed (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentPullbackToBorrowed components

pbpoComplementPullbackToMatch :: PBPOComplementWitness c -> Mor c
pbpoComplementPullbackToMatch (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentPullbackToMatch components

pbpoComplementPushoutObject :: PBPOComplementWitness c -> Ob c
pbpoComplementPushoutObject (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentPushoutObject components

pbpoComplementPushoutFromComplement :: PBPOComplementWitness c -> Mor c
pbpoComplementPushoutFromComplement (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentPushoutFromComplement components

pbpoComplementPushoutFromMatch :: PBPOComplementWitness c -> Mor c
pbpoComplementPushoutFromMatch (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentPushoutFromMatch components

pbpoComplementBorrowedLeg :: PBPOComplementWitness c -> Mor c
pbpoComplementBorrowedLeg (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentBorrowedLeg components

pbpoComplementResidualLeg :: PBPOComplementWitness c -> Mor c
pbpoComplementResidualLeg (PBPOComplementWitness _ _ components) =
  pbpoComplementComponentResidualLeg components

-- | Check the square carried by a pushout-complement witness.
pushoutComplementSquareCommutes :: (Category c, Eq (Mor c)) => c -> PushoutComplementWitness c -> Bool
pushoutComplementSquareCommutes categoryValue witness =
  case
    ( composeMor categoryValue (pushoutComplementBorrowedLeg witness) (pushoutComplementResidualLeg witness),
      composeMor categoryValue (monicMatchArrow (pushoutComplementMonicMatch witness)) (pushoutComplementRuleLeg witness)
    )
    of
      (Right leftMorphism, Right rightMorphism) -> leftMorphism == rightMorphism
      _ -> False

-- | Check the pullback square carried by a PBPO witness.
pbpoPullbackSquareCommutes :: (Category c, Eq (Mor c)) => c -> PBPOComplementWitness c -> Bool
pbpoPullbackSquareCommutes categoryValue witness =
  case
    ( composeMor categoryValue (pbpoComplementBorrowedLeg witness) (pbpoComplementPullbackToBorrowed witness),
      composeMor categoryValue (monicMatchArrow (pbpoComplementMonicMatch witness)) (pbpoComplementPullbackToMatch witness)
    )
    of
      (Right leftMorphism, Right rightMorphism) -> leftMorphism == rightMorphism
      _ -> False

-- | Check the pushout square carried by a PBPO witness.
pbpoPushoutSquareCommutes :: (Category c, Eq (Mor c)) => c -> PBPOComplementWitness c -> Bool
pbpoPushoutSquareCommutes categoryValue witness =
  case
    ( composeMor categoryValue (pbpoComplementPushoutFromComplement witness) (pbpoComplementResidualLeg witness),
      composeMor categoryValue (pbpoComplementPushoutFromMatch witness) (pbpoComplementRuleLeg witness)
    )
    of
      (Right leftMorphism, Right rightMorphism) -> leftMorphism == rightMorphism
      _ -> False
