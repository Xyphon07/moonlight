{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Integer-signed multiplicities and their changes: states stay non-negative,
deltas carry sign, application is checked. A @Signed@ stores integer changes
while the state retains non-negative @Multiplicity@ values.

> import Data.Map.Strict (Map)
> import Data.Map.Strict qualified as Map
> import Moonlight.Delta.Signed qualified as Signed
>
> inventory :: Map String Signed.Multiplicity
> inventory = Map.singleton "rose" (Signed.Multiplicity 2)
>
> sale :: Signed.Signed String
> sale = Signed.signedFromList [("rose", -1), ("lily", 2)]
>
> afterSale
>   :: Either (Signed.SignedApplyError String) (Map String Signed.Multiplicity)
> afterSale = Signed.applySignedToMap sale inventory

An underflow returns @SignedMultiplicityUnderflow@; a zero result removes the
row. Combine changes with @combineSigned@ and reverse one with @negateSigned@.
-}
module Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..),
    multiplicityValue,
    multiplicityChangeValue,
    zeroMultiplicity,
    zeroMultiplicityChange,
    addMultiplicity,
    subtractMultiplicity,
    addMultiplicityChange,
    negateMultiplicityChange,
    multiplicityAsChange,
    positiveMultiplicityChange,
    applyMultiplicityChange,
    SignedApplyError (..),
    Signed,
    emptySigned,
    singletonSigned,
    signedFromList,
    signedFromChangeMap,
    signedToAscList,
    signedToChangeMap,
    mapSignedKeys,
    traverseSignedKeysWith,
    signedNull,
    support,
    combineSigned,
    negateSigned,
    applySignedToMap,
  )
where

import Data.Bool (Bool, otherwise)
import Data.Eq (Eq, (==), (/=))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord, Ordering (..), compare, (<), (>), (>=))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Delta.Normalize
  ( DeltaNormalize (..),
  )
import Moonlight.Delta.Support
  ( DeltaSupport (..),
  )
import Prelude
  ( Either (Left, Right),
    Int,
    Integer,
    Num (..),
    Show,
    fmap,
    foldr,
    fromIntegral,
    fst,
    toInteger,
    traverse,
    ($),
    (.),
  )
import Numeric.Natural (Natural)

type Multiplicity :: Type
newtype Multiplicity = Multiplicity
  { unMultiplicity :: Natural
  }
  deriving stock (Eq, Ord, Show)

type MultiplicityChange :: Type
newtype MultiplicityChange = MultiplicityChange
  { unMultiplicityChange :: Integer
  }
  deriving stock (Eq, Ord, Show)

type SignedApplyError :: Type -> Type
data SignedApplyError key = SignedMultiplicityUnderflow
  { saeKey :: !key,
    saeOldMultiplicity :: !Multiplicity,
    saeDeltaMultiplicity :: !MultiplicityChange
  }
  deriving stock (Eq, Ord, Show)

type Signed :: Type -> Type
-- | Canonical ascending changes. The constructor is private; every public
-- introduction path consolidates equal keys and removes zero changes.
newtype Signed key = Signed
  { entries :: Vector (key, MultiplicityChange)
  }
  deriving stock (Eq, Ord, Show)

emptySigned :: Signed key
emptySigned =
  Signed Vector.empty

singletonSigned ::
  key ->
  Int ->
  Signed key
singletonSigned key diff =
  if diff == 0
    then emptySigned
    else Signed (Vector.singleton (key, MultiplicityChange (fromIntegral diff)))

signedFromList ::
  Ord key =>
  [(key, Int)] ->
  Signed key
signedFromList =
  Signed . consolidateChangeEntries . fmap (\(key, value) -> (key, MultiplicityChange (fromIntegral value)))

signedFromChangeMap ::
  Map key MultiplicityChange ->
  Signed key
signedFromChangeMap =
  Signed . Vector.fromList . Map.toAscList . Map.filter nonZeroChange

signedToAscList ::
  Signed key ->
  [(key, MultiplicityChange)]
signedToAscList (Signed entries) =
  Vector.toList entries

signedToChangeMap ::
  Signed key ->
  Map key MultiplicityChange
signedToChangeMap =
  Map.fromDistinctAscList . signedToAscList

mapSignedKeys ::
  Ord target =>
  (source -> target) ->
  Signed source ->
  Signed target
mapSignedKeys project (Signed entries) =
  Signed (consolidateChangeEntries (fmap (\(key, multiplicity) -> (project key, multiplicity)) (Vector.toList entries)))

traverseSignedKeysWith ::
  Ord target =>
  (source -> Either err target) ->
  Signed source ->
  Either err (Signed target)
traverseSignedKeysWith project (Signed entries) =
  fmap (Signed . consolidateChangeEntries) $
    traverse
      (\(key, multiplicity) -> fmap (\target -> (target, multiplicity)) (project key))
      (Vector.toList entries)

signedNull ::
  Signed key ->
  Bool
signedNull =
  Vector.null . entries

support :: Signed key -> Set key
support =
  Set.fromDistinctAscList . fmap fst . signedToAscList

combineSigned ::
  Ord key =>
  Signed key ->
  Signed key ->
  Signed key
combineSigned (Signed newer) (Signed older) =
  Signed (Vector.unfoldr mergeStep (0, 0))
  where
    mergeStep (newerIndex, olderIndex) =
      case (newer Vector.!? newerIndex, older Vector.!? olderIndex) of
        (Nothing, Nothing) ->
          Nothing
        (Just newerEntry, Nothing) ->
          Just (newerEntry, (newerIndex + 1, olderIndex))
        (Nothing, Just olderEntry) ->
          Just (olderEntry, (newerIndex, olderIndex + 1))
        (Just newerEntry@(newerKey, newerMultiplicity), Just olderEntry@(olderKey, olderMultiplicity)) ->
          case compare newerKey olderKey of
            LT ->
              Just (newerEntry, (newerIndex + 1, olderIndex))
            GT ->
              Just (olderEntry, (newerIndex, olderIndex + 1))
            EQ ->
              let combined =
                    addMultiplicityChange newerMultiplicity olderMultiplicity
               in if nonZeroChange combined
                    then Just ((newerKey, combined), (newerIndex + 1, olderIndex + 1))
                    else mergeStep (newerIndex + 1, olderIndex + 1)

negateSigned ::
  Signed key ->
  Signed key
negateSigned (Signed rows) =
  Signed (Vector.map (\(key, multiplicity) -> (key, negateMultiplicityChange multiplicity)) rows)

applySignedToMap ::
  forall key.
  Ord key =>
  Signed key ->
  Map key Multiplicity ->
  Either (SignedApplyError key) (Map key Multiplicity)
applySignedToMap (Signed deltaRows) state0 =
  Vector.foldM applyOne state0 deltaRows
  where
    applyOne ::
      Map key Multiplicity ->
      (key, MultiplicityChange) ->
      Either (SignedApplyError key) (Map key Multiplicity)
    applyOne state (key, diff) =
      let old =
            Map.findWithDefault zeroMultiplicity key state
       in case applyMultiplicityChange old diff of
            Nothing ->
              Left
                SignedMultiplicityUnderflow
                  { saeKey = key,
                    saeOldMultiplicity = old,
                    saeDeltaMultiplicity = diff
                  }
            Just newMultiplicity ->
              Right (writeMultiplicity key newMultiplicity state)

zeroMultiplicity :: Multiplicity
zeroMultiplicity =
  Multiplicity 0

multiplicityValue :: Multiplicity -> Natural
multiplicityValue =
  unMultiplicity

multiplicityChangeValue :: MultiplicityChange -> Integer
multiplicityChangeValue =
  unMultiplicityChange

zeroMultiplicityChange :: MultiplicityChange
zeroMultiplicityChange =
  MultiplicityChange 0

nonZeroChange :: MultiplicityChange -> Bool
nonZeroChange (MultiplicityChange value) =
  value /= 0

addMultiplicity :: Multiplicity -> Multiplicity -> Multiplicity
addMultiplicity (Multiplicity left) (Multiplicity right) =
  Multiplicity (left + right)

subtractMultiplicity :: Multiplicity -> Multiplicity -> Maybe Multiplicity
subtractMultiplicity (Multiplicity left) (Multiplicity right)
  | left >= right =
      Just (Multiplicity (left - right))
  | otherwise =
      Nothing

addMultiplicityChange :: MultiplicityChange -> MultiplicityChange -> MultiplicityChange
addMultiplicityChange (MultiplicityChange left) (MultiplicityChange right) =
  MultiplicityChange (left + right)

negateMultiplicityChange :: MultiplicityChange -> MultiplicityChange
negateMultiplicityChange (MultiplicityChange value) =
  MultiplicityChange (negate value)

multiplicityAsChange :: Multiplicity -> MultiplicityChange
multiplicityAsChange (Multiplicity value) =
  MultiplicityChange (toInteger value)

positiveMultiplicityChange :: MultiplicityChange -> Maybe Multiplicity
positiveMultiplicityChange (MultiplicityChange value)
  | value > 0 =
      Just (Multiplicity (fromInteger value))
  | otherwise =
      Nothing

applyMultiplicityChange :: Multiplicity -> MultiplicityChange -> Maybe Multiplicity
applyMultiplicityChange (Multiplicity oldValue) (MultiplicityChange changeValue) =
  let newValue =
        toInteger oldValue + changeValue
   in if newValue < 0
        then Nothing
        else Just (Multiplicity (fromInteger newValue))

writeMultiplicity ::
  Ord key =>
  key ->
  Multiplicity ->
  Map key Multiplicity ->
  Map key Multiplicity
writeMultiplicity key multiplicity@(Multiplicity value) state =
  if value == 0
    then Map.delete key state
    else Map.insert key multiplicity state

consolidateChangeEntries ::
  forall key.
  Ord key =>
  [(key, MultiplicityChange)] ->
  Vector (key, MultiplicityChange)
consolidateChangeEntries entries =
  Vector.fromList
    ( case consolidateAscendingEntries entries of
        Just consolidatedEntries ->
          consolidatedEntries
        Nothing ->
          consolidateSortedEntries (List.sortBy compareEntryKeys entries)
    )
  where
    compareEntryKeys ::
      (key, MultiplicityChange) ->
      (key, MultiplicityChange) ->
      Ordering
    compareEntryKeys (leftKey, _) (rightKey, _) =
      compare leftKey rightKey

    consolidateAscendingEntries ::
      [(key, MultiplicityChange)] ->
      Maybe [(key, MultiplicityChange)]
    consolidateAscendingEntries =
      fmap List.reverse . List.foldl' insertAscendingEntry (Just [])

    insertAscendingEntry ::
      Maybe [(key, MultiplicityChange)] ->
      (key, MultiplicityChange) ->
      Maybe [(key, MultiplicityChange)]
    insertAscendingEntry maybeEntries entry@(key, multiplicity) =
      case maybeEntries of
        Nothing ->
          Nothing
        Just entriesSoFar ->
          case entriesSoFar of
            [] ->
              Just (if nonZeroChange multiplicity then [entry] else [])
            (previousKey, previousMultiplicity) : rest ->
              case compare previousKey key of
                GT ->
                  Nothing
                EQ ->
                  let combined =
                        addMultiplicityChange previousMultiplicity multiplicity
                   in Just (if nonZeroChange combined then (key, combined) : rest else rest)
                LT ->
                  Just (if nonZeroChange multiplicity then entry : entriesSoFar else entriesSoFar)

    consolidateSortedEntries ::
      [(key, MultiplicityChange)] ->
      [(key, MultiplicityChange)]
    consolidateSortedEntries =
      foldr insertSortedEntry []

    insertSortedEntry ::
      (key, MultiplicityChange) ->
      [(key, MultiplicityChange)] ->
      [(key, MultiplicityChange)]
    insertSortedEntry entry@(key, multiplicity) consolidatedEntries =
      case consolidatedEntries of
        (nextKey, nextMultiplicity) : rest
          | key == nextKey ->
              let combined =
                    addMultiplicityChange multiplicity nextMultiplicity
               in if nonZeroChange combined
                    then (key, combined) : rest
                    else rest
        _ ->
          if nonZeroChange multiplicity
            then entry : consolidatedEntries
            else consolidatedEntries

instance DeltaNormalize (Signed key) where
  normalizeDelta signedValue =
    signedValue

  deltaNull =
    signedNull

instance DeltaSupport (Signed key) where
  type DeltaSupportSet (Signed key) = Set key

  emptySupport =
    Set.empty

  deltaSupport =
    support
