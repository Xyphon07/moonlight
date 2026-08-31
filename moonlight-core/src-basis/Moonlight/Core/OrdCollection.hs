{-# LANGUAGE TypeFamilies #-}

-- | The 'OrdSet' and 'OrdMap' classes abstracting over ordered set and map
-- backends ('Data.Set'/'Data.IntSet', 'Data.Map'/'Data.IntMap').
module Moonlight.Core.OrdCollection
  ( OrdSet (..),
    OrdMap (..),
  )
where

import Data.Kind (Constraint, Type)
import Prelude (Bool, Int, Maybe, Ord)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type OrdSet :: Type -> Constraint
class OrdSet s where
  type SetKey s
  emptySet :: s
  nullSet :: s -> Bool
  singletonSet :: SetKey s -> s
  memberSet :: SetKey s -> s -> Bool
  unionSet :: s -> s -> s
  intersectionSet :: s -> s -> s
  differenceSet :: s -> s -> s
  unionsSet :: [s] -> s
  toAscListSet :: s -> [SetKey s]
  fromListSet :: [SetKey s] -> s
  sizeSet :: s -> Int

instance OrdSet IntSet where
  type SetKey IntSet = Int
  emptySet = IntSet.empty
  nullSet = IntSet.null
  singletonSet = IntSet.singleton
  memberSet = IntSet.member
  unionSet = IntSet.union
  intersectionSet = IntSet.intersection
  differenceSet = IntSet.difference
  unionsSet = IntSet.unions
  toAscListSet = IntSet.toAscList
  fromListSet = IntSet.fromList
  sizeSet = IntSet.size

instance Ord k => OrdSet (Set k) where
  type SetKey (Set k) = k
  emptySet = Set.empty
  nullSet = Set.null
  singletonSet = Set.singleton
  memberSet = Set.member
  unionSet = Set.union
  intersectionSet = Set.intersection
  differenceSet = Set.difference
  unionsSet = Set.unions
  toAscListSet = Set.toAscList
  fromListSet = Set.fromList
  sizeSet = Set.size

type OrdMap :: Type -> Constraint
class OrdMap m where
  type MapKey m
  type MapValue m
  emptyMap :: m
  nullMap :: m -> Bool
  lookupMap :: MapKey m -> m -> Maybe (MapValue m)
  insertMap :: MapKey m -> MapValue m -> m -> m
  insertWithMap :: (MapValue m -> MapValue m -> MapValue m) -> MapKey m -> MapValue m -> m -> m
  deleteMap :: MapKey m -> m -> m
  unionWithMap :: (MapValue m -> MapValue m -> MapValue m) -> m -> m -> m
  toAscListMap :: m -> [(MapKey m, MapValue m)]
  fromListMap :: [(MapKey m, MapValue m)] -> m
  fromListWithMap :: (MapValue m -> MapValue m -> MapValue m) -> [(MapKey m, MapValue m)] -> m
  sizeMap :: m -> Int

instance OrdMap (IntMap v) where
  type MapKey (IntMap v) = Int
  type MapValue (IntMap v) = v
  emptyMap = IntMap.empty
  nullMap = IntMap.null
  lookupMap = IntMap.lookup
  insertMap = IntMap.insert
  insertWithMap = IntMap.insertWith
  deleteMap = IntMap.delete
  unionWithMap = IntMap.unionWith
  toAscListMap = IntMap.toAscList
  fromListMap = IntMap.fromList
  fromListWithMap = IntMap.fromListWith
  sizeMap = IntMap.size

instance Ord k => OrdMap (Map k v) where
  type MapKey (Map k v) = k
  type MapValue (Map k v) = v
  emptyMap = Map.empty
  nullMap = Map.null
  lookupMap = Map.lookup
  insertMap = Map.insert
  insertWithMap = Map.insertWith
  deleteMap = Map.delete
  unionWithMap = Map.unionWith
  toAscListMap = Map.toAscList
  fromListMap = Map.fromList
  fromListWithMap = Map.fromListWith
  sizeMap = Map.size
