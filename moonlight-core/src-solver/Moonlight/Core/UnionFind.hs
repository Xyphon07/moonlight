-- | A persistent union-find over e-graph class identifiers. The public value
-- remains immutable; batched mutation is available explicitly from
-- "Moonlight.Core.UnionFind.Transaction".
module Moonlight.Core.UnionFind
  ( UnionFind,
    UnionFindAllocationError (..),
    emptyUnionFind,
    fromClassIds,
    makeSet,
    insertClassId,
    member,
    find,
    findExisting,
    canonicalClass,
    union,
    unionWithCanonicalRoots,
    equivalent,
    samePartition,
    canonicalMap,
    canonicalMapAndCompress,
    compress,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.Core.Identifier.EGraph
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.Core.UnionFind.Internal.Semantics
  ( LinkDecision (..),
    chooseLink,
  )
import Moonlight.Core.UnionFind.Internal.Types
  ( UnionFind (..),
    UnionFindAllocationError,
    advanceNextFreshForClassIdKey,
    allocateNextClassId,
  )
import Moonlight.Core.UnionFind.Transaction qualified as Transaction
import Prelude
  ( Bool,
    Eq ((==)),
    Foldable,
    Either,
    Int,
    Maybe (..),
    fmap,
    min,
    otherwise,
    pure,
    snd,
    toInteger,
    (+),
    (.),
    (>=),
  )

emptyUnionFind :: UnionFind
emptyUnionFind =
  UnionFind
    { ufParent = IntMap.empty,
      ufRank = IntMap.empty,
      ufNextFresh = 0
    }

fromClassIds :: Foldable t => t ClassId -> UnionFind
fromClassIds classIds =
  UnionFind
    { ufParent = IntMap.fromSet ClassId classKeys,
      -- Rank zero is the semantic default. Materializing one zero per class
      -- only duplicates that law and makes every insertion update two trees.
      ufRank = IntMap.empty,
      ufNextFresh =
        case IntSet.lookupMax classKeys of
          Just greatestKey
            | greatestKey >= 0 ->
                toInteger greatestKey + 1
          _ ->
            0
    }
  where
    classKeys =
      IntSet.fromList (fmap classIdKey (Foldable.toList classIds))

insertClassId :: ClassId -> UnionFind -> UnionFind
insertClassId classId unionFind =
  let key = classIdKey classId
   in if IntMap.member key (ufParent unionFind)
        then unionFind {ufNextFresh = advanceNextFreshForClassIdKey key (ufNextFresh unionFind)}
        else
          unionFind
            { ufParent = IntMap.insert key classId (ufParent unionFind),
              ufNextFresh = advanceNextFreshForClassIdKey key (ufNextFresh unionFind)
            }

member :: ClassId -> UnionFind -> Bool
member classId =
  IntMap.member (classIdKey classId) . ufParent

makeSet :: UnionFind -> Either UnionFindAllocationError (ClassId, UnionFind)
makeSet unionFind = do
  (classId, nextFresh) <- allocateNextClassId (ufNextFresh unionFind)
  let key = classIdKey classId
  pure
    ( classId,
      unionFind
        { ufParent = IntMap.insert key classId (ufParent unionFind),
          ufNextFresh = nextFresh
        }
    )

find :: ClassId -> UnionFind -> (ClassId, UnionFind)
find classId unionFind =
  let (rootKey, compressedParents) =
        compressRootKey (ufParent unionFind) (classIdKey classId)
   in ( ClassId rootKey,
        unionFind {ufParent = compressedParents}
      )

findExisting :: ClassId -> UnionFind -> Maybe (ClassId, UnionFind)
findExisting classId unionFind =
  if member classId unionFind
    then Just (find classId unionFind)
    else Nothing

canonicalClass :: ClassId -> UnionFind -> Maybe ClassId
canonicalClass classId unionFind =
  case IntMap.lookup key (ufParent unionFind) of
    Nothing ->
      Nothing
    Just parentClassId ->
      Just
        ( ClassId
            ( if classIdKey parentClassId == key
                then key
                else rootKeyOf unionFind (classIdKey parentClassId)
            )
        )
  where
    key = classIdKey classId
{-# INLINE canonicalClass #-}

union :: ClassId -> ClassId -> UnionFind -> UnionFind
union leftClassId rightClassId unionFind =
  let (_canonicalEndpoints, _mergedRoot, mergedUnionFind) =
        unionWithCanonicalRoots leftClassId rightClassId unionFind
   in mergedUnionFind
{-# INLINE union #-}

-- | Union two classes once, retaining both canonical input roots and the
-- chosen result root. Clients that must emit a semantic mutation trace can
-- consume this result without repeating the same root searches.
unionWithCanonicalRoots ::
  ClassId ->
  ClassId ->
  UnionFind ->
  ((ClassId, ClassId), ClassId, UnionFind)
unionWithCanonicalRoots leftClassId rightClassId unionFind =
  let seededUnionFind = insertClassId rightClassId (insertClassId leftClassId unionFind)
      (leftRoot, unionFindAfterLeft) = find leftClassId seededUnionFind
      (rightRoot, unionFindAfterRight) = find rightClassId unionFindAfterLeft
   in if leftRoot == rightRoot
        then ((leftRoot, rightRoot), leftRoot, unionFindAfterRight)
        else
          let linkDecision =
                chooseLink
                  leftRoot
                  (rankOf leftRoot unionFindAfterRight)
                  rightRoot
                  (rankOf rightRoot unionFindAfterRight)
              mergedRoot =
                case linkDecision of
                  AttachRoot _childRoot parentRoot ->
                    parentRoot
                  AttachRootAndRaise _childRoot parentRoot _raisedRank ->
                    parentRoot
           in ( (leftRoot, rightRoot),
                mergedRoot,
                applyLinkDecision linkDecision unionFindAfterRight
              )
{-# INLINE unionWithCanonicalRoots #-}

equivalent :: ClassId -> ClassId -> UnionFind -> Bool
equivalent leftClassId rightClassId unionFind =
  rootKeyOf unionFind (classIdKey leftClassId)
    == rootKeyOf unionFind (classIdKey rightClassId)

canonicalMap :: UnionFind -> IntMap ClassId
canonicalMap unionFind =
  IntMap.mapWithKey
    (\key _ -> ClassId (rootKeyOf unionFind key))
    (ufParent unionFind)

samePartition :: UnionFind -> UnionFind -> Bool
samePartition leftUnionFind rightUnionFind =
  partitionProjection leftUnionFind == partitionProjection rightUnionFind

partitionProjection :: UnionFind -> IntMap ClassId
partitionProjection unionFind =
  IntMap.map canonicalRootClass canonicalParents
  where
    canonicalParents =
      canonicalMap unionFind
    classMinima =
      IntMap.foldlWithKey' insertMinimum IntMap.empty canonicalParents
    insertMinimum minima memberKey rootClass =
      IntMap.insertWith min (classIdKey rootClass) (ClassId memberKey) minima
    canonicalRootClass rootClass =
      IntMap.findWithDefault rootClass (classIdKey rootClass) classMinima

canonicalMapAndCompress :: UnionFind -> (IntMap ClassId, UnionFind)
canonicalMapAndCompress unionFind =
  Transaction.runUnionFindTransaction unionFind Transaction.transactionCanonicalMapAndCompress

-- | Flatten every parent chain to its root, returning the compressed
-- structure.
compress :: UnionFind -> UnionFind
compress unionFind =
  snd (Transaction.runUnionFindTransaction unionFind Transaction.transactionCompress)

parentKeyOf :: UnionFind -> Int -> Maybe Int
parentKeyOf unionFind key =
  fmap classIdKey (IntMap.lookup key (ufParent unionFind))

rankOf :: ClassId -> UnionFind -> Int
rankOf classId unionFind =
  IntMap.findWithDefault 0 (classIdKey classId) (ufRank unionFind)

rootKeyOf :: UnionFind -> Int -> Int
rootKeyOf unionFind key =
  case parentKeyOf unionFind key of
    Nothing ->
      key
    Just parentKey
      | parentKey == key ->
          key
      | otherwise ->
          rootKeyOf unionFind parentKey
{-# INLINE rootKeyOf #-}

compressRootKey :: IntMap ClassId -> Int -> (Int, IntMap ClassId)
compressRootKey parents key =
  case IntMap.lookup key parents of
    Nothing ->
      (key, parents)
    Just parentClassId ->
      let parentKey = classIdKey parentClassId
       in if parentKey == key
            then (key, parents)
            else
              let (rootKey, compressedParents) =
                    compressRootKey parents parentKey
               in (rootKey, IntMap.insert key (ClassId rootKey) compressedParents)

applyLinkDecision :: LinkDecision -> UnionFind -> UnionFind
applyLinkDecision decision unionFind =
  case decision of
    AttachRoot childRoot parentRoot ->
      setParent childRoot parentRoot unionFind
    AttachRootAndRaise childRoot parentRoot raisedRank ->
      (setParent childRoot parentRoot unionFind)
        { ufRank =
            IntMap.insert
              (classIdKey parentRoot)
              raisedRank
              (ufRank unionFind)
        }

setParent :: ClassId -> ClassId -> UnionFind -> UnionFind
setParent childClassId parentClassId unionFind =
  unionFind
    { ufParent =
        IntMap.insert
          (classIdKey childClassId)
          parentClassId
          (ufParent unionFind)
    }
