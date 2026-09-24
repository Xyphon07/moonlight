{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Core.UnionFind.Internal.Types
  ( UnionFind (..),
    UnionFindAllocationError (..),
    advanceNextFreshForClassIdKey,
    allocateNextClassId,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.Kind
  ( Type,
  )
import Data.Vector.Unboxed (Vector)
import Moonlight.Core.Identifier.EGraph
  ( ClassId (..),
  )
import Prelude
  ( Either (..),
    Eq,
    Int,
    Integer,
    Ord,
    Read,
    Show,
    fromInteger,
    max,
    maxBound,
    otherwise,
    toInteger,
    (+),
    (<),
    (>),
  )

type UnionFind :: Type
data UnionFind = UnionFind
  { ufParent :: !(IntMap ClassId),
    ufRank :: !(IntMap Int),
    -- | Dense, hole-free prefix of canonical roots. This is a derived read
    -- view populated only for a flattened partition; mutations that can
    -- change roots discard it.
    ufDenseCanonicalRoots :: !(Vector Int),
    ufNextFresh :: !Integer
  }
  deriving stock (Show)

type UnionFindAllocationError :: Type
data UnionFindAllocationError
  = ClassIdSpaceExhausted
  deriving stock (Eq, Ord, Show, Read)

instance NFData UnionFindAllocationError where
  rnf ClassIdSpaceExhausted = ()

advanceNextFreshForClassIdKey :: Int -> Integer -> Integer
advanceNextFreshForClassIdKey key current
  | key < 0 = current
  | otherwise = max current (toInteger key + 1)

allocateNextClassId :: Integer -> Either UnionFindAllocationError (ClassId, Integer)
allocateNextClassId nextFresh
  | nextFresh > toInteger (maxBound :: Int) = Left ClassIdSpaceExhausted
  | otherwise = Right (ClassId (fromInteger nextFresh), nextFresh + 1)
