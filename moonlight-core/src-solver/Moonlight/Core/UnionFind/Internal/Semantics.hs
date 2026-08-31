{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Core.UnionFind.Internal.Semantics
  ( LinkDecision (..),
    chooseLink,
    rootKeyBy,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core.Identifier.EGraph
  ( ClassId,
  )
import Prelude
  ( Eq ((==)),
    Int,
    Maybe (..),
    Monad,
    Ord (compare, max, min),
    Ordering (..),
    Show,
    otherwise,
    (+),
    pure,
    (>>=),
  )

type LinkDecision :: Type
data LinkDecision
  = AttachRoot !ClassId !ClassId
  | AttachRootAndRaise !ClassId !ClassId !Int
  deriving stock (Eq, Show)

-- | Decide how two distinct roots are linked. Equal-rank links deterministically
-- retain the lesser 'ClassId'.
chooseLink :: ClassId -> Int -> ClassId -> Int -> LinkDecision
chooseLink leftRoot leftRank rightRoot rightRank =
  case compare leftRank rightRank of
    LT ->
      AttachRoot leftRoot rightRoot
    GT ->
      AttachRoot rightRoot leftRoot
    EQ ->
      AttachRootAndRaise
        (max leftRoot rightRoot)
        (min leftRoot rightRoot)
        (leftRank + 1)
{-# INLINE chooseLink #-}

-- | Shared forest ascent law for both persistent and transient storage.
-- A missing parent is a singleton root, and a self-parent terminates the path.
rootKeyBy ::
  Monad m =>
  (Int -> m (Maybe Int)) ->
  Int ->
  m Int
rootKeyBy readParentKey startKey =
  readParentKey startKey >>= \maybeParentKey ->
    case maybeParentKey of
      Nothing ->
        pure startKey
      Just parentKey
        | parentKey == startKey ->
            pure startKey
        | otherwise ->
            rootKeyBy readParentKey parentKey
{-# INLINE rootKeyBy #-}
