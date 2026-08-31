{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Invariant
  ( ContextPlanInvariantError (..),
    invariantLookup,
    boxedIndexInvariant,
    unboxedIndexInvariant,
  )
where

import Control.Exception (Exception, assert, throw)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector

-- | A validated runtime plan failed to produce the result established by its
-- constructor. The two integers are context-key ordinals, except for cover
-- failures where the second integer is the join-irreducible mask index.
data ContextPlanInvariantError
  = ContextPlanJoinMissing !Int !Int
  | ContextPlanMeetMissing !Int !Int
  | ContextPlanResidualMissing !Int !Int
  | ContextPlanUpperCoverMissing !Int !Int
  | ContextPlanLowerCoverMissing !Int !Int
  deriving stock (Eq, Ord, Show, Read)

instance Exception ContextPlanInvariantError

-- | Eliminate a constructor-private lookup whose presence was proved when the
-- plan was compiled. A miss is an internal defect, never a lattice value and
-- never a recoverable query failure.
invariantLookup :: ContextPlanInvariantError -> Maybe result -> result
invariantLookup obstruction = fromMaybe (throw obstruction)
{-# INLINE invariantLookup #-}

boxedIndexInvariant :: Vector.Vector a -> Int -> a
boxedIndexInvariant vector index =
  assert (index >= 0 && index < Vector.length vector) $
    Vector.unsafeIndex vector index
{-# INLINE boxedIndexInvariant #-}

unboxedIndexInvariant :: UVector.Unbox a => UVector.Vector a -> Int -> a
unboxedIndexInvariant vector index =
  assert (index >= 0 && index < UVector.length vector) $
    UVector.unsafeIndex vector index
{-# INLINE unboxedIndexInvariant #-}
