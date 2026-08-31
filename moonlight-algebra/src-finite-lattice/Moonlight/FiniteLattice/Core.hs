{-# LANGUAGE GHC2024 #-}

-- | The 'ContextLattice' carrier, built by checked compilation from a declared
-- finite order, and the value-level 'joinContext', 'meetContext', 'leqContext'
-- queries against it.
module Moonlight.FiniteLattice.Core
  ( ContextLattice,
    clTop,
    clBottom,
    ContextOrderDecl (..),
    ContextCompileLimits (..),
    defaultContextCompileLimits,
    unlimitedContextCompileLimits,
    ContextRepresentation (..),
    ContextLatticeCompileError (..),
    ContextLatticeLookupError (..),
    contextOrderDecl,
    compileContextLattice,
    compileContextLatticeWith,
    contextLatticeFromClosedOrder,
    contextLatticeFromClosedOrderWith,
    singletonContextLattice,
    latticeContext,
    orderedLatticeContext,
    contextLatticeSize,
    contextLatticeElements,
    contextMember,
    joinContext,
    meetContext,
    leqContext,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.FiniteLattice.Internal.Compile
  ( compileContextLattice,
    compileContextLatticeWith,
    contextLatticeFromClosedOrder,
    contextLatticeFromClosedOrderWith,
    singletonContextLattice,
  )
import Moonlight.FiniteLattice.Internal.Key (ContextKey)
import Moonlight.FiniteLattice.Internal.Plan
  ( contextPlanJoinKey,
    contextPlanLeq,
    contextPlanMeetKey,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextCompileLimits (..),
    ContextLatticeCompileError (..),
    ContextLatticeLookupError (..),
    ContextOrderDecl (..),
    ContextRepresentation (..),
    contextKeyForMaybe,
    contextValueForKey,
    defaultContextCompileLimits,
    unlimitedContextCompileLimits,
  )
import Moonlight.Algebra.Pure.Lattice
  ( BoundedJoinSemilattice (bottom),
    BoundedMeetSemilattice (top),
    JoinSemilattice (join),
    Lattice,
    MeetSemilattice (meet),
    OrderedLattice,
  )
import Moonlight.Core
  ( FiniteUniverse,
    finiteUniverseList,
    leq,
  )

contextOrderDecl ::
  Ord c =>
  c ->
  c ->
  [(c, c)] ->
  ContextOrderDecl c
contextOrderDecl topValue bottomValue generatingPairs =
  ContextOrderDecl
    { codTop = topValue,
      codBottom = bottomValue,
      codGeneratingPairs = Set.fromList generatingPairs
    }

contextLatticeSize :: ContextLattice c -> Int
contextLatticeSize = clSize
{-# INLINE contextLatticeSize #-}

contextLatticeElements :: ContextLattice c -> [c]
contextLatticeElements =
  Vector.toList . clContextsByKey

contextMember :: Ord c => ContextLattice c -> c -> Bool
contextMember lattice =
  (`Map.member` clKeyByContext lattice)

-- | Value-level boundary query; repeated validation is a performance tax, so descend once into 'Moonlight.FiniteLattice.Resident' for steady-state algebra.
joinContext ::
  Ord c =>
  ContextLattice c ->
  c ->
  c ->
  Either (ContextLatticeLookupError c) c
joinContext lattice leftContext rightContext = do
  leftKey <- lookupContextKey lattice leftContext
  rightKey <- lookupContextKey lattice rightContext
  let joinedKey = contextPlanJoinKey (clPlan lattice) leftKey rightKey
  pure
    ( contextValueForKey
        lattice
        joinedKey
    )

meetContext ::
  Ord c =>
  ContextLattice c ->
  c ->
  c ->
  Either (ContextLatticeLookupError c) c
meetContext lattice leftContext rightContext = do
  leftKey <- lookupContextKey lattice leftContext
  rightKey <- lookupContextKey lattice rightContext
  let metKey = contextPlanMeetKey (clPlan lattice) leftKey rightKey
  pure
    ( contextValueForKey
        lattice
        metKey
    )

leqContext ::
  Ord c =>
  ContextLattice c ->
  c ->
  c ->
  Either (ContextLatticeLookupError c) Bool
leqContext lattice leftContext rightContext = do
  leftKey <- lookupContextKey lattice leftContext
  rightKey <- lookupContextKey lattice rightContext
  pure (contextPlanLeq (clPlan lattice) leftKey rightKey)

lookupContextKey ::
  Ord c =>
  ContextLattice c ->
  c ->
  Either (ContextLatticeLookupError c) ContextKey
lookupContextKey lattice contextValue =
  maybe
    (Left (ContextLatticeUnknownContext contextValue))
    Right
    (contextKeyForMaybe lattice contextValue)

-- | Checked construction from the declared finite instances. This deliberately
-- returns 'Either': typeclass laws are promises, not runtime evidence.
latticeContext ::
  ( Ord c,
    Lattice c,
    BoundedJoinSemilattice c,
    BoundedMeetSemilattice c,
    Enum c,
    Bounded c
  ) =>
  Either (ContextLatticeCompileError c) (ContextLattice c)
latticeContext =
  contextLatticeFromClosedOrder
    top
    bottom
    [minBound .. maxBound]
    (\leftContext rightContext -> join leftContext rightContext == rightContext)
    join
    meet

orderedLatticeContext ::
  ( Ord c,
    OrderedLattice c,
    BoundedJoinSemilattice c,
    BoundedMeetSemilattice c,
    FiniteUniverse c
  ) =>
  Either (ContextLatticeCompileError c) (ContextLattice c)
orderedLatticeContext =
  contextLatticeFromClosedOrder
    top
    bottom
    finiteUniverseList
    leq
    join
    meet
