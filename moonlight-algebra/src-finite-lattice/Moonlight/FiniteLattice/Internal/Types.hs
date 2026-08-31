{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextOrderDecl (..),
    ContextCompileLimits (..),
    defaultContextCompileLimits,
    unlimitedContextCompileLimits,
    ContextRepresentation (..),
    ContextLatticeCompileError (..),
    ContextLatticeLookupError (..),
    ResidentContext (..),
    ResidentContextKey (..),
    ResidentContextKeySet (..),
    ResidentContextElement (..),
    contextKeyForMaybe,
    contextValueForKey,
    residentKeyFromContextKey,
    contextKeyFromResidentKey,
    residentContextElementForKey,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Vector qualified as Vector
import Moonlight.FiniteLattice.Internal.Invariant
  ( boxedIndexInvariant,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
  )
import Moonlight.FiniteLattice.Internal.Plan (ContextPlan)
import Numeric.Natural (Natural)

type ContextLattice :: Type -> Type
data ContextLattice c = ContextLattice
  { clTop :: !c,
    clBottom :: !c,
    clTopKey :: !ContextKey,
    clBottomKey :: !ContextKey,
    clContextsByKey :: !(Vector.Vector c),
    clKeyByContext :: !(Map c ContextKey),
    clPlan :: !ContextPlan,
    clSize :: !Int
  }

type role ContextLattice nominal

type ContextOrderDecl :: Type -> Type
data ContextOrderDecl c = ContextOrderDecl
  { codTop :: !c,
    codBottom :: !c,
    -- | Generating pairs @(lower, upper)@. Reflexive and transitive pairs may
    -- be omitted; compilation takes the reflexive-transitive closure.
    codGeneratingPairs :: !(Set (c, c))
  }
  deriving stock (Eq, Ord, Show, Read)

type role ContextOrderDecl nominal

type ContextCompileLimits :: Type
data ContextCompileLimits = ContextCompileLimits
  { cclMaximumRelationBytes :: !(Maybe Natural),
    cclMaximumBinaryTableBytes :: !(Maybe Natural)
  }
  deriving stock (Eq, Ord, Show, Read)

defaultContextCompileLimits :: ContextCompileLimits
defaultContextCompileLimits =
  ContextCompileLimits
    { cclMaximumRelationBytes = Just (mebibytes 512),
      cclMaximumBinaryTableBytes = Just (mebibytes 1024)
    }

unlimitedContextCompileLimits :: ContextCompileLimits
unlimitedContextCompileLimits =
  ContextCompileLimits
    { cclMaximumRelationBytes = Nothing,
      cclMaximumBinaryTableBytes = Nothing
    }

mebibytes :: Natural -> Natural
mebibytes count =
  count * 1024 * 1024

type ContextRepresentation :: Type
data ContextRepresentation
  = ContextRelationWords
  deriving stock (Eq, Ord, Show, Read)

type ContextLatticeCompileError :: Type -> Type
data ContextLatticeCompileError c
  = ContextLatticeEmptyUniverse
  | ContextLatticeDuplicateElement !c
  | ContextLatticeUnknownTop !c
  | ContextLatticeUnknownBottom !c
  | ContextLatticeUnknownRelationEndpoint !(c, c)
  | ContextLatticeAntisymmetryViolation !c !c
  | ContextLatticeTopNotGreatest !c
  | ContextLatticeBottomNotLeast !c
  | ContextLatticeJoinDoesNotExist !c !c !(Set c)
  | ContextLatticeMeetDoesNotExist !c !c !(Set c)
  | ContextLatticeNotReflexive !c
  | ContextLatticeNotTransitive !c !c !c
  | ContextLatticeJoinOutsideUniverse !c !c !c
  | ContextLatticeMeetOutsideUniverse !c !c !c
  | ContextLatticeInvalidJoin !c !c !c
  | ContextLatticeInvalidMeet !c !c !c
  | ContextLatticeRepresentationOverflow !ContextRepresentation !Integer
  | ContextLatticeRepresentationLimitExceeded
      !ContextRepresentation
      !Integer
      !Natural
  deriving stock (Eq, Ord, Show, Read)

type role ContextLatticeCompileError nominal

type ContextLatticeLookupError :: Type -> Type
data ContextLatticeLookupError c
  = ContextLatticeUnknownContext !c
  deriving stock (Eq, Ord, Show, Read)

type role ContextLatticeLookupError nominal

type ResidentContext :: Type -> Type -> Type
newtype ResidentContext s c = ResidentContext
  { residentContextLattice :: ContextLattice c
  }

type role ResidentContext nominal nominal

type ResidentContextKey :: Type -> Type
newtype ResidentContextKey s = ResidentContextKey
  { residentContextKeyOrdinal :: Int
  }
  deriving stock (Eq, Ord, Show)

type role ResidentContextKey nominal

type ResidentContextKeySet :: Type -> Type
newtype ResidentContextKeySet s = ResidentContextKeySet ContextKeySet
  deriving stock (Eq, Show)

type role ResidentContextKeySet nominal

type ResidentContextElement :: Type -> Type -> Type
data ResidentContextElement s c = ResidentContextElement
  { residentContextElementKey :: !(ResidentContextKey s),
    residentContextElementValue :: !c
  }
  deriving stock (Eq, Ord, Show)

type role ResidentContextElement nominal nominal

contextKeyForMaybe :: Ord c => ContextLattice c -> c -> Maybe ContextKey
contextKeyForMaybe lattice contextValue =
  Map.lookup contextValue (clKeyByContext lattice)

-- | Total for internal keys produced by this lattice. The constructor of
-- 'ContextKey' is never exported by a public module.
contextValueForKey :: ContextLattice c -> ContextKey -> c
contextValueForKey lattice (ContextKey keyOrdinal) =
  contextValueAtOrdinal lattice keyOrdinal
{-# INLINE contextValueForKey #-}

residentKeyFromContextKey :: ContextKey -> ResidentContextKey s
residentKeyFromContextKey (ContextKey keyOrdinal) =
  ResidentContextKey keyOrdinal
{-# INLINE residentKeyFromContextKey #-}

contextKeyFromResidentKey :: ResidentContextKey s -> ContextKey
contextKeyFromResidentKey (ResidentContextKey keyOrdinal) =
  ContextKey keyOrdinal
{-# INLINE contextKeyFromResidentKey #-}

residentContextElementForKey ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextElement s c
residentContextElementForKey (ResidentContext lattice) residentKey =
  ResidentContextElement
    { residentContextElementKey = residentKey,
      residentContextElementValue =
        contextValueForKey lattice (contextKeyFromResidentKey residentKey)
    }

contextValueAtOrdinal :: ContextLattice c -> Int -> c
contextValueAtOrdinal lattice =
  boxedIndexInvariant (clContextsByKey lattice)
{-# INLINE contextValueAtOrdinal #-}
