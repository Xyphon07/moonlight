{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Core.EGraph.Program
  ( EGraphProgramPhase (..),
    EGraphProgramOp (..),
    EGraphProgram,
    pureEGraphProgram,
    bindEGraphProgram,
    thenEGraphProgram,
    EGraphProgramEffect,
    emptyEGraphProgramEffect,
    repeatEGraphProgramEffect,
    insertedFreshNodeEffect,
    requiredClassMergeEffect,
    eGraphProgramEffectCount,
    eGraphProgramChanged,
    insertedFreshNode,
    eGraphProgramRequiredClassMerge,
    foldEGraphProgram,
    abortProgram,
    canonicalizeClass,
    canonicalizeClasses,
    addNode,
    addCanonicalNode,
    mergeClasses,
    mergeCanonicalClasses,
  )
where

import Data.Kind (Type)
import Data.Monoid (Any (..), Sum (..))
import Data.Semigroup (stimes)
import GHC.Generics (Generic, Generically (..))
import Moonlight.Core.Identifier.EGraph (ClassId)
import Prelude
  ( Applicative (..),
    Bool (..),
    Eq,
    Functor (..),
    Int,
    Monad (..),
    Monoid,
    Ord,
    Semigroup,
    Show,
    Traversable,
    const,
    mempty,
    traverse,
    (<=),
    (>),
    (.),
  )

-- | The construction phase may inspect the current graph and insert nodes.
-- Once a merge is planned, only further merges or typed aborts may follow.
type EGraphProgramPhase :: Type
data EGraphProgramPhase
  = ConstructionPhase
  | MergePhase

-- | One phase-indexed host-neutral e-graph instruction.
type EGraphProgramOp :: Type -> Type -> EGraphProgramPhase -> EGraphProgramPhase -> Type -> Type
data EGraphProgramOp programError node from to resultValue where
  CanonicalizeClass ::
    !ClassId ->
    EGraphProgramOp programError node 'ConstructionPhase 'ConstructionPhase ClassId
  AddNode ::
    !node ->
    EGraphProgramOp programError node 'ConstructionPhase 'ConstructionPhase ClassId
  MergeClasses ::
    !ClassId ->
    !ClassId ->
    EGraphProgramOp programError node from 'MergePhase ClassId
  AbortProgram ::
    !programError ->
    EGraphProgramOp programError node from to resultValue

-- | An indexed free program. 'EGraphProgramBind' glues only instructions whose
-- phase endpoints agree, so construction operations cannot be sequenced after
-- the first merge.
type EGraphProgram :: Type -> Type -> EGraphProgramPhase -> EGraphProgramPhase -> Type -> Type
data EGraphProgram programError node from to resultValue where
  EGraphProgramPure ::
    resultValue ->
    EGraphProgram programError node phase phase resultValue
  EGraphProgramBind ::
    EGraphProgram programError node from middle intermediate ->
    (intermediate -> EGraphProgram programError node middle to resultValue) ->
    EGraphProgram programError node from to resultValue
  EGraphProgramInstruction ::
    EGraphProgramOp programError node from to resultValue ->
    EGraphProgram programError node from to resultValue

instance Functor (EGraphProgram programError node phase phase) where
  fmap project program =
    bindEGraphProgram program (pureEGraphProgram . project)

instance Applicative (EGraphProgram programError node phase phase) where
  pure =
    pureEGraphProgram

  functionProgram <*> valueProgram =
    bindEGraphProgram
      functionProgram
      (\project -> fmap project valueProgram)

instance Monad (EGraphProgram programError node phase phase) where
  (>>=) =
    bindEGraphProgram

pureEGraphProgram ::
  resultValue ->
  EGraphProgram programError node phase phase resultValue
pureEGraphProgram =
  EGraphProgramPure
{-# INLINE pureEGraphProgram #-}

bindEGraphProgram ::
  EGraphProgram programError node from middle intermediate ->
  (intermediate -> EGraphProgram programError node middle to resultValue) ->
  EGraphProgram programError node from to resultValue
bindEGraphProgram =
  EGraphProgramBind
{-# INLINE bindEGraphProgram #-}

thenEGraphProgram ::
  EGraphProgram programError node from middle ignored ->
  EGraphProgram programError node middle to resultValue ->
  EGraphProgram programError node from to resultValue
thenEGraphProgram firstProgram secondProgram =
  bindEGraphProgram firstProgram (const secondProgram)
{-# INLINE thenEGraphProgram #-}

type EGraphProgramEffect :: Type
data EGraphProgramEffect = EGraphProgramEffect
  { egpeEffectiveApplications :: !(Sum Int),
    insertedFreshNode :: !Any,
    egpeRequiredClassMerge :: !Any
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically EGraphProgramEffect)

emptyEGraphProgramEffect :: EGraphProgramEffect
emptyEGraphProgramEffect =
  mempty

repeatEGraphProgramEffect :: Int -> EGraphProgramEffect -> EGraphProgramEffect
repeatEGraphProgramEffect count effectValue =
  if count <= 0
    then emptyEGraphProgramEffect
    else stimes count effectValue
{-# INLINE repeatEGraphProgramEffect #-}

insertedFreshNodeEffect :: EGraphProgramEffect
insertedFreshNodeEffect =
  EGraphProgramEffect
    { egpeEffectiveApplications = Sum 1,
      insertedFreshNode = Any True,
      egpeRequiredClassMerge = mempty
    }

requiredClassMergeEffect :: EGraphProgramEffect
requiredClassMergeEffect =
  EGraphProgramEffect
    { egpeEffectiveApplications = Sum 1,
      insertedFreshNode = mempty,
      egpeRequiredClassMerge = Any True
    }

eGraphProgramEffectCount :: EGraphProgramEffect -> Int
eGraphProgramEffectCount EGraphProgramEffect {egpeEffectiveApplications = count} =
  getSum count

eGraphProgramChanged :: EGraphProgramEffect -> Bool
eGraphProgramChanged =
  (> 0) . eGraphProgramEffectCount

insertedFreshNode :: EGraphProgramEffect -> Bool
insertedFreshNode EGraphProgramEffect {insertedFreshNode = inserted} =
  getAny inserted

eGraphProgramRequiredClassMerge :: EGraphProgramEffect -> Bool
eGraphProgramRequiredClassMerge EGraphProgramEffect {egpeRequiredClassMerge = required} =
  getAny required

foldEGraphProgram ::
  Monad m =>
  ( forall opFrom opTo value.
    EGraphProgramOp programError node opFrom opTo value ->
    m value
  ) ->
  EGraphProgram programError node from to resultValue ->
  m resultValue
foldEGraphProgram interpret program =
  case program of
    EGraphProgramPure resultValue ->
      pure resultValue
    EGraphProgramBind firstProgram continueProgram ->
      foldEGraphProgram interpret firstProgram
        >>= foldEGraphProgram interpret . continueProgram
    EGraphProgramInstruction instruction ->
      interpret instruction

abortProgram ::
  programError ->
  EGraphProgram programError node from to resultValue
abortProgram =
  EGraphProgramInstruction . AbortProgram

canonicalizeClass ::
  ClassId ->
  EGraphProgram programError node 'ConstructionPhase 'ConstructionPhase ClassId
canonicalizeClass =
  EGraphProgramInstruction . CanonicalizeClass

canonicalizeClasses ::
  Traversable t =>
  t ClassId ->
  EGraphProgram programError node 'ConstructionPhase 'ConstructionPhase (t ClassId)
canonicalizeClasses =
  traverse canonicalizeClass

addNode ::
  node ->
  EGraphProgram programError node 'ConstructionPhase 'ConstructionPhase ClassId
addNode =
  EGraphProgramInstruction . AddNode

addCanonicalNode ::
  Traversable f =>
  f ClassId ->
  EGraphProgram programError (f ClassId) 'ConstructionPhase 'ConstructionPhase ClassId
addCanonicalNode node = do
  canonicalNode <- canonicalizeClasses node
  addNode canonicalNode >>= canonicalizeClass

mergeClasses ::
  ClassId ->
  ClassId ->
  EGraphProgram programError node phase 'MergePhase ClassId
mergeClasses leftClassId rightClassId =
  EGraphProgramInstruction (MergeClasses leftClassId rightClassId)

mergeCanonicalClasses ::
  ClassId ->
  ClassId ->
  EGraphProgram programError node 'ConstructionPhase 'MergePhase ClassId
mergeCanonicalClasses leftClassId rightClassId =
  bindEGraphProgram
    (canonicalizeClass leftClassId)
    ( \canonicalLeftClassId ->
        bindEGraphProgram
          (canonicalizeClass rightClassId)
          (mergeClasses canonicalLeftClassId)
    )
